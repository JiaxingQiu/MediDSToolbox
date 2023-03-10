# define logit function
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
# define inv-logit function
prob2logit <- function(prob){
  logit <- log(prob/(1-prob))
  return(logit)
}



uni_tag_nums <- function(data, 
                         num_cols, 
                         tag_col, 
                         cluster_col, 
                         num_adjust_col=NULL, 
                         method=c("logit_rcs", "loess", "mean", "bootstrap")[1], 
                         y_map_func=c("fold_risk", "probability", "log_odds")[1],
                         y_map_max=3,
                         new_dd=NULL  # new datadist to be predicted on if given
){
  library(dplyr)
  # tag (1 default primary outcome) ~ num (pctl)
  base_mean <- base::mean(as.numeric( as.character( data[[tag_col]] ) ),na.rm=TRUE)
  print(paste0("--- baseline responce mean --- ", base_mean))
  if(y_map_func == "log_odds"){
    ymap <- function(y_prob){
      y_logodds <- ifelse(prob2logit(y_prob) > y_map_max, y_map_max, prob2logit(y_prob))
      return(y_logodds)
    }
  }else if(y_map_func == "fold_risk"){
    ymap <- function(y_prob){
      y_fold_risk <- ifelse(y_prob/base_mean > y_map_max, y_map_max, y_prob/base_mean)
      return(y_fold_risk)
    }
  }else{
    ymap <- function(y_prob){
      y_prob <- ifelse(y_prob > y_map_max, y_map_max, y_prob)
      return(y_prob)
    }
  }
  
  df_result_all <- NULL
  for(num_col in num_cols){
    print(paste0("--- working on ",num_col," ---"))
    df_mdl <- data[,intersect(colnames(data), unique(c(num_cols,tag_col,cluster_col,num_adjust_col))) ]
    tryCatch({
      if(attr(data[[num_col]],"unique")){
        df_mdl <- dplyr::distinct(df_mdl)
      }
    },error=function(e){print(e)})
    tryCatch({
      if(attr(data[[num_col]],"unique_per_sbj")){
        df_mdl <- dplyr::distinct(df_mdl)
      }
    },error=function(e){print(e)})
    # always use raw scale of num_col to train the regression model, if user want to train on percentile scale, convert outside this function
    df_mdl[[num_col]] <- as.numeric(df_mdl[[num_col]])
    df_mdl <- df_mdl[complete.cases(df_mdl[[tag_col]]),]
    stopifnot(all(unique(as.numeric(as.character( df_mdl[[tag_col]])))%in% c(0,1))) #assert tag y is 01 binary
    afford_dof <- n_distinct(df_mdl[which(df_mdl[[tag_col]]==1), cluster_col])/15
    print(paste0("---- affordable degree of freedom ---- ", afford_dof))
    # initiate dof
    dof_list <- c(3,4,5,6)
    dof <- max(3, max(dof_list[which(dof_list<=afford_dof)],na.rm=TRUE)) # at least 3 knots
    print(paste0("---- initiate degree of freedom ", dof, " ---- "))
    
    # get the new_x vector from new_dd dataframe
    new_x <- NULL
    if(!is.null(new_dd)){
      tryCatch({
        new_x <- new_dd[[num_col]]
      },error=function(e){
        print(e)
      })
    }
    
    # calculate prediction data frame, always make predictions in scale of percentiles
    df_result <- NULL
    tryCatch({
      if (method=="bootstrap"){
        #df_result <- uni_tag_num_bootstrap(df_mdl, num_col, tag_col)
      }else if(method=="loess"){
        df_result <- uni_tag_num_loess(df_mdl, num_col, tag_col, new_x=new_x)
      }else if (method=="logit_rcs"){
        df_result <- uni_tag_num_rcs(df_mdl, num_col, tag_col, cluster_col, dof, num_adjust_col, new_x=new_x)
        df_result$y_prob <- logit2prob(df_result$y_logodds)
      }else if(method=="mean"){
        df_result <- uni_tag_num_mean(df_mdl, num_col, tag_col)
      }
    }, error=function(e){
      print(e)
    })
    
    if (!is.null(df_result)){
      print(paste0("---- final dof used: ",dof," ----"))
      if(is.null(df_result_all)) {
        df_result_all <- df_result
      }else{
        df_result_all <- bind_rows(df_result_all, df_result)
      }
    }else{
      print(paste0("uni_tag_num failed on ", num_col))
    }
  }
  
  # final returned result dataframe or NULL object
  if(!is.null(df_result_all)){
    df_result_all$y_hat <- ymap(df_result_all$y_prob)
    if("y_logodds_upper"%in%colnames(df_result_all) & "y_logodds_lower"%in%colnames(df_result_all)){
      df_result_all$y_hat_upper <- ymap( logit2prob(df_result_all$y_logodds_upper) )
      df_result_all$y_hat_lower <- ymap( logit2prob(df_result_all$y_logodds_lower) )
    }
    if("y_logodds_baseline"%in%colnames(df_result_all)){
      df_result_all$y_hat_baseline <- ymap( logit2prob(df_result_all$y_logodds_baseline) )
      df_result_all$y_hat_signif <- ifelse(df_result_all$y_hat_lower<=df_result_all$y_hat_baseline&df_result_all$y_hat_upper>=df_result_all$y_hat_baseline, 0, 1)
    }
    # rearrange column orders
    main_cols <- intersect(c("x_name", "x_pctl", "x_raw", "y_hat", "y_prob", "y_logodds", "c_score"),colnames(df_result_all))
    df_result_all <- df_result_all[,c(main_cols,setdiff(colnames(df_result_all),main_cols))]
  }
  return(df_result_all)
}


uni_tag_num_rcs <- function(df_mdl, num_col, tag_col, cluster_col, dof=6, num_adjust_col=NULL, new_x=NULL){
  df_mdl$num_col <- df_mdl[[num_col]]
  df_mdl$tag_col <- df_mdl[[tag_col]]
  df_mdl <- df_mdl[,c("num_col", "tag_col", cluster_col, num_adjust_col)] # only keep columns that will be used later
  mdl <- NULL # model object to monitor
  df_result <- NULL
  while((is.null(mdl))&(dof>=3)){
    if (length(num_adjust_col)==0){
      tryCatch({
        fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,")"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
      },error=function(e){
        print(e)
      })
    }else{
      tryCatch({
        df_mdl$num_adjust_col <- df_mdl[[num_adjust_col]]
        fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,")*num_adjust_col")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
        
      },error=function(e){
        print(e)
      })
      if(is.null(mdl)){
        tryCatch({
          df_mdl$num_adjust_col <- df_mdl[[num_adjust_col]]
          fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + I(num_adjust_col)")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
          dd <- rms::datadist(df_mdl)
          base::options(datadist=dd, na.action=na.omit)
          mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
          
        },error=function(e){
          print(e)
        } )
      }
    }
    dof <- dof - 1
  }
  # if none of >=3 knots work or given dof is < 3, use linear term
  if(is.null(mdl)){
    if (length(num_adjust_col)==0){
      tryCatch({
        fml <- formula(paste0("as.factor(tag_col) ~ I(num_col)"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
        
      },error=function(e){
        print(e)
      } )
    }else{
      tryCatch({
        df_mdl$num_adjust_col <- df_mdl[[num_adjust_col]]
        fml <- formula(paste0("as.factor(tag_col) ~ I(num_col)*I(num_adjust_col)")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
        
      },error=function(e){
        print(e)
      } )
      if(is.null(mdl)){
        tryCatch({
          df_mdl$num_adjust_col <- df_mdl[[num_adjust_col]]
          fml <- formula(paste0("as.factor(tag_col) ~ I(num_col) + I(num_adjust_col)")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
          dd <- rms::datadist(df_mdl)
          base::options(datadist=dd, na.action=na.omit)
          mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
          
        },error=function(e){
          print(e)
        } )
      }
    }
  }
  if(is.null(mdl)) stop(paste0("Error: uni_tag_num_rcs failed on ", num_col))
  
  # make prediction at each percentile (newdata df_result)
  df_result <- data.frame(num_pctl=seq(0,100,1), num_col=as.numeric(quantile(df_mdl$num_col, probs=seq(0,1,0.01),na.rm=TRUE)))
  # if new_x is provided externally
  if(!is.null(new_x)){
    # in this case, x_pctl will be the index of new_x
    df_result <- data.frame(num_pctl=seq(1,length(new_x),1),num_col=new_x) 
    # keep reasonable scale and boundaries
    df_result <- df_result[which(df_result$num_col>=min(df_mdl$num_col,na.rm=TRUE) &df_result$num_col<=max(df_mdl$num_col,na.rm=TRUE) ),]
  }
  # add y_logodds
  df_result$y_logodds <- rms::predictrms(mdl, newdata = df_result, conf.int = 0.95)$linear.predictors
  df_result$y_logodds_upper <- rms::predictrms(mdl, newdata = df_result, conf.int = 0.95)$upper
  df_result$y_logodds_lower <- rms::predictrms(mdl, newdata = df_result, conf.int = 0.95)$lower
  # add "significancy"
  df_result$y_logodds_baseline <- log(mean(as.numeric(as.character(mdl$y)),na.rm=TRUE)/(1-mean(as.numeric(as.character(mdl$y)),na.rm=TRUE)) )
  df_result$y_logodds_signif <- ifelse(df_result$y_logodds_lower<=df_result$y_logodds_baseline&df_result$y_logodds_upper>=df_result$y_logodds_baseline, 0, 1)
  
  # add c-stat
  df_result$c_score <- mdl$stats[["C"]]
  # add x variable name
  df_result$x_name <- num_col
  # add y_prob
  df_result$y_prob <- logit2prob(df_result$y_logodds)
  # # check dimension
  # stopifnot(dim(df_result)[1]==101) # from 0th to 100th
  # refine column names
  colnames(df_result)[which(colnames(df_result)=="num_pctl")] <- "x_pctl"
  colnames(df_result)[which(colnames(df_result)=="num_col")] <- "x_raw"
  
  # reformat object to return
  if(!is.null(df_result)){
    keep_cols <- c("x_name", "x_raw", "x_pctl", "y_prob", colnames(df_result)[startsWith(colnames(df_result),"y_logodds")], "c_score")
    df_result <- df_result[,keep_cols]
  }
  
  return(df_result)
}


uni_tag_num_loess <- function(df_mdl, num_col, tag_col, new_x=NULL){
  
  df_result <- NULL
  df_mdl$num_col <- as.numeric(df_mdl[[num_col]])
  df_mdl$tag_col <- df_mdl[[tag_col]]

  fml <- formula("tag_col ~ num_col")
  mdl <- loess(fml, data=df_mdl, span=0.25)
  df_result <- data.frame(x_pctl=seq(0,100,1), x_raw=as.numeric(quantile(df_mdl$num_col, probs=seq(0,1,0.01),na.rm=TRUE)))
  
  # if new_x is provided externally
  if(!is.null(new_x)){
    # in this case, x_pctl will be the index of new_x
    df_result <- data.frame(x_pctl=seq(1,length(new_x),1),x_raw=new_x) 
    # keep reasonable scale and boundaries
    df_result <- df_result[which(df_result$x_raw>=min(df_mdl$num_col,na.rm=TRUE) &df_result$x_raw<=max(df_mdl$num_col,na.rm=TRUE) ),]
  }
  y_prob <- predict(mdl,df_result$x_raw)
  y_prob[which(y_prob<=0)] <- 1e-6 # note! loess treat 0,1 outcome as numeric, there might be predicted prob below 0
  df_result$y_prob <- y_prob
  
  df_result$x_name <- num_col
  df_result$y_logodds <- prob2logit(df_result$y_prob)
  df_result$c_score <- NA
  tryCatch({
    df_result$c_score <- mdl_test(y_true=df_mdl$tag_col, y_prob=predict(mdl,df_mdl$num_col) )$res_df$AUROC
  },error=function(e){print(e)})

  # reformat object to return
  if(!is.null(df_result)){
    keep_cols <- c("x_name", "x_raw", "x_pctl", "y_prob", "y_logodds", "c_score")
    df_result <- df_result[,keep_cols]
  }
  
  return(df_result)
}

uni_tag_num_mean <- function(df_mdl, num_col, tag_col, nbin=10){
  
  # find deciles of event group
  df_mdl$tag_col <- as.numeric(as.character(df_mdl[[tag_col]])) # force tag col to be 01
  df_mdl$tag_col[which(df_mdl$tag_col==min(df_mdl$tag_col,na.rm=TRUE))] <- 0
  df_mdl$tag_col[which(df_mdl$tag_col==max(df_mdl$tag_col,na.rm=TRUE))] <- 1
  df_mdl$num_col <- as.numeric(df_mdl[[num_col]])
  if(n_distinct(df_mdl$num_col[which(df_mdl$tag_col==1)])<nbin){
    stop(paste0("less than ",nbin," unique values in the event group!"))
  }
  breaks <- as.numeric(quantile(df_mdl[which(df_mdl$tag_col==1),num_col],seq(0,1,length.out=nbin),na.rm=TRUE))
  df_mdl$x_pctl <- NA
  for(i in 1:(length(breaks)-1) ){
    if(i==1){
      df_mdl$x_pctl[which(df_mdl$num_col<=breaks[i+1])] <- i
    }else if(i==length(breaks)-1){
      df_mdl$x_pctl[which(df_mdl$num_col>=breaks[i])] <- i
    }else{
      df_mdl$x_pctl[which(df_mdl$num_col>=breaks[i] & df_mdl$num_col<=breaks[i+1])] <- i
    }
  }
  df_result <- df_mdl %>% filter(!is.na(x_pctl)) %>% group_by(x_pctl) %>% summarise(x_raw=mean(num_col,na.rm=TRUE),
                                                                                    y_prob=mean(tag_col,na.rm=TRUE)) %>% as.data.frame()
  df_result$x_name <- num_col
  df_result$y_logodds <- prob2logit(df_result$y_prob)
  # reformat object to return
  if(!is.null(df_result)){
    keep_cols <- c("x_name", "x_raw", "x_pctl", "y_prob", "y_logodds")
    df_result <- df_result[,keep_cols]
  }
  
  # # tuning the binwidth for percentile
  # # if percentile surpass the missingness threshold (0.8)
  # est_pctl_vec <- est_pctl(df_mdl[[num_col]])
  # print("---- tuning resolution / n_digit ----")
  # n_digits <- c()
  # for(n_digit in c(4,3,2,1)){
  #   est_pctl_vec_bin_values <- unique(round(est_pctl_vec,n_digit))
  #   est_pctl_vec_bin_values <- est_pctl_vec_bin_values[which(!is.na(est_pctl_vec_bin_values))]
  #   binwidth <- 1/(10^n_digit)
  #   n_bins <- length(est_pctl_vec_bin_values)
  #   frac_bins <- round(length(est_pctl_vec_bin_values)/(10^n_digit+1),3)  
  #   print(paste0("binwidth=",binwidth, "; n_bins=",n_bins, "; frac_bins=",frac_bins) )
  #   if(frac_bins >= frac_avail) n_digits <- c(n_digits, n_digit)
  # }
  # 
  # n_digit_opt <- n_digits[1]
  # df_mdl$num_col <- round(est_pctl_vec, n_digit_opt)
  # df_mdl$tag_col <- df_mdl[[tag_col]]
  # df_result_raw <- df_mdl %>% group_by(num_col) %>% summarise(prob_raw = mean(tag_col,na.rm=TRUE)) %>% as.data.frame()
  # colnames(df_result_raw) <- c("x_pctl","y_prob")
  # # interpolate to fill gaps in 
  # inter <- approx(df_result_raw$x_pctl, df_result_raw$y_prob, method="linear", n=10^n_digit_opt)
  # df_result <- data.frame(x_pctl=round(inter$x,n_digit_opt),y_prob=inter$y)
  # # expand it to be 100 bins
  # inter <- approx(df_result$x_pctl, df_result$y_prob, method="constant", n=100)
  # df_result <- data.frame(x_pctl=round(inter$x,2),y_prob=inter$y)
  
  return(df_result)
  
}


uni_tag_num_bootstrap <- function(df_mdl, num_col, tag_col, ncut=50){
  df_mdl$num_col <- df_mdl[[num_col]]
  df_mdl$tag_col <- df_mdl[[tag_col]]
  df_mdl <- df_mdl[complete.cases(df_mdl),]
  df_mdl$num_output <- df_mdl$tag_col
  cuts <- c(seq(0,ncut,1)/ncut)
  us <- cuts[2:length(cuts)]
  ls <- cuts[1:length(cuts)-1]
  prob_vec <- c()
  for (i in 1:ncut){
    y_prob <- NA
    out_lst <- df_mdl[which(df_mdl$num_col>=ls[i] & df_mdl$num_col<us[i]),'num_output']
    if (length(out_lst)>0){
      y_prob <- mean(sample(out_lst,100,replace=TRUE))
    }
    prob_vec <- c(prob_vec,y_prob)
  }
  df_result <- data.frame(x_pctl=us, y_prob=prob_vec)
  df_result$x_pctl <- round(df_result$x_pctl,2)
  
  # expand it to be 100 bins
  inter <- approx(df_result$x_pctl, df_result$y_prob, method="constant", n=100)
  df_result <- data.frame(x_pctl=round(inter$x,2),y_prob=inter$y)
  
  return(df_result)
}

