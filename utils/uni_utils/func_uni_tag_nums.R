uni_tag_nums <- function(data, 
                         num_cols, 
                         tag_col, 
                         cluster_col, 
                         num_adjust_col=NULL, 
                         method=c("logit_rcs", "loess", "mean", "bootstrap")[1], 
                         pct=TRUE,
                         y_map_func=c("fold_risk", "probability", "log_odds")[1],
                         y_map_max=3
){
  library(dplyr)
  # tag (1 default primary outcome) ~ num (pctl)
  # define logit reversing function
  prob2logit <- function(prob){
    logit <- log(prob/(1-prob))
    return(logit)
  }
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
    if(pct){
      df_mdl[[num_col]] <- est_pctl(df_mdl[[num_col]])
    }else{
      # num_col might be estimated percentile already
      # df_mdl[[num_col]] <- (df_mdl[[num_col]]-min(df_mdl[[num_col]],na.rm=TRUE))/(max(df_mdl[[num_col]],na.rm=TRUE)-min(df_mdl[[num_col]],na.rm=TRUE))
      # df_mdl[which( df_mdl[[num_col]]<0 | df_mdl[[num_col]]>1),num_col]<- NA
      print("-- using raw scale --")
      df_mdl[[num_col]] <- as.numeric(df_mdl[[num_col]])
    }
    
    df_mdl <- df_mdl[complete.cases(df_mdl[[tag_col]]),]
    stopifnot(all(unique(as.numeric(as.character( df_mdl[[tag_col]])))%in% c(0,1))) #assert tag y is 01 binary
    afford_dof <- n_distinct(df_mdl[which(df_mdl[[tag_col]]==1), cluster_col])/15
    print(paste0("---- affordable degree of freedom ---- ", afford_dof))
    # initiate dof
    dof_list <- c(3,4,5,6)
    dof <- max(3, max(dof_list[which(dof_list<=afford_dof)],na.rm=TRUE)) # at least 3 knots
    print(paste0("---- initiate degree of freedom ", dof, " ---- "))
    
    # calculate prediction matrix
    df_result <- NULL
    tryCatch({
      if (method=="bootstrap"){
        df_result <- uni_tag_num_bootstrap(df_mdl, num_col, tag_col)
      }else if(method=="loess"){
        df_result <- uni_tag_num_loess(df_mdl, num_col, tag_col)
      }else if (method=="logit_rcs"){
        df_result <- uni_tag_num_rcs(df_mdl, num_col, tag_col, cluster_col, dof, num_adjust_col)
      }else if(method=="mean"){
        df_result <- uni_tag_num_mean(df_mdl, num_col, tag_col)
      }
    }, error=function(e){
      print(e)
    })
    
    if (!is.null(df_result)){
      print(paste0("---- final dof used: ",dof," ----"))
      df_result$var_name <- num_col
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
    df_result_all$yhat <- ymap(df_result_all$prob)
  }else{
    print("uni_tag_nums returns NULL")
  }
  
  return(df_result_all)
}


uni_tag_num_rcs <- function(df_mdl, num_col, tag_col, cluster_col, dof=6, num_adjust_col=NULL){
  df_mdl$num_col <- df_mdl[[num_col]]
  df_mdl$tag_col <- df_mdl[[tag_col]]
  df_mdl <- df_mdl[,c("num_col", "tag_col", cluster_col, num_adjust_col)] # only keep columns that will be used later
  df_result <- NULL
  while((is.null(df_result))&(dof>=3)){
    if (length(num_adjust_col)==0){
      tryCatch({
        fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,")"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
        df_fit <- rms::Predict(mdl,num_col, fun=plogis)
        df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
        df_result$pctl <- round(df_result$pctl,2)
        df_result$c_score <- mdl$stats[["C"]]
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
        df_fit <- rms::Predict(mdl,num_col, fun=plogis)
        df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
        df_result$pctl <- round(df_result$pctl,2)
        df_result$c_score <- mdl$stats[["C"]]
      },error=function(e){
        print(e)
      })
      if(is.null(df_result)){
        tryCatch({
          df_mdl$num_adjust_col <- df_mdl[[num_adjust_col]]
          fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + I(num_adjust_col)")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
          dd <- rms::datadist(df_mdl)
          base::options(datadist=dd, na.action=na.omit)
          mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
          df_fit <- rms::Predict(mdl,num_col, fun=plogis)
          df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
          df_result$pctl <- round(df_result$pctl,2)
          df_result$c_score <- mdl$stats[["C"]]
        },error=function(e){
          print(e)
        } )
      }
    }
    dof <- dof - 1
  }
  # if none of >=3 knots work or given dof is < 3, use linear term
  if(is.null(df_result)){
    if (length(num_adjust_col)==0){
      tryCatch({
        fml <- formula(paste0("as.factor(tag_col) ~ I(num_col)"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
        df_fit <- rms::Predict(mdl, num_col, fun=plogis)
        df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
        df_result$pctl <- round(df_result$pctl,2)
        df_result$c_score <- mdl$stats[["C"]]
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
        df_fit <- rms::Predict(mdl,num_col, fun=plogis)
        df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
        df_result$pctl <- round(df_result$pctl,2)
        df_result$c_score <- mdl$stats[["C"]]
      },error=function(e){
        print(e)
      } )
      if(is.null(df_result)){
        tryCatch({
          df_mdl$num_adjust_col <- df_mdl[[num_adjust_col]]
          fml <- formula(paste0("as.factor(tag_col) ~ I(num_col) + I(num_adjust_col)")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
          dd <- rms::datadist(df_mdl)
          base::options(datadist=dd, na.action=na.omit)
          mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[[cluster_col]])
          df_fit <- rms::Predict(mdl,num_col, fun=plogis)
          df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
          df_result$pctl <- round(df_result$pctl,2)
          df_result$c_score <- mdl$stats[["C"]]
        },error=function(e){
          print(e)
        } )
      }
    }
  }
  if(is.null(df_result)) print(paste0("uni_tag_num_rcs failed on ", num_col))
  # only keep 2 decimal places
  df_result <- df_result %>% group_by(pctl) %>% summarise(prob=mean(prob,na.rm=TRUE)) %>% as.data.frame()
  # check boundaries 0 and 1
  if(min(df_result$pctl)>0){
    ld <- data.frame(pctl=seq(0,min(df_result$pctl),0.01), prob=rep(df_result$prob[which(df_result$pctl==min(df_result$pctl))],length(seq(0,min(df_result$pctl),0.01))))
    df_result <- bind_rows(ld,df_result)
  }
  if(max(df_result$pctl)<1){
    ud <- data.frame(pctl=seq(min(1,max(df_result$pctl)+0.01),1,0.01), prob=rep(df_result$prob[which(df_result$pctl==max(df_result$pctl))],length(seq(min(1,max(df_result$pctl)+0.01),1,0.01))))
    df_result <- bind_rows(df_result,ud)
  }
  df_result<-df_result[!duplicated(df_result),]
  stopifnot(length(df_result$pctl)==101) # from 0th to 100th
  return(df_result)
}


uni_tag_num_loess <- function(df_mdl, num_col, tag_col){
  
  df_result <- NULL
  df_mdl$num_col <- est_pctl(df_mdl[[num_col]])
  df_mdl$tag_col <- df_mdl[[tag_col]]
  tryCatch({
    fml <- formula("tag_col ~ num_col")
    mdl <- loess(fml, data=df_mdl, span=0.25)
    newx <- c(seq(0,100,1)/100)[2:101]
    yhat <- predict(mdl,newx)
    df_result <- data.frame(pctl=newx, prob=yhat)
    df_result$pctl <- round(df_result$pctl,2)
    df_result$c_score <- NA
    tryCatch({
      y_prob <- df_result$prob
      y_true <- df_mdl$tag_col
      res_df <- mdl_test(y_true, y_prob)
      df_result$c_score <- res_df$AUROC
    },error=function(e){
      print(e)
    })
    
  },error=function(e){message(paste0("skip ",num_col, "for ", tag_col, " because: ", e))})
  
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
    prob <- NA
    out_lst <- df_mdl[which(df_mdl$num_col>=ls[i] & df_mdl$num_col<us[i]),'num_output']
    if (length(out_lst)>0){
      prob <- mean(sample(out_lst,100,replace=TRUE))
    }
    prob_vec <- c(prob_vec,prob)
  }
  df_result <- data.frame(pctl=us, prob=prob_vec)
  df_result$pctl <- round(df_result$pctl,2)
  
  # expand it to be 100 bins
  inter <- approx(df_result$pctl, df_result$prob, method="constant", n=100)
  df_result <- data.frame(pctl=round(inter$x,2),prob=inter$y)
  
  return(df_result)
}




uni_tag_num_mean <- function(df_mdl, num_col, tag_col, frac_avail = 0.7){
  
  # tuning the binwidth for percentile
  # if percentile surpass the missingness threshold (0.8)
  est_pctl_vec <- est_pctl(df_mdl[[num_col]])
  print("---- tuning resolution / n_digit ----")
  n_digits <- c()
  for(n_digit in c(4,3,2,1)){
    est_pctl_vec_bin_values <- unique(round(est_pctl_vec,n_digit))
    est_pctl_vec_bin_values <- est_pctl_vec_bin_values[which(!is.na(est_pctl_vec_bin_values))]
    binwidth <- 1/(10^n_digit)
    n_bins <- length(est_pctl_vec_bin_values)
    frac_bins <- round(length(est_pctl_vec_bin_values)/(10^n_digit+1),3)  
    print(paste0("binwidth=",binwidth, "; n_bins=",n_bins, "; frac_bins=",frac_bins) )
    if(frac_bins >= frac_avail) n_digits <- c(n_digits, n_digit)
  }
  
  n_digit_opt <- n_digits[1]
  df_mdl$num_col <- round(est_pctl_vec, n_digit_opt)
  df_mdl$tag_col <- df_mdl[[tag_col]]
  df_result_raw <- df_mdl %>% group_by(num_col) %>% summarise(prob_raw = mean(tag_col,na.rm=TRUE)) %>% as.data.frame()
  colnames(df_result_raw) <- c("pctl","prob")
  # interpolate to fill gaps in 
  inter <- approx(df_result_raw$pctl, df_result_raw$prob, method="linear", n=10^n_digit_opt)
  df_result <- data.frame(pctl=round(inter$x,n_digit_opt),prob=inter$y)
  # expand it to be 100 bins
  inter <- approx(df_result$pctl, df_result$prob, method="constant", n=100)
  df_result <- data.frame(pctl=round(inter$x,2),prob=inter$y)
  
  return(df_result)
  
}

