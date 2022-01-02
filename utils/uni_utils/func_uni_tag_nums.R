uni_tag_nums <- function(data, 
                         num_cols, 
                         tag_col, 
                         cluster_col, 
                         num_adjust_col=NULL, 
                         method="logit_rcs", 
                         pct=TRUE,
                         y_map_func=c("fold_risk", "probability", "log_odds")[1],
                         y_map_max=3
){
  # tag (1 default primary outcome) ~ num (pctl)
  # define logit reversing function
  prob2logit <- function(prob){
    logit <- log(prob/(1-prob))
    return(logit)
  }
  base_mean <- mean(data[,tag_col],na.rm=TRUE)
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
  
  df_result_all = data.frame()
  for(num_col in num_cols){
    df_mdl <- data[,c(num_cols,tag_col,cluster_col,num_adjust_col)]
    try({
      if(attr(data[,num_col],"unique")){
        df_mdl <- dplyr::distinct(df_mdl)
      }
    },TRUE)
    if(pct){
      df_mdl[,num_col] <- est_pctl(df_mdl[,num_col])
    }else{
      # num_col might be estimated percentile already
      df_mdl[,num_col] <- (df_mdl[,num_col]-min(df_mdl[,num_col],na.rm=TRUE))/(max(df_mdl[,num_col],na.rm=TRUE)-min(df_mdl[,num_col],na.rm=TRUE))
      df_mdl[which( df_mdl[,num_col]<0 | df_mdl[,num_col]>1),num_col]<- NA
    }
    
    df_mdl <- df_mdl[complete.cases(df_mdl[,tag_col]),]
    stopifnot(all(unique(as.numeric(as.character( df_mdl[,tag_col])))%in% c(0,1))) #assert tag y is 01 binary
    afford_dof <- n_distinct(df_mdl[which(df_mdl[,tag_col]==1), cluster_col])/15
    dof_list <- c(3,4,5,6)
    dof <- max(dof_list[which(dof_list<=afford_dof)],na.rm=TRUE)
    
    if (method=="bootstrap"){
      df_result <- uni_tag_num_bootstrap(df_mdl, num_col, tag_col)
    }else if(method=="loess"){
      df_result <- uni_tag_num_loess(df_mdl, num_col, tag_col)
    }else if (method=="logit_rcs"){
      df_result <- uni_tag_num_rcs(df_mdl, num_col, tag_col, cluster_col, dof, num_adjust_col)
    }
    if (!is.null(df_result)){
      df_result$var_name <- num_col
      df_result_all <- bind_rows(df_result_all, df_result)
    }
  }
  df_result_all$yhat <- ymap(df_result_all$prob)
  
  return(df_result_all)
}


uni_tag_num_rcs <- function(df_mdl,num_col,tag_col, cluster_col, dof=3, num_adjust_col=NULL){
  df_mdl$num_col <- df_mdl[, num_col]
  df_mdl$tag_col <- df_mdl[, tag_col]
  df_result <- NULL
  if (is.null(num_adjust_col)){
    try({
      fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,")"))
      dd <- rms::datadist(df_mdl)
      base::options(datadist=dd, na.action=na.omit)
      mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[,cluster_col])
      df_fit <- rms::Predict(mdl,num_col, fun=plogis)
      df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
      df_result$pctl <- round(df_result$pctl,2)
      
    },TRUE )
  }else{
    df_result <- NULL
    try({
      df_mdl$num_adjust_col <- df_mdl[, num_adjust_col]
      fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,")*num_adjust_col")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
      dd <- rms::datadist(df_mdl)
      base::options(datadist=dd, na.action=na.omit)
      mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[,cluster_col])
      df_fit <- rms::Predict(mdl,num_col, fun=plogis)
      df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
      df_result$pctl <- round(df_result$pctl,2)
      
    },TRUE )
    if(is.null(df_result)){
      try({
        df_mdl$num_adjust_col <- df_mdl[, num_adjust_col]
        fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,3)*num_adjust_col")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[,cluster_col])
        df_fit <- rms::Predict(mdl,num_col, fun=plogis)
        df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
        df_result$pctl <- round(df_result$pctl,2)
      },TRUE )
    }
    if(is.null(df_result)){
      try({
        df_mdl$num_adjust_col <- df_mdl[, num_adjust_col]
        fml <- formula(paste0("as.factor(tag_col) ~ rcs(num_col,3) + I(num_adjust_col)")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[,cluster_col])
        df_fit <- rms::Predict(mdl,num_col, fun=plogis)
        df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
        df_result$pctl <- round(df_result$pctl,2)
      },TRUE )
    }
    if(is.null(df_result)){
      try({
        df_mdl$num_adjust_col <- df_mdl[, num_adjust_col]
        fml <- formula(paste0("as.factor(tag_col) ~ I(num_col) + I(num_adjust_col)")) #formula(paste0("as.factor(tag_col) ~ rcs(num_col,",dof,") + num_adjust_col"))
        dd <- rms::datadist(df_mdl)
        base::options(datadist=dd, na.action=na.omit)
        mdl <- rms::robcov(rms::lrm(fml, x=TRUE, y=TRUE, data=df_mdl), cluster=df_mdl[,cluster_col])
        df_fit <- rms::Predict(mdl,num_col, fun=plogis)
        df_result <- data.frame(pctl=df_fit$num_col, prob=df_fit$yhat)
        df_result$pctl <- round(df_result$pctl,2)
      },TRUE )
    }
    
  }
  
  return(df_result)
}


uni_tag_num_loess <- function(df_mdl, num_col, tag_col){
  
  df_result <- NULL
  df_mdl$num_col <- df_mdl[, num_col]
  df_mdl$tag_col <- df_mdl[, tag_col]
  tryCatch({
    df_mdl$num_output <- df_mdl$tag_col
    fml <- formula("num_output ~ num_col")
    mdl <- loess(fml,data=df_mdl)
    newx <- c(seq(0,100,1)/100)[2:101]
    yhat <- predict(mdl,newx)
    df_result <- data.frame(pctl=newx, prob=yhat)
    df_result$pctl <- round(df_result$pctl,2)
  },error=function(e){message(paste0("skip ",num_col, "for ", tag_col, " because: ", e))})
  
  
  
  return(df_result)
}

uni_tag_num_bootstrap <- function(df_mdl, num_col, tag_col, ncut=50){
  df_mdl$num_col <- df_mdl[, num_col]
  df_mdl$tag_col <- df_mdl[, tag_col]
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
  return(df_result)
}
