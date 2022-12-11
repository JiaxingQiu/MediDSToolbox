do_lrm_cv <- function(df=df, 
                      dict_df=dict_df, 
                      na_frac_max=0.3, 
                      external_df=NULL, 
                      cv_nfold=5, 
                      tune_by=c("logloss","auroc","aic","bic")[1],  
                      lambda_value = NULL,
                      stratified_cv=TRUE, 
                      samepen_patience=10,
                      fold_idx_df_ex=NULL){
  # ---- Usage ----
  # dictionary oriented (do) cross validation logistic regression machine learning pipeline
  
  # ---- Arguments ----
  # df: a dataframe object with essential dictionary information as attributes on each column
  # dict_df: dictionary for corresponding data frame input
  # na_frac_max: maximum allowed na/missingness fraction for each variable
  # valid_df: external validation dataframe, if given cv will use the whole df as the training set
  # cv_nfold: N folds cross validation intager
  # stratified_cv:  whether or not to sample subjects stratified by outcome
  # samepen_patience: same-penalty-step-size patience
  
  # ---- Value ----
  # mdl_report_df: model report object
  # calibration_plot: calibration curve
  
  
  # prepare dictionary for model term/roles
  dict_mdl <- dict_df
  dict_mdl <- dict_mdl%>%filter(na_frac<=na_frac_max) %>% as.data.frame() # filter na missingness by max threshold
  
  rcs5_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="rcs5"),])
  rcs4_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="rcs4"),])
  rcs3_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="rcs3"),])
  linear_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="linear"),])
  fct_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="factor"),])
  tag_y_col <- rownames(dict_df[which(dict_df$mdl_term_init=="y"),])
  cluster_col <- rownames(dict_df[which(dict_df$mdl_term_init=="cluster"),])
  
  # fine tune formula string object
  fml_obj <- lrm_formula(data = df,
                        rcs5_x_cols = rcs5_x_cols,
                        rcs4_x_cols = rcs4_x_cols,
                        rcs3_x_cols = rcs3_x_cols,
                        linear_x_cols = linear_x_cols,
                        fct_x_cols = fct_x_cols,
                        tag_y_col = tag_y_col,
                        cluster_col = cluster_col)
  
  
  
  score_trace_all <- data.frame() # monitor the trace of all kind of evaluation matrices while tuning the penalty
  if(length(lambda_value)>0){
    # manually tuning penalty by list
    print(paste0("Warning -- using penalty list [", paste0(lambda_value, collapse = ", "), "]"))
    penalty_list <- c(lambda_value)
    for(penalty in penalty_list){
      cv_obj <- NULL
      try({
        print(paste0("--- ",fml_obj$fml_string," ---"))
        cv_obj <- lrm_cv(df = df, 
                         external_df = external_df, 
                         fml = fml_obj$fml_string, 
                         penalty = penalty, 
                         cluster_col = cluster_col, 
                         nfold  = cv_nfold, 
                         stratified = stratified_cv,
                         fold_idx_df_ex = fold_idx_df_ex)
      },TRUE)
      if(!is.null(cv_obj)){
        # get current score
        current_score <- NULL
        res_score_df <- mdl_test(y_true=cv_obj$cv_yhat_df$y_true, y_prob=cv_obj$cv_yhat_df$y_prob, mean(cv_obj$cv_yhat_df$y_true, na.rm=TRUE))
        res_score_df <- res_score_df$res_df
        mdl_all <- cv_obj$mdl_all
        if(tune_by=="logloss"){
          tryCatch({
            current_score <- -res_score_df$logloss
          },error = function(e){
            print(e)
          })
          if(is.null(current_score)){
            print("using 10 fold estimation")
            current_score <- -cv_obj$model_scores$valid_logloss_mean
          }
        } else if (tune_by=="auroc"){
          tryCatch({
            current_score <- res_score_df$AUROC
          },error = function(e){
            print(e)
          })
          if(is.null(current_score)){
            print("using 10 fold estimation")
            current_score <- cv_obj$model_scores$valid_AUROC_mean
          }
        } else if (tune_by=="aic"){
          current_score <- -AIC(mdl_all)
          if(is.null(current_score)){
            print("using 10 fold estimation")
            current_score <- -cv_obj$model_scores$model_AIC_mean
          }
        } else if (tune_by=="bic"){
          current_score <- -BIC(mdl_all)
          if(is.null(current_score)){
            print("using 10 fold estimation")
            current_score <- -cv_obj$model_scores$model_BIC_mean
          }
        }
        print(paste0("--- penalty --- ", penalty, " --- ",tune_by," --- ", current_score))
      }else{
        print(paste0("--- penalty --- ", penalty, " --- ",tune_by, " --- failed"))
      }
      score_trace <- res_score_df[,c("logloss","AUROC","AUPRC","accuracy","f1score")]
      score_trace$penalty <- penalty
      score_trace$AIC <- AIC(mdl_all)
      score_trace$BIC <- BIC(mdl_all)
      score_trace$tune_by <- tune_by
      score_trace_all <- bind_rows(score_trace_all, score_trace)
    }
  }else{
    # automatically tune penalty 
    failed <- 0 # count of failed model penalty
    base_score <- NULL
    penalty <- 0 # initiate penalty
    penalty_max <- ifelse (fml_obj$use_dof <= fml_obj$afford_dof, 0, 1e06) # find max penalty based on dof report
    pen_step <- 5 # initial penalty step size
    same_pen_step_count <- 0  # patience of how many same-step-size penalty has been tuned
    
    while (penalty <= penalty_max){
      cv_obj <- NULL
      try({
        print(paste0("--- ",fml_obj$fml_string," ---"))
        cv_obj <- lrm_cv(df = df, 
                         external_df = external_df, 
                         fml = fml_obj$fml_string, 
                         penalty = penalty, 
                         cluster_col = cluster_col, 
                         nfold  = cv_nfold, 
                         stratified = stratified_cv,
                         fold_idx_df_ex = fold_idx_df_ex)
      },TRUE)
      
      if (is.null(cv_obj) & failed < 3)  {# try cv for at most 3 times if current penalty tuning failed
        print(paste0("--- penalty --- ", penalty, " --- ",tune_by, " --- failed"))
        failed = failed + 1
      } else{
        # get current score
        current_score <- NULL
        res_score_df <- mdl_test(y_true=cv_obj$cv_yhat_df$y_true, y_prob=cv_obj$cv_yhat_df$y_prob, mean(cv_obj$cv_yhat_df$y_true, na.rm=TRUE))
        res_score_df <- res_score_df$res_df
        mdl_all <- cv_obj$mdl_all
        if(tune_by=="logloss"){
          tryCatch({
            current_score <- -res_score_df$logloss
          },error = function(e){
            print(e)
          })
          if(is.null(current_score)){
            print("using 10 fold estimation")
            current_score <- -cv_obj$model_scores$valid_logloss_mean
          }
        } else if (tune_by=="auroc"){
          tryCatch({
            current_score <- res_score_df$AUROC
          },error = function(e){
            print(e)
          })
          if(is.null(current_score)){
            print("using 10 fold estimation")
            current_score <- cv_obj$model_scores$valid_AUROC_mean
          }
        } else if (tune_by=="aic"){
          current_score <- -AIC(mdl_all)
          if(is.null(current_score)){
            print("using 10 fold estimation")
            current_score <- -cv_obj$model_scores$model_AIC_mean
          }
        } else if (tune_by=="bic"){
          current_score <- -BIC(mdl_all)
          if(is.null(current_score)){
            print("using 10 fold estimation")
            current_score <- -cv_obj$model_scores$model_BIC_mean
          }
        }
        print(paste0("--- penalty --- ", penalty, " --- ",tune_by," --- ", current_score))
        if(is.null(base_score)){
          base_score <- current_score
        } else {
          if (current_score > base_score) {
            base_score <- current_score
            same_pen_step_count  <- same_pen_step_count + 1 # count how many successfull tuning has been done using the same pen_step
            if (same_pen_step_count >= samepen_patience) {
              pen_step <- pen_step*10 # patience of 5 and scale up penalty step size
              same_pen_step_count <- 0 # reset same_pen_step_count
            }
          } else { # score no longer increase, fine tune complete
            break
          }
        }
      }
      penalty <- penalty + pen_step # update penalty
      
      score_trace <- res_score_df[,c("logloss","AUROC","AUPRC","accuracy","f1score")]
      score_trace$penalty <- penalty
      score_trace$AIC <- AIC(mdl_all)
      score_trace$BIC <- BIC(mdl_all)
      score_trace$tune_by <- tune_by
      score_trace_all <- bind_rows(score_trace_all, score_trace)
      
    }
  }
  
  
  
  # finalize the optimized model
  fml <- cv_obj$model_info$formula
  cluster_col <- cv_obj$model_info$cluster_col
  penalty <- cv_obj$model_info$penalty
  dd <- datadist(df)
  options(datadist=dd, na.action=na.omit)
  mdl_final <- NULL
  # allow a gitter in penalty for final model object
  step_size = 0.5
  count = 0
  while(is.null(mdl_final)){
    print(paste0("--- final penalty ",penalty," ---"))
    print(paste0("--- final formula ",fml," ---"))
    try({
      mdl_final <- rms::robcov(rms::lrm(as.formula(fml),x=TRUE, y=TRUE, data=df, penalty=penalty),cluster=df[,cluster_col])
    },TRUE)
    penalty <- penalty + step_size
    count <- count + 1
    if(count == 10) step_size <- step_size * 3
    stopifnot(count <= 20) 
  }
  
 
  return(list("df_final" = df,
              "dict_final" = dict_mdl,
              "fml_obj"=fml_obj,
              "cv_obj"=cv_obj,
              "mdl_obj"=mdl_final,
              "score_trace_df"=score_trace_all
  ))
  
}


