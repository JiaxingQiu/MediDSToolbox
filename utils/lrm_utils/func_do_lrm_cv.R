do_lrm_cv <- function(df=df, 
                      dict_df=dict_df, 
                      na_frac_max=0.3, 
                      external_df=NULL, 
                      cv_nfold=5, 
                      tune_by="logloss",  
                      stratified_cv=TRUE, 
                      samepen_patience=5){
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
  
  
  penalty_max <- ifelse (fml_obj$use_dof <= fml_obj$afford_dof, 0, 1e06) # find max penalty based on dof report
  failed <- 0 # count of failed model penalty
  base_score <- NULL
  penalty = 0 # initiate penalty
  pen_step = 10 # initial pentalty step size
  same_pen_step_count = 0  # patience of how many same-step-size penalty has been tuned
  # fine tune penalty 
  while (penalty <= penalty_max){
    cv_obj <- NULL
    try({
      cv_obj <- lrm_cv(df = df, 
                       external_df = external_df, 
                       fml = fml_obj$fml_string, 
                       penalty = penalty, 
                       cluster_col = cluster_col, 
                       nfold  = cv_nfold, 
                       stratified = stratified_cv)
    },TRUE)
    
    if (is.null(cv_obj) & failed < 3)  {# try cv for at most 3 times if current penalty tuning failed
      print(paste0("--- penalty --- ", penalty, " --- ",tune_by, " --- failed"))
      failed = failed + 1
    } else{
      # get current score
      if(tune_by=="logloss"){
        current_score <- -cv_obj$model_scores$valid_logloss_mean
      } else if (tune_by=="auroc"){
        current_score <- cv_obj$model_scores$valid_AUROC_mean
      } else if (tune_by=="aic"){
        current_score <- -cv_obj$model_scores$model_AIC_mean
      } else if (tune_by=="bic"){
        current_score <- -cv_obj$model_scores$model_BIC_mean
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
    print(penalty)
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
              "mdl_obj"=mdl_final
  ))
  
}


