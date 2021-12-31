do_ols_cv <- function(df=df, 
                      dict_df=dict_df, 
                      na_frac_max=1, 
                      external_df=NULL, 
                      cv_nfold=5, 
                      tune_by="logloss",  
                      stratified_cv=TRUE){
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
  dict_mdl <- dict_mdl %>% filter(na_frac<=na_frac_max) %>% as.data.frame() # filter na missingness by max threshold
  
  rcs5_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="rcs5"),])
  rcs4_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="rcs4"),])
  rcs3_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="rcs3"),])
  linear_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="linear"),])
  fct_x_cols <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="factor"),])
  y_col <- rownames(dict_df[which(dict_df$mdl_term_init=="y"),]) # use dict_df for y
  cluster_col <- rownames(dict_mdl[which(dict_mdl$mdl_term_init=="cluster"),])
  
  # fine tune formula string object
  fml_obj <- ols_formula(df = df,
                        rcs5_x_cols = rcs5_x_cols,
                        rcs4_x_cols = rcs4_x_cols,
                        rcs3_x_cols = rcs3_x_cols,
                        linear_x_cols = linear_x_cols,
                        fct_x_cols = fct_x_cols,
                        y_col = y_col,
                        cluster_col = cluster_col)
  
  
  # cross-validation
  cv_obj <- NULL
  cv_obj <- ols_cv(df = df, 
                   external_df = external_df, 
                   fml = fml_obj$fml_string, 
                   cluster_col = cluster_col, 
                   nfold  = cv_nfold)
    
  
    
  return(list("fml_obj"=fml_obj,
              "cv_obj"=cv_obj))
  
}


