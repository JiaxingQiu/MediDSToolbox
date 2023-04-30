front_X_clus <- function(
  data,
  dict_data,
  x_labels, 
  y_label, 
  cluster_label,
  # --- engineer ---
  trim_by_label=NULL,
  trim_vec = c(-Inf, Inf), # trim relative time [from, to)
  time_unit = 1, # the increment scale of relative time
  pctcut_num_labels = c(),
  pctcut_num_vec = c(0.1, 99.9),
  pctcut_num_coerce=TRUE,
  filter_tag_labels=c(),
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1],
  aggregate_conditioned_on_labels = c(),
  # --- local ---
  r2=0.9,
  rcs5_low="70%",
  rcs4_low="50%",
  r_abs=0.8, 
  type=c("pearson","spearman")[1],
  rank=TRUE,
  trim_ctrl = TRUE
){
  
  # ---- pre-processing ----
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  # --- find corresponding column names ---
  x_cols <- dict_data$varname[which(dict_data$label%in%x_labels)]
  y_col <- dict_data$varname[which(dict_data$label==y_label)]
  num_cols <- intersect(union(x_cols,y_col), dict_data$varname[which(dict_data$type=="num")])#dict_data$mlrole=="input"&
  fct_cols <- setdiff(union(x_cols,y_col), num_cols)
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  pctcut_num_cols <- dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <- dict_data$varname[which(dict_data$label%in%filter_tag_labels)]
  aggregate_conditioned_on_cols <- intersect(colnames(data), dict_data$varname[which(dict_data$label %in% aggregate_conditioned_on_labels)])
  
  print("---- prepare engineered train and validation dataset (internal) ----")
  tryCatch({
    if(trim_ctrl){
      data_in <- engineer(data = data,
                          num_cols = num_cols,
                          fct_cols = fct_cols,
                          cluster_col = cluster_col,
                          trim_by_col = trim_by_col,
                          trim_min = trim_vec[1],
                          trim_max = trim_vec[2],
                          trim_step_size = time_unit,
                          pctcut_num_cols = pctcut_num_cols,
                          pctcut_num_vec = pctcut_num_vec,
                          pctcut_num_coerce = pctcut_num_coerce,
                          filter_tag_cols = filter_tag_cols,
                          imputation = imputation,
                          impute_per_cluster = impute_per_cluster,
                          winsorizing = winsorizing,
                          aggregate_per = aggregate_per,
                          aggregate_conditioned_on_cols = aggregate_conditioned_on_cols)
    }else{
      if (all(unique(as.character(data[,y_col])) %in% c(1,0,NA))){
        data_event <- engineer(data = data[which(data[,y_col]==1),],
                               num_cols = num_cols,
                               fct_cols = fct_cols,
                               cluster_col = cluster_col,
                               trim_by_col = trim_by_col,
                               trim_min=trim_vec[1],
                               trim_max=trim_vec[2],
                               trim_step_size = time_unit,
                               pctcut_num_cols = pctcut_num_cols,
                               pctcut_num_vec = pctcut_num_vec,
                               pctcut_num_coerce = pctcut_num_coerce,
                               filter_tag_cols = filter_tag_cols,
                               imputation = imputation,
                               impute_per_cluster = impute_per_cluster,
                               winsorizing = winsorizing,
                               aggregate_per = aggregate_per,
                               aggregate_conditioned_on_cols = aggregate_conditioned_on_cols)
        data_cntrl <- engineer(data = data[which(data[,y_col]==0),],
                               num_cols = num_cols,
                               fct_cols = fct_cols,
                               cluster_col = cluster_col,
                               trim_by_col = trim_by_col,
                               trim_min=-Inf,
                               trim_max=Inf,
                               trim_step_size = time_unit,
                               trim_keepna = TRUE,
                               pctcut_num_cols = pctcut_num_cols,
                               pctcut_num_vec = pctcut_num_vec,
                               pctcut_num_coerce = pctcut_num_coerce,
                               filter_tag_cols = filter_tag_cols,
                               imputation = imputation,
                               impute_per_cluster = impute_per_cluster,
                               winsorizing = winsorizing,
                               aggregate_per = aggregate_per,
                               aggregate_conditioned_on_cols = aggregate_conditioned_on_cols)
        data_in <- bind_rows(data_cntrl, data_event)
      }
    }
    data_in <- assign.dict(data_in, dict_data)
  },error=function(e){
    print("Error!")
    print(e)
  })
  
  # ---- redundancy clus ----
  print("---- do_x_redun ----")
  results <- do_x_redun(df=data_in, 
                        dict_df=dict_data, 
                        x_cols=x_cols, 
                        r_abs=r_abs, 
                        type=type, 
                        rank=rank, 
                        r2=r2)
  
  
  # redundency object
  if(!is.null(results$rmcor_obj)){
    x_corre_obj <- list("trace" = results$rmcor_obj$cor_trace,
                        "cor_df_org" = results$rmcor_obj$cor_df_org,
                        "keep" = paste0("Correlation criteria keep variables --- ",paste0(results$rmcor_obj$keep,collapse = ", ")),
                        "delete" = paste0("Correlation criteria delete variables --- ",paste0(results$rmcor_obj$delete,collapse = ", ")) )
    
  }else{
    x_corre_obj <- list("trace" = data.frame( fail_message = "correlation analysis not run", stringsAsFactors = FALSE),
                        "cor_df_org" = data.frame( fail_message = "less than 2 numeric variables", stringsAsFactors = FALSE), 
                        "keep" = "Correlation criteria keep variables --- ",
                        "delete" =  "Correlation criteria delete variables --- ")
  }
  
  
  # ---- missingness and summary clus limited to refined dataset ----
  summ_cols <-  intersect(colnames(results$df_final), unique(c(x_cols, num_cols, fct_cols, y_col, cluster_col, trim_by_col)) )
  ml_summ_obj <- front_summary_tbl(data = results$df_final[,summ_cols],
                                   dict_data = results$dict_final,
                                   trim_by_label=trim_by_label,
                                   trim_vec = trim_vec,
                                   time_unit = time_unit,
                                   stratify_by=y_label,
                                   cluster_label=cluster_label,
                                   trim_ctrl = trim_ctrl)
  
  
  # ---- spearman2 clus for degree of freedom plot at front ----
  print("---- spearman2 clus for degree of freedom plot ----")
  dof_obj <- NULL
  if (length(num_cols)>0){
    fml <- formula(paste(y_col," ~ ", paste(num_cols,collapse = "+")))
    rho <- spearman2(fml, data=data, p=2)
    rho_df <- data.frame(adj_rho=rho[,"Adjusted rho2"])
    rho_df$cols <- rownames(rho)
    rho_df <- rho_df[order(rho_df$adj_rho, decreasing = TRUE),]
    scaler <- data.frame(q=quantile(rho_df$adj_rho, probs = seq(0, 1, 0.05) , na.rm = TRUE))
    rcs5_cut <- scaler[rcs5_low,"q"]
    rcs4_cut <- scaler[rcs4_low,"q"]
    dof_obj <- list("rho" = rho,
                    "rcs5_cut" = rcs5_cut,
                    "rcs4_cut" = rcs4_cut)
  }
  
  
  return(list("x_corre_obj"=x_corre_obj,
              "x_redun_obj"=results$redun_obj,
              "ml_summ_obj"=ml_summ_obj,
              "dof_obj"=dof_obj))
  
  
}
