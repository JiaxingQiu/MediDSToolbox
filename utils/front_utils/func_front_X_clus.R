front_X_clus <- function(
  data = data_ml,
  dict_data=dict_ml,
  trim_by_label="Post-menstrual Age",
  trim_vec = c(22, 40),
  time_unit=7,
  x_labels=c(c("Gestational Age", "PeriodicBreathing_v2 log(duration proportion) per day", "Birth weight"),"baby_insurance", "Mode of respiratory support (Positive Airway Pressure) without endotracheal tube (EN)___Nasal Cannula with flow"), 
  y_label="Primary outcome (EN)___Unfavorable", 
  cluster_label="PreVent study ID",
  r2=0.9,
  rcs5_low="70%",
  rcs4_low="50%",
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=FALSE,
  aggregation=FALSE,
  r_abs=0.8, 
  type=c("pearson","spearman")[1],
  rank=TRUE,
  trim_ctrl=TRUE 
){
  
  # ---- pre-processing ----
  data <- assign.dict(data, dict_data)
  dict_data <- get.dict(data)
  
  # --- find corresponding column names ---
  x_cols <- rownames(dict_data[which(dict_data$label_front%in%x_labels), ])
  y_col <- rownames(dict_data[which(dict_data$label_front==y_label),])
  num_cols <- intersect(union(x_cols,y_col), rownames(dict_data[which(dict_data$mlrole=="input"&dict_data$type=="num"), ]))
  fct_cols <- setdiff(union(x_cols,y_col), num_cols)
  cluster_col <- rownames(dict_data[which(dict_data$label_front==cluster_label),])
  trim_by_col <- rownames(dict_data[which(dict_data$label_front==trim_by_label), ])
  
  # ---- prepare engineered training and validation dataset (internal data) ----
  if(trim_ctrl){
    data <- engineer(data = data,
                     trim_by_col = trim_by_col,
                     trim_min=trim_vec[1]*time_unit,
                     trim_max=trim_vec[2]*time_unit,
                     num_cols = num_cols,
                     fct_cols = fct_cols,
                     cluster_col = cluster_col,
                     imputation = imputation,
                     impute_per_cluster = impute_per_cluster,
                     winsorizing = winsorizing,
                     aggregation = aggregation)
  }else{
    data_event <- engineer(data = data[which(data[,y_col]==1),],
                           trim_by_col = trim_by_col,
                           trim_min=trim_vec[1]*time_unit,
                           trim_max=trim_vec[2]*time_unit,
                           num_cols = num_cols,
                           fct_cols = fct_cols,
                           cluster_col = cluster_col,
                           imputation = imputation,
                           impute_per_cluster = impute_per_cluster,
                           winsorizing = winsorizing,
                           aggregation = aggregation)
    data_cntrl <- engineer(data = data[which(data[,y_col]==0),],
                           trim_by_col = trim_by_col,
                           trim_min=-Inf,
                           trim_max=Inf,
                           num_cols = num_cols,
                           fct_cols = fct_cols,
                           cluster_col = cluster_col,
                           imputation = imputation,
                           impute_per_cluster = impute_per_cluster,
                           winsorizing = winsorizing,
                           aggregation = aggregation)
    data <- bind_rows(data_cntrl, data_event)
  }
  data_in <- assign.dict(data, dict_data)
  
  # ---- redundancy clus ----
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
                                   stratify_by=y_label,
                                   cluster_label=cluster_label,
                                   trim_ctrl = trim_ctrl)
  
  
  # ---- spearman2 clus for degree of freedom plot at front ----
  dof_obj <- NULL
  if (length(num_cols)>0){
    fml <- formula(paste(y_col," ~ ", paste(num_cols,collapse = "+")))
    rho <- spearman2(fml, data=data, p=2)
    rho_df <- data.frame(adj_rho=rho[,"Adjusted rho2"])
    rho_df$cols <- rownames(rho)
    rho_df <- rho_df[order(rho_df$adj_rho, decreasing = TRUE),]
    scaler <- data.frame(q=quantile(rho_df$adj_rho, probs = seq(0, 1, 0.05) , na.rm = TRUE))
    print(scaler)
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
