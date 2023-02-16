front_viz_2d_stats <- function(
  data,
  dict_data,
  y_label,
  x_label1,
  x_label2,
  cluster_label,
  group_by_label=NULL,
  # --- engineering ---
  trim_by_label, #trim by time column
  trim_vec = c(-Inf, Inf), 
  time_unit = 1,
  pctcut_num_labels = c(), # cutoff by percentile of one or more numeric variable
  pctcut_num_vec = c(0, 100),
  pctcut_num_coerce = TRUE,
  filter_tag_labels = c(), # tag columns
  imputation=c("None","Mean", "Median", "Zero")[1],
  impute_per_cluster=FALSE,
  winsorizing=TRUE,
  aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[1],
  aggregate_conditioned_on_labels = c()
){
  # ---- Usage ----
  
   
  # ---- Arguments ----
  
  
  # ---- Value ----
  
  tryCatch({
    data <- assign.dict(data, dict_data, overwrite = TRUE)
    dict_data <- get.dict(data)
  },error=function(e){
    print("--- Skip refine dictionary from data ---")
    print(e)
  })
  
  # find columns names from input labels
  cluster_col <- dict_data$varname[which(dict_data$label==cluster_label)]
  group_by_col <- dict_data$varname[which(dict_data$label==group_by_label)]
  x_col1 <- dict_data$varname[which(dict_data$label%in%x_label1)]
  x_col2 <- dict_data$varname[which(dict_data$label%in%x_label2)]
  y_col <- dict_data$varname[which(dict_data$label==y_label)]
  trim_by_col <- dict_data$varname[which(dict_data$label==trim_by_label)]
  pctcut_num_cols <- dict_data$varname[which(dict_data$label%in%pctcut_num_labels)]
  filter_tag_cols <- dict_data$varname[which(dict_data$label%in%filter_tag_labels & dict_data$unit=="tag01")]
  num_cols <- unique(intersect( dict_data$varname[which(dict_data$type=="num")], c(x_col1, x_col2, y_col)))
  fct_cols <- unique(intersect( dict_data$varname[which(dict_data$type=="fct")], c(x_col1, x_col2, y_col, group_by_col)))
  aggregate_conditioned_on_cols <- intersect(colnames(data), dict_data$varname[which(dict_data$label %in% aggregate_conditioned_on_labels)])
  
  data <- engineer(data = data,
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
  
  
  
  # --- 2d heatmap plots ----
  plot_2d_stats <- NULL
  if(dict_data[x_col1,"type"]=="num" & dict_data[x_col2,"type"]=="num"){
    plot_2d_stats <- viz_2d_stats(data=data,
                                  dict_data = dict_data,
                                  x_col1 = x_col1,
                                  x_col2 = x_col2,
                                  y_col = y_col,
                                  cluster_col=cluster_col,
                                  group_by_col=group_by_col)
  }
  
  
  
  return(plot_2d_stats)
}


# #################### not run ######################3
# data = data_ml
# dict_data = dict_ml
# y_label = "IH SPO2<90% [10s,300s) duration proportion per day"
# x_label1 = "Gestational Age"
# x_label2 = "Birth weight"
# cluster_label = "PreVent study ID"
# group_by_label = "Primary outcome (EN) == Unfavorable"
# # --- engineering ---
# trim_by_label = "Post-menstrual Age" #trim by time column
# trim_vec = c(-Inf, Inf)
# time_unit = 1
# pctcut_num_labels = c() # cutoff by percentile of one or more numeric variable
# pctcut_num_vec = c(0, 100)
# pctcut_num_coerce = TRUE
# filter_tag_labels = c() # tag columns
# imputation=c("None","Mean", "Median", "Zero")[1]
# impute_per_cluster=FALSE
# winsorizing=TRUE
# aggregate_per=c("row", "cluster_trim_by_unit", "cluster")[2]
