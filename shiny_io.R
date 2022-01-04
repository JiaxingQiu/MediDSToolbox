# -------------------------------------------------- global -------------------------------------------------- 
if(!"label" %in% colnames(dict_viz)) dict_viz$label <- dict_viz$varname
if(!"label" %in% colnames(dict_ml)) dict_ml$label <- dict_ml$varname
if(!"label" %in% colnames(dict_unml)) dict_unml$label <- dict_unml$varname
if(!"label_front" %in% colnames(dict_viz)) dict_viz$label_front <- dict_viz$label
if(!"label_front" %in% colnames(dict_ml)) dict_ml$label_front <- dict_ml$label
if(!"label_front" %in% colnames(dict_unml)) dict_unml$label_front <- dict_unml$label
dict_viz <- get.dict(assign.dict(data_viz, dict_viz, overwrite = TRUE))
dict_ml <- get.dict(assign.dict(data_ml, dict_ml, overwrite = TRUE))
dict_unml <- get.dict(assign.dict(data_unml, dict_unml, overwrite = TRUE))



# -------------------------------------------------- prj info -----------------------------------------------------
prj_name <- "Pre-Vent"
prj_link <- "https://github.com/JiaxingQiu/FAIRStream/wiki"


# ---------------------------------------------------- ui.R -----------------------------------------------------------------

# ---- 1. eda ----
in.eda_cluster_label <- dict_viz$label_front[which(dict_viz$type=="key")]
in.eda_trim_by_label <- c("Post-menstrual Age", "Chronological Age")
in.eda_trim_time_unit <- 7
in.eda_pctcut_num_labels <- dict_viz$label_front[which(dict_viz$type=="num")]
in.eda_filter_tag_labels <- dict_viz$label_front[which(dict_viz$unit=="tag01")]

in.eda_y_label_stats1d <- dict_viz$label_front[which(dict_viz$type=="num" | (dict_viz$unit=="tag01") )]
in.eda_x_label_stats1d <- c("Chronological Age", "Post-menstrual Age", setdiff(dict_viz$label_front[which(dict_viz$type!="")],c("Chronological Age", "Post-menstrual Age") ) )
in.eda_group_by_label_stats1d <- c("None", "Primary outcome (EN)","GA weeks binned","Site (EN)", setdiff(dict_viz$label_front[which(dict_viz$type=="fct")],c("Primary outcome (EN)","Site (EN)")) )

in.eda_y_label_stats2d <- dict_viz$label_front[which(dict_viz$type=="num" | (dict_viz$unit=="tag01") )]
in.eda_x_label1_stats2d <- c("Post-menstrual Age", "Chronological Age", setdiff(dict_viz$label_front[which(dict_viz$type=="num")],c("Chronological Age", "Post-menstrual Age") ) )
in.eda_x_label2_stats2d <- c("Gestational Age", "Post-menstrual Age", setdiff(dict_viz$label_front[which(dict_viz$type=="num")],c("Gestational Age", "Post-menstrual Age") ) )

in.eda_y_label_star <- dict_viz$label_front[which(dict_viz$type=="num" | (dict_viz$unit=="tag01") )]
in.eda_sort_by_label <- c("Post-menstrual Age", "Chronological Age", "Gestational Age")
in.eda_align_by_label <- c("Post-menstrual Age","Chronological Age") 
in.eda_group_by_label_star <- c("None", "Primary outcome (EN)","GA weeks binned","Site (EN)", setdiff(dict_viz$label_front[which(dict_viz$type=="fct")],c("Primary outcome (EN)","Site (EN)")) )
in.eda_tag_label <- c("None", dict_viz$label_front[which(dict_viz$unit=="tag01")])


in.eda_y_label_allu <- dict_viz$label_front[which(dict_viz$type=="num")]
in.eda_tag_labels_allu <- dict_viz$label_front[which(dict_viz$type=="fct"&dict_viz$unit=="tag01")]
in.eda_time_label_allu <- c("Post-menstrual Age", "Chronological Age") 

# ---- 2. supervised ml ----
in.ml_trim_time_unit <- 7
in.ml_cluster_label <- cluster_front_labels
in.ml_y_label.selected <- "Primary outcome (EN)___Unfavorable"
in.ml_y_label <- c(y_tag_front_labels, y_num_front_labels, "Site (EN)___Miami")
in.ml_trim_by_label <- c("Chronological Age", "Post-menstrual Age", "Gestational Age")
in.ml_trim_ctrl <- TRUE
in.ml_imputation.selected <-  "None"

in.ml_num_adjust_label <- c("Gestational Age","None")
in.ml_num_labels.choices <- x_num_front_labels
in.ml_num_labels.selected <- intersect(x_num_front_labels, dict_ml$label_front[which(dict_ml$source_file=="base")] )

in.ml_nonlin_rcs5_labels.choices <- x_num_front_labels
in.ml_nonlin_rcs5_labels.selected <- c("Maternal age")
in.ml_nonlin_rcs4_labels.choices <- x_num_front_labels
in.ml_nonlin_rcs4_labels.selected <- c("Birth weight",
                                      "Maternal age")
in.ml_nonlin_rcs3_labels.choices <- x_num_front_labels
in.ml_nonlin_rcs3_labels.selected <- c("APGAR score at 1 minute",
                                      "Doses of any medication today")
in.ml_linear_num_labels.choices <- x_num_front_labels
in.ml_linear_num_labels.selected <- c("Gestational Age")
in.ml_fct_labels_mdl.choices <- x_fct_front_labels
in.ml_fct_labels_mdl.selected <- c("Site (EN)")
in.ml_tag_labels_mdl.choices <- x_tag_front_labels
in.ml_tag_labels_mdl.selected <- c("Baby Gender (EN)___Female",
                                   "Resus at birth: CPAP (EN)___Yes",
                                   "Maternal race (EN)___Black_African_American",
                                   "Maternal ethnicity (EN)___Hispanic_or_Latino")
in.ml_stratified_cv <- TRUE
in.ml_fix_knots <- TRUE
in.ml_joint_col2_label <- c("None","Gestational Age") 

# ---- 3. unsupervised ml ----
in.unml_cluster_label <- "PreVent study ID"
in.unml_trim_by_label <- c("Chronological Age", "Post-menstrual Age", "Gestational Age")
in.unml_trim_time_unit <- 7
in.unml_input_labels.choices <- dict_unml$label[which(dict_unml$type=="num"|dict_unml$unit=="tag01")]
in.unml_input_labels.selected <- dict_unml$label[which(dict_unml$varname%in%colnames(data_unml%>%select(starts_with("cpd_"))) )]












# -------------------------------------------------- server.R ------------------------------------------------------------------
sv.offset_label = "Post-menstrual Age"
sv.na_label = "Post-menstrual Age"
sv.na_vec = c(22, 40)
sv.default_tag_labels = c("Date of birth tag", "Date of death tag")



