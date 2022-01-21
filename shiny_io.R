# -------------------------------------------------- global -------------------------------------------------- 
if(!"label" %in% colnames(dict_ml)) dict_ml$label <- dict_ml$varname
dict_ml <- get.dict(assign.dict(data_ml, dict_ml, overwrite = TRUE))


# -------------------------------------------------- prj info -----------------------------------------------------
prj_name <- "Pre-Vent"
prj_link <- "https://github.com/JiaxingQiu/FAIRStream/wiki"


# ---------------------------------------------------- ui.R -----------------------------------------------------------------
# ---- 1. setup ----
in.setup_cluster_label <- dict_ml$label[which(dict_ml$type=="key")]
in.setup_trim_by_label <- c("Post-menstrual Age", "Chronological Age")
in.setup_trim_time_unit <- 7
in.setup_pctcut_num_labels <- dict_ml$label[which(dict_ml$type=="num")]
in.setup_filter_tag_labels <- dict_ml$label[which(dict_ml$unit=="tag01")]
in.setup_imputation.selected <-  "None"
in.setup_strat_by <- dict_ml$label[which(dict_ml$type=="fct")]

# ---- 2. eda ----
in.eda_y_label_stats1d <- dict_ml$label[which(dict_ml$type=="num" | (dict_ml$unit=="tag01") )]
in.eda_x_label_stats1d <- c("Chronological Age", "Post-menstrual Age", setdiff(dict_ml$label[which(dict_ml$type!="")],c("Chronological Age", "Post-menstrual Age") ) )
in.eda_group_by_label_stats1d <- c("None", "Primary outcome (EN)","GA weeks binned","Site (EN)", setdiff(dict_ml$label[which(dict_ml$type=="fct")],c("Primary outcome (EN)","Site (EN)")) )

in.eda_y_label_stats2d <- dict_ml$label[which(dict_ml$type=="num" | (dict_ml$unit=="tag01") )]
in.eda_x_label1_stats2d <- c("Post-menstrual Age", "Chronological Age", setdiff(dict_ml$label[which(dict_ml$type=="num")],c("Chronological Age", "Post-menstrual Age") ) )
in.eda_x_label2_stats2d <- c("Gestational Age", "Post-menstrual Age", setdiff(dict_ml$label[which(dict_ml$type=="num")],c("Gestational Age", "Post-menstrual Age") ) )

in.eda_y_label_star <- dict_ml$label[which(dict_ml$type=="num" | (dict_ml$unit=="tag01") )]
in.eda_sort_by_label <- c("Post-menstrual Age", "Chronological Age", "Gestational Age")
in.eda_align_by_label <- c("Post-menstrual Age","Chronological Age") 
in.eda_group_by_label_star <- c("None", "Primary outcome (EN)","GA weeks binned","Site (EN)", setdiff(dict_ml$label[which(dict_ml$type=="fct")],c("Primary outcome (EN)","Site (EN)")) )
in.eda_tag_label <- c("None", dict_ml$label[which(dict_ml$unit=="tag01")])


in.eda_y_label_allu <- c("None", dict_ml$label[which(dict_ml$type=="num")])
in.eda_tag_labels_allu <- dict_ml$label[which(dict_ml$type=="fct"&dict_ml$unit=="tag01")]

# ---- 3. supervised ml ----
in.ml_y_label <- c(dict_ml$label[which(dict_ml$mlrole=="output"&dict_ml$type=="fct"&dict_ml$unit=="tag01")], 
                   dict_ml$label[which(startsWith(dict_ml$varname, "Period")|
                                           startsWith(dict_ml$varname, "Apnea")|
                                           startsWith(dict_ml$varname, "ABD")|
                                           startsWith(dict_ml$varname, "Brady")|
                                           startsWith(dict_ml$varname, "Desat")|
                                           startsWith(dict_ml$varname, "Hyperoxemia"))], 
                   "Site (EN) == Miami")
in.ml_y_label.selected <- "Primary outcome (EN) == Unfavorable"
in.ml_trim_ctrl <- TRUE

in.ml_num_adjust_label <- c("Gestational Age","None")
in.ml_num_labels.choices <- dict_ml$label[which(dict_ml$mlrole=="input"&dict_ml$type=="num")]
in.ml_num_labels.selected <- intersect(dict_ml$label[which(dict_ml$mlrole=="input"&dict_ml$type=="num")], dict_ml$label[which(dict_ml$source_file=="base")] )

in.ml_nonlin_rcs5_labels.choices <- dict_ml$label[which(dict_ml$mlrole=="input"&dict_ml$type=="num")]
in.ml_nonlin_rcs5_labels.selected <- c("Maternal age")
in.ml_nonlin_rcs4_labels.choices <- dict_ml$label[which(dict_ml$mlrole=="input"&dict_ml$type=="num")]
in.ml_nonlin_rcs4_labels.selected <- c("Birth weight")
in.ml_nonlin_rcs3_labels.choices <- dict_ml$label[which(dict_ml$mlrole=="input"&dict_ml$type=="num")]
in.ml_nonlin_rcs3_labels.selected <- c("APGAR score at 1 minute","Doses of any medication today")
in.ml_linear_num_labels.choices <- dict_ml$label[which(dict_ml$mlrole=="input"&dict_ml$type=="num")]
in.ml_linear_num_labels.selected <- c("Gestational Age")
in.ml_fct_labels_mdl.choices <- dict_ml$label[which(dict_ml$mlrole=="input"&dict_ml$type=="fct"&dict_ml$unit!="tag01")]
in.ml_fct_labels_mdl.selected <- c("Site (EN)")
in.ml_tag_labels_mdl.choices <- dict_ml$label[which(dict_ml$mlrole=="input"&dict_ml$type=="fct"&dict_ml$unit=="tag01")]
in.ml_tag_labels_mdl.selected <- c("Baby Gender (EN) == Female",
                                   "Resus at birth: CPAP (EN) == Yes",
                                   "Maternal race (EN) == Black_African_American",
                                   "Maternal ethnicity (EN) == Hispanic_or_Latino")
in.ml_stratified_cv <- TRUE
in.ml_fix_knots <- TRUE
in.ml_joint_col2_label <- c("None","Gestational Age") 

# ---- 4. unsupervised ml ----
in.unml_cluster_label <- "PreVent study ID"
in.unml_trim_by_label <- c("Chronological Age", "Post-menstrual Age", "Gestational Age")
in.unml_trim_time_unit <- 7
in.unml_input_labels.choices <- dict_ml$label[which(dict_ml$type=="num"|dict_ml$unit=="tag01")]
in.unml_input_labels.selected <- dict_ml$label[which(dict_ml$varname%in%colnames(data_ml%>%select(starts_with("cpd_"))) )]












# -------------------------------------------------- server.R ------------------------------------------------------------------
sv.offset_label = "Post-menstrual Age"
sv.na_label = "Post-menstrual Age"
sv.na_vec = c(22, 40)
sv.default_tag_labels = c("Date of birth tag", "Date of death tag")



