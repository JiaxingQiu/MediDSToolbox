library(shinydashboard)
library(shinyjs)
library(shinydashboardPlus)
library(shiny)
library(shinyBS)
library(shinyWidgets)
options(shiny.maxRequestSize = 5000*1024^2) # 5G data


shinyServer(function(input, output, session) {
  
  values <- reactiveValues(dict_ml = dict_ml_demo,
                           data_ml = data_ml_demo,
                           time_over_labels = time_over_labels_demo)
  
  #-------------------------------------------- Event control --------------------------------------------
  # ---- 0. project upload ----
  observeEvent(input$demo_go,{
    shinyWidgets::updateProgressBar(session = session, id = "pb_demo_go", value = 10)
    # reset to demo project
    shiny_obj <- dictup_shiny(data_org_demo, dict_org_demo)
    values$data_ml <- shiny_obj$data_ml
    values$dict_ml <- shiny_obj$dict_ml
    values$time_over_labels <- time_over_labels_demo
    # find the valid labels within dict_ml
    values$time_over_labels <- intersect(values$dict_ml$label, values$time_over_labels)
    shinyWidgets::updateProgressBar(session = session, id = "pb_demo_go", value = 50)
    updateSelectInput(inputId = "setup_source_file",
                      choices = unique(values$dict_ml$source_file),
                      selected = unique(values$dict_ml$source_file) )
    updateSelectInput(inputId = "setup_cluster_label",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="key")])
    updateSelectInput(inputId = "setup_trim_by_label",
                      choices = values$time_over_labels)
    updateSelectInput(inputId = "setup_pctcut_num_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")])
    updateSelectInput(inputId = "setup_filter_tag_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$unit=="tag01")])
    updateSelectInput(inputId = "setup_strat_by",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="fct")]))
    updateSelectInput(inputId = "setup_aggregate_conditioned_on_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct")] )
    updateSelectInput(inputId = "eda_y_label_stats0d",
                      choices = values$dict_ml$label[which(values$dict_ml$type %in% c("num","fct") )])
    updateSelectInput(inputId = "eda_group_by_label_stats0d",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="fct")]) )
    updateSelectInput(inputId = "eda_y_label_stats1d",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num"|(values$dict_ml$unit=="tag01") )])
    updateSelectInput(inputId = "eda_x_label_stats1d",
                      choices = values$dict_ml$label[which(values$dict_ml$type %in% c("num","fct"))] )
    updateSelectInput(inputId = "eda_group_by_label_stats1d",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="fct")]) )
    updateSelectInput(inputId = "eda_y_label_stats2d",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num" | (values$dict_ml$unit=="tag01") )] )
    updateSelectInput(inputId = "eda_x_label1_stats2d",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")] )
    updateSelectInput(inputId = "eda_x_label2_stats2d",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")] )
    updateSelectInput(inputId = "eda_group_by_label_stats2d",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="fct")]) )
    updateSelectInput(inputId = "eda_y_label_star",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num" | (values$dict_ml$unit=="tag01") )] )
    updateSelectInput(inputId = "eda_sort_by_label",
                      choices = values$time_over_labels)
    updateSelectInput(inputId = "eda_group_by_label_star",
                      choices = c("None",values$dict_ml$label[which(values$dict_ml$type=="fct")]) )
    updateSelectInput(inputId = "eda_tag_label",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$unit=="tag01")]) )
    updateSelectInput(inputId = "eda_y_label_allu",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="num")]) )
    updateSelectInput(inputId = "eda_tag_labels_allu",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")] )
    updateSelectInput(inputId = "ml_y_label",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")],#| values$dict_ml$type=="num"
                      selected = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")][1] )#| values$dict_ml$type=="num"
    updateSelectInput(inputId = "ml_num_adjust_label",
                      choices = c("None",values$dict_ml$label[which(values$dict_ml$type=="num")]),
                      selected = "None" )
    updateSelectInput(inputId = "ml_uni_group_label",
                      choices = c("None",values$dict_ml$label[which(values$dict_ml$type=="fct")]),
                      selected = "None" )
    updateSelectInput(inputId = "ml_num_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_nonlin_rcs5_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_nonlin_rcs4_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_nonlin_rcs3_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_linear_num_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_fct_labels_mdl",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit!="tag01")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_tag_labels_mdl",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")],
                      selected = NULL )
    updateSelectInput(inputId = "unml_input_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num"|values$dict_ml$unit=="tag01")],
                      selected = NULL )
    
    shinyWidgets::updateProgressBar(session = session, id = "pb_demo_go", value = 100)
    
  })
  observeEvent(input$upload_go,{
    shinyWidgets::updateProgressBar(session = session, id = "pb_upload_go", value = 20)
    # reset dict_org in values
    up_dict_org_obj <- UpDictOrg()
    up_dict_org <- up_dict_org_obj$dict_org
    # reset data_org in values
    ext <- tools::file_ext(input$up_data_org$datapath)
    req(input$up_data_org)
    try({validate(need(ext == "csv", "Please upload a csv file"))},TRUE)
    up_data_org <- read.csv(input$up_data_org$datapath)
    # reset values$dict_ml and values$data_ml
    shiny_obj <- dictup_shiny(up_data_org, up_dict_org)
    values$data_ml <- shiny_obj$data_ml
    values$dict_ml <- shiny_obj$dict_ml
    if(length(input$up_time_over_labels)>0){
      values$time_over_labels <- input$up_time_over_labels
    }else{
      values$time_over_labels <- c()
    }
    # find the valid labels within dict_ml
    values$time_over_labels <- intersect(values$dict_ml$label, values$time_over_labels)
    shinyWidgets::updateProgressBar(session = session, id = "pb_upload_go", value = 50)
    updateSelectInput(inputId = "setup_source_file",
                      choices = unique(values$dict_ml$source_file),
                      selected = unique(values$dict_ml$source_file) )
    updateSelectInput(inputId = "setup_cluster_label",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="key")])
    updateSelectInput(inputId = "setup_trim_by_label",
                      choices = values$time_over_labels)
    updateSelectInput(inputId = "setup_pctcut_num_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")])
    updateSelectInput(inputId = "setup_filter_tag_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$unit=="tag01")])
    updateSelectInput(inputId = "setup_strat_by",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="fct")]))
    updateSelectInput(inputId = "setup_aggregate_conditioned_on_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct")] )
    updateSelectInput(inputId = "eda_y_label_stats0d",
                      choices = values$dict_ml$label[which(values$dict_ml$type %in% c("num","fct"))])
    updateSelectInput(inputId = "eda_group_by_label_stats0d",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="fct")]) )
    updateSelectInput(inputId = "eda_y_label_stats1d",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num"|(values$dict_ml$unit=="tag01") )])
    updateSelectInput(inputId = "eda_x_label_stats1d",
                      choices = values$dict_ml$label[which(values$dict_ml$type %in% c("num","fct") )] )
    updateSelectInput(inputId = "eda_group_by_label_stats1d",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="fct")]) )
    updateSelectInput(inputId = "eda_y_label_stats2d",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num" | (values$dict_ml$unit=="tag01") )] )
    updateSelectInput(inputId = "eda_x_label1_stats2d",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")] )
    updateSelectInput(inputId = "eda_x_label2_stats2d",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")] )
    updateSelectInput(inputId = "eda_group_by_label_stats2d",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="fct")]) )
    updateSelectInput(inputId = "eda_y_label_star",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num" | (values$dict_ml$unit=="tag01") )] )
    updateSelectInput(inputId = "eda_sort_by_label",
                      choices = values$time_over_labels)
    updateSelectInput(inputId = "eda_group_by_label_star",
                      choices = c("None",values$dict_ml$label[which(values$dict_ml$type=="fct")]) )
    updateSelectInput(inputId = "eda_tag_label",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$unit=="tag01")]) )
    updateSelectInput(inputId = "eda_y_label_allu",
                      choices = c("None", values$dict_ml$label[which(values$dict_ml$type=="num")]) )
    updateSelectInput(inputId = "eda_tag_labels_allu",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")] )
    updateSelectInput(inputId = "ml_y_label",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")],#| values$dict_ml$type=="num"
                      selected = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")][1] )#| values$dict_ml$type=="num"
    updateSelectInput(inputId = "ml_num_adjust_label",
                      choices = c("None",values$dict_ml$label[which(values$dict_ml$type=="num")]),
                      selected = "None" )
    updateSelectInput(inputId = "ml_uni_group_label",
                      choices = c("None",values$dict_ml$label[which(values$dict_ml$type=="fct")]),
                      selected = "None" )
    updateSelectInput(inputId = "ml_num_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_nonlin_rcs5_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_nonlin_rcs4_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_nonlin_rcs3_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_linear_num_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_fct_labels_mdl",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit!="tag01")],
                      selected = NULL )
    updateSelectInput(inputId = "ml_tag_labels_mdl",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")],
                      selected = NULL )
    updateSelectInput(inputId = "unml_input_labels",
                      choices = values$dict_ml$label[which(values$dict_ml$type=="num"|values$dict_ml$unit=="tag01")],
                      selected = NULL )
    shinyWidgets::updateProgressBar(session = session, id = "pb_upload_go", value = 100)
  })

  # ---- 1. setup ----
  
  observeEvent(input$setup_trim_by_label, {
    # # if time index is not given by user
    # if(input$setup_trim_by_label=="Fake Time Index"){
    #   data_tmp <- assign.dict(values$data_ml, values$dict_ml)
    #   data_tmp$fake_time <- 333
    #   attr(data_tmp$fake_time,"varname") <- "fake_time"
    #   attr(data_tmp$fake_time, "label") <- "Fake Time Index"
    #   attr(data_tmp$fake_time, "type") <- "tim"
    #   attr(data_tmp$fake_time, "unit") <- "fake unit"
    #   attr(data_tmp$fake_time, "source_file") <- "drvd"
    #   attr(data_tmp$fake_time, "unique_per_sbj") <- "FALSE"
    #   # update global values$data_ml
    #   values$data_ml <<- data_tmp
    #   # update global dictionary
    #   values$dict_ml <<- get.dict(values$data_ml)
    #   rm(data_tmp)
    # }
      
    trim_by_col <- values$dict_ml$varname[which(values$dict_ml$label==input$setup_trim_by_label)]
    min_value = min(values$data_ml[,trim_by_col],na.rm=TRUE)
    max_value = max(values$data_ml[,trim_by_col],na.rm=TRUE)
    updateSliderInput(inputId = "setup_trim_vec", 
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
    updateNumericInput(inputId = "setup_trim_time_unit",
                       value = 1)
    
  })  
  observeEvent(input$setup_trim_time_unit, {
    trim_by_col <- values$dict_ml$varname[which(values$dict_ml$label==input$setup_trim_by_label)]
    min_value = floor(min(values$data_ml[,trim_by_col],na.rm=TRUE)/input$setup_trim_time_unit)
    max_value = floor(max(values$data_ml[,trim_by_col],na.rm=TRUE)/input$setup_trim_time_unit)
    updateSliderInput(inputId = "setup_trim_vec", 
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
  })  
  observeEvent(input$setup_trim_vec, {
    time_breaks <- seq(input$setup_trim_vec[1],input$setup_trim_vec[2],1)
    if(length(time_breaks)>20){
      time_breaks <- floor(as.numeric(quantile(seq(input$setup_trim_vec[1],input$setup_trim_vec[2],1), seq(0,1,1/20))))
    }
    updateCheckboxGroupInput(inputId = "eda_time_breaks_allu", 
                             choices = time_breaks,
                             label = input$setup_trim_by_label,
                             inline = TRUE)
    # calculate min and max number of observations per cluster
    cluster_col <- values$dict_ml$varname[which(values$dict_ml$label==input$setup_cluster_label)]
    trim_by_col <- values$dict_ml$varname[which(values$dict_ml$label==input$setup_trim_by_label)]
    print(paste0("update input$eda_time_breaks_allu by ",trim_by_col))
    data_tmp <- values$data_ml[which(values$data_ml[,trim_by_col]>=input$setup_trim_vec[1]*input$setup_trim_time_unit & values$data_ml[,trim_by_col]<=input$setup_trim_vec[2]*input$setup_trim_time_unit ),]
    data_tmp$cluster_col_tmp <- data_tmp[,cluster_col]
    data_nrow <- data_tmp %>% group_by(cluster_col_tmp) %>% summarise(nobs = n()) %>% as.data.frame()
    min_value = min(data_nrow$nobs,na.rm=TRUE)
    max_value = max(data_nrow$nobs,na.rm=TRUE)
    # updateSliderInput(inputId = "ml_uni_sample_per_cluster", 
    #                   min = min_value,
    #                   max = max_value,
    #                   value = round((min_value+max_value)/2) )
    rm(data_tmp)
    rm(data_nrow)
  })  
  observeEvent(input$setup_strat_by, {
    setup_trim_ctrl <- input$setup_trim_ctrl
    if(input$setup_strat_by=="None") setup_trim_ctrl <- TRUE
    updateCheckboxInput(inputId = "setup_trim_ctrl", 
                        value = setup_trim_ctrl)
  })
  summReport <- eventReactive(input$setup_summ_go, {
    source_list <- input$setup_source_file
    if(length(input$setup_source_file)<1) {
      source_list <- unique(values$dict_ml$source_file)
    }
    cols_selected <- values$dict_ml$varname[which(values$dict_ml$source_file %in% c(source_list))]
    cols_selected <- c(cols_selected, values$dict_ml$varname[which(values$dict_ml$label==input$setup_cluster_label)]) 
    cols_selected <- c(cols_selected, values$dict_ml$varname[which(values$dict_ml$label==input$setup_trim_by_label)]) 
    cols_selected <- c(cols_selected, values$dict_ml$varname[which(values$dict_ml$label%in%input$setup_pctcut_num_labels)]) 
    cols_selected <- c(cols_selected, values$dict_ml$varname[which(values$dict_ml$label%in%input$setup_filter_tag_labels)]) 
    cols_selected <- c(cols_selected, values$dict_ml$varname[which(values$dict_ml$label==input$setup_strat_by)]) 
    cols_selected <- unique(cols_selected)
    data_ml_sub <- values$data_ml[,cols_selected]
    front_summary_tbl(
      data=data_ml_sub,
      dict_data=values$dict_ml,
      cluster_label=input$setup_cluster_label, 
      # --- engineer ---
      trim_by_label=input$setup_trim_by_label, 
      trim_vec=as.numeric(input$setup_trim_vec), 
      time_unit=input$setup_trim_time_unit,
      pctcut_num_labels = input$setup_pctcut_num_labels,
      pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
      pctcut_num_coerce = input$setup_pctcut_num_coerce,
      filter_tag_labels = input$setup_filter_tag_labels,
      imputation=input$setup_imputation,
      impute_per_cluster=input$setup_impute_per_cluster,
      winsorizing=input$setup_winsorizing,
      aggregate_per = input$setup_aggregate_per,
      aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
      # --- local ---
      trim_ctrl = input$setup_trim_ctrl,
      stratify_by = input$setup_strat_by
    )
  })
  # ---- 2. eda ----
  stats0dViz <- eventReactive(input$eda_stats0d_go, {
    front_viz_0d_stats(data = values$data_ml,
                       dict_data = values$dict_ml,
                       y_label = input$eda_y_label_stats0d,
                       cluster_label = input$setup_cluster_label,
                       trim_by_label = input$setup_trim_by_label,
                       trim_vec = as.numeric(input$setup_trim_vec),
                       time_unit = input$setup_trim_time_unit,
                       pctcut_num_labels = input$setup_pctcut_num_labels,
                       pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
                       pctcut_num_coerce = input$setup_pctcut_num_coerce,
                       filter_tag_labels = input$setup_filter_tag_labels,
                       imputation = input$setup_imputation,
                       impute_per_cluster = input$setup_impute_per_cluster,
                       winsorizing = input$setup_winsorizing,
                       aggregate_per = input$setup_aggregate_per,
                       aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
                       group_by_label = input$eda_group_by_label_stats0d
    )
  })
  stats1dViz <- eventReactive(input$eda_stats1d_go, {
    front_viz_1d_stats(data = values$data_ml,
                       dict_data = values$dict_ml,
                       y_label = input$eda_y_label_stats1d,
                       x_label = input$eda_x_label_stats1d,
                       cluster_label = input$setup_cluster_label,
                       trim_by_label = input$setup_trim_by_label,
                       trim_vec = as.numeric(input$setup_trim_vec),
                       time_unit = input$setup_trim_time_unit,
                       pctcut_num_labels = input$setup_pctcut_num_labels,
                       pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
                       pctcut_num_coerce = input$setup_pctcut_num_coerce,
                       filter_tag_labels = input$setup_filter_tag_labels,
                       imputation = input$setup_imputation,
                       impute_per_cluster = input$setup_impute_per_cluster,
                       winsorizing = input$setup_winsorizing,
                       aggregate_per = input$setup_aggregate_per,
                       aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
                       group_by_label = input$eda_group_by_label_stats1d
    ) 
  })
  
  
  stats2dViz <- eventReactive(input$eda_stats2d_go, {
    front_viz_2d_stats(data = values$data_ml,
                       dict_data = values$dict_ml,
                       y_label = input$eda_y_label_stats2d,
                       x_label1 = input$eda_x_label1_stats2d,
                       x_label2 = input$eda_x_label2_stats2d,
                       cluster_label = input$setup_cluster_label,
                       trim_by_label = input$setup_trim_by_label,
                       trim_vec = as.numeric(input$setup_trim_vec),
                       time_unit = input$setup_trim_time_unit,
                       pctcut_num_labels = input$setup_pctcut_num_labels,
                       pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
                       pctcut_num_coerce = input$setup_pctcut_num_coerce,
                       filter_tag_labels = input$setup_filter_tag_labels,
                       imputation = input$setup_imputation,
                       impute_per_cluster = input$setup_impute_per_cluster,
                       winsorizing = input$setup_winsorizing,
                       aggregate_per = input$setup_aggregate_per,
                       aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
                       group_by_label = input$eda_group_by_label_stats2d
                      
    ) 
  })
  
  starViz <- eventReactive(input$eda_star_go, {
    front_viz_death_star(data = values$data_ml,
                         dict_data = values$dict_ml,
                         trim_by_label = input$setup_trim_by_label,
                         trim_vec = as.numeric(input$setup_trim_vec),
                         time_unit = input$setup_trim_time_unit,
                         pctcut_num_labels = input$setup_pctcut_num_labels,
                         pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
                         pctcut_num_coerce = input$setup_pctcut_num_coerce,
                         filter_tag_labels = input$setup_filter_tag_labels,
                         cluster_label = input$setup_cluster_label,
                         # --- user interface ----
                         y_label = input$eda_y_label_star,
                         sort_by_label = input$eda_sort_by_label, 
                         align_by_label = input$setup_trim_by_label, 
                         group_by_label = input$eda_group_by_label_star,
                         tag_label = input$eda_tag_label, 
                         scale = input$eda_scale,
                         # --- developer control ---
                         offset_label = NULL, 
                         default_tag_labels = c()
    ) 
  })
  
  alluvialViz <- eventReactive(input$eda_allu_go, {
    front_viz_alluvial(data = values$data_ml,
                       dict_data = values$dict_ml,
                       y_label = input$eda_y_label_allu,
                       cluster_label = input$setup_cluster_label,
                       trim_by_label = input$setup_trim_by_label,
                       trim_vec = as.numeric(input$setup_trim_vec),
                       time_unit = input$setup_trim_time_unit,
                       pctcut_num_labels = input$setup_pctcut_num_labels,
                       pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
                       pctcut_num_coerce = input$setup_pctcut_num_coerce,
                       filter_tag_labels = input$setup_filter_tag_labels,
                       # alluvial plot
                       time_label = input$setup_trim_by_label, 
                       time_breaks = input$eda_time_breaks_allu, 
                       time_quantity = "average",#input$eda_time_quantity_allu, 
                       tag_labels = input$eda_tag_labels_allu,
                       includeNA = input$eda_includeNA_allu
    ) 
  })
  
  # ---- 3. supervised ml ----
  X2listen <- reactive({
    list(input$ml_linear_num_labels,
         input$ml_nonlin_rcs3_labels,
         input$ml_nonlin_rcs4_labels,
         input$ml_nonlin_rcs5_labels,
         input$ml_fct_labels_mdl,
         input$ml_tag_labels_mdl)
  })
  observeEvent(X2listen(), {
    X_labels <- unique(c(
      input$ml_linear_num_labels,
      input$ml_nonlin_rcs3_labels,
      input$ml_nonlin_rcs4_labels,
      input$ml_nonlin_rcs5_labels,
      input$ml_fct_labels_mdl,
      input$ml_tag_labels_mdl
    ))
    X_labels <- setdiff(X_labels, input$ml_y_label)
    updateSelectInput(inputId = "ml_joint_col2_label", 
                      choices = union("None", X_labels))
    updateSelectInput(inputId = "ml_num_adjust_label",
                      choices = c("None",unique(X_labels)),
                      selected = "None" )
    updateSelectInput(inputId = "ml_num_labels", 
                      selected = unique(X_labels))
  })
  # --- ml_linear_num_labels ---
  X2listen4linear <- reactive({
    list(input$ml_nonlin_rcs3_labels,
         input$ml_nonlin_rcs4_labels,
         input$ml_nonlin_rcs5_labels)
  })
  observeEvent(X2listen4linear(), {
    X_labels_used <- unique(c(
      input$ml_nonlin_rcs3_labels,
      input$ml_nonlin_rcs4_labels,
      input$ml_nonlin_rcs5_labels,
      input$ml_y_label
    ))
    updateSelectInput(inputId = "ml_linear_num_labels", 
                      choices = setdiff(values$dict_ml$label[which(values$dict_ml$type=="num")], X_labels_used),
                      selected = input$ml_linear_num_labels)
  })
  # --- ml_nonlin_rcs3_labels ---
  X2listen4rcs3 <- reactive({
    list(input$ml_linear_num_labels,
         input$ml_nonlin_rcs4_labels,
         input$ml_nonlin_rcs5_labels)
  })
  observeEvent(X2listen4rcs3(), {
    X_labels_used <- unique(c(
      input$ml_linear_num_labels,
      input$ml_nonlin_rcs4_labels,
      input$ml_nonlin_rcs5_labels,
      input$ml_y_label
    ))
    updateSelectInput(inputId = "ml_nonlin_rcs3_labels", 
                      choices = setdiff(values$dict_ml$label[which(values$dict_ml$type=="num")], X_labels_used),
                      selected = input$ml_nonlin_rcs3_labels)
  })
  # --- ml_nonlin_rcs4_labels ---
  X2listen4rcs4 <- reactive({
    list(input$ml_linear_num_labels,
         input$ml_nonlin_rcs3_labels,
         input$ml_nonlin_rcs5_labels)
  })
  observeEvent(X2listen4rcs4(), {
    X_labels_used <- unique(c(
      input$ml_linear_num_labels,
      input$ml_nonlin_rcs3_labels,
      input$ml_nonlin_rcs5_labels,
      input$ml_y_label
    ))
    updateSelectInput(inputId = "ml_nonlin_rcs4_labels", 
                      choices = setdiff(values$dict_ml$label[which(values$dict_ml$type=="num")], X_labels_used),
                      selected = input$ml_nonlin_rcs4_labels)
  })
  # --- ml_nonlin_rcs5_labels ---
  X2listen4rcs5 <- reactive({
    list(input$ml_linear_num_labels,
         input$ml_nonlin_rcs3_labels,
         input$ml_nonlin_rcs4_labels)
  })
  observeEvent(X2listen4rcs5(), {
    X_labels_used <- unique(c(
      input$ml_linear_num_labels,
      input$ml_nonlin_rcs3_labels,
      input$ml_nonlin_rcs4_labels,
      input$ml_y_label
    ))
    updateSelectInput(inputId = "ml_nonlin_rcs5_labels", 
                      choices = setdiff(values$dict_ml$label[which(values$dict_ml$type=="num")], X_labels_used),
                      selected = input$ml_nonlin_rcs5_labels)
  })
  # --- ml_fct_labels_mdl ---
  observeEvent(input$ml_y_label, {
    updateSelectInput(inputId = "ml_fct_labels_mdl", 
                      choices = setdiff(values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit!="tag01")], input$ml_y_label),
                      selected = input$ml_fct_labels_mdl)
  })
  # --- ml_tag_labels_mdl ---
  observeEvent(input$ml_y_label, {
    updateSelectInput(inputId = "ml_tag_labels_mdl", 
                      choices = setdiff(values$dict_ml$label[which(values$dict_ml$type=="fct"&values$dict_ml$unit=="tag01")], input$ml_y_label),
                      selected = input$ml_tag_labels_mdl)
  })
  observeEvent(input$ml_y_map_func, {
    if(input$ml_y_map_func=="probability"){
      updateNumericInput(inputId = "ml_y_max", 
                         min = 0,
                         max = 1,
                         value = 1 )
    }
  })
  
  observeEvent(input$ml_y_max, {
    updateSliderInput(inputId = "ml_uni_heat_limits", 
                      min = 0,
                      max = input$ml_y_max,
                      value = c(0, input$ml_y_max) )
  })
  uniHeatmap <- eventReactive(input$ml_uni_go, {
    heat_limits <- NULL
    if(input$ml_uni_custom_heat){
      heat_limits <- input$ml_uni_heat_limits
    }
    uni_obj <- front_uni_heatmap_group(
      data=values$data_ml,
      dict_data=values$dict_ml,
      num_labels=input$ml_num_labels,
      y_label=input$ml_y_label,
      cluster_label=input$setup_cluster_label,
      # --- engineer ---
      trim_by_label = input$setup_trim_by_label,
      trim_vec = as.numeric(input$setup_trim_vec),
      time_unit=input$setup_trim_time_unit,
      pctcut_num_labels = input$setup_pctcut_num_labels,
      pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
      pctcut_num_coerce = input$setup_pctcut_num_coerce,
      filter_tag_labels = input$setup_filter_tag_labels,
      imputation=input$setup_imputation,
      impute_per_cluster=input$setup_impute_per_cluster,
      winsorizing=input$setup_winsorizing,
      aggregate_per = input$setup_aggregate_per,
      aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
      # --- local ---
      trim_ctrl = input$ml_trim_ctrl,
      num_adjust_label=input$ml_num_adjust_label,
      method=input$ml_method,
      heat_limits = heat_limits,
      y_map_func = input$ml_y_map_func,
      y_map_max = input$ml_y_max,
      group_label = input$ml_uni_group_label,
      sample_per_cluster = NULL,#input$ml_uni_sample_per_cluster
    )
    return(uni_obj)
  })
  # --- ml_select_lambda ---
  observeEvent(input$ml_select_lasso_by, {
    if(input$ml_select_lasso_by=="cluster"){
      updateSelectInput(inputId = "ml_select_lambda", 
                        choices = c("AIC"="AIC",
                                    "BIC"="BIC",
                                    "10 fold cvAUC" = "cvAUC",
                                    "10 fold cvLogLoss"="cvLogLoss"),
                        selected = "BIC")
    }else{
      updateSelectInput(inputId = "ml_select_lambda", 
                        choices = c(
                          "Automated" = "auto",
                          "lambda.min" = "min",
                          "lambda.1se" = "1se"
                        ),
                        selected="auto")
    }
  })
  XselectReports <- eventReactive(input$ml_select_go, {
    test_data <- NULL
    if(!is.null(input$ex_test_csv)){
      ext <- tools::file_ext(input$ex_test_csv$datapath)
      req(input$ex_test_csv)
      try({validate(need(ext == "csv", "Please upload a csv file"))},TRUE)
      test_data <- read.csv(input$ex_test_csv$datapath)
    }
    front_lasso_select(
      data = values$data_ml,
      dict_data = values$dict_ml,
      y_label=input$ml_y_label, 
      cluster_label=input$setup_cluster_label,
      x_labels_linear=input$ml_linear_num_labels,
      x_labels_nonlin_rcs5 = input$ml_nonlin_rcs5_labels,
      x_labels_nonlin_rcs4 = input$ml_nonlin_rcs4_labels,
      x_labels_nonlin_rcs3 = input$ml_nonlin_rcs3_labels,
      x_labels_fct = input$ml_fct_labels_mdl,
      x_labels_tag = input$ml_tag_labels_mdl,
      # --- engineer ---
      trim_by_label = input$setup_trim_by_label, 
      trim_vec = as.numeric(input$setup_trim_vec),  
      time_unit=input$setup_trim_time_unit,
      pctcut_num_labels = input$setup_pctcut_num_labels,
      pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
      pctcut_num_coerce = input$setup_pctcut_num_coerce,
      filter_tag_labels = input$setup_filter_tag_labels,
      imputation=input$setup_imputation,
      impute_per_cluster=input$setup_impute_per_cluster,
      standardize_df = input$ml_select_standardize_df,
      winsorizing=input$setup_winsorizing,
      aggregate_per = input$setup_aggregate_per,
      aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
      # --- local ---
      trim_ctrl = input$ml_trim_ctrl,
      test_data=test_data,
      y_map_func=input$ml_y_map_func,
      y_map_max=input$ml_y_max,
      return_performance=input$ml_select_return_performance,
      lambda = input$ml_select_lambda,
      lasso_by = input$ml_select_lasso_by,
      tune_by = input$ml_select_tune_by
    )
  })
  
  XclusReports <- eventReactive(input$ml_clus_go, {
    front_X_clus(
      data = values$data_ml,
      dict_data = values$dict_ml,
      x_labels=unique(c(input$ml_tag_labels_mdl, input$ml_fct_labels_mdl,input$ml_linear_num_labels,input$ml_nonlin_rcs3_labels,input$ml_nonlin_rcs4_labels,input$ml_nonlin_rcs5_labels)), 
      y_label=input$ml_y_label, 
      cluster_label=input$setup_cluster_label,
      # --- engineer ---
      trim_by_label = input$setup_trim_by_label, 
      trim_vec = as.numeric(input$setup_trim_vec),  
      time_unit=input$setup_trim_time_unit,
      pctcut_num_labels = input$setup_pctcut_num_labels,
      pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
      pctcut_num_coerce = input$setup_pctcut_num_coerce,
      filter_tag_labels = input$setup_filter_tag_labels,
      imputation=input$setup_imputation,
      impute_per_cluster=input$setup_impute_per_cluster,
      winsorizing=input$setup_winsorizing,
      aggregate_per = input$setup_aggregate_per,
      aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
      # --- local ---
      r2=input$ml_r2,
      rcs5_low=paste0(input$ml_rcs_vec[1],"%"),
      rcs4_low=paste0(input$ml_rcs_vec[2],"%"),
      r_abs=input$ml_r_abs, 
      type=input$ml_type,
      rank=FALSE,
      trim_ctrl = input$ml_trim_ctrl
    ) 
  })
  
  MLreports <- eventReactive(input$ml_multi_go, {
    test_data <- NULL
    if(!is.null(input$ex_test_csv)){
      ext <- tools::file_ext(input$ex_test_csv$datapath)
      req(input$ex_test_csv)
      try({validate(need(ext == "csv", "Please upload a csv file"))},TRUE)
      test_data <- read.csv(input$ex_test_csv$datapath)
    }
    front_multi_regression(
      data = values$data_ml,
      dict_data = values$dict_ml,
      y_label=input$ml_y_label, 
      cluster_label=input$setup_cluster_label,
      x_labels_linear=input$ml_linear_num_labels,
      x_labels_nonlin_rcs5 = input$ml_nonlin_rcs5_labels,
      x_labels_nonlin_rcs4 = input$ml_nonlin_rcs4_labels,
      x_labels_nonlin_rcs3 = input$ml_nonlin_rcs3_labels,
      x_labels_fct = input$ml_fct_labels_mdl,
      x_labels_tag = input$ml_tag_labels_mdl,
      x_labels = unique(c(input$ml_tag_labels_mdl, input$ml_fct_labels_mdl,input$ml_linear_num_labels,input$ml_nonlin_rcs3_labels,input$ml_nonlin_rcs4_labels,input$ml_nonlin_rcs5_labels)), 
      # --- engineer ---
      trim_by_label = input$setup_trim_by_label, 
      trim_vec = as.numeric(input$setup_trim_vec),  
      time_unit=input$setup_trim_time_unit,
      pctcut_num_labels = input$setup_pctcut_num_labels,
      pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
      pctcut_num_coerce = input$setup_pctcut_num_coerce,
      filter_tag_labels = input$setup_filter_tag_labels,
      imputation=input$setup_imputation,
      impute_per_cluster=input$setup_impute_per_cluster,
      winsorizing=input$setup_winsorizing,
      aggregate_per = input$setup_aggregate_per,
      aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
      # --- local ---
      trim_ctrl = input$ml_trim_ctrl,
      r2=input$ml_r2,
      rcs5_low=paste0(input$ml_rcs_vec[1],"%"),
      rcs4_low=paste0(input$ml_rcs_vec[2],"%"),
      cv_nfold = as.numeric(input$ml_cv_nfold),
      na_frac_max=input$ml_na_frac_max, 
      test_data = test_data,
      joint_col2_label = input$ml_joint_col2_label,
      stratified_cv = input$ml_stratified_cv,
      r_abs = input$ml_r_abs, 
      type = input$ml_type,
      fix_knots = input$ml_fix_knots,
      y_map_func = input$ml_y_map_func,  
      y_map_max = input$ml_y_max,
      tune_by=input$ml_tune_by) 
  })
  
  MLreports_timely <- eventReactive(input$ml_timely_go, {
    front_multi_regression_timely(
      data = values$data_ml,
      dict_data = values$dict_ml,
      y_label=input$ml_y_label, 
      cluster_label=input$setup_cluster_label,
      x_labels_linear=input$ml_linear_num_labels,
      x_labels_nonlin_rcs5 = input$ml_nonlin_rcs5_labels,
      x_labels_nonlin_rcs4 = input$ml_nonlin_rcs4_labels,
      x_labels_nonlin_rcs3 = input$ml_nonlin_rcs3_labels,
      x_labels_fct = input$ml_fct_labels_mdl,
      x_labels_tag = input$ml_tag_labels_mdl,
      x_labels=unique(c(input$ml_tag_labels_mdl, input$ml_fct_labels_mdl,input$ml_linear_num_labels,input$ml_nonlin_rcs3_labels,input$ml_nonlin_rcs4_labels,input$ml_nonlin_rcs5_labels)), 
      # --- engineer ---
      trim_by_label = input$setup_trim_by_label, 
      trim_vec = as.numeric(input$setup_trim_vec),  
      time_unit=input$setup_trim_time_unit,
      pctcut_num_labels = input$setup_pctcut_num_labels,
      pctcut_num_vec = as.numeric(input$setup_pctcut_num_vec),
      pctcut_num_coerce = input$setup_pctcut_num_coerce,
      filter_tag_labels = input$setup_filter_tag_labels,
      imputation=input$setup_imputation,
      impute_per_cluster=input$setup_impute_per_cluster,
      winsorizing=input$setup_winsorizing,
      aggregate_per = input$setup_aggregate_per,
      aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
      # --- local ---
      trim_ctrl = input$ml_trim_ctrl,
      r2=input$ml_r2,
      rcs5_low=paste0(input$ml_rcs_vec[1],"%"),
      rcs4_low=paste0(input$ml_rcs_vec[2],"%"),
      cv_nfold = as.numeric(input$ml_cv_nfold),
      na_frac_max=input$ml_na_frac_max, 
      stratified_cv=input$ml_stratified_cv,
      r_abs=input$ml_r_abs, 
      type=input$ml_type,
      window_size = input$ml_window_size,
      step_size = input$ml_step_size,
      test_size = input$ml_test_size,
      lag_size = input$ml_lag_size,
      fix_knots = input$ml_fix_knots) 
  })
  
  # ---- 4. unsupervised ml ----
  observeEvent(input$unml_trim_by_label, {
    trim_by_col <- values$dict_ml$varname[which(values$dict_ml$label==input$unml_trim_by_label)]
    min_value = min(values$data_ml[,trim_by_col],na.rm=TRUE)
    max_value = max(values$data_ml[,trim_by_col],na.rm=TRUE)
    updateSliderInput(inputId = "unml_trim_vec",
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
    updateNumericInput(inputId = "unml_trim_time_unit",
                       value = 1)
  })
  observeEvent(input$unml_trim_time_unit, {
    trim_by_col <- values$dict_ml$varname[which(values$dict_ml$label==input$unml_trim_by_label)]
    min_value = floor(min(values$data_ml[,trim_by_col],na.rm=TRUE)/input$unml_trim_time_unit)
    max_value = floor(max(values$data_ml[,trim_by_col],na.rm=TRUE)/input$unml_trim_time_unit)
    updateSliderInput(inputId = "unml_trim_vec",
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
  })
  MLuns_cluster <- eventReactive(input$umml_cluster_go, {
    front_uns_cluster(
      # global parameters (unsupervised setup page)
      data=values$data_ml,
      dict_data=values$dict_ml,
      cluster_label=input$setup_cluster_label,
      # --- engineer ---
      trim_by_label=input$setup_trim_by_label,
      trim_vec = input$setup_trim_vec,
      time_unit= input$setup_trim_time_unit,
      pctcut_num_labels = input$setup_pctcut_num_labels, # cutoff by percentile of one or more numeric variable
      pctcut_num_vec = input$setup_pctcut_num_vec,
      pctcut_num_coerce = input$setup_pctcut_num_coerce,
      filter_tag_labels=input$setup_filter_tag_labels,
      imputation=input$setup_imputation,
      impute_per_cluster=input$setup_impute_per_cluster,
      winsorizing=input$setup_winsorizing,
      aggregate_per=input$setup_aggregate_per,
      aggregate_conditioned_on_labels = input$setup_aggregate_conditioned_on_labels,
      # --- local ---
      input_labels=input$unml_input_labels,
      nc_vec = input$unml_nc_vec,
      min_nobs_per_clst=input$unml_min_nobs_per_clst,
      max_iter=input$unml_max_iter
    )
  })
  
  
  # --------------------------------------------- output object ------------------------------------------------
  # ---- 0. project upload ----
  output$time_over_valid <- reactive({
    return(length(values$time_over_labels)>0)
  })
  outputOptions(output, 'time_over_valid', suspendWhenHidden=FALSE)
  
  UpDataOrg <- reactive({
    msg <- NULL
    valid <- FALSE
    if(!is.null(input$up_data_org)){
      ext <- tools::file_ext(input$up_data_org$datapath)
      req(input$up_data_org)
      try({validate(need(ext == "csv", "Please upload a csv file"))},TRUE)
      df <- read.csv(input$up_data_org$datapath, nrows = 1000)
      # change status message
      msg <-"Using uploaded dataframe status:"
      # TBD error checks
      # final status
      if(grepl("invalid",msg)){
        msg <- paste(msg, "Please upload again.", sep="<br/>")
      }else{
        msg <- paste(msg, "Valid.", sep="<br/>")
        valid <- TRUE
      }
    }
    return(list(msg = msg,
                valid = valid))
  })
  output$up_data_org_valid <- reactive({
    up_obj <- UpDataOrg()
    return(up_obj$valid)
  })
  outputOptions(output, 'up_data_org_valid', suspendWhenHidden=FALSE)
  output$up_data_org_msg <- renderUI({
    up_obj <- UpDataOrg()
    HTML(up_obj$msg) 
  })
  
  
  UpDictOrg <- reactive({
    msg <- NULL 
    valid <- FALSE
    d <- NULL
    if(!is.null(input$up_dict_org)){
      ext <- tools::file_ext(input$up_dict_org$datapath)
      req(input$up_dict_org)
      try({validate(need(ext == "csv", "Please upload a csv file"))},TRUE)
      d <- read.csv(input$up_dict_org$datapath)
      # change status message
      msg <-"Using uploaded dictionary status:"
      # check dictionary quality by each required columns
      if(!"varname" %in% colnames(d)){
        msg <- paste(msg, "invalid -- 'varname' field is missing.", sep="<br/>")
      }
      if(!"label" %in% colnames(d)){
        msg <- paste(msg, "invalid -- 'label' field is missing.", sep="<br/>")
      }
      if(!"type" %in% colnames(d)){
        msg <- paste(msg, "invalid -- 'type' field is missing.", sep="<br/>")
      }else{
        if(!"key"%in%unique(d[,"type"])){
          msg <- paste(msg, "invalid -- 'type' field must include at least one 'key' value.", sep="<br/>")
        }
        if(!"num"%in%unique(d[,"type"])){
          msg <- paste(msg, "invalid -- 'type' field must include at least one 'num' value.", sep="<br/>")
        }
        if(!"fct"%in%unique(d[,"type"])){
          msg <- paste(msg, "invalid -- 'type' field must include at least one 'fct' value.", sep="<br/>")
        }
      }
      
      if(!"unique_per_sbj" %in% colnames(d)){
        msg <- paste(msg, "invalid -- 'unique_per_sbj' field is missing.", sep="<br/>")
      }else{
        if( length(setdiff(unique(d[,"unique_per_sbj"]), c("TRUE","FALSE")))>0 ){
          msg <- paste(msg, "invalid -- 'unique_per_sbj' field value can only be 'TRUE' or 'FALSE'.", sep="<br/>")
        }
      }
      if(!"unit" %in% colnames(d)){
        msg <- paste(msg, "warning -- 'unit' field is not specified, system defaults ''.", sep="<br/>")
      }
      if(!"source_file" %in% colnames(d)){
        msg <- paste(msg, "warning -- 'source_file' field is not specified, system defaults 'source'.", sep="<br/>")
      }else{
        if(all(d[,"source_file"]=="")){
          d[,"source_file"] <- "default"
          msg <- paste(msg, "warning -- 'source_file' field is not specified, system defaults 'source'.", sep="<br/>")
        }
      }
      # final status
      if(grepl("invalid",msg)){
        msg <- paste(msg, "Please upload again.", sep="<br/>")
      }else{
        msg <- paste(msg, "Valid.", sep="<br/>")
        valid <- TRUE
      }
    }
    return(list(msg = msg,
                valid = valid,
                dict_org = d))
  })
  output$up_dict_org_valid <- reactive({
    up_obj <- UpDictOrg()
    return(up_obj$valid)
  })
  outputOptions(output, 'up_dict_org_valid', suspendWhenHidden=FALSE)
  output$up_dict_org_msg <- renderUI({
    up_obj <- UpDictOrg()
    HTML(up_obj$msg)
  })
  
  observeEvent(input$up_dict_org,{
    up_obj <- UpDictOrg()
    dict_org <- up_obj$dict_org
    updateSelectInput(inputId = "up_time_over_labels",
                      choices = dict_org$label[which(dict_org$type=="num"&as.character(dict_org$unique_per_sbj)=="FALSE")])
  })
  
  output$download_created_dict_org <- downloadHandler(
    filename = function() {
      paste0('dict_data_init', Sys.Date(), ".csv")
    },
    content = function(file) {
      ext <- tools::file_ext(input$up_data_org$datapath)
      req(input$up_data_org)
      try({validate(need(ext == "csv", "Please upload a csv file"))},TRUE)
      df <- read.csv(input$up_data_org$datapath)
      dict_init <- get.dict(df)
      write.csv(dict_init, file, row.names = FALSE)
    }
  )
  
  
  # ---- 1. setup ----
  output$dictionary_setup  <- renderDataTable(
    values$dict_ml[which(values$dict_ml$source_file%in%input$setup_source_file),c("varname","label","type","unit","unit_label","unique_per_sbj","source_file")]
  )
  # Summary Table ----
  output$summary_table <- renderDataTable({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   summ_obj <- summReport()
                   setProgress(1)
                 })
    summ_obj$summ_df_reformat
  })
  output$download_summary_table <- downloadHandler(
    filename = function() {
      paste0('summ_', Sys.Date(), ".csv")
    },
    content = function(file) {
      summ_obj <- summReport()
      write.csv(summ_obj$summ_df_reformat, file, row.names = TRUE)
    }
  )
  output$num_detail_table <- renderDataTable({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   summ_obj <- summReport()
                   setProgress(1)
                 })
    summ_obj$num_detail_df
  })
  output$download_num_detail_table <- downloadHandler(
    filename = function() {
      paste0('summ_num_', Sys.Date(), ".csv")
    },
    content = function(file) {
      summ_obj <- summReport()
      write.csv(summ_obj$num_detail_df, file, row.names = FALSE)
    }
  )
  output$fct_detail_table <- renderDataTable({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   summ_obj <- summReport()
                   setProgress(1)
                 })
    summ_obj$fct_detail_df
  })
  output$download_fct_detail_table <- downloadHandler(
    filename = function() {
      paste0('summ_fct_', Sys.Date(), ".csv")
    },
    content = function(file) {
      summ_obj <- summReport()
      write.csv(summ_obj$fct_detail_df, file, row.names = FALSE)
    }
  )
  output$rsps_table <- renderDataTable({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   summ_obj <- summReport()
                   setProgress(1)
                 })
    summ_obj$rsps_df
  })
  # output$na_plot <- renderPlot({
  #   summ_obj <- summReport()
  #   if(!is.null(summ_obj$na_obj)){
  #     plot(summ_obj$na_obj)
  #   } 
  # })
  # ---- 2. eda ----
  output$eda_0d_p <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   eda_0d_obj <- stats0dViz()
                   setProgress(1)
                 })
    eda_0d_obj
  })
  
  output$eda_1d_p_violin <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   eda_1d_obj <- stats1dViz()
                   setProgress(1)
                 })
    eda_1d_obj$p_violin+
      coord_cartesian(xlim = ranges_eda_1d_p_violin$x, ylim = ranges_eda_1d_p_violin$y, expand = FALSE)
  })
  ranges_eda_1d_p_violin <- reactiveValues(x = NULL, y = NULL)# Single zoomable plot (on left)
  observeEvent(input$eda_1d_p_violin_dblclick, {
    brush <- input$eda_1d_p_violin_brush
    if (!is.null(brush)) {
      ranges_eda_1d_p_violin$x <- c(brush$xmin, brush$xmax)
      ranges_eda_1d_p_violin$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_eda_1d_p_violin$x <- NULL
      ranges_eda_1d_p_violin$y <- NULL
    }
  })
  output$eda_1d_p_mean <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   eda_1d_obj <- stats1dViz()
                   setProgress(1)
                 })
    eda_1d_obj$p_mean+
      coord_cartesian(xlim = ranges_eda_1d_p_mean$x, ylim = ranges_eda_1d_p_mean$y, expand = FALSE)
  })
  ranges_eda_1d_p_mean <- reactiveValues(x = NULL, y = NULL)# Single zoomable plot (on left)
  observeEvent(input$eda_1d_p_mean_dblclick, {
    brush <- input$eda_1d_p_mean_brush
    if (!is.null(brush)) {
      ranges_eda_1d_p_mean$x <- c(brush$xmin, brush$xmax)
      ranges_eda_1d_p_mean$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_eda_1d_p_mean$x <- NULL
      ranges_eda_1d_p_mean$y <- NULL
    }
  })
  output$eda_1d_p_pct <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   eda_1d_obj <- stats1dViz()
                   setProgress(1)
                 })
    eda_1d_obj$p_pct+
      coord_cartesian(xlim = ranges_eda_1d_p_pct$x, ylim = ranges_eda_1d_p_pct$y, expand = FALSE)
  })
  ranges_eda_1d_p_pct <- reactiveValues(x = NULL, y = NULL)# Single zoomable plot (on left)
  observeEvent(input$eda_1d_p_pct_dblclick, {
    brush <- input$eda_1d_p_pct_brush
    if (!is.null(brush)) {
      ranges_eda_1d_p_pct$x <- c(brush$xmin, brush$xmax)
      ranges_eda_1d_p_pct$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_eda_1d_p_pct$x <- NULL
      ranges_eda_1d_p_pct$y <- NULL
    }
  })
  output$eda_1d_p_denom <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   eda_1d_obj <- stats1dViz()
                   setProgress(1)
                 })
    eda_1d_obj$p_denom+
      coord_cartesian(xlim = ranges_eda_1d_p_denom$x, ylim = ranges_eda_1d_p_denom$y, expand = FALSE)
  })
  ranges_eda_1d_p_denom <- reactiveValues(x = NULL, y = NULL)# Single zoomable plot (on left)
  observeEvent(input$eda_1d_p_denom_dblclick, {
    brush <- input$eda_1d_p_denom_brush
    if (!is.null(brush)) {
      ranges_eda_1d_p_denom$x <- c(brush$xmin, brush$xmax)
      ranges_eda_1d_p_denom$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_eda_1d_p_denom$x <- NULL
      ranges_eda_1d_p_denom$y <- NULL
    }
  })
  output$eda_1d_df_summ <- renderDataTable({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   eda_1d_obj <- stats1dViz()
                   setProgress(1)
                 })
    eda_1d_obj$df_summ
  })
  output$eda_1d_p_1stat_stt <- renderPlot({
    eda_1d_obj <- stats1dViz()
    p_1stat_set <- eda_1d_obj$p_1stat_set[[as.character( input$eda_1d_p_1stat_sttname )]]
    p1 <- ggpubr::ggarrange(p_1stat_set$numer,
                            p_1stat_set$denom,
                            nrow=1,
                            common.legend = TRUE,
                            legend = "top")
    ggpubr::ggarrange(p_1stat_set$pctall,
                      p1,
                      nrow=1,
                      widths = c(1,2),
                      common.legend = FALSE,
                      legend = "top")
  })
  output$download_eda_1d_df_summ <- downloadHandler(
    filename = function() {
      paste0("plot_summary_y_",
        as.character(input$eda_y_label_stats1d),
        "_x_",
        as.character(input$eda_x_label_stats1d),
        "_group_",
        as.character(input$eda_group_by_label_stats1d),
        "_",
        Sys.Date(), 
        ".csv")
    },
    content = function(file) {
      eda_1d_obj <- stats1dViz()
      write.csv(eda_1d_obj$df_summ, file, row.names = FALSE)
    }
  )
  
  output$plot_2d_stats <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   plot_obj <- stats2dViz()
                   setProgress(1)
                 })
    plot_obj
  })
  
  output$plot_death_star <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   plot_obj <- starViz()
                   setProgress(1)
                 })
    plot_obj +
      coord_cartesian(xlim = ranges_star$x, ylim = ranges_star$y, expand = FALSE)
  })
  ranges_star <- reactiveValues(x = NULL, y = NULL)# Single zoomable plot (on left)
  observeEvent(input$plot_death_star_dblclick, {
    brush <- input$plot_death_star_brush
    if (!is.null(brush)) {
      ranges_star$x <- c(brush$xmin, brush$xmax)
      ranges_star$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_star$x <- NULL
      ranges_star$y <- NULL
    }
  })
  
  output$plot_alluvial <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   plot_obj <- alluvialViz()
                   setProgress(1)
                 })
    plot_obj
  })
  
  # ---- 3. supervised ml ----
  # setup ----
  output$dictionary_table_ml <- renderDataTable(
    values$dict_ml[,c("label","source_file","varname","type","unit")]
  )
  # Univariate Heatmap ----
  output$plot_uniheat <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   uni_obj <- uniHeatmap()
                   setProgress(1)
                 })
    if(!input$ml_uni_raw_scale){
      plot_obj <- uni_obj$plot_obj
    }else{
      plot_obj <- ggpubr::ggarrange(plotlist=uni_obj$plot_list,
                                    ncol=3,
                                    nrow=ceiling(length(uni_obj$plot_list)/3),
                                    common.legend = TRUE,
                                    legend = "right")
    }
    plot_obj
  })
  # Univariate Signature of Illness ----
  output$plot_unisig <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   uni_obj <- uniHeatmap()
                   setProgress(1)
                 })
    return(uni_obj$plot_obj_signat)
    # w <- 800
    # h <- ceiling( n_distinct(uni_obj$plot_df$x_name)/4 )*200
  })
  
  # Feature Selection ----
  output$ml_select_tune_trace_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   x_select_report <- XselectReports()
                   setProgress(1)
                 })
    x_select_report$infer_obj$tune_trace_plot
  })
  output$ml_select_coef_trace_plot <- renderPlot({
    x_select_report <- XselectReports()
    x_select_report$infer_obj$coef_trace_plot
  })
  output$ml_select_opt_model_df <- renderTable({
    x_select_report <- XselectReports()
    x_select_report$infer_obj$opt_model_df
  })
  output$ml_select_lam_df <- renderTable({
    x_select_report <- XselectReports()
    x_select_report$infer_obj$lambda_zero_coef[,c("varname", "lambda")]
  })
  
  # lasso performance
  output$perform_download_df_hat_lasso <- downloadHandler(
    filename = function() {
      paste0('lasso_y_hat_', Sys.Date(), ".csv")
    },
    content = function(file) {
      x_select_report <- XselectReports()
      df_hat <- NULL
      if(input$perform_from_lasso=="Internal"){
        if(input$perform_dataset_lasso=="Engineered"){
          df_hat <- x_select_report$perform_obj$perform_in_df_hat
        }else if(input$perform_dataset_lasso=="Original"){
          df_hat <- x_select_report$perform_obj$perform_inorg_df_hat
        }
      }else if(input$perform_from_lasso=="External"){
        if(input$perform_dataset_lasso=="Engineered"){
          df_hat <- x_select_report$perform_obj$perform_ex_df_hat
        }else if(input$perform_dataset_lasso=="Original"){
          df_hat <- x_select_report$perform_obj$perform_exorg_df_hat
        }
      }
      write.csv(df_hat, file, row.names = FALSE)
    }
  )
  output$perform_download_scores_tbl_lasso <- downloadHandler(
    filename = function() {
      paste0('lasso_x_rank_', Sys.Date(), ".csv")
    },
    content = function(file) {
      x_select_report <- XselectReports()
      scores_tbl <- NULL
      if(input$perform_from_lasso=="Internal"){
        if(input$perform_dataset_lasso=="Engineered"){
          scores_tbl <- x_select_report$perform_obj$perform_in_scores_tbl
        }else if(input$perform_dataset_lasso=="Original"){
          scores_tbl <- x_select_report$perform_obj$perform_inorg_scores_tbl
        }
      }else if(input$perform_from_lasso=="External"){
        if(input$perform_dataset_lasso=="Engineered"){
          scores_tbl <- x_select_report$perform_obj$perform_ex_scores_tbl
        }else if(input$perform_dataset_lasso=="Original"){
          scores_tbl <- x_select_report$perform_obj$perform_exorg_scores_tbl
        }
      }
      write.csv(scores_tbl, file, row.names = FALSE)
    }
  )
  output$perform_cali_plot_lasso <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   x_select_report <- XselectReports()
                   setProgress(1)
                 })
    cali_plot <- NULL
    if(input$perform_from_lasso=="Internal"){
      if(input$perform_dataset_lasso=="Engineered"){
        cali_plot <- x_select_report$perform_obj$perform_in_cali_plot
      }else if(input$perform_dataset_lasso=="Original"){
        cali_plot <- x_select_report$perform_obj$perform_inorg_cali_plot
      }
    }else if(input$perform_from_lasso=="External"){
      if(input$perform_dataset_lasso=="Engineered"){
        cali_plot <- x_select_report$perform_obj$perform_ex_cali_plot
      }else if(input$perform_dataset_lasso=="Original"){
        cali_plot <- x_select_report$perform_obj$perform_exorg_cali_plot
      }
    }
    cali_plot
  })
  output$perform_fitted_eff_plot_lasso <- renderPlot({
    x_select_report <- XselectReports()
    fitted_eff_plot <- NULL
    if(input$perform_from_lasso=="Internal"){
      if(input$perform_dataset_lasso=="Engineered"){
        fitted_eff_plot <- x_select_report$perform_obj$perform_in_fitted_eff_plot
      }else if(input$perform_dataset_lasso=="Original"){
        fitted_eff_plot <- x_select_report$perform_obj$perform_inorg_fitted_eff_plot
      }
    }else if(input$perform_from_lasso=="External"){
      if(input$perform_dataset_lasso=="Engineered"){
        fitted_eff_plot <- x_select_report$perform_obj$perform_ex_fitted_eff_plot
      }else if(input$perform_dataset_lasso=="Original"){
        fitted_eff_plot <- x_select_report$perform_obj$perform_exorg_fitted_eff_plot
      }
    }
    fitted_eff_plot
  })
  output$perform_tradeoff_plot_lasso <- renderPlot({
    x_select_report <- XselectReports()
    tradeoff_plot <- NULL
    if(input$perform_from_lasso=="Internal"){
      if(input$perform_dataset_lasso=="Engineered"){
        tradeoff_plot <- x_select_report$perform_obj$perform_in_tradeoff_plot
      }else if(input$perform_dataset_lasso=="Original"){
        tradeoff_plot <- x_select_report$perform_obj$perform_inorg_tradeoff_plot
      }
    }else if(input$perform_from_lasso=="External"){
      if(input$perform_dataset_lasso=="Engineered"){
        tradeoff_plot <- x_select_report$perform_obj$perform_ex_tradeoff_plot
      }else if(input$perform_dataset_lasso=="Original"){
        tradeoff_plot <- x_select_report$perform_obj$perform_exorg_tradeoff_plot
      }
    }
    tradeoff_plot
  })
  output$perform_tte_plot_lasso <- renderPlot({
    x_select_report <- XselectReports()
    tte_plot <- NULL
    if(input$perform_from_lasso=="Internal"){
      if(input$perform_dataset_lasso=="Engineered"){
        tte_plot <- x_select_report$perform_obj$perform_in_tte_plot
      }else if(input$perform_dataset_lasso=="Original"){
        tte_plot <- x_select_report$perform_obj$perform_inorg_tte_plot
      }
    }else if(input$perform_from_lasso=="External"){
      if(input$perform_dataset_lasso=="Engineered"){
        tte_plot <- x_select_report$perform_obj$perform_ex_tte_plot
      }else if(input$perform_dataset_lasso=="Original"){
        tte_plot <- x_select_report$perform_obj$perform_exorg_tte_plot
      }
    }
    tte_plot
  })
  output$perform_scores_plot_lasso <- renderPlot({
    x_select_report <- XselectReports()
    scores_plot <- NULL
    if(input$perform_from_lasso=="Internal"){
      if(input$perform_dataset_lasso=="Engineered"){
        scores_plot <- x_select_report$perform_obj$perform_in_scores_plot
      }else if(input$perform_dataset_lasso=="Original"){
        scores_plot <- x_select_report$perform_obj$perform_inorg_scores_plot
      }
    }else if(input$perform_from_lasso=="External"){
      if(input$perform_dataset_lasso=="Engineered"){
        scores_plot <- x_select_report$perform_obj$perform_ex_scores_plot
      }else if(input$perform_dataset_lasso=="Original"){
        scores_plot <- x_select_report$perform_obj$perform_exorg_scores_plot
      }
    }
    scores_plot
  })
  output$perform_scores_tbl_lasso <- renderTable({
    x_select_report <- XselectReports()
    scores_tbl <- NULL
    if(input$perform_from_lasso=="Internal"){
      if(input$perform_dataset_lasso=="Engineered"){
        scores_tbl <- x_select_report$perform_obj$perform_in_scores_tbl
      }else if(input$perform_dataset_lasso=="Original"){
        scores_tbl <- x_select_report$perform_obj$perform_inorg_scores_tbl
      }
    }else if(input$perform_from_lasso=="External"){
      if(input$perform_dataset_lasso=="Engineered"){
        scores_tbl <- x_select_report$perform_obj$perform_ex_scores_tbl
      }else if(input$perform_dataset_lasso=="Original"){
        scores_tbl <- x_select_report$perform_obj$perform_exorg_scores_tbl
      }
    }
    scores_tbl
  })
  
  
  # Variable Clues ----
  output$dof_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   XclusReports <- XclusReports()
                   setProgress(1)
                 })
    plot(XclusReports$dof_obj$rho, cex=0.7, main="Quadratic Spearman Rank")
    abline(v=c(XclusReports$dof_obj$rcs5_cut,XclusReports$dof_obj$rcs4_cut), col=c("blue", "red"))
  })
  output$x_redun_obj <- renderPrint({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   XclusReports <- XclusReports()
                   setProgress(1)
                 })
    print(XclusReports$x_redun_obj)
  })
  output$x_corre_in <- renderText({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   XclusReports <- XclusReports()
                   setProgress(1)
                 })
    print(XclusReports$x_corre_obj$keep)
  })
  output$x_corre_out <- renderText({
    XclusReports <- XclusReports()
    print(XclusReports$x_corre_obj$delete)
  })
  output$x_corre_trace <- renderTable({
    XclusReports <- XclusReports()
    XclusReports$x_corre_obj$trace 
  })
  output$x_corre_df_org <- renderTable({
    XclusReports <- XclusReports()
    XclusReports$x_corre_obj$cor_df_org 
  })
  output$ml_summary_table <- renderDataTable({
    XclusReports <- XclusReports()
    XclusReports$ml_summ_obj$summ_df_reformat
  })
  output$ml_na_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   XclusReports <- XclusReports()
                   setProgress(1)
                 })
    plot(XclusReports$ml_summ_obj$na_obj)
  })
  # Regression ----
  # development
  output$devel_model_info_tbl <- renderTable({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   MLreports <- MLreports()
                   setProgress(1)
                 })
    MLreports$devel_model_info_tbl # model info table
  })
  output$devel_penal_trace_tbl <- renderTable({
    MLreports <- MLreports()
    MLreports$devel_penal_trace_tbl # model cvScores table
  })
  # output$devel_score_summ_tbl <- renderTable({
  #   MLreports <- MLreports()
  #   MLreports$devel_score_summ_tbl # model score table
  # })
  # inference
  output$devel_download_mdl <- downloadHandler(
    filename = function() {
      paste0('mdl_', Sys.Date(), ".rda")
    },
    content = function(file) {
      MLreports <- MLreports()
      saveRDS(MLreports$devel_final_model_obj, file)
    }
  )
  output$infer_effect_plot_1d_continuous <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   MLreports <- MLreports()
                   setProgress(1)
                 })
    infer_effect_plot_1d_continuous <- NULL
    if(is.null(infer_effect_plot_1d_continuous)){
      infer_effect_plot_1d_continuous <- MLreports$infer_effect_plot_diy_list$continuous
    }
    if(is.null(infer_effect_plot_1d_continuous)) {
      infer_effect_plot_1d_continuous <- MLreports$infer_effect_plot_1d
    }
    if(is.null(infer_effect_plot_1d_continuous)) {
      infer_effect_plot_1d_continuous <- MLreports$infer_effect_plot_list$continuous
    }
    infer_effect_plot_1d_continuous
  })
  output$infer_effect_plot_1d_discrete <- renderPlot({
    MLreports <- MLreports()
    infer_effect_plot_1d_discrete <- NULL
    if(is.null(infer_effect_plot_1d_discrete)){
      infer_effect_plot_1d_discrete <- MLreports$infer_effect_plot_diy_list$discrete
    }
    if(is.null(infer_effect_plot_1d_discrete)) {
      infer_effect_plot_1d_discrete <- MLreports$infer_effect_plot_list$discrete
    }
    infer_effect_plot_1d_discrete
  })
  output$infer_effect_plot_2d <- renderPlot({
    MLreports <- MLreports()
    MLreports$infer_effect_plot_2d
  })
  output$infer_anova_plot <- renderPlot({
    MLreports <- MLreports()
    plot(anova(MLreports$devel_final_model_obj))
  })
  output$infer_model_prt <- renderPrint({
    MLreports <- MLreports()
    print(MLreports$devel_final_model_obj) # model coef table
  })
  
  # performance
  output$perform_download_df_hat <- downloadHandler(
    filename = function() {
      paste0('ridge_y_hat_', Sys.Date(), ".csv")
    },
    content = function(file) {
      MLreports <- MLreports()
      df_hat <- NULL
      if(input$perform_from=="Internal"){
        if(input$perform_dataset=="Engineered"){
          df_hat <- MLreports$perform_in_df_hat
        }else if(input$perform_dataset=="Original"){
          df_hat <- MLreports$perform_inorg_df_hat
        }
      }else if(input$perform_from=="External"){
        if(input$perform_dataset=="Engineered"){
          df_hat <- MLreports$perform_ex_df_hat
        }else if(input$perform_dataset=="Original"){
          df_hat <- MLreports$perform_exorg_df_hat
        }
      }
      write.csv(df_hat, file, row.names = FALSE)
    }
  )
  output$perform_download_scores_tbl <- downloadHandler(
    filename = function() {
      paste0('ridge_x_rank_', Sys.Date(), ".csv")
    },
    content = function(file) {
      MLreports <- MLreports()
      scores_tbl <- NULL
      if(input$perform_from=="Internal"){
        if(input$perform_dataset=="Engineered"){
          scores_tbl <- MLreports$perform_in_scores_tbl
        }else if(input$perform_dataset=="Original"){
          scores_tbl <- MLreports$perform_inorg_scores_tbl
        }
      }else if(input$perform_from=="External"){
        if(input$perform_dataset=="Engineered"){
          scores_tbl <- MLreports$perform_ex_scores_tbl
        }else if(input$perform_dataset=="Original"){
          scores_tbl <- MLreports$perform_exorg_scores_tbl
        }
      }
      write.csv(scores_tbl, file, row.names = FALSE)
    }
  )
  output$perform_cali_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   MLreports <- MLreports()
                   setProgress(1)
                 })
    cali_plot <- NULL
    if(input$perform_from=="Internal"){
      if(input$perform_dataset=="Engineered"){
        cali_plot <- MLreports$perform_in_cali_plot
      }else if(input$perform_dataset=="Original"){
        cali_plot <- MLreports$perform_inorg_cali_plot
      }
    }else if(input$perform_from=="External"){
      if(input$perform_dataset=="Engineered"){
        cali_plot <- MLreports$perform_ex_cali_plot
      }else if(input$perform_dataset=="Original"){
        cali_plot <- MLreports$perform_exorg_cali_plot
      }
    }
    cali_plot
  })
  output$perform_tte_plot <- renderPlot({
    MLreports <- MLreports()
    tte_plot <- NULL
    if(input$perform_from=="Internal"){
      if(input$perform_dataset=="Engineered"){
        tte_plot <- MLreports$perform_in_tte_plot
      }else if(input$perform_dataset=="Original"){
        tte_plot <- MLreports$perform_inorg_tte_plot
      }
    }else if(input$perform_from=="External"){
      if(input$perform_dataset=="Engineered"){
        tte_plot <- MLreports$perform_ex_tte_plot
      }else if(input$perform_dataset=="Original"){
        tte_plot <- MLreports$perform_exorg_tte_plot
      }
    }
    tte_plot
  })
  output$perform_tradeoff_plot <- renderPlot({
    MLreports <- MLreports()
    tradeoff_plot <- NULL
    if(input$perform_from=="Internal"){
      if(input$perform_dataset=="Engineered"){
        tradeoff_plot <- MLreports$perform_in_tradeoff_plot
      }else if(input$perform_dataset=="Original"){
        tradeoff_plot <- MLreports$perform_inorg_tradeoff_plot
      }
    }else if(input$perform_from=="External"){
      if(input$perform_dataset=="Engineered"){
        tradeoff_plot <- MLreports$perform_ex_tradeoff_plot
      }else if(input$perform_dataset=="Original"){
        tradeoff_plot <- MLreports$perform_exorg_tradeoff_plot
      }
    }
    tradeoff_plot
  })
  output$perform_fitted_eff_plot<- renderPlot({
    MLreports <- MLreports()
    fitted_eff_plot <- NULL
    if(input$perform_from=="Internal"){
      if(input$perform_dataset=="Engineered"){
        fitted_eff_plot <- MLreports$perform_in_fitted_eff_plot
      }else if(input$perform_dataset=="Original"){
        fitted_eff_plot <- MLreports$perform_inorg_fitted_eff_plot
      }
    }else if(input$perform_from=="External"){
      if(input$perform_dataset=="Engineered"){
        fitted_eff_plot <- MLreports$perform_ex_fitted_eff_plot
      }else if(input$perform_dataset=="Original"){
        fitted_eff_plot <- MLreports$perform_exorg_fitted_eff_plot
      }
    }
    fitted_eff_plot
  })
  output$perform_scores_plot<- renderPlot({
    MLreports <- MLreports()
    scores_plot <- NULL
    if(input$perform_from=="Internal"){
      if(input$perform_dataset=="Engineered"){
        scores_plot <- MLreports$perform_in_scores_plot
      }else if(input$perform_dataset=="Original"){
        scores_plot <- MLreports$perform_inorg_scores_plot
      }
    }else if(input$perform_from=="External"){
      if(input$perform_dataset=="Engineered"){
        scores_plot <- MLreports$perform_ex_scores_plot
      }else if(input$perform_dataset=="Original"){
        scores_plot <- MLreports$perform_exorg_scores_plot
      }
    }
    scores_plot
  })
  output$perform_scores_tbl <- renderTable({
    MLreports <- MLreports()
    scores_tbl <- NULL
    if(input$perform_from=="Internal"){
      if(input$perform_dataset=="Engineered"){
        scores_tbl <- MLreports$perform_in_scores_tbl
      }else if(input$perform_dataset=="Original"){
        scores_tbl <- MLreports$perform_inorg_scores_tbl
      }
    }else if(input$perform_from=="External"){
      if(input$perform_dataset=="Engineered"){
        scores_tbl <- MLreports$perform_ex_scores_tbl
      }else if(input$perform_dataset=="Original"){
        scores_tbl <- MLreports$perform_exorg_scores_tbl
      }
    }
    scores_tbl
  })
  
  
  # Timely Models ----
  output$timely_freq_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   MLreports_timely <- MLreports_timely()
                   setProgress(1)
                 })
    MLreports_timely$timely_freq_plot 
  })
  output$timely_score_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   MLreports_timely <- MLreports_timely()
                   setProgress(1)
                 })
    MLreports_timely$timely_score_plot 
  })
  output$timely_infer_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   MLreports_timely <- MLreports_timely()
                   setProgress(1)
                 })
    MLreports_timely$timely_infer_plot 
  })
  output$timely_test_plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.1, {
                   Sys.sleep(0.25)
                   MLreports_timely <- MLreports_timely()
                   setProgress(1)
                 })
    MLreports_timely$timely_test_plot 
  })
  output$timely_freq_table <- renderTable({
    MLreports_timely <- MLreports_timely()
    MLreports_timely$timely_freq_table 
  })
  output$timely_score_table <- renderTable({
    MLreports_timely <- MLreports_timely()
    MLreports_timely$timely_score_table
  })
  output$timely_infer_table <- renderTable({
    MLreports_timely <- MLreports_timely()
    MLreports_timely$timely_infer_table 
  })
  output$timely_test_table <- renderTable({
    MLreports_timely <- MLreports_timely()
    MLreports_timely$timely_test_table 
  })
  
  
  # ---- 4. unsupervised ml ----
  # setup ----
  output$dictionary_table_unml <- renderDataTable(values$dict_ml[which(values$dict_ml$source_file!=""),c("source_file","varname","label","type","unit")])
  
  # kmeans clustering ---
  output$unml_wss_plot <- renderPlot({
    clst_obj <- MLuns_cluster()
    wss <- clst_obj$wss
    par(mfrow=c(2,2))
    try({
      plot(1:clst_obj$nc_max, wss, type="b", xlab="Number of Clusters",
           ylab="Within groups sum of squares")
    },TRUE)
    try({
      plot(1:clst_obj$nc_max, log(wss), type="b", xlab="Number of Clusters",
           ylab="log of Within groups sum of squares")
    },TRUE)
    try({
      plot(2:clst_obj$nc_max, diff(wss), type="b", xlab="Number of Clusters",
           ylab="diff( Within groups sum of squares )")
    },TRUE)
    try({
      plot(2:clst_obj$nc_max, diff(log(wss)), type="b", xlab="Number of Clusters",
           ylab="diff( log of Within groups sum of squares )")
    },TRUE)
  })
  
  output$unml_cluster_pca_plot <- renderPlot({
    clst_obj <- MLuns_cluster()
    clst_obj$cluster_pca_plot 
  })
  output$unml_df_cluster_info <- renderDataTable({
    clst_obj <- MLuns_cluster()
    clst_obj$df_cluster_info 
  })
  output$unml_df_minor_org_trace <- renderDataTable({
    clst_obj <- MLuns_cluster()
    clst_obj$df_minor_org_trace 
  })
  output$unml_dict_df_org_clustered <- renderDataTable({
    clst_obj <- MLuns_cluster()
    clst_obj$dict_df_org_clustered
  })
  
  
})
