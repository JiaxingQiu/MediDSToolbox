library(shinydashboard)
library(shinyjs)
library(shinydashboardPlus)
library(shiny)
options(shiny.maxRequestSize = 500*1024^2)

shinyServer(function(input, output, session) {
  
  #-------------------------------------------- Event control --------------------------------------------
  # ---- 1. eda ----
  observeEvent(input$eda_trim_by_label, {
    trim_by_col <- dict_viz$varname[which(dict_viz$label_front==input$eda_trim_by_label)]
    min_value = min(data_viz[,trim_by_col],na.rm=TRUE)
    max_value = max(data_viz[,trim_by_col],na.rm=TRUE)
    updateSliderInput(inputId = "eda_trim_vec", 
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
    updateNumericInput(inputId = "eda_trim_time_unit",
                       value = 1)
  })  
  observeEvent(input$eda_trim_time_unit, {
    trim_by_col <- dict_viz$varname[which(dict_viz$label_front==input$eda_trim_by_label)]
    min_value = floor(min(data_viz[,trim_by_col],na.rm=TRUE)/input$eda_trim_time_unit)
    max_value = floor(max(data_viz[,trim_by_col],na.rm=TRUE)/input$eda_trim_time_unit)
    updateSliderInput(inputId = "eda_trim_vec", 
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
  })  
  observeEvent(input$eda_trim_vec, {
    time_breaks <- floor(as.numeric(quantile(seq(input$eda_trim_vec[1],input$eda_trim_vec[2],1), seq(0,1,0.1))))
    updateCheckboxGroupInput(inputId = "eda_time_breaks_allu", 
                             choices = time_breaks,
                             inline = TRUE)
  })  
  
  stats1dViz <- eventReactive(input$eda_stats1d_go, {
    front_viz_1d_stats(data = data_viz,
                       dict_data = dict_viz,
                       trim_by_label = input$eda_trim_by_label,
                       trim_vec = as.numeric(input$eda_trim_vec),
                       time_unit = input$eda_trim_time_unit,
                       pctcut_num_labels = input$eda_pctcut_num_labels,
                       pctcut_num_vec = as.numeric(input$eda_pctcut_num_vec),
                       coerce = input$eda_coerce,
                       filter_tag_labels = input$eda_filter_tag_labels,
                       y_label = input$eda_y_label_stats1d,
                       x_label = input$eda_x_label_stats1d,
                       cluster_label = input$eda_cluster_label,
                       group_by_label = input$eda_group_by_label_stats1d
    ) 
  })
  
  
  stats2dViz <- eventReactive(input$eda_stats2d_go, {
    front_viz_2d_stats(data = data_viz,
                       dict_data = dict_viz,
                       trim_by_label = input$eda_trim_by_label,
                       trim_vec = as.numeric(input$eda_trim_vec),
                       time_unit = input$eda_trim_time_unit,
                       pctcut_num_labels = input$eda_pctcut_num_labels,
                       pctcut_num_vec = as.numeric(input$eda_pctcut_num_vec),
                       coerce = input$eda_coerce,
                       filter_tag_labels = input$eda_filter_tag_labels,
                       y_label = input$eda_y_label_stats2d,
                       x_label1 = input$eda_x_label1_stats2d,
                       x_label2 = input$eda_x_label2_stats2d,
                       cluster_label = input$eda_cluster_label
    ) 
  })
  
  starViz <- eventReactive(input$eda_star_go, {
    front_viz_death_star(data = data_viz,
                         dict_data = dict_viz,
                         trim_by_label = input$eda_trim_by_label,
                         trim_vec = as.numeric(input$eda_trim_vec),
                         time_unit = input$eda_trim_time_unit,
                         pctcut_num_labels = input$eda_pctcut_num_labels,
                         pctcut_num_vec = as.numeric(input$eda_pctcut_num_vec),
                         coerce = input$eda_coerce,
                         filter_tag_labels = input$eda_filter_tag_labels,
                         # --- user interface ----
                         y_label = input$eda_y_label_star,
                         sort_by_label = input$eda_sort_by_label, 
                         align_by_label = input$eda_align_by_label, 
                         cluster_label = input$eda_cluster_label,
                         group_by_label = input$eda_group_by_label_star,
                         tag_label = input$eda_tag_label, 
                         scale = input$eda_scale,
                         # --- developer control ---
                         offset_label = sv.offset_label, 
                         na_label = sv.na_label,
                         na_vec = sv.na_vec, 
                         default_tag_labels = sv.default_tag_labels
    ) 
  })
  
  alluvialViz <- eventReactive(input$eda_allu_go, {
    front_viz_alluvial(data = data_viz,
                       dict_data = dict_viz,
                       trim_by_label = input$eda_trim_by_label,
                       trim_vec = as.numeric(input$eda_trim_vec),
                       time_unit = input$eda_trim_time_unit,
                       pctcut_num_labels = input$eda_pctcut_num_labels,
                       pctcut_num_vec = as.numeric(input$eda_pctcut_num_vec),
                       coerce = input$eda_coerce,
                       filter_tag_labels = input$eda_filter_tag_labels,
                       cluster_label = input$eda_cluster_label,
                       # alluvial plot
                       time_label = input$eda_trim_by_label, 
                       time_breaks = input$eda_time_breaks_allu, 
                       time_quantity = input$eda_time_quantity_allu, 
                       y_label = input$eda_y_label_allu,
                       tag_labels = input$eda_tag_labels_allu
                       
    ) 
  })
  
  # ---- 2. supervised ml ----
  observeEvent(input$ml_trim_by_label, {
    trim_by_col <- dict_ml$varname[which(dict_ml$label_front==input$eda_trim_by_label)]
    min_value = min(data_ml[,trim_by_col],na.rm=TRUE)
    max_value = max(data_ml[,trim_by_col],na.rm=TRUE)
    updateSliderInput(inputId = "ml_trim_vec", 
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
    updateNumericInput(inputId = "ml_trim_time_unit",
                       value = 1)
  }) 
  observeEvent(input$ml_trim_time_unit, {
    trim_by_col <- dict_ml$varname[which(dict_ml$label_front==input$ml_trim_by_label)]
    min_value = floor(min(data_ml[,trim_by_col],na.rm=TRUE)/input$ml_trim_time_unit)
    max_value = floor(max(data_ml[,trim_by_col],na.rm=TRUE)/input$ml_trim_time_unit)
    updateSliderInput(inputId = "ml_trim_vec", 
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
  })  
  summReport <- eventReactive(input$ml_summ_go, {
    stratify_by_string <- ifelse(input$ml_summ_stratify_by, as.character(input$ml_y_label), "None")
    print("--- stratify by ---")
    print(stratify_by_string)
    front_summary_tbl(
      data=data_ml,
      dict_data=dict_ml,
      trim_by_label=input$ml_trim_by_label, 
      trim_vec=as.numeric(input$ml_trim_vec), 
      time_unit=input$ml_trim_time_unit,
      stratify_by=stratify_by_string, 
      cluster_label=input$ml_cluster_label,
      imputation=input$ml_imputation,
      impute_per_cluster=input$ml_impute_per_cluster,
      winsorizing=input$ml_winsorizing,
      aggregation = input$ml_aggregation,
      trim_ctrl = input$ml_trim_ctrl
    )
  })
  uniHeatmap <- eventReactive(input$ml_uni_go, {
    front_uni_heatmap(data=data_ml,
                      dict_data=dict_ml,
                      trim_by_label=input$ml_trim_by_label, 
                      trim_vec=as.numeric(input$ml_trim_vec),  
                      num_labels=input$ml_num_labels, 
                      y_label=input$ml_y_label, 
                      cluster_label=input$ml_cluster_label,
                      num_adjust_label=input$ml_num_adjust_label, 
                      method=input$ml_method, 
                      imputation=input$ml_imputation,
                      winsorizing=input$ml_winsorizing,
                      aggregation = input$ml_aggregation,
                      time_unit=input$ml_trim_time_unit,
                      impute_per_cluster=input$ml_impute_per_cluster,
                      trim_ctrl = input$ml_trim_ctrl,
                      y_map_func = input$ml_uni_y_map_func,
                      y_map_max = input$ml_uni_y_max
    )
  })
  XselectReports <- eventReactive(input$ml_select_go, {
    front_X_select(
      data = data_ml,
      dict_data = dict_ml,
      trim_by_label=input$ml_trim_by_label,
      trim_vec = as.numeric(input$ml_trim_vec), 
      time_unit=input$ml_trim_time_unit,
      x_labels_linear=input$ml_linear_num_labels,
      x_labels_nonlin_rcs5 = input$ml_nonlin_rcs5_labels,
      x_labels_nonlin_rcs4 = input$ml_nonlin_rcs4_labels,
      x_labels_nonlin_rcs3 = input$ml_nonlin_rcs3_labels,
      x_labels_fct = input$ml_fct_labels_mdl,
      x_labels_tag = input$ml_tag_labels_mdl,
      y_label=input$ml_y_label, 
      cluster_label=input$ml_cluster_label,
      imputation=input$ml_imputation,
      impute_per_cluster=input$ml_impute_per_cluster,
      winsorizing=input$ml_winsorizing,
      standardize=input$ml_select_standardize,
      trim_ctrl = input$ml_trim_ctrl
    )
  })
  
  XclusReports <- eventReactive(input$ml_clus_go, {
    front_X_clus(data = data_ml,
                 dict_data = dict_ml,
                 trim_by_label = input$ml_trim_by_label,
                 trim_vec = as.numeric(input$ml_trim_vec),
                 time_unit=input$ml_trim_time_unit,
                 x_labels=unique(c(input$ml_tag_labels_mdl, input$ml_fct_labels_mdl,input$ml_linear_num_labels,input$ml_nonlin_rcs3_labels,input$ml_nonlin_rcs4_labels,input$ml_nonlin_rcs5_labels)), 
                 y_label=input$ml_y_label, 
                 cluster_label=input$ml_cluster_label,
                 r2=input$ml_r2,
                 imputation=input$ml_imputation,
                 impute_per_cluster=input$ml_impute_per_cluster,
                 winsorizing=input$ml_winsorizing,
                 aggregation=input$ml_aggregation,
                 r_abs=input$ml_r_abs, 
                 type=input$ml_type,
                 rank=FALSE,
                 rcs5_low=paste0(input$ml_rcs_vec[1],"%"),
                 rcs4_low=paste0(input$ml_rcs_vec[2],"%"),
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
    front_multi_regression(data = data_ml,
                           dict_data = dict_ml,
                           trim_by_label=input$ml_trim_by_label, 
                           trim_vec=as.numeric(input$ml_trim_vec), 
                           time_unit = input$ml_trim_time_unit,
                           x_labels_linear=input$ml_linear_num_labels,
                           x_labels_nonlin_rcs5 = input$ml_nonlin_rcs5_labels,
                           x_labels_nonlin_rcs4 = input$ml_nonlin_rcs4_labels,
                           x_labels_nonlin_rcs3 = input$ml_nonlin_rcs3_labels,
                           x_labels_fct = input$ml_fct_labels_mdl,
                           x_labels_tag = input$ml_tag_labels_mdl,
                           x_labels=unique(c(input$ml_tag_labels_mdl, input$ml_fct_labels_mdl,input$ml_linear_num_labels,input$ml_nonlin_rcs3_labels,input$ml_nonlin_rcs4_labels,input$ml_nonlin_rcs5_labels)), 
                           y_label=input$ml_y_label, 
                           cluster_label=input$ml_cluster_label,
                           r2=input$ml_r2,
                           rcs5_low=paste0(input$ml_rcs_vec[1],"%"),
                           rcs4_low=paste0(input$ml_rcs_vec[2],"%"),
                           cv_nfold = as.numeric(input$ml_cv_nfold),
                           na_frac_max=input$ml_na_frac_max, 
                           test_data = test_data,
                           joint_col2_label=input$ml_joint_col2_label, 
                           imputation=input$ml_imputation,
                           impute_per_cluster=input$ml_impute_per_cluster,
                           winsorizing=input$ml_winsorizing,
                           aggregation = input$ml_aggregation,
                           stratified_cv=input$ml_stratified_cv,
                           r_abs=input$ml_r_abs, 
                           type=input$ml_type,
                           fix_knots = input$ml_fix_knots,
                           trim_ctrl = input$ml_trim_ctrl,
                           y_map_func = input$ml_y_map_func,  
                           y_map_max = input$ml_y_max) 
  })
  
  MLreports_timely <- eventReactive(input$ml_timely_go, {
    front_multi_regression_timely(data = data_ml,
                                  dict_data = dict_ml,
                                  trim_by_label=input$ml_trim_by_label, 
                                  trim_vec=as.numeric(input$ml_trim_vec), 
                                  x_labels_linear=input$ml_linear_num_labels,
                                  x_labels_nonlin_rcs5 = input$ml_nonlin_rcs5_labels,
                                  x_labels_nonlin_rcs4 = input$ml_nonlin_rcs4_labels,
                                  x_labels_nonlin_rcs3 = input$ml_nonlin_rcs3_labels,
                                  x_labels_fct = input$ml_fct_labels_mdl,
                                  x_labels_tag = input$ml_tag_labels_mdl,
                                  x_labels=unique(c(input$ml_tag_labels_mdl, input$ml_fct_labels_mdl,input$ml_linear_num_labels,input$ml_nonlin_rcs3_labels,input$ml_nonlin_rcs4_labels,input$ml_nonlin_rcs5_labels)), 
                                  y_label=input$ml_y_label, 
                                  cluster_label=input$ml_cluster_label,
                                  rcs5_low=paste0(input$ml_rcs_vec[1],"%"),
                                  rcs4_low=paste0(input$ml_rcs_vec[2],"%"),
                                  na_frac_max=input$ml_na_frac_max, 
                                  imputation=input$ml_imputation,
                                  winsorizing=input$ml_winsorizing,
                                  aggregation = input$ml_aggregation,
                                  stratified_cv=input$ml_stratified_cv,
                                  cv_nfold = as.numeric(input$ml_cv_nfold),
                                  time_unit=input$ml_trim_time_unit,
                                  impute_per_cluster=input$ml_impute_per_cluster,
                                  r_abs=input$ml_r_abs, 
                                  r2=input$ml_r2,
                                  type=input$ml_type,
                                  window_size = input$ml_window_size,
                                  step_size = input$ml_step_size,
                                  test_size = input$ml_test_size,
                                  lag_size = input$ml_lag_size,
                                  fix_knots = input$ml_fix_knots,
                                  trim_ctrl = input$ml_trim_ctrl) 
  })
  # ---- 3. unsupervised ml ----
  observeEvent(input$unml_trim_by_label, {
    trim_by_col <- dict_unml$varname[which(dict_unml$label_front==input$unml_trim_by_label)]
    min_value = min(data_unml[,trim_by_col],na.rm=TRUE)
    max_value = max(data_unml[,trim_by_col],na.rm=TRUE)
    updateSliderInput(inputId = "unml_trim_vec",
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
    updateNumericInput(inputId = "unml_trim_time_unit",
                       value = 1)
  })
  observeEvent(input$unml_trim_time_unit, {
    trim_by_col <- dict_unml$varname[which(dict_unml$label_front==input$unml_trim_by_label)]
    min_value = floor(min(data_unml[,trim_by_col],na.rm=TRUE)/input$unml_trim_time_unit)
    max_value = floor(max(data_unml[,trim_by_col],na.rm=TRUE)/input$unml_trim_time_unit)
    updateSliderInput(inputId = "unml_trim_vec",
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
  })
  MLuns_cluster <- eventReactive(input$umml_cluster_go, {
    front_front_uns_cluster(
      # global parameters (unsupervised setup page)
      data = data_unml,
      dict_data = dict_unml,
      cluster_label = input$unml_cluster_label,
      trim_by_label = input$unml_trim_by_label,
      trim_vec = input$unml_trim_vec,
      time_unit = input$unml_trim_time_unit,
      pctcut_num_labels = input$unml_pctcut_num_labels, # cutoff by percentile of one or more numeric variable
      pctcut_num_vec = input$unml_pctcut_num_vec,
      coerce = input$unml_coerce,
      filter_tag_labels = input$unml_filter_tag_labels, # tag columns
      imputation = input$unml_imputation,
      impute_per_cluster=input$unml_impute_per_cluster,
      winsorizing=input$unml_winsorizing,
      aggregation=input$unml_aggregation, # always set to be true
      # local parameters
      input_labels=input$unml_input_labels,
      nc_vec = input$unml_nc_vec,
      min_nobs_per_clst = input$unml_min_nobs_per_clst,
      max_iter = input$unml_max_iter
    )
  })
  
  
  # --------------------------------------------- output object ------------------------------------------------
  # ---- 1. eda ----
  output$dictionary_table_eda <- renderDataTable(dict_viz[which(dict_viz$type!=""),c("varname", "label", "unit", "type")])
  
  output$eda_1d_p_violin <- renderPlot({
    eda_1d_obj <- stats1dViz()
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
    eda_1d_obj <- stats1dViz()
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
    eda_1d_obj <- stats1dViz()
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
    eda_1d_obj <- stats1dViz()
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
    eda_1d_obj <- stats1dViz()
    head(eda_1d_obj$df_summ,10)
  })
  output$eda_1d_p_1stat_set <- renderPlot({
    eda_1d_obj <- stats1dViz()
    eda_1d_obj$p_1stat_set
  })
  output$download_eda_1d_df_summ <- downloadHandler(
    filename = function() {
      paste0('plot_summary_', Sys.Date(), ".csv")
    },
    content = function(file) {
      eda_1d_obj <- stats1dViz()
      write.csv(eda_1d_obj$df_summ, file, row.names = FALSE)
    }
  )
  
  output$plot_2d_stats <- renderPlot({
    stats2dViz()
  })
  
  output$plot_death_star <- renderPlot({
    starViz()+
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
    alluvialViz()
  })
  
  # ---- 2. supervised ml ----
  # setup ----
  output$dictionary_table_ml <- renderDataTable(dict_ml[which(dict_ml$mlrole!=""),c("source_file","varname","label_front","type","unit","mlrole")])
  # Summary Table ----
  output$summary_table <- renderDataTable({
    summ_obj <- summReport()
    summ_obj$summ_df
  })
  output$download_summary_table <- downloadHandler(
    filename = function() {
      paste0('summ_', Sys.Date(), ".csv")
    },
    content = function(file) {
      summ_obj <- summReport()
      write.csv(summ_obj$summ_df, file, row.names = FALSE)
    }
  )
  output$num_detail_table <- renderDataTable({
    summ_obj <- summReport()
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
    summ_obj <- summReport()
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
    summ_obj <- summReport()
    summ_obj$rsps_df
  })
  output$na_plot <- renderPlot({
    summ_obj <- summReport()
    if(!is.null(summ_obj$na_obj)){
      plot(summ_obj$na_obj)
    } 
  })
  # Univariate Heatmap ----
  output$plot_uniheat <- renderPlot({
    uniHeatmap()
  })
  # Feature Selection ----
  output$ml_select_lasso_tuning_plot <- renderPlot({
    x_select_report <- XselectReports()
    x_select_obj <- x_select_report$x_select_mdls # raw lasso regression
    
    lasso_cv <- x_select_obj$cv_mdls$lasso_cv
    ridge_cv <- x_select_obj$cv_mdls$ridge_cv
    lasso_trace <- x_select_obj$trace_mdls$lasso_trace
    ridge_trace <- x_select_obj$trace_mdls$ridge_trace
    print(lasso_cv$lambda.min)
    print(ridge_cv$lambda.min)
    
    # plot results
    par(mfrow = c(2, 2))
    plot(lasso_cv, main = "Lasso penalty\n\n")
    plot(ridge_cv, main = "Ridge penalty\n\n")
    plot(lasso_trace, xvar = "lambda", main = "Lasso penalty\n\n")
    abline(v = log(lasso_cv$lambda.min), col = "red", lty = "dashed")
    abline(v = log(lasso_cv$lambda.1se), col = "blue", lty = "dashed")
    plot(ridge_trace, xvar = "lambda", main = "Ridge penalty\n\n")
    abline(v = log(ridge_cv$lambda.min), col = "red", lty = "dashed")
    abline(v = log(ridge_cv$lambda.1se), col = "blue", lty = "dashed")
    
  })
  
  output$ml_select_lasso_vip <- renderPlot({
    x_select_report <- XselectReports()
    x_select_obj <- x_select_report$x_select_mdls # raw lasso regression
    
    lasso_optimal <- x_select_obj$optimal_mdls$lasso_optimal
    ridge_optimal <- x_select_obj$optimal_mdls$ridge_optimal
    coef_df <- data.frame(coef=as.numeric(lasso_optimal$beta))
    coef_df$vars <- as.character(rownames(lasso_optimal$beta))
    coef_df$penalty <- "Lasso"
    coef_df$coef_abs <- abs(coef_df$coef)
    # coef_df <- coef_df[order(-coef_df$coef_abs),]
    # coef_df$vars <- paste0(c(1:nrow(coef_df)),"__",coef_df$vars)
    coef_df_all <- coef_df
    coef_df <- data.frame(coef=as.numeric(ridge_optimal$beta))
    coef_df$vars <- as.character(rownames(ridge_optimal$beta))
    coef_df$penalty <- "Ridge"
    coef_df$coef_abs <- abs(coef_df$coef)
    # coef_df <- coef_df[order(-coef_df$coef_abs),]
    # coef_df$vars <- paste0(c(1:nrow(coef_df)),"__",coef_df$vars)
    coef_df_all <- bind_rows(coef_df_all, coef_df)
    
    ggplot(data = coef_df_all, aes(x = abs(coef), y = vars ) )+ 
      geom_point() + 
      facet_grid(~ penalty) + 
      ylab(NULL) + 
      xlab(" | coefficients | ") 
  })
  
  output$ml_select_lasso_coef_df <- renderTable({
    x_select_report <- XselectReports()
    x_select_obj <- x_select_report$x_select_mdls # raw lasso regression
    
    lasso_optimal <- x_select_obj$optimal_mdls$lasso_optimal
    ridge_optimal <- x_select_obj$optimal_mdls$ridge_optimal
    coef_df <- data.frame(coef=as.numeric(lasso_optimal$beta))
    coef_df$vars <- as.character(rownames(lasso_optimal$beta))
    coef_df$penalty <- "Lasso"
    coef_df$coef_abs <- abs(coef_df$coef)
    coef_df_all <- coef_df
    coef_df <- data.frame(coef=as.numeric(ridge_optimal$beta))
    coef_df$vars <- as.character(rownames(ridge_optimal$beta))
    coef_df$penalty <- "Ridge"
    coef_df$coef_abs <- abs(coef_df$coef)
    coef_df_all <- bind_rows(coef_df_all, coef_df)
    coef_df_all
  })
  
  output$ml_select_group_lasso_tuning_plot <- renderPlot({
    x_select_report <- XselectReports()
    x_select_obj <- x_select_report$x_select_mdls_grouped # grouped lasso regression
    
    lasso_cv <- x_select_obj$lasso_cv
    lasso_trace <- x_select_obj$lasso_trace
    
    # plot results
    par(mfrow = c(2, 1))
    plot(lasso_cv, main = "Lasso penalty\n\n")
    plot(lasso_trace, xvar = "lambda", main = "Lasso penalty\n\n")
    abline(v = log(lasso_cv$lambda.min), col = "red", lty = "dashed")
    abline(v = log(lasso_cv$lambda.1se), col = "blue", lty = "dashed")
    
  })
  
  output$ml_select_group_lasso_vip <- renderPlot({
    x_select_report <- XselectReports()
    x_select_obj <- x_select_report$x_select_mdls_grouped # raw lasso regression
    lasso_optimal <- x_select_obj$lasso_optimal
    coef_df <- data.frame(coef=as.numeric(lasso_optimal$beta))
    coef_df$vars <- as.character(rownames(lasso_optimal$beta))
    coef_df$coef_abs <- abs(coef_df$coef)
    coef_df_all <- coef_df
    ggplot(data = coef_df_all, aes(x = abs(coef), y = vars)) + # reorder(vars, abs(coef))
      geom_point() + 
      ylab(NULL) + 
      xlab(" | coefficients | ") +
      xlim(0,max(abs(coef_df_all$coef))+0.1)
  })
  
  output$ml_select_group_lasso_coef_df <- renderTable({
    x_select_report <- XselectReports()
    x_select_obj <- x_select_report$x_select_mdls_grouped # raw lasso regression
    lasso_optimal <- x_select_obj$lasso_optimal
    coef_df <- data.frame(coef=as.numeric(lasso_optimal$beta))
    coef_df$vars <- as.character(rownames(lasso_optimal$beta))
    coef_df$coef_abs <- abs(coef_df$coef)
    coef_df_all <- coef_df
    coef_df_all
  })
  
  # Variable Clus ----
  output$dof_plot <- renderPlot({
    XclusReports <- XclusReports()
    plot(XclusReports$dof_obj$rho, cex=0.7, main="Quadratic Spearman Rank")
    abline(v=c(XclusReports$dof_obj$rcs5_cut,XclusReports$dof_obj$rcs4_cut), col=c("blue", "red"))
  })
  output$x_redun_obj <- renderPrint({
    XclusReports <- XclusReports()
    print(XclusReports$x_redun_obj)
  })
  output$x_corre_in <- renderText({
    XclusReports <- XclusReports()
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
    XclusReports$ml_summ_obj$summ_df
  })
  output$ml_na_plot <- renderPlot({
    XclusReports <- XclusReports()
    plot(XclusReports$ml_summ_obj$na_obj)
  })
  # Regression ----
  ## development
  output$devel_cali_plot <- renderPlot({
    MLreports <- MLreports()
    MLreports$devel_cali_plot #MLreports$time_pred_plot
  })
  output$devel_model_info_tbl <- renderTable({
    MLreports <- MLreports()
    MLreports$devel_model_info_tbl # model info table
  })
  output$devel_cv_eval_trace_tbl <- renderTable({
    MLreports <- MLreports()
    MLreports$devel_cv_eval_trace_tbl # model info table
  })
  output$devel_score_summ_tbl <- renderTable({
    MLreports <- MLreports()
    MLreports$devel_score_summ_tbl # model score table
  })
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
  output$infer_effect_plot_1d <- renderPlot({
    MLreports <- MLreports()
    MLreports$infer_effect_plot_1d
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
      paste0('y_hat_', Sys.Date(), ".csv")
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
      paste0('x_rank_', Sys.Date(), ".csv")
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
    MLreports <- MLreports()
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
    MLreports_timely <- MLreports_timely()
    MLreports_timely$timely_freq_plot 
  })
  output$timely_score_plot <- renderPlot({
    MLreports_timely <- MLreports_timely()
    MLreports_timely$timely_score_plot 
  })
  output$timely_infer_plot <- renderPlot({
    MLreports_timely <- MLreports_timely()
    MLreports_timely$timely_infer_plot 
  })
  output$timely_test_plot <- renderPlot({
    MLreports_timely <- MLreports_timely()
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
  
  
  # ---- 3. unsupervised ml ----
  # setup ----
  output$dictionary_table_unml <- renderDataTable(dict_unml[which(dict_unml$source_file!=""),c("source_file","varname","label_front","type","unit","mlrole")])
  
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
