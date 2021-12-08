library(shinydashboard)
library(shinyjs)
library(shinydashboardPlus)
library(shiny)


shinyServer(function(input, output, session) {
  
  #-------------------------------------------- Event control --------------------------------------------
  # ---- 1. eda ----
  observeEvent(input$eda_trim_by_label, {
    trim_by_col <- rownames(dict_viz[which(dict_viz$label_front==input$eda_trim_by_label),])
    min_value = min(data_viz[,trim_by_col],na.rm=TRUE)
    max_value = max(data_viz[,trim_by_col],na.rm=TRUE)
    updateSliderInput(inputId = "eda_trim_vec", 
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
  })  
  observeEvent(input$eda_trim_time_unit, {
    trim_by_col <- rownames(dict_viz[which(dict_viz$label_front==input$eda_trim_by_label),])
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
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_death_star_dblclick, {
    brush <- input$plot_death_star_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
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
  observeEvent(input$ml_trim_time_unit, {
    trim_by_col <- rownames(dict_viz[which(dict_viz$label_front==input$ml_trim_by_label),])
    min_value = floor(min(data_viz[,trim_by_col],na.rm=TRUE)/input$ml_trim_time_unit)
    max_value = floor(max(data_viz[,trim_by_col],na.rm=TRUE)/input$ml_trim_time_unit)
    updateSliderInput(inputId = "ml_trim_vec", 
                      min = min_value,
                      max = max_value,
                      value = c(min_value, max_value) )
  })  
  summReport <- eventReactive(input$ml_summ_go, {
    front_summary_tbl(
      data=data_ml,
      dict_data=dict_ml,
      trim_by_label=input$ml_trim_by_label, 
      trim_vec=as.numeric(input$ml_trim_vec), 
      time_unit=input$ml_trim_time_unit,
      stratify_by=input$ml_y_label, 
      cluster_label=input$ml_cluster_label
    )
  })
  uniHeatmap <- eventReactive(input$ml_uni_go, {
    front_multi_heatmap(data=data_ml,
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
                        impute_per_cluster=input$ml_impute_per_cluster)
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
      aggregation=input$ml_aggregation
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
                 rcs4_low=paste0(input$ml_rcs_vec[2],"%")
    ) 
  })
  
  MLreports <- eventReactive(input$ml_multi_go, {
    front_multi_regression(data = data_ml,
                           dict_data = dict_ml,
                           trim_by_label=input$ml_trim_by_label, 
                           trim_vec=as.numeric(input$ml_trim_vec), 
                           x_labels=unique(c(input$ml_tag_labels_mdl, input$ml_fct_labels_mdl,input$ml_linear_num_labels,input$ml_nonlin_rcs3_labels,input$ml_nonlin_rcs4_labels,input$ml_nonlin_rcs5_labels)), 
                           y_label=input$ml_y_label, 
                           cluster_label=input$ml_cluster_label,
                           rcs5_low=paste0(input$ml_rcs_vec[1],"%"),
                           rcs4_low=paste0(input$ml_rcs_vec[2],"%"),
                           num_labels_linear=input$ml_linear_num_labels, 
                           num_col2_label=input$ml_num_col2_label, 
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
                           rank=FALSE,
                           seed_value=333) 
  })
  
  MLreports_timely <- eventReactive(input$ml_timely_go, {
    front_multi_regression_timely(data = data_ml,
                                  dict_data = dict_ml,
                                  trim_by_label=input$ml_trim_by_label, 
                                  trim_vec=as.numeric(input$ml_trim_vec), 
                                  x_labels=unique(c(input$ml_tag_labels_mdl, input$ml_fct_labels_mdl,input$ml_linear_num_labels,input$ml_nonlin_rcs3_labels,input$ml_nonlin_rcs4_labels,input$ml_nonlin_rcs5_labels)), 
                                  y_label=input$ml_y_label, 
                                  cluster_label=input$ml_cluster_label,
                                  rcs5_low=paste0(input$ml_rcs_vec[1],"%"),
                                  rcs4_low=paste0(input$ml_rcs_vec[2],"%"),
                                  num_labels_linear=input$ml_linear_num_labels, 
                                  num_col2_label=input$ml_num_col2_label, 
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
                                  rank=FALSE,
                                  seed_value=333,
                                  window_size = input$ml_window_size,
                                  step_size = input$ml_step_size) 
  })
  # ---- 3. unsupervised ml ----
  
  
  
  # --------------------------------------------- output object ------------------------------------------------
  # ---- 1. eda ----
  output$dictionary_table_eda <- renderDataTable(dict_viz[which(dict_viz$type!=""),c("varname", "label", "unit", "type")])
  
  output$plot_1d_stats <- renderPlot({
    stats1dViz()
  })
  output$plot_2d_stats <- renderPlot({
    stats2dViz()
  })
  output$plot_death_star <- renderPlot({
    starViz()+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
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
  output$na_plot <- renderPlot({
    summ_obj <- summReport()
    plot(summ_obj$na_obj) 
  })
  # Univariate Heatmap ----
  output$plot_uniheat <- renderPlot({
    uniHeatmap()
  })
  # Feature Selection ----
  output$ml_select_tuning_plot <- renderPlot({
    XselectReports <- XselectReports()
    lasso_cv <- XselectReports$cv_mdls$lasso_cv
    ridge_cv <- XselectReports$cv_mdls$ridge_cv
    lasso_trace <- XselectReports$trace_mdls$lasso_trace
    ridge_trace <- XselectReports$trace_mdls$ridge_trace
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
  
  output$ml_select_vip <- renderPlot({
    XselectReports <- XselectReports()
    lasso_optimal <- XselectReports$optimal_mdls$lasso_optimal
    ridge_optimal <- XselectReports$optimal_mdls$ridge_optimal
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
    
    ggplot(data = coef_df_all, aes(x = abs(coef), y = vars) ) + 
      geom_point() + 
      facet_grid(~ penalty) + 
      ylab(NULL) + 
      xlab(" | coefficients | ") 
    #   # + geom_text(aes(label=gsub("0","0", gsub("-1","(-)",as.character(sign(coef))))), hjust = 1, vjust = 1.5, size = 10)
    # ggpubr::ggarrange(vip::vip(lasso_optimal, horizontal = TRUE, geom = "point", include_type=TRUE),
    #                   vip::vip(ridge_optimal, horizontal = TRUE, geom = "point", include_type=TRUE),
    #                   nrow=1)
    
    
    
  })
  
  output$ml_select_coef_df <- renderTable({
    XselectReports <- XselectReports()
    lasso_optimal <- XselectReports$optimal_mdls$lasso_optimal
    ridge_optimal <- XselectReports$optimal_mdls$ridge_optimal
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
  output$effect_plot <- renderPlot({
    MLreports <- MLreports()
    MLreports$effect_plot
  })
  output$anova_plot <- renderPlot({
    MLreports <- MLreports()
    plot(anova(MLreports$mdl_obj))
  })
  output$infer_plot <- renderPlot({
    MLreports <- MLreports()
    ggpubr::ggarrange(MLreports$cali_plot,
                      MLreports$time_pred_plot,
                      ncol=2,nrow=1)
  })
  output$model_tbl <- renderTable({
    MLreports <- MLreports()
    MLreports$model_tbl # model info table
  })
  output$cv_eval_trace_tbl <- renderTable({
    MLreports <- MLreports()
    MLreports$cv_eval_trace_tbl # model info table
  })
  output$score_tbl <- renderTable({
    MLreports <- MLreports()
    MLreports$score_tbl # model score table
  })
  output$model_prt <- renderPrint({
    MLreports <- MLreports()
    print(MLreports$mdl_obj) # model score table
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
  
  
  # ---- 3. unsupervised ml ----

})
