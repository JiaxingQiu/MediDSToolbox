library(shiny)

shinyServer(function(input, output, session) {
  
  
  XclusReports <- eventReactive(input$clus_go, {
    front_X_clus(data = subset_df(data_ml,"40w"),
                 dict_data = dict_ml,
                 trim_by_label = input$trim_by_label,
                 trim_vec = as.numeric(input$trim_vec),
                 time_unit=7,
                 x_labels=union(union(union(input$nonlin_num_labels, input$linear_num_labels),input$fct_labels_mdl),input$tag_labels_mdl), 
                 y_label=input$y_label, 
                 cluster_label=input$cluster_label,
                 r2=input$r2,
                 imputation=input$imputation,
                 impute_per_cluster=input$impute_per_cluster,
                 winsorizing=input$winsorizing,
                 aggregation=input$aggregation,
                 r_abs=input$r_abs, 
                 type=input$type,
                 rank=FALSE,
                 rcs5_low=paste0(input$rcs_vec[1],"%"),
                 rcs4_low=paste0(input$rcs_vec[2],"%")
    ) 
  })
  
  
  MLreports <- eventReactive(input$multi_go, {
    front_multi_regression(data = subset_df(data_ml,"40w"),
                           dict_data = dict_ml,
                           trim_by_label=input$trim_by_label, 
                           trim_vec=as.numeric(input$trim_vec), 
                           x_labels=union(union(union(input$nonlin_num_labels, input$linear_num_labels),input$fct_labels_mdl),input$tag_labels_mdl), 
                           y_label=input$y_label, 
                           cluster_label=input$cluster_label,
                           rcs5_low=paste0(input$rcs_vec[1],"%"),
                           rcs4_low=paste0(input$rcs_vec[2],"%"),
                           num_labels_linear=input$linear_num_labels, 
                           num_col2_label=input$num_col2_label, 
                           na_frac_max=input$na_frac_max, 
                           imputation=input$imputation,
                           winsorizing=input$winsorizing,
                           aggregation = input$aggregation,
                           stratified_cv=input$stratified_cv,
                           cv_nfold = as.numeric(input$cv_nfold),
                           time_unit=7,
                           impute_per_cluster=input$impute_per_cluster,
                           r_abs=input$r_abs, 
                           r2=input$r2,
                           type=input$type,
                           rank=FALSE,
                           seed_value=333) 
  })
  
  MLreports_timely <- eventReactive(input$timely_go, {
    front_multi_regression_timely(data = subset_df(data_ml,"40w"),
                                  dict_data = dict_ml,
                                  trim_by_label=input$trim_by_label, 
                                  trim_vec=as.numeric(input$trim_vec), 
                                  x_labels=union(union(union(input$nonlin_num_labels, input$linear_num_labels),input$fct_labels_mdl),input$tag_labels_mdl), 
                                  y_label=input$y_label, 
                                  cluster_label=input$cluster_label,
                                  rcs5_low=paste0(input$rcs_vec[1],"%"),
                                  rcs4_low=paste0(input$rcs_vec[2],"%"),
                                  num_labels_linear=input$linear_num_labels, 
                                  num_col2_label=input$num_col2_label, 
                                  na_frac_max=input$na_frac_max, 
                                  imputation=input$imputation,
                                  winsorizing=input$winsorizing,
                                  aggregation = input$aggregation,
                                  stratified_cv=input$stratified_cv,
                                  cv_nfold = as.numeric(input$cv_nfold),
                                  time_unit=7,
                                  impute_per_cluster=input$impute_per_cluster,
                                  r_abs=input$r_abs, 
                                  r2=input$r2,
                                  type=input$type,
                                  rank=FALSE,
                                  seed_value=333,
                                  window_size = input$window_size,
                                  step_size = input$step_size) 
  })
  
  summReport <- eventReactive(input$summ_go, {
    front_summary_tbl(
      data=subset_df(data_ml,"40w"),
      dict_data=dict_ml,
      trim_by_label=input$trim_by_label, 
      trim_vec=as.numeric(input$trim_vec), 
      time_unit=7,
      stratify_by=input$y_label, 
      cluster_label=input$cluster_label
    )
  })
  
  
  
  uniHeatmap <- eventReactive(input$uni_go, {
    front_multi_heatmap(data=subset_df(data_ml,"40w"),
                      dict_data=dict_ml,
                      trim_by_label=input$trim_by_label, 
                      trim_vec=as.numeric(input$trim_vec),  
                      num_labels=input$num_labels, 
                      y_label=input$y_label, 
                      cluster_label=input$cluster_label,
                      num_adjust_label=input$num_adjust_label, 
                      method=input$method, 
                      imputation=input$imputation,
                      winsorizing=input$winsorizing,
                      aggregation = input$aggregation,
                      time_unit=7,
                      impute_per_cluster=input$impute_per_cluster)
  })
  
  # ---- setup ----
  output$dictionary_table <- renderDataTable(dict_ml[which(dict_ml$mlrole!=""),c("source_file","varname","label_front","type","unit","mlrole")])
  
  # ---- Summary Table ----
  output$summary_table <- renderDataTable({
    summ_obj <- summReport()
    summ_obj$summ_df
  })
  
  output$na_plot <- renderPlot({
    summ_obj <- summReport()
    plot(summ_obj$na_obj) 
  })
  
  
  # ---- Univariate Heatmap ----
  output$plot_uniheat <- renderPlot({
    uniHeatmap()
  })
  
  
  # ---- Regression ----
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
  
  
  # ---- Variable Clus ----
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
  
  
  # ---- Timely Models ----
  
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
  
  
})
