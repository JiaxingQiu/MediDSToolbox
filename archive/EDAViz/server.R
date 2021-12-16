
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
    stats1dViz <- eventReactive(input$stats1d_go, {
      front_viz_1d_stats(data = subset_df(data_viz,"40w"),
                      dict_data = dict_viz,
                      trim_by_label = input$trim_by_label,
                      trim_vec = as.numeric(input$trim_vec),
                      time_unit = 7,
                      pctcut_num_labels = input$pctcut_num_labels,
                      pctcut_num_vec = as.numeric(input$pctcut_num_vec),
                      coerce = input$coerce,
                      filter_tag_labels = input$filter_tag_labels,
                      y_label = input$y_label_stats1d,
                      x_label = input$x_label_stats1d,
                      group_by_label = input$group_by_label_stats1d
      ) 
    })
    
    stats2dViz <- eventReactive(input$stats2d_go, {
      front_viz_2d_stats(data = subset_df(data_viz,"40w"),
                         dict_data = dict_viz,
                         trim_by_label = input$trim_by_label,
                         trim_vec = as.numeric(input$trim_vec),
                         time_unit = 7,
                         pctcut_num_labels = input$pctcut_num_labels,
                         pctcut_num_vec = as.numeric(input$pctcut_num_vec),
                         coerce = input$coerce,
                         filter_tag_labels = input$filter_tag_labels,
                         y_label = input$y_label_stats2d,
                         x_label1 = input$x_label1_stats2d,
                         x_label2 = input$x_label2_stats2d
      ) 
    })
  
    starViz <- eventReactive(input$star_go, {
      front_viz_death_star(data = data_viz,
                      dict_data = dict_viz,
                      trim_by_label = input$trim_by_label,
                      trim_vec = as.numeric(input$trim_vec),
                      time_unit = 7,
                      pctcut_num_labels = input$pctcut_num_labels,
                      pctcut_num_vec = as.numeric(input$pctcut_num_vec),
                      coerce = input$coerce,
                      filter_tag_labels = input$filter_tag_labels,
                      y_label = input$y_label_star,
                      group_by_label = input$group_by_label_star,
                      tag_label = input$tag_label, 
                      sort_by_label = input$sort_by_label, 
                      align_by_label = input$align_by_label, 
                      scale = input$scale
      ) 
    })
    
    alluvialViz <- eventReactive(input$allu_go, {
      front_viz_alluvial(data = subset_df(data_viz,"40w"),
                         dict_data = dict_viz,
                         trim_by_label = input$trim_by_label,
                         trim_vec = as.numeric(input$trim_vec),
                         time_unit = 7,
                         pctcut_num_labels = input$pctcut_num_labels,
                         pctcut_num_vec = as.numeric(input$pctcut_num_vec),
                         coerce = input$coerce,
                         filter_tag_labels = input$filter_tag_labels,
                         # alluvial plot
                         time_label = input$time_label_allu, 
                         time_breaks = input$time_breaks_allu, 
                         time_quantity = input$time_quantity_allu, 
                         y_label = input$y_label_allu,
                         cluster_label = input$cluster_label_allu,
                         tag_labels = input$tag_labels_allu
                         
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
    
    output$dictionary_table <- renderDataTable(dict_viz[which(dict_viz$type!=""),c("varname", "label", "unit", "type")])
    
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
    
    # dictInput <- reactive({
    #   switch(input$dictionary5,
    #          "dict_deid_data_final" = dict_org,
    #          "dict_deid_data_viz" = dict_viz
    #          )
    # })
    # 
    # output$downloadDict <- downloadHandler(
    #   filename = function() {
    #     paste(input$dictionary4, ".csv", sep = "")
    #   },
    #   content = function(file) {
    #     write.csv(dictInput(), file, row.names = FALSE)
    #   }
    # )
    # 
    # datasetInput <- reactive({
    #   switch(input$dataset5,
    #          "deid_data_final" = data_org,
    #          "deid_data_viz" = data_viz
    #          )
    # })
    # output$downloadData <- downloadHandler(
    #     filename = function() {
    #         paste(input$dataset5, ".csv", sep = "")
    #     },
    #     content = function(file) {
    #       write.csv(datasetInput(), file, row.names = FALSE)
    #     }
    #   )
    

})
