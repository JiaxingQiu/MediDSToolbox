library(shiny)
source("./shiny.R")


shinyUI(navbarPage("MVTS EDA",
                   tabPanel(
                     "Setup",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "trim_by_label",
                           label="Trim by",
                           choices = c("Chronological Age", "Post-menstrual Age"),
                           selected = "Post-menstrual Age"
                         ),
                         helpText("week [from, to)"),
                         sliderInput(
                           "trim_vec",
                           label=NULL,
                           min = 0,  max = 40, step = 1, value = c(24, 37)
                         ),
                         hr(),
                         selectInput(
                           "pctcut_num_labels",
                           label="Numeric cutoffs",
                           multiple = TRUE,
                           choices = dict_viz$label_front[which(dict_viz$type=="num")],
                           selected = NULL
                         ),
                         helpText("percentile [from (th), to (th)]"),
                         sliderInput(
                           "pctcut_num_vec",
                           label=NULL,
                           min = 0,  max = 100, step = 0.1, value = c(0.1, 99.9)
                         ),
                         checkboxInput(
                           "coerce",
                           "Coerce extremum",
                           value = TRUE
                         ),
                         hr(),
                         selectInput(
                           "filter_tag_labels",
                           label="Binary filter(s)",
                           multiple = TRUE,
                           choices = dict_viz$label_front[which(dict_viz$unit=="tag01")],
                           selected = NULL
                         )
                       ),
                       mainPanel(
                         dataTableOutput("dictionary_table")
                       )
                     )
                   ),
                   
                   tabPanel(
                     "1D Stats",
                     fluidRow(
                       column(1,
                              actionButton("stats1d_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "y_label_stats1d",
                           "Response (y)",
                           choices = dict_viz$label_front[which(dict_viz$type=="num" | (dict_viz$unit=="tag01") )],
                           selected = "Apnea_v3 number of events per day"
                         ),
                         selectInput(
                           "x_label_stats1d",
                           "Explainer (x)",
                           choices = c("Chronological Age", "Post-menstrual Age", setdiff(dict_viz$label_front[which(dict_viz$type!="")],c("Chronological Age", "Post-menstrual Age") ) ),
                           selected = "Chronological Age"
                         ),
                         helpText("Group by"),
                         selectInput(
                           "group_by_label_stats1d",
                           label=NULL,
                           choices = c("None", "Primary outcome (EN)","GA weeks","Site (EN)", setdiff(dict_viz$label_front[which(dict_viz$type=="fct")],c("Primary outcome (EN)","Site (EN)")) ),
                           selected = "None"
                         )
                         
                       ),
                       mainPanel(
                         plotOutput("plot_1d_stats", height = "1200px")
                       )
                     )
                   ),
                   tabPanel(
                     "2D Stats",
                     fluidRow(
                       column(1,
                              actionButton("stats2d_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "y_label_stats2d",
                           "Response (heat)",
                           choices = dict_viz$label_front[which(dict_viz$type=="num" | (dict_viz$unit=="tag01") )]
                         ),
                         selectInput(
                           "x_label1_stats2d",
                           "Explainer 1 (x)",
                           choices = c("Chronological Age", "Post-menstrual Age", setdiff(dict_viz$label_front[which(dict_viz$type=="num")],c("Chronological Age", "Post-menstrual Age") ) ),
                           selected = "Post-menstrual Age"
                         ),
                         selectInput(
                           "x_label2_stats2d",
                           "Explainer 2 (y)",
                           choices = c("Gestational Age", "Post-menstrual Age", setdiff(dict_viz$label_front[which(dict_viz$type=="num")],c("Gestational Age", "Post-menstrual Age") ) ),
                           selected = "Gestational Age"
                         )
                         
                       ),
                       mainPanel(
                         plotOutput("plot_2d_stats", height = "1000px")              
                       )
                     )
                   ),
                   
                   tabPanel(
                     "Death Star",
                     fluidRow(
                       column(1,
                              actionButton("star_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "y_label_star",
                           "Response (heat)",
                           choices = dict_viz$label_front[which(dict_viz$type=="num" | (dict_viz$unit=="tag01") )]
                         ),
                         selectInput(
                           "align_by_label",
                           label = "Explainer (x)",
                           choices =c("Chronological Age", "Post-menstrual Age"),
                           selected = "Post-menstrual Age"
                         ),
                         helpText("Sort by length of"),
                         selectInput(
                           "sort_by_label",
                           label = NULL,
                           choices = c("Chronological Age", "Post-menstrual Age", "Gestational Age"), 
                           selected = "Post-menstrual Age"
                         ),
                         helpText("Scale"),
                         radioButtons(
                           "scale", 
                           label = NULL,
                           choices = c('Raw', 'Percentile (2D)', 'Percentile (1D)'),
                           selected="Raw",
                           inline = TRUE
                         ),
                         helpText("Add a tag"),
                         selectInput(
                           "tag_label",
                           label = NULL,
                           choices = c("None", dict_viz$label_front[which(dict_viz$unit=="tag01")]),
                           selected = "None"
                         ),  
                         helpText("Group by"),
                         selectInput(
                           "group_by_label_star",
                           label = NULL,
                           choices = c("None", "Primary outcome (EN)","GA weeks","Site (EN)", setdiff(dict_viz$label_front[which(dict_viz$type=="fct")],c("Primary outcome (EN)","Site (EN)")) ),
                           selected = "None"
                         ),
                         helpText("Brush and double-click to zoom in, double-click again to zoom out.")
                       ),
                       mainPanel(
                         plotOutput("plot_death_star",
                                    height = "700px",
                                    dblclick = "plot_death_star_dblclick",
                                    brush = brushOpts(
                                      id = "plot_death_star_brush",
                                      resetOnNew = TRUE
                                    ))
                       )
                     )
                   ),
                   tabPanel(
                     "Alluvial Flow",
                     fluidRow(
                       column(1,
                              actionButton("allu_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "y_label_allu",
                           label="Response",
                           choices = dict_viz$label_front[which(dict_viz$type=="num")]
                         ),
                         selectInput(
                           "cluster_label_allu",
                           label="Cluster",
                           choices = dict_viz$label_front[which(dict_viz$type=="key")]
                         ),
                         selectInput(
                           "tag_labels_allu",
                           label="Add binary status",
                           choices = dict_viz$label_front[which(dict_viz$type=="fct"&dict_viz$unit=="tag01")],
                           multiple = TRUE
                         ),
                         selectInput(
                           "time_label_allu",
                           label="Time by",
                           choices = c("Chronological Age", "Post-menstrual Age"),
                           selected = "Post-menstrual Age"
                         ),
                         radioButtons(
                           "time_quantity_allu",
                           "Quantify by",
                           c("Average"="average","1st Record"="1st record"),
                           inline = TRUE
                         ),
                         checkboxGroupInput(
                           "time_breaks_allu",
                           "Weeks",
                           seq(0,40,2),
                           selected = c(24,26,28,30,32,34,36,38,40),
                           inline = TRUE
                         )
                       ),
                       mainPanel(plotOutput("plot_alluvial", height = "800px"))
                     )
                   )
                   # ,
                   # tabPanel(
                   #   "Dictionary",
                   #   sidebarLayout(
                   #     sidebarPanel(
                   #       # textInput("pswd",
                   #       #           "Password:",
                   #       #           value = ""),
                   #       selectInput("dataset5",
                   #                   "Dataset (de-id):",
                   #                   choices = c(
                   #                     "deid_data_final",
                   #                     "deid_data_viz")
                   #                   ),
                   #       # downloadButton("downloadData",
                   #       #                "Download"),
                   #       hr(),
                   #       selectInput("dictionary5",
                   #                   "Dictionary:",
                   #                   choices = c(
                   #                     "dict_deid_data_final",
                   #                     "dict_deid_data_viz"
                   #                   )
                   #                   )#,
                   #       # downloadButton("downloadDict",
                   #       #                "Download")
                   # 
                   # 
                   #     ),
                   #     mainPanel(
                   #       dataTableOutput('dictionary_table')
                   #     )
                   #   )
                   # )
))






# write.csv(data.frame(Measurement=num_var_label_sorted,stringsAsFactors = FALSE), "./EDAViz/front_labels/Setup_Measurement.csv")
# write.csv(data.frame(Group_by=c("None", "Primary outcome (EN)","Site (EN)", setdiff(c(fct_var_label, tag_var_label),c("Primary outcome (EN)","Site (EN)")) ),stringsAsFactors = FALSE), "./EDAViz/front_labels/Setup_Group_by.csv")
# 
# 
# 
