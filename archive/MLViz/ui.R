
library(shiny)
source("./shiny.R")


shinyUI(navbarPage("MVTS ML (supervised)",
                   tabPanel(
                     "Setup",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "cluster_label",
                           "Cluster",
                           choices = cluster_front_labels,
                           selected = cluster_front_labels[1]
                         ),
                         selectInput(
                           "y_label",
                           "Response",
                           choices = c(y_tag_front_labels, y_num_front_labels, "Site (EN)___Miami"),
                           selected = "Primary outcome (EN)___Unfavorable"
                         ),
                         helpText("For continuous response ('num'), Linear Regression will be used. For binary response ('tag'), Logistic Regression will be used."),
                         hr(),
                         selectInput(
                           "trim_by_label",
                           "Trim by",
                           choices = c("Chronological Age", "Post-menstrual Age", "Gestational Age"),
                           selected = "Chronological Age"
                         ),
                         helpText("week [from, to)"),
                         sliderInput(
                           "trim_vec",
                           label=NULL,
                           min = 0,  max = 40, step = 1, value = c(0, 16)
                         ),
                         hr(),
                         selectInput(
                           "imputation",
                           "Imputation",
                           choices = c("Zero", "None", "Mean", "Median"),
                           selected = "Zero"
                         ),
                         checkboxInput(
                           "impute_per_cluster", 
                           "Impute by cluster", 
                           value = FALSE
                         ),
                         checkboxInput(
                           "aggregation", 
                           "Aggregate by cluster", 
                           value = TRUE
                         ),
                         checkboxInput(
                           "winsorizing",
                           "Winsorizing",
                           value = FALSE
                         )
                       ),
                       mainPanel(
                         dataTableOutput("dictionary_table")
                       )
                     )
                   ),
                   tabPanel(
                     "Summary",
                     fluidRow(
                       column(1,
                              actionButton("summ_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     fluidPage(
                       mainPanel(
                         plotOutput("na_plot", width ="2000px"),
                         dataTableOutput("summary_table")
                       )
                     )
                   ),
                   
                   tabPanel(
                     "Uni Heatmap",
                     fluidRow(
                       column(1,
                              actionButton("uni_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "method",
                           "Method (smoother)",
                           choices = c('Kernel Density Estimates', "logit_rcs", "loess", "bootstrap"),
                           selected = "logit_rcs"
                         ),
                         selectInput(
                           "num_adjust_label",
                           "Adjust for", 
                           choices = c("None","Gestational Age"),
                           selected="Gestational Age"
                         ),
                         selectInput(
                           "num_labels",
                           "Numeric predictors",
                           multiple = TRUE,
                           choices = x_num_front_labels,
                           selected = intersect(x_num_front_labels, dict_ml$label_front[which(dict_ml$source_file=="base")] )
                         )
                       ),
                       mainPanel(plotOutput("plot_uniheat", height = "800px"))
                     )
                   ),
                   tabPanel(
                     "Var Clus",
                     fluidRow(
                       column(1,
                              actionButton("clus_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         helpText("Correlation"),
                         selectInput(
                           "type",
                           label = NULL,
                           choices = c("spearman","pearson")
                         ),
                         sliderInput(
                           "r_abs",
                           label = "Cutoff (pearson r / spearman rho)",
                           min = 0,  max = 1, step = 0.05, value = 0.9),
                         hr(),
                         helpText("Redundancy"),
                         # helpText("Cutoff (R2)"),
                         sliderInput(
                           "r2",
                           label = "Cutoff (R2)",
                           min = 0,  max = 1, step = 0.05, value = 0.9),
                         hr(),
                         helpText("Missingness"),
                         sliderInput(
                           "na_frac_max",
                           "Max NA fraction",
                           min = 0,  max = 1, step = 0.1, value = 0.3),
                         hr(),
                         helpText("Spearman2 dof"),
                         sliderInput(
                           "rcs_vec",
                           label="Rcs knots breaks (r2 rank)",
                           min = 0,  max = 100, step = 5, value = c(30, 50)
                         )
                       ),
                       mainPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Correlation", 
                                              tableOutput("x_corre_trace"),
                                              tableOutput("x_corre_df_org"),
                                              textOutput("x_corre_in"),
                                              textOutput("x_corre_out")
                                     ),
                                     tabPanel("Redundancy", verbatimTextOutput("x_redun_obj")),
                                     tabPanel("Missingness",
                                              plotOutput("ml_na_plot", width ="500px"),
                                              dataTableOutput("ml_summary_table")
                                     ),
                                     tabPanel("Spearman2 dof",
                                              plotOutput("dof_plot")
                                     )
                         )
                       )
                     )
                   ),
                   tabPanel(
                     "Regression",
                     fluidRow(
                       column(1,
                              actionButton("multi_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "nonlin_num_labels",
                           "Continous (non-linear)",
                           multiple = TRUE,
                           choices = x_num_front_labels,
                           selected = c("Birth weight",
                                        "APGAR score at 1 minute",
                                        "Maternal age",
                                        "Doses of any medication today")
                         ),
                         selectInput(
                           "linear_num_labels",
                           "Continous (linear)",
                           multiple = TRUE,
                           choices = x_num_front_labels,
                           selected = c("Gestational Age") 
                         ),
                         selectInput(
                           "fct_labels_mdl",
                           "Discrete (categorical)",
                           multiple = TRUE,
                           choices = x_fct_front_labels,
                           selected = c("Site (EN)")
                         ),
                         selectInput(
                           "tag_labels_mdl",
                           "Discrete (binary)",
                           multiple = TRUE,
                           choices = x_tag_front_labels,
                           selected = c("Baby Gender (EN)___Female",
                                        "Resus at birth: CPAP (EN)___Yes",
                                        "Maternal race (EN)___Black_African_American",
                                        "Maternal ethnicity (EN)___Hispanic_or_Latino",
                                        "Any  Doses of any medication today"
                                        )
                         ),
                         radioButtons(
                           "cv_nfold",
                           "CV folds",
                           choices = c("1", "2", "5", "10"),
                           selected = c("10"),
                           inline = TRUE
                         ),
                         checkboxInput(
                           "stratified_cv", 
                           "Stratified CV", 
                           value = TRUE),
                         
                         selectInput(
                           "num_col2_label",
                           "Joint effect (continuous predictors only)",
                           choices = c("None","Gestational Age")
                         ),
                         helpText("please refer to 'Var Clus' page for correlation, redundancy, missingness and complexity information within / across predictors, for advanced model structure tuning")
                       ),
                       mainPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Evaluation", 
                                              tableOutput("model_tbl"),
                                              tableOutput("score_tbl"),
                                              tableOutput("cv_eval_trace_tbl")
                                     ),
                                     tabPanel("Inference", 
                                              plotOutput("effect_plot", height = "600px"),
                                              plotOutput("infer_plot"),
                                              plotOutput("anova_plot"),
                                              verbatimTextOutput("model_prt")
                                     )
                         )
                       )
                     )
                   ),
                   
                   tabPanel(
                     "Over Time",
                     fluidRow(
                       column(1,
                              actionButton("timely_go", "Go",icon=icon("play-circle"))
                       )      
                     ),
                     sidebarLayout(
                       sidebarPanel(
                         sliderInput(
                           "window_size",
                           "Window size (weeks)",
                           min = 1,  max = 5, step = 1, value = 1
                         ),
                         helpText("Window size defines the length of data inputted to each model."),
                         sliderInput(
                           "step_size",
                           "Step size (weeks)",
                           min = 1,  max = 5, step = 1, value = 1
                         ),
                         helpText("Step size defines the gap between two neighbour windows."),
                         hr(),
                         helpText("Time models are trained and validated at given 'Trim by' time points between '[from,to)' you defined in the 'Setup' page."),
                         helpText("Outcome group distribution, model performance and predictor importance will be reported as a function of time.")
                         
                       ),
                       mainPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Distribution",
                                              plotOutput("timely_freq_plot"), 
                                              tableOutput("timely_freq_table")
                                     ),
                                     tabPanel("Scores", 
                                              plotOutput("timely_score_plot"),
                                              tableOutput("timely_score_table")
                                     ),
                                     tabPanel("Chi-Square", 
                                              plotOutput("timely_infer_plot"),
                                              tableOutput("timely_infer_table")
                                     )
                         )
                       )
                     )
                   )
                   ))

