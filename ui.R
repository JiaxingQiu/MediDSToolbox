library(shinydashboard)
library(shinyjs)
library(shinydashboardPlus)
library(shiny)
source("./shiny.R")


#### maintain global constants ####
if(!exists("time_over_labels")){
  time_over_labels <- c("Fake Time Index")
}
if(!exists("prj_name")){
  prj_name <- "Unnamed Project -- You can specify project name / desciption / link in shiny.R script"
}
if(!exists("prj_link")){
  prj_link <- "https://github.com/JiaxingQiu/MediDSToolbox"
}

#### header ####
header <- dashboardHeader(
  title = HTML("Medical Data Science Toolbox"),
  disable = FALSE,
  titleWidth = 333
)

sidebar <- dashboardSidebar(
  width = 333,
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",
    
    # ---- -1. toolbox info ----
    menuItem("About", tabName = "about"),
    
    # ---- 0. project info ----
    menuItem("Project Info", tabName = "prj_info"),
    
    # ---- 1. setup ----
    menuItem("Set up", tabName = "setup", startExpanded = FALSE,
             menuSubItem('Engineer', tabName = 'setup_engineer'),
             menuSubItem('Summary', tabName = 'setup_summ')),
    useShinyjs(),
    div(id = 'sidebar_setup_engineer',
        conditionalPanel("input.sidebar == 'setup_engineer'"
        )),
    div(id = 'sidebar_setup_summ',
        conditionalPanel("input.sidebar == 'setup_summ'", 
                         selectInput("setup_strat_by",
                                     "Stratified by",
                                     choices = c()),
                         selectInput("setup_summ_aggregate_per", 
                                     "Aggregate by", 
                                     choices=c("None"="row", 
                                               "Time Unit"="cluster_trim_by_unit", 
                                               "Cluster"="cluster"),
                                     selected = "cluster"),
                         checkboxInput("setup_trim_ctrl",
                                       "Trim Control Group",
                                       TRUE)
        )),
    # ---- 2. eda ----
    menuItem("Explore Data", tabName = "eda", startExpanded = FALSE,
             menuSubItem('1D Stats', tabName = 'eda_1dstats'),
             menuSubItem('2D Stats', tabName = 'eda_2dstats'),
             menuSubItem('Death Star', tabName = 'eda_star'),
             menuSubItem('Alluvial Flow', tabName = 'eda_allu')),
    ## Show panel only when sidebar is selected
    useShinyjs(),
    div(id = 'sidebar_eda_1dstats',
        conditionalPanel("input.sidebar == 'eda_1dstats'",
                         selectInput("eda_y_label_stats1d",
                                     "Response (y)",
                                     choices = c()),
                         selectInput("eda_x_label_stats1d",
                                     "Explainer (x)",
                                     choices = c()),
                         selectInput("eda_group_by_label_stats1d",
                                     label="Group by",
                                     choices = c())
        )),
    div(id = 'sidebar_eda_2dstats',
        conditionalPanel("input.sidebar == 'eda_2dstats'",
                         selectInput("eda_y_label_stats2d",
                                     "Response (heat)",
                                     choices = c()),
                         selectInput("eda_x_label1_stats2d",
                                     "Explainer 1 (x)",
                                     choices = c()),
                         selectInput("eda_x_label2_stats2d",
                                     "Explainer 2 (y)",
                                     choices = c()),
                         selectInput("eda_group_by_label_stats2d",
                                     "Group by",
                                     choices = c())
        )),
    div(id = 'sidebar_eda_star',
        conditionalPanel("input.sidebar == 'eda_star'",
                         selectInput("eda_y_label_star",
                                     "Response (heat)",
                                     choices = c()),
                         selectInput("eda_sort_by_label",
                                     label = "Sort by length of",
                                     choices = time_over_labels),
                         # selectInput("eda_align_by_label",
                         #             label = "Explainer (x)",
                         #             choices = time_over_labels),
                         selectInput("eda_group_by_label_star",
                                     label = "Group by",
                                     choices = c()),
                         selectInput("eda_tag_label",
                                     label = "Tag",
                                     choices = c()), 
                         radioButtons("eda_scale", 
                                      label = "Scale",
                                      choices = c('Raw', 'Percentile (2D)', 'Percentile (1D)'),
                                      selected="Raw",
                                      inline = TRUE),
                         helpText("Brush and double-click to zoom in/out.")
        )),
    div(id = 'sidebar_eda_allu',
        conditionalPanel("input.sidebar == 'eda_allu'",
                         selectInput("eda_y_label_allu",
                                     label="Numeric Percentiles",
                                     choices = c() ),
                         selectInput("eda_tag_labels_allu",
                                     label="Binary Status",
                                     multiple = TRUE,
                                     choices = c()),
                         # radioButtons("eda_time_quantity_allu",
                         #              "Quantify by",
                         #              c("Average"="average","1st Record"="1st record"),
                         #              inline = TRUE),
                         checkboxGroupInput("eda_time_breaks_allu",
                                            "Time breaks",
                                            choices = seq(0,40,2),
                                            inline = TRUE),
                         checkboxInput("eda_includeNA_allu",
                                       "Include NA",
                                       TRUE)
        )),
    
    # ---- 3. supervised ml ----
    menuItem("ML (supervised)", tabName = "ml", startExpanded = FALSE,
             menuSubItem('Select X and Y', tabName = 'ml_setup'),
             menuSubItem('Uni-Predictor Effect', tabName = 'ml_uni'), # univariable regression
             menuSubItem('Multi-Predictors Clus', tabName = 'ml_clus'),
             menuSubItem('RCS LASSO Regression', tabName = 'ml_select'),
             menuSubItem('RCS Ridge Regression', tabName = 'ml_multi'), # multivariable regression
             menuSubItem('RCS Ridge Regression over Time', tabName = 'ml_timely')),
    ## Show panel only when sidebar is selected
    useShinyjs(),
    div(id = 'sidebar_ml_setup',
        conditionalPanel("input.sidebar == 'ml_setup'",
                         
                         checkboxInput("ml_trim_ctrl", 
                                       "Trim Control Group", 
                                       value = TRUE),
                        
                         fileInput("ex_test_csv", "External Test Dataset(.csv)",
                                   multiple = FALSE,
                                   accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
        )),
    div(id = 'sidebar_ml_uni',
        conditionalPanel("input.sidebar == 'ml_uni'",
                         checkboxInput("ml_uni_pct",
                                       "X by percentile",
                                       value = TRUE),
                         checkboxInput("ml_uni_custom_heat",
                                       "Customize Heat Limits",
                                       value = FALSE),
                         conditionalPanel(
                           condition = "input.ml_uni_custom_heat",
                           sliderInput("ml_uni_heat_limits",
                                       label = NULL,
                                       min = 0,  max = 10, step = 0.1, value = c(0,10)),
                           
                         ),
                         selectInput("ml_method",
                                     "Method (smoother)",
                                     choices = c(
                                       "Y - Average Estimated" = "mean",
                                       "Y - RCS Smoothed" = "logit_rcs", 
                                       "Y - LOESS Smoothed" = "loess", 
                                       "Y - Bootstrap Estimated" = "bootstrap",
                                       "X - Data Availability" = 'Kernel Density Estimates'),
                                     selected = "logit_rcs"),
                         selectInput("ml_num_adjust_label",
                                     "Adjust for",
                                     choices = c()),
                         selectInput("ml_uni_group_label",
                                     "Group by",
                                     choices = c()),
                         selectInput("ml_num_labels",
                                     "Numeric predictors",
                                     multiple = TRUE,
                                     choices = c())
                         # ,
                         # sliderInput("ml_uni_sample_per_cluster",
                         #             label = "Sample per cluster",
                         #             min = 1,  max = 200, step = 1, value = 20)
                         
        )),
    div(id = 'sidebar_ml_select',
        conditionalPanel("input.sidebar == 'ml_select'",
                         selectInput("ml_select_lasso_by",
                                     "Special LASSO",
                                     choices = c(
                                       "None" = "none",
                                       "Grouped" = "group",
                                       "Clustered" = "cluster"
                                     ),
                                     selected="group"),
                         checkboxInput("ml_select_return_performance",
                                       "Return Performance",
                                       value = FALSE),
                         selectInput("ml_select_tune_by",
                                     "Tune by",
                                     choices=c("cvAUROC"="auc",
                                               "cvLogloss"="logloss",
                                               "MisClassError"="misclass",
                                               "AIC"="AIC",
                                               "BIC"="BIC")),
                         selectInput("ml_select_lambda",
                                     "Optimal penalty",
                                     choices = c(
                                       "Automated" = "auto",
                                       "lambda.min" = "min",
                                       "lambda.1se" = "1se"
                                     ),
                                     selected="auto")
                         
        )),
    div(id = 'sidebar_ml_clus',
        conditionalPanel("input.sidebar == 'ml_clus'",
                         helpText(">>> ------ Correlation ------"),
                         selectInput("ml_type",
                                     label = NULL,
                                     choices = c("pearson","spearman")),
                         sliderInput("ml_r_abs",
                                     label = "Max |pearson r| or |spearman rho|",
                                     min = 0,  max = 1, step = 0.05, value = 0.9),
                         helpText(">>> ------ Redundancy ------"),
                         sliderInput("ml_r2",
                                     label = "Max spearman rho^2",
                                     min = 0,  max = 1, step = 0.05, value = 0.9),
                         helpText(">>> ------ Missingness ------"),
                         sliderInput("ml_na_frac_max",
                                     "Max NA fraction",
                                     min = 0,  max = 1, step = 0.1, value = 0.5),
                         helpText(">>> ------ Degree of Freedom ------"),
                         sliderInput("ml_rcs_vec",
                                     label="# knots breaks by rho^2 rank",
                                     min = 0,  max = 100, step = 5, value = c(30, 50))
        )),
    div(id = 'sidebar_ml_multi',
        conditionalPanel("input.sidebar == 'ml_multi'",
                         radioButtons("ml_cv_nfold",
                                      "CV folds",
                                      choices = c("1", "5", "10"),#, "2"
                                      selected = c("10"),
                                      inline = TRUE),
                         checkboxInput("ml_stratified_cv", 
                                       "Stratified CV", 
                                       value = TRUE),
                         checkboxInput("ml_fix_knots", 
                                       "Fix Knots X", 
                                       value = TRUE),
                         selectInput("ml_tune_by",
                                     "Tune by",
                                     choices=c("cvAUROC"="auroc",
                                               "cvLogloss"="logloss",
                                               "AIC"="aic",
                                               "BIC"="bic")),
                         selectInput("ml_joint_col2_label",
                                     "Joint effect with",
                                     choices = c("None"))
        )),
    div(id = 'sidebar_ml_timely',
        conditionalPanel("input.sidebar == 'ml_timely'",
                         helpText(">>> ------ Train and Validation ------"),
                         fluidRow(
                           column(width=6, numericInput("ml_window_size", "Train-set size", 1)),
                           column(width=6, numericInput("ml_step_size", "Step", 1))
                         ),
                         helpText(">>> ------ Test and Forecasting ------"),
                         fluidRow(
                           column(width=6, numericInput("ml_test_size", "Test-set size", 1)),
                           column(width=6, numericInput("ml_lag_size", "Lag", 1))
                         )
                         
        )),
    
    # ---- 4. unsupervised ml (unml) ----
    menuItem("ML (unsupervised)", tabName = "unml", startExpanded = FALSE,
             menuSubItem('Kmeans Clustering', tabName = 'unml_cluster')
    ),
    ## Show panel only when sidebar is selected
    useShinyjs(),
    div(id = 'sidebar_unml_cluster',
        conditionalPanel("input.sidebar == 'unml_cluster'",
                         sliderInput("unml_nc_vec",
                                     label="Number of Clusters [ min, max ]",
                                     min = 1,  max = 20, step = 1, value = c(1,10)),
                         fluidRow(
                           column(width=6,numericInput("unml_min_nobs_per_clst", label="Min #obs in each cluster", 1)),
                           column(width=6,numericInput("unml_max_iter", "Max #iter to remove outliers", 5))
                         ),
                         selectInput("unml_input_labels",
                                     "Predictors",
                                     multiple = TRUE,
                                     choices = c())
        ))
  )
)

body <- dashboardBody(
  tabItems(
    # ---- -1. about ----
    tabItem(tabName = "about",
            ## we can customize our shiny UI with HTML using alist of function named "tags"
            ## each function in the list create an HTML tag to use in the layout
            ## documentation is here: https://shiny.rstudio.com/articles/tag-glossary.html # names(tags)
            tags$h1("Medical Data Science Toolbox"),
            tags$blockquote("For tools in the box, we think outside the box. ", cite = "Jiaxing Qiu"),
            
            tags$p(tags$b("Author: "),"Jiaxing (Joy) Qiu, M.S in Data Science, University of Virginia"),
            tags$p("Thanks to the Center for Advanced Medical Analytics (CAMA), School of Medicine, University of Virginia."),
            
            
            tags$h2("Objectives"),
            tags$p("The objectives of this toolbox include --"),
            tags$ol(
              tags$li("conduct statistical learning in ",
                      tags$b("medical"),
                      " context ",
                      tags$b("ethically and correctly")),
              tags$li("facilitate medical research hypothesis generation"),
              tags$li("bridge communitations between clinicians and technicians interactively"),
              tags$li("visualize biomedical research data and especially longitudinal datasets"),
              tags$li("provide baseline predictive models by supervised learning, such as logistic and linear regression"),
              tags$li("provide reliable model evaluation and rubost inference results that adjust for", 
                      tags$b("repeated measures"),
                      "commonly seen in biomedical research"),
              tags$li("provide clustering results by unsupervised learning, potentially reduce cost of medical data science research")
            ),
            tags$h3("Exploratory Data Analysis"),
            tags$em("Exploring your data does as much harm as not to explore it at all. -- Frank E Harrell"),
            tags$p("Interactively visualize --",
                   tags$ul(
                     tags$li("the major statistical charactoristics of 1 variable"), 
                     tags$li("the relationship between 2 or more variables"), 
                     tags$li("the trajectory of each subject in a medical research cohort (maximum population 1000)")
                   )),
            tags$hr(),
            tags$h3("Supervised Machine Learning"),
            tags$p("Following Regression Modeling Strstegy by Frank E Harrell, compute restricted cubic spline regression that satisfys most user cased in medical machine learning.",
                   tags$ul(
                     tags$li("train restricted cubic spline logistic regression if binary response is selected;"), 
                     tags$li("train restricted cubic spline linear regression if continuous numeric response is selected;"), 
                     tags$li("train non-linear univariate regression, and plot effect probablity against the percentile of each variable in heatmap."),
                     tags$li("explore clues of predictor variables in terms of predicting the response variable, including correlation information, redundancy analysis, missingness and spearman squared correlation information each predictor carry;"), 
                     tags$li("model development and evaluation use subject-wise 5-10 fold cross-validation;"),
                     tags$li("train multiple models every defined step size using given window size, report model performance and featuer importance overtime;")
                     
                   )),
            tags$hr(),
            tags$h3("Unsupervised Machine Learning"),
            tags$p("Use unsupervised learning methods, such as K-means --",
                   tags$ul(
                     tags$li("to cluster row-wise record, i.e. infant daily records, by optimizing the objective functions of a given mothed such as k-means, 
                             based on customized high-dimensional input variables"), 
                     tags$li("to detect outliers, by splitting minor cluster(s) from major cluster(s)"), 
                     tags$li("to explore and describe distinct charactoristics / reason of in clustered groups")
                   )
            )
    ),
    
    # ---- 0.project info ----
    tabItem(tabName = "prj_info",
            tags$h1(prj_name),
            tags$p("Click ",
                   tags$a(href = prj_link, "here"),
                   " for more information.")
    ),
    # ---- 1. setup ----
    tabItem(tabName = "setup_engineer",
            h3(" Set Up -- Engineer"),
            tabsetPanel(type = "tabs", selected = "Data",
                        tabPanel("Dictionary",
                                 selectInput("setup_source_file",
                                             label="Include source(s)",
                                             choices = unique(dict_ml$source_file),
                                             selected = unique(dict_ml$source_file),
                                             multiple = TRUE),
                                 dataTableOutput("dictionary_setup")
                        ),
                        tabPanel("Data",
                                 hr(),
                                 tags$h4("--- Step 1 ---"),
                                 hr(),
                                 fluidRow(
                                   column(4, style='border-right: 1px solid grey',
                                          selectInput("setup_cluster_label",
                                                      label="Cluster by",
                                                      choices = dict_ml$label[which(dict_ml$type=="key")] ),
                                          selectInput("setup_trim_by_label",
                                                      label="Trim time by",
                                                      choices = time_over_labels),
                                          numericInput("setup_trim_time_unit", "/", 1),
                                          sliderInput("setup_trim_vec",
                                                      label="[ from, to )",
                                                      min = 0,  max = 100, step = 1, value = c(0, 99))
                                   ),
                                   column(4, style='border-right: 1px solid grey',
                                          selectInput("setup_pctcut_num_labels",
                                                      label="Numeric cutoffs",
                                                      multiple = TRUE,
                                                      selected = NULL,
                                                      choices = c()),
                                          sliderInput("setup_pctcut_num_vec",
                                                      label="percentile [ from (th), to (th) ]",
                                                      min = 0,  max = 100, step = 0.1, value = c(0.1, 99.9)),
                                          checkboxInput("setup_pctcut_num_coerce",
                                                        "Coerce Outliers",
                                                        value = FALSE)
                                   ),
                                   column(4,
                                          selectInput("setup_filter_tag_labels",
                                                      label="Binary filter(s)",
                                                      multiple = TRUE,
                                                      selected = NULL,
                                                      choices = c()) 
                                   )
                                 ),
                                 hr(),
                                 tags$h4("--- Step 2 ---"),
                                 hr(),
                                 fluidRow(
                                   column(4, style='border-right: 1px solid grey', 
                                          selectInput("setup_aggregate_per", 
                                                      "Aggregate by", 
                                                      choices=c("None"="row", 
                                                                "Time Unit"="cluster_trim_by_unit", 
                                                                "Cluster"="cluster")),
                                          checkboxInput("setup_winsorizing",
                                                        "Winsorize all numeric",
                                                        value = FALSE)
                                   ),
                                   column(4, style='border-right: 1px solid grey',
                                          selectInput("setup_imputation",
                                                      "Imputation",
                                                      choices = c("Zero", "None", "Mean", "Median"),
                                                      selected = "None"),
                                          checkboxInput("setup_impute_per_cluster", 
                                                        "Impute within clusters", 
                                                        value = FALSE)
                                   )
                                 )
                        )
                        
            )
    ),
    tabItem(tabName = "setup_summ",
            h3("Engineering -- Summary Table"),
            fluidRow(column(1,actionButton("setup_summ_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Summary",
                                 downloadButton("download_summary_table","csv"),
                                 #plotOutput("na_plot", width ="2000px", height = "500px"),
                                 tags$p(tags$b("Numeric ---")," N= ; Mean(sd);  Median [25th, 75th] (min, max)"),
                                 tags$p(tags$b("Factor ---"), " % (n/N), where n is count of data belong to a category, N is count of non-missing data"),
                                 dataTableOutput("summary_table")
                        ),
                        tabPanel("Numeric",
                                 downloadButton("download_num_detail_table","csv"),
                                 dataTableOutput("num_detail_table")
                        ),
                        tabPanel("Factor",
                                 downloadButton("download_fct_detail_table","csv"),
                                 dataTableOutput("fct_detail_table")
                        ),
                        tabPanel("StratifyBy",
                                 dataTableOutput("rsps_table")
                        )
            )
    ),
    # ---- 2. eda ----
    tabItem(tabName = "eda_1dstats",
            h3("Exploratory Data Analysis -- 1D Statistics"),
            fluidRow(column(1,actionButton("eda_stats1d_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Mean", 
                                 plotOutput("eda_1d_p_mean",
                                            height = "500px",
                                            dblclick = "eda_1d_p_mean_dblclick",
                                            brush = brushOpts(
                                              id = "eda_1d_p_mean_brush",
                                              resetOnNew = TRUE))
                        ),
                        tabPanel("Percentiles", 
                                 plotOutput("eda_1d_p_pct",
                                            height = "500px",
                                            dblclick = "eda_1d_p_pct_dblclick",
                                            brush = brushOpts(
                                              id = "eda_1d_p_pct_brush",
                                              resetOnNew = TRUE))
                        ),
                        tabPanel("Denomenator", 
                                 plotOutput("eda_1d_p_denom",
                                            height = "500px",
                                            dblclick = "eda_1d_p_denom_dblclick",
                                            brush = brushOpts(
                                              id = "eda_1d_p_denom_brush",
                                              resetOnNew = TRUE))
                        ),
                        tabPanel("Violin Box", 
                                 plotOutput("eda_1d_p_violin",
                                            height = "500px",
                                            dblclick = "eda_1d_p_violin_dblclick",
                                            brush = brushOpts(
                                              id = "eda_1d_p_violin_brush",
                                              resetOnNew = TRUE))
                        ),
                        tabPanel("Statistics Table",
                                 fluidRow(column(4, selectInput("eda_1d_p_1stat_sttname",
                                                                label=NULL,
                                                                choices = c("q50", "avg", "q25" , "q75", "q90", "q95"))),
                                          column(3, downloadButton("download_eda_1d_df_summ","csv"))
                                 ),
                                 plotOutput("eda_1d_p_1stat_stt",height = "400px"),
                                 dataTableOutput("eda_1d_df_summ")
                        )
            )
    ),
    tabItem(tabName = "eda_2dstats",
            h3("Exploratory Data Analysis -- LOESS 2D Statistics"),
            fluidRow(column(1,actionButton("eda_stats2d_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("plot_2d_stats", height = "1000px"))),
    tabItem(tabName = "eda_star",
            h3("Exploratory Data Analysis -- Death Star"),
            fluidRow(column(1,actionButton("eda_star_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("plot_death_star",
                                height = "800px",
                                width="700px",
                                dblclick = "plot_death_star_dblclick",
                                brush = brushOpts(
                                  id = "plot_death_star_brush",
                                  resetOnNew = TRUE)))),
    tabItem(tabName = "eda_allu",
            h3("Exploratory Data Analysis -- Alluvial Flow"),
            fluidRow(column(1,actionButton("eda_allu_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("plot_alluvial", height = "800px"))),
    
    # ---- 3. ml (supervised) ----
    tabItem(tabName = "ml_setup",
            h3("Machine Learning (supervised) -- Set up"),
            tabsetPanel(type = "tabs",
                        tabPanel("Setup", 
                                 hr(),
                                 h4("--- Response ---"),
                                 hr(),
                                 fluidRow(column(6,
                                                 selectInput("ml_y_label",
                                                             "Response",width = 400,
                                                             choices = c())),
                                          column(3,
                                                 selectInput("ml_y_map_func", 
                                                             "type", 
                                                             choices = c("fold_risk", "probability", "log_odds")  ) ),
                                          column(2,
                                                 numericInput("ml_y_max", "max", 10)
                                                 )
                                          ),
                                 hr(),
                                 h4("--- Predictors ---"),
                                 hr(),
                                 fluidRow(column(6, 
                                                 selectInput("ml_nonlin_rcs5_labels",
                                                             "Restricted Cubic Spline (5 knots)",
                                                             multiple = TRUE, width = 400,
                                                             choices = c()),
                                                 selectInput("ml_nonlin_rcs4_labels",
                                                             "Restricted Cubic Spline (4 knots)",
                                                             multiple = TRUE, width = 400,
                                                             choices = c()),
                                                 selectInput("ml_nonlin_rcs3_labels",
                                                             "Restricted Cubic Spline (3 knots)",
                                                             multiple = TRUE, width = 400,
                                                             choices = c()),
                                                 selectInput("ml_linear_num_labels",
                                                             "Continous (linear)",
                                                             multiple = TRUE, width = 400,
                                                             choices = c())
                                          ),
                                          column(6,
                                                 selectInput("ml_fct_labels_mdl",
                                                             "Discrete (categorical)",
                                                             multiple = TRUE, width = 400,
                                                             choices = c()),
                                                 selectInput("ml_tag_labels_mdl",
                                                             "Discrete (binary)",
                                                             multiple = TRUE, width = 400,
                                                             choices = c())
                                                 )
                                          )
                                 ),
                        tabPanel("Dictionary", 
                                 dataTableOutput("dictionary_table_ml"))
            )
    ),
    
    tabItem(tabName = "ml_uni",
            h3("Machine Learning (supervised) -- Univariable Percentile Heatmap"),
            fluidRow(column(1,actionButton("ml_uni_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("plot_uniheat", height = "800px"))),
    tabItem(tabName = "ml_select",
            h3("Machine Learning (supervised) -- RCS LASSO Regression (Feature Selection)"),
            fluidRow(column(1,actionButton("ml_select_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Selection",
                                 plotOutput("ml_select_tune_trace_plot", height = "400px"),
                                 plotOutput("ml_select_coef_trace_plot", height = "400px"),
                                 tableOutput("ml_select_opt_model_df")
                        ),
                        tabPanel("Performance",
                                 fluidRow(column(3,selectInput("perform_from_lasso",
                                                               label=NULL,
                                                               choices = c("Internal", "External")) ),
                                          column(3,selectInput("perform_dataset_lasso",
                                                               label=NULL,
                                                               choices = c("Engineered", "Original")) ),
                                          downloadButton("perform_download_df_hat_lasso","Y hat (.csv)"),
                                          downloadButton("perform_download_scores_tbl_lasso","X rank (.csv)")
                                 ),
                                 fluidRow(column(6, plotOutput("perform_cali_plot_lasso",height = "400px")),
                                          column(6, plotOutput("perform_tte_plot_lasso",height = "400px"))
                                          ),
                                 plotOutput("perform_tradeoff_plot_lasso", height = "300px"),
                                 plotOutput("perform_scores_plot_lasso",height = "600px"),
                                 tableOutput("perform_scores_tbl_lasso"),
                                 plotOutput("perform_fitted_eff_plot_lasso",height = "1000px")
                        )
                        )),
    
    tabItem(tabName = "ml_clus",
            h3("Machine Learning (supervised) -- Predictor clus"),
            fluidRow(column(1,actionButton("ml_clus_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Correlation", 
                                 tableOutput("x_corre_trace"),
                                 tableOutput("x_corre_df_org"),
                                 textOutput("x_corre_in"),
                                 textOutput("x_corre_out")),
                        tabPanel("Redundancy", verbatimTextOutput("x_redun_obj")),
                        tabPanel("Missingness",
                                 plotOutput("ml_na_plot", width ="400px"),
                                 dataTableOutput("ml_summary_table")),
                        tabPanel("Spearman2 dof",
                                 plotOutput("dof_plot")))),
    tabItem(tabName = "ml_multi",
            h3("Machine Learning (supervised) -- RCS Ridge Regression (Inference)"),
            fluidRow(column(1, actionButton("ml_multi_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Development", 
                                 tableOutput("devel_penal_trace_tbl"),
                                 tableOutput("devel_model_info_tbl")
                                 # ,
                                 # tableOutput("devel_score_summ_tbl")
                        ),
                        tabPanel("Inference", 
                                 downloadButton("devel_download_mdl","Model (.rda)"),
                                 plotOutput("infer_effect_plot_1d_continuous",height = "600px"),
                                 plotOutput("infer_effect_plot_1d_discrete"),
                                 plotOutput("infer_anova_plot"),
                                 verbatimTextOutput("infer_model_prt"),
                                 plotOutput("infer_effect_plot_2d",height = "1000px")
                        ),
                        tabPanel("Performance",
                                 fluidRow(column(3,selectInput("perform_from",
                                                             label=NULL,
                                                             choices = c("Internal", "External")) ),
                                          column(3,selectInput("perform_dataset",
                                                             label=NULL,
                                                             choices = c("Engineered", "Original")) ),
                                          downloadButton("perform_download_df_hat","Y hat (.csv)"),
                                          downloadButton("perform_download_scores_tbl","X rank (.csv)")
                                 ),
                                 fluidRow(column(6, plotOutput("perform_cali_plot",height = "400px")),
                                          column(6, plotOutput("perform_tte_plot",height = "400px"))
                                 ),
                                 plotOutput("perform_tradeoff_plot", height = "300px"),
                                 plotOutput("perform_scores_plot",height = "600px"),
                                 tableOutput("perform_scores_tbl"),
                                 plotOutput("perform_fitted_eff_plot",height = "1000px")
                        )
            )
    ),
    tabItem(tabName = "ml_timely",
            h3("Machine Learning (supervised) -- Regression over time"),
            fluidRow(column(1,actionButton("ml_timely_go", "Go",icon=icon("play-circle"))) ),
            tabsetPanel(type = "tabs",
                        tabPanel("Distribution",
                                 plotOutput("timely_freq_plot"), 
                                 tableOutput("timely_freq_table") ),
                        tabPanel("Performance", 
                                 plotOutput("timely_score_plot"),
                                 tableOutput("timely_score_table") ),
                        tabPanel("Forecasting", 
                                 plotOutput("timely_test_plot"),
                                 tableOutput("timely_test_table") ),
                        tabPanel("Chi-Square", 
                                 plotOutput("timely_infer_plot"),
                                 tableOutput("timely_infer_table") )
            )
    ),
    # ---- 4. unml (unsupervised) ----
    tabItem(tabName = "unml_setup",
            h3("Machine Learning (unsupervised)"),
            fluidRow(dataTableOutput("dictionary_table_unml")) 
    ),
    tabItem(tabName = "unml_cluster",
            h3("Machine Learning (unsupervised) -- Clustering"),
            fluidRow(column(1,actionButton("umml_cluster_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Trace",
                                 plotOutput("unml_wss_plot") ),
                        tabPanel("Centriods", 
                                 plotOutput("unml_cluster_pca_plot"),
                                 dataTableOutput("unml_df_cluster_info") ),
                        tabPanel("Abnormal",
                                 dataTableOutput("unml_df_minor_org_trace") )
            )
            
    )
    
    # ---- 5. other businesses ---- 
  )
)


ui <- dashboardPage(header, sidebar, body)

