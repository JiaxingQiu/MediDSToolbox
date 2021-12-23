library(shinydashboard)
library(shinyjs)
library(shinydashboardPlus)
library(shiny)
source("./shiny.R")
source("./shiny_io.R")

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
    
    # ---- 1. eda ----
    menuItem("Explore Data", tabName = "eda", startExpanded = FALSE,
             menuSubItem('Set up', tabName = 'eda_setup'),
             menuSubItem('1D Stats', tabName = 'eda_1dstats'),
             menuSubItem('2D Stats', tabName = 'eda_2dstats'),
             menuSubItem('Death Star', tabName = 'eda_star'),
             menuSubItem('Alluvial Flow', tabName = 'eda_allu')),
    ## Show panel only when sidebar is selected
    useShinyjs(),
    div(id = 'sidebar_eda_setup',
        conditionalPanel("input.sidebar == 'eda_setup'",
                         selectInput("eda_cluster_label",
                                     label="Cluster",
                                     choices = in.eda_cluster_label ),
                         
                         fluidRow(
                           column(width=8,selectInput("eda_trim_by_label",
                                                      label="Time over",
                                                      choices = in.eda_trim_by_label) ),
                           column(width=4,numericInput("eda_trim_time_unit", "/", in.eda_trim_time_unit))
                         ),
                         sliderInput("eda_trim_vec",
                                     label="[ from, to )",
                                     min = 0,  max = 100, step = 1, value = c(0, 99)),
                         fluidRow(
                           column(width=8, selectInput("eda_pctcut_num_labels",
                                                       label="Numeric cutoffs",
                                                       multiple = TRUE,
                                                       choices = dict_viz$label_front[which(dict_viz$type=="num")],
                                                       selected = NULL) ),
                           column(width=4, checkboxInput("eda_coerce","coerce",value = FALSE))
                         ),
                         sliderInput("eda_pctcut_num_vec",
                                     label="percentile [ from (th), to (th) ]",
                                     min = 0,  max = 100, step = 0.1, value = c(0.1, 99.9)),
                         
                         selectInput("eda_filter_tag_labels",
                                     label="Binary filter(s)",
                                     multiple = TRUE,
                                     choices = dict_viz$label_front[which(dict_viz$unit=="tag01")],
                                     selected = NULL)
        )),
    div(id = 'sidebar_eda_1dstats',
        conditionalPanel("input.sidebar == 'eda_1dstats'",
                         selectInput("eda_y_label_stats1d",
                                     "Response (y)",
                                     choices = in.eda_y_label_stats1d),
                         selectInput("eda_x_label_stats1d",
                                     "Explainer (x)",
                                     choices = in.eda_x_label_stats1d),
                         selectInput("eda_group_by_label_stats1d",
                                     label="Group by",
                                     choices = in.eda_group_by_label_stats1d)
        )),
    div(id = 'sidebar_eda_2dstats',
        conditionalPanel("input.sidebar == 'eda_2dstats'",
                         selectInput("eda_y_label_stats2d",
                                     "Response (heat)",
                                     choices = in.eda_y_label_stats2d),
                         selectInput("eda_x_label1_stats2d",
                                     "Explainer 1 (x)",
                                     choices = in.eda_x_label1_stats2d),
                         selectInput("eda_x_label2_stats2d",
                                     "Explainer 2 (y)",
                                     choices = in.eda_x_label2_stats2d)
        )),
    div(id = 'sidebar_eda_star',
        conditionalPanel("input.sidebar == 'eda_star'",
                         selectInput("eda_y_label_star",
                                     "Response (heat)",
                                     choices = in.eda_y_label_star ),
                         selectInput("eda_sort_by_label",
                                     label = "Sort by length of",
                                     choices = in.eda_sort_by_label),
                         selectInput("eda_align_by_label",
                                     label = "Explainer (x)",
                                     choices = in.eda_align_by_label),
                         selectInput("eda_group_by_label_star",
                                     label = "Group by",
                                     choices = in.eda_group_by_label_star),
                         selectInput("eda_tag_label",
                                     label = "Tag",
                                     choices = in.eda_tag_label), 
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
                                     label="Response",
                                     choices = in.eda_y_label_allu ),
                         selectInput("eda_tag_labels_allu",
                                     label="Add binary status",
                                     choices = in.eda_tag_labels_allu,
                                     multiple = TRUE),
                         radioButtons("eda_time_quantity_allu",
                                      "Quantify by",
                                      c("Average"="average","1st Record"="1st record"),
                                      inline = TRUE),
                         checkboxGroupInput("eda_time_breaks_allu",
                                            "Time breaks",
                                            choices = seq(0,40,2),
                                            inline = TRUE)
        )),
    
    # ---- 2. supervised ml ----
    menuItem("ML (supervised)", tabName = "ml", startExpanded = FALSE,
             menuSubItem('Set up', tabName = 'ml_setup'),
             menuSubItem('Summary', tabName = 'ml_summ'),
             menuSubItem('Univariate Effect', tabName = 'ml_uni'), # univariable regression
             menuSubItem('Feature Selection', tabName = 'ml_select'),
             menuSubItem('Predictor Clus', tabName = 'ml_clus'),
             menuSubItem('Regression', tabName = 'ml_multi'), # multivariable regression
             menuSubItem('Regression over Time', tabName = 'ml_timely')),
    ## Show panel only when sidebar is selected
    useShinyjs(),
    div(id = 'sidebar_ml_setup',
        conditionalPanel("input.sidebar == 'ml_setup'",
                         selectInput("ml_cluster_label",
                                     "Cluster",
                                     choices = in.ml_cluster_label),
                         selectInput("ml_y_label",
                                     "Response",
                                     choices = in.ml_y_label,
                                     selected = in.ml_y_label.selected),
                         #helpText("For continuous response ('num'), Linear Regression will be used. For binary response ('tag'), Logistic Regression will be used."),
                         fluidRow(
                           column(width=8,selectInput("ml_trim_by_label",
                                                      label="Time over",
                                                      choices = in.ml_trim_by_label) ),
                           column(width=4, numericInput("ml_trim_time_unit", "/", in.ml_trim_time_unit) )
                         ),
                         sliderInput("ml_trim_vec",
                                     label="[ from, to )",
                                     min = 0,  max = 100, step = 1, value = c(0, 99)),
                         fluidRow(
                           column(width=5,selectInput("ml_imputation",
                                                      "Imputation",
                                                      choices = c("Zero", "None", "Mean", "Median"),
                                                      selected = "None") ),
                           column(width=6,checkboxInput("ml_impute_per_cluster", "within cluster", value = FALSE))
                         ),
                         fluidRow(
                           column(width=5,checkboxInput("ml_aggregation", 
                                                        "Aggregation", 
                                                        value = FALSE) ),
                           column(width=5,checkboxInput("ml_winsorizing",
                                                        "Winsorizing",
                                                        value = FALSE))
                         )
        )),
    div(id = 'sidebar_ml_summ',
        conditionalPanel("input.sidebar == 'ml_summ'"
        )),
    div(id = 'sidebar_ml_uni',
        conditionalPanel("input.sidebar == 'ml_uni'",
                         selectInput("ml_method",
                                     "Method (smoother)",
                                     choices = c('Kernel Density Estimates', "logit_rcs", "loess", "bootstrap"),
                                     selected = "logit_rcs"),
                         selectInput("ml_num_adjust_label",
                                     "Adjust for", 
                                     choices = in.ml_num_adjust_label),
                         selectInput("ml_num_labels",
                                     "Numeric predictors",
                                     multiple = TRUE,
                                     choices = in.ml_num_labels.choices,
                                     selected = in.ml_num_labels.selected)
        )),
    div(id = 'sidebar_ml_select',
        conditionalPanel("input.sidebar == 'ml_select'",
                         # selectInput("ml_nonlin_num_labels",
                         #             "Continous (non-linear)",
                         #             multiple = TRUE,
                         #             choices = in.ml_nonlin_num_labels.choices,
                         #             selected = in.ml_nonlin_num_labels.selected ),
                         selectInput("ml_nonlin_rcs5_labels",
                                     "Restricted Cubic Spline (5 knots)",
                                     multiple = TRUE,
                                     choices = in.ml_nonlin_rcs5_labels.choices,
                                     selected = in.ml_nonlin_rcs5_labels.selected ),
                         selectInput("ml_nonlin_rcs4_labels",
                                     "Restricted Cubic Spline (4 knots)",
                                     multiple = TRUE,
                                     choices = in.ml_nonlin_rcs4_labels.choices,
                                     selected = in.ml_nonlin_rcs4_labels.selected ),
                         selectInput("ml_nonlin_rcs3_labels",
                                     "Restricted Cubic Spline (3 knots)",
                                     multiple = TRUE,
                                     choices = in.ml_nonlin_rcs3_labels.choices,
                                     selected = in.ml_nonlin_rcs3_labels.selected ),
                         selectInput("ml_linear_num_labels",
                                     "Continous (linear)",
                                     multiple = TRUE,
                                     choices = in.ml_linear_num_labels.choices,
                                     selected = in.ml_linear_num_labels.selected ),
                         selectInput("ml_fct_labels_mdl",
                                     "Discrete (categorical)",
                                     multiple = TRUE,
                                     choices = in.ml_fct_labels_mdl.choices,
                                     selected = in.ml_fct_labels_mdl.selected ),
                         selectInput("ml_tag_labels_mdl",
                                     "Discrete (binary)",
                                     multiple = TRUE,
                                     choices = in.ml_tag_labels_mdl.choices,
                                     selected = in.ml_tag_labels_mdl.selected ),
                         checkboxInput("ml_select_standardize", 
                                       "Standardize", 
                                       value = TRUE)
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
                                      choices = c("1", "2", "5", "10"),
                                      selected = c("10"),
                                      inline = TRUE),
                         fluidRow(
                           column(width=6,checkboxInput("ml_fix_knots", 
                                                        "Fix # Knots", 
                                                        value = TRUE) ),
                           column(width=6,checkboxInput("ml_stratified_cv", 
                                                        "Stratified CV", 
                                                        value = TRUE))
                         ),
                         # Input: Select a file ----
                         fileInput("ex_test_csv", "External CSV",
                                   multiple = FALSE,
                                   accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                         selectInput("ml_num_col2_label",
                                     "Joint effect (2D continuous predictors)",
                                     choices = in.ml_num_col2_label)
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
    
    # ---- 3. unsupervised ml (unml) ----
    menuItem("ML (unsupervised)", tabName = "unml", startExpanded = FALSE,
             menuSubItem('Set up', tabName = 'unml_setup'),
             menuSubItem('Kmeans Clustering', tabName = 'unml_cluster')
    ),
    ## Show panel only when sidebar is selected
    useShinyjs(),
    div(id = 'sidebar_unml_setup',
        conditionalPanel("input.sidebar == 'unml_setup'",
                         selectInput("unml_cluster_label",
                                     "Cluster",
                                     choices = in.unml_cluster_label),
                         fluidRow(
                           column(width=8,selectInput("unml_trim_by_label",
                                                      label="Time over",
                                                      choices = in.unml_trim_by_label,
                                                      selected = in.unml_trim_by_label[1]) ),
                           column(width=4, numericInput("unml_trim_time_unit", "/", in.unml_trim_time_unit) )
                         ),
                         sliderInput("unml_trim_vec",
                                     label="[ from, to )",
                                     min = 0,  max = 100, step = 1, value = c(0, 99)),
                         fluidRow(
                           column(width=8, selectInput("unml_pctcut_num_labels",
                                                       label="Numeric cutoffs",
                                                       multiple = TRUE,
                                                       choices = dict_ml$label_front[which(dict_ml$type=="num")],
                                                       selected = NULL) ),
                           column(width=4, checkboxInput("unml_coerce","coerce",value = FALSE))
                         ),
                         sliderInput("unml_pctcut_num_vec",
                                     label="percentile [ from (th), to (th) ]",
                                     min = 0,  max = 100, step = 0.1, value = c(0.1, 99.9)),
                         
                         selectInput("unml_filter_tag_labels",
                                     label="Binary filter(s)",
                                     multiple = TRUE,
                                     choices = dict_ml$label_front[which(dict_ml$unit=="tag01")],
                                     selected = NULL),
                         fluidRow(
                           column(width=5,selectInput("unml_imputation",
                                                      "Imputation",
                                                      choices = c("Zero", "None", "Mean", "Median"),
                                                      selected = "None") ),
                           column(width=6,checkboxInput("unml_impute_per_cluster", "within cluster", value = FALSE))
                         ),
                         fluidRow(
                           column(width=5,checkboxInput("unml_aggregation", 
                                                        "Aggregation", 
                                                        value = TRUE) ),
                           column(width=5,checkboxInput("unml_winsorizing",
                                                        "Winsorizing",
                                                        value = FALSE))
                         )
        )),
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
                                     choices = in.unml_input_labels.choices,
                                     selected = in.unml_input_labels.selected)
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
            
            tags$p(tags$b("Inventor: "),"Jiaxing (Joy) Qiu, M.S., Data Science, School of Data Science, University of Virginia"),
            tags$p("Thanks to Center of Advanced Medical Analytics (CAMA), School of Medicine, University of Virginia,
                   the helpful comments and professional expriences inspire these creative tools in the box!"),
            
            
            tags$h2("Objectives"),
            tags$p("The objectives of this toolbox include but not limited to --"),
            tags$ol(
              tags$li("facilitate medical data science research hypothesis generation"),
              tags$li("bridge communitations between clinitians and technicians interactively"),
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
            tags$p("Unsupervise machine learning --",
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
                   " for more information.")),
    
    # ---- 1. eda ----
    tabItem(tabName = "eda_setup",
            h3("Exploratory Data Analysis -- Set up"),
            fluidRow(dataTableOutput("dictionary_table_eda"))),
    tabItem(tabName = "eda_1dstats",
            h3("Exploratory Data Analysis -- 1D Statistics"),
            fluidRow(column(1,actionButton("eda_stats1d_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Mean", 
                                 plotOutput("eda_1d_p_mean",
                                            height = "400px",
                                            dblclick = "eda_1d_p_mean_dblclick",
                                            brush = brushOpts(
                                              id = "eda_1d_p_mean_brush",
                                              resetOnNew = TRUE))
                        ),
                        tabPanel("Percentiles", 
                                 plotOutput("eda_1d_p_pct",
                                            height = "400px",
                                            dblclick = "eda_1d_p_pct_dblclick",
                                            brush = brushOpts(
                                              id = "eda_1d_p_pct_brush",
                                              resetOnNew = TRUE))
                        ),
                        tabPanel("Denomenator", 
                                 plotOutput("eda_1d_p_denom",
                                            height = "400px",
                                            dblclick = "eda_1d_p_denom_dblclick",
                                            brush = brushOpts(
                                              id = "eda_1d_p_denom_brush",
                                              resetOnNew = TRUE))
                        ),
                        tabPanel("Violin+Box", 
                                 plotOutput("eda_1d_p_violin",
                                            height = "400px",
                                            dblclick = "eda_1d_p_violin_dblclick",
                                            brush = brushOpts(
                                              id = "eda_1d_p_violin_brush",
                                              resetOnNew = TRUE))
                        )
            )
    ),
    tabItem(tabName = "eda_2dstats",
            h3("Exploratory Data Analysis -- 2D Statistics"),
            fluidRow(column(1,actionButton("eda_stats2d_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("plot_2d_stats", height = "1000px"))),
    tabItem(tabName = "eda_star",
            h3("Exploratory Data Analysis -- Death Star"),
            fluidRow(column(1,actionButton("eda_star_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("plot_death_star",
                                height = "700px",
                                dblclick = "plot_death_star_dblclick",
                                brush = brushOpts(
                                  id = "plot_death_star_brush",
                                  resetOnNew = TRUE)))),
    tabItem(tabName = "eda_allu",
            h3("Exploratory Data Analysis -- Alluvial Flow"),
            fluidRow(column(1,actionButton("eda_allu_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("plot_alluvial", height = "800px"))),
    
    # ---- 2. ml (supervised) ----
    tabItem(tabName = "ml_setup",
            h3("Machine Learning (supervised)"),
            fluidRow(dataTableOutput("dictionary_table_ml"))),
    tabItem(tabName = "ml_summ",
            h3("Machine Learning (supervised) -- Summary Table"),
            fluidRow(column(1,actionButton("ml_summ_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("na_plot", width ="2000px"),
                     dataTableOutput("summary_table"))),
    tabItem(tabName = "ml_uni",
            h3("Machine Learning (supervised) -- Univariable Percentile Heatmap"),
            fluidRow(column(1,actionButton("ml_uni_go", "Go",icon=icon("play-circle")))),
            fluidRow(plotOutput("plot_uniheat", height = "800px"))),
    tabItem(tabName = "ml_select",
            h3("Machine Learning (supervised) -- LASSO Regression Feature Selection"),
            fluidRow(column(1,actionButton("ml_select_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Reports",
                                 plotOutput("ml_select_tuning_plot"),
                                 plotOutput("ml_select_vip"),
                                 tableOutput("ml_select_coef_df")
                                 ))),
    
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
                                 plotOutput("ml_na_plot", width ="500px"),
                                 dataTableOutput("ml_summary_table")),
                        tabPanel("Spearman2 dof",
                                 plotOutput("dof_plot")))),
    tabItem(tabName = "ml_multi",
            h3("Machine Learning (supervised) -- Multivariable Regression"),
            fluidRow(column(1, actionButton("ml_multi_go", "Go",icon=icon("play-circle")))),
            tabsetPanel(type = "tabs",
                        tabPanel("Inference", 
                                 plotOutput("effect_plot", height = "600px"),
                                 plotOutput("infer_plot"),
                                 plotOutput("anova_plot"),
                                 verbatimTextOutput("model_prt")),
                        tabPanel("Internal Validation", 
                                 downloadButton("download_mdl","Model"),
                                 tableOutput("model_tbl"),
                                 tableOutput("score_tbl"),
                                 tableOutput("cv_eval_trace_tbl")
                        ),
                        tabPanel("External Validation",
                                 downloadButton("download_test_data","Y-hat (engineered)"),
                                 downloadButton("download_test_data_org","Y-hat (original)"),
                                 tableOutput("test_tbl"),
                                 dataTableOutput("test_data_org")
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
    # ---- 3. unml (unsupervised) ----
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
                        # ,
                        # tabPanel('Labeled dict',
                        #          dataTableOutput("unml_dict_df_org_clustered") )
            )
            
    )
    
   # ----- 4. other businesses ---- 
  )
)


ui <- dashboardPage(header, sidebar, body)

