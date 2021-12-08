lrm_formula <- function(data, tag_y_col, cluster_col, rcs5_x_cols=c(), rcs4_x_cols=c(), rcs3_x_cols=c(), linear_x_cols=c(), fct_x_cols=c()){
  
  
  # ---- Usage  ----
  # for multivariable lrm model using rcs function, partially singularity will cause error in rms package, 
  # this function is used to modify the complexity / dof assigned to each predictor, and finalize the model formula
  
  # ---- Arguments ----
  # data: dataframe oject to train the model
  # rcs5_x_cols: a list of predictor column names that will be assigned with 5 knots rcs in the formula
  # rcs4_x_cols: a list of predictor column names that will be assigned with 4 knots rcs in the formula
  # rcs3_x_cols: a list of predictor column names that will be assigned with 3 knots rcs in the formula
  # linear_x_cols: a list of predictor column names that will be designed as linear term in the formula
  # fct_x_cols: a list of predictor column names that will be factor term in the formula
  # tag_y_col: binary responce column name
  # cluster_col: cluster column name for repeated measure
  
  
  # ---- Values  ----
  # fml_string: finalized string object of formula 
  # rcs5_x_cols: finalized list of predictor column names that will be assigned with 5 knots rcs in the formula
  # rcs4_x_cols: finalized list of predictor column names that will be assigned with 4 knots rcs in the formula
  # rcs3_x_cols: finalized list of predictor column names that will be assigned with 3 knots rcs in the formula
  # linear_x_cols: finalized list of predictor column names that will be designed as linear term in the formula
  # fct_x_cols: finalized list of predictor column names that will be factor term in the formula
  # afford_dof: affordable degree of freedom from data
  # use_dof: how many degree of freedom the finalized formula is using 
  
  # prepare modeling dataframe
  df_mdl <- data[, c(tag_y_col, fct_x_cols ,linear_x_cols, rcs3_x_cols, rcs4_x_cols, rcs5_x_cols, cluster_col)]
  df_mdl[,tag_y_col] <- as.factor(as.character( df_mdl[,tag_y_col]) )
  dd <- datadist(as.data.frame( df_mdl))
  options(datadist=dd, na.action=na.omit)
  
  # prepare fml by input term lists
  fml_y <- paste0(tag_y_col," ~ ")
  fml_rcs5_x <- ifelse(length(rcs5_x_cols)==0, "", paste(paste0("rcs(",rcs5_x_cols,",5)"), collapse=" + "))
  fml_rcs4_x <- ifelse(length(rcs4_x_cols)==0, "", paste(paste0("rcs(",rcs4_x_cols,",4)"), collapse=" + "))
  fml_rcs3_x <- ifelse(length(rcs3_x_cols)==0, "", paste(paste0("rcs(",rcs3_x_cols,",3)"), collapse=" + "))
  fml_linear_x <- ifelse(length(linear_x_cols)==0, "",paste(paste0("I(",linear_x_cols,")"), collapse=" + ") )
  fml_fct_x <- ifelse(length(fct_x_cols)==0, "", paste(paste0("catg(as.factor(",fct_x_cols,"))"), collapse=" + "))
  fml_x_list <- c(fml_rcs5_x,fml_rcs4_x, fml_rcs3_x,fml_linear_x,fml_fct_x)
  fml_x <- paste(fml_x_list[which(fml_x_list!="")],collapse ="+")
  fml_string <- paste0(fml_y,fml_x) # initial formula string object
  
  
  # fix offending variables
  mdl <- lrm(formula(fml_string),x=TRUE, y=TRUE, data=df_mdl, penalty = 0) # with no penalty
  
  while (!is.null(mdl$offending_var)){
    off_var_list <- rev(strsplit(mdl$offending_var," ")[[1]])
    off_var_list <- unique(gsub("'","",off_var_list))
    for (var in off_var_list){
      # modify factor term
      if (length(strsplit(var, "=")[[1]])>1){
        stopifnot(length(strsplit(var, "=")[[1]])==2)
        freq_t <- table(df_mdl[,strsplit(var, "=")[[1]][1]])
        # if already most frequent level remove variable
        if(strsplit(var, "=")[[1]][2] == names(freq_t[which.max(freq_t)])){
          fct_x_cols <- setdiff(fct_x_cols, strsplit(var, "=")[[1]][1])
          next
        } else{
          df_mdl[which(df_mdl[,strsplit(var, "=")[[1]][1]]==strsplit(var, "=")[[1]][2]),strsplit(var, "=")[[1]][1]]<- names(freq_t[which.max(freq_t)])
          dd <- datadist(df_mdl)
          options(datadist=dd, na.action=na.omit)
          next
        }
      }
      # modify rcs5 term
      if(var %in% rcs5_x_cols){
        rcs4_x_cols <- c(rcs4_x_cols, var)
        rcs5_x_cols <- setdiff(rcs5_x_cols, var)
        next
      }
      # modify rcs4 term
      if(var %in% rcs4_x_cols){
        rcs3_x_cols <- c(rcs3_x_cols, var)
        rcs4_x_cols <- setdiff(rcs4_x_cols, var)
        next
      }
      #  modify  rcs3 term
      if(var %in% rcs3_x_cols){
        linear_x_cols <- c(linear_x_cols, var)
        rcs3_x_cols <- setdiff(rcs3_x_cols, var)
        next
      }
      # modify linear term
      if(var %in% linear_x_cols){
        linear_x_cols <- setdiff(linear_x_cols, var)
        next
      } 
    }
    # renew formula string
    fml_y <- paste0(tag_y_col," ~ ")
    fml_rcs5_x <- ifelse(length(rcs5_x_cols)==0, "", paste(paste0("rcs(",rcs5_x_cols,",5)"), collapse=" + "))
    fml_rcs4_x <- ifelse(length(rcs4_x_cols)==0, "", paste(paste0("rcs(",rcs4_x_cols,",4)"), collapse=" + "))
    fml_rcs3_x <- ifelse(length(rcs3_x_cols)==0, "", paste(paste0("rcs(",rcs3_x_cols,",3)"), collapse=" + "))
    fml_linear_x <- ifelse(length(linear_x_cols)==0, "",paste(paste0("I(",linear_x_cols,")"), collapse=" + ") )
    fml_fct_x <- ifelse(length(fct_x_cols)==0, "", paste(paste0("catg(as.factor(",fct_x_cols,"))"), collapse=" + "))
    fml_x_list <- c(fml_rcs5_x,fml_rcs4_x, fml_rcs3_x,fml_linear_x,fml_fct_x)
    fml_x <- paste(fml_x_list[which(fml_x_list!="")],collapse ="+")
    fml_string <- paste0(fml_y,fml_x)
    mdl <- rms::lrm(formula(fml_string),x=TRUE, y=TRUE,data=df_mdl)
  }
  
  # check affordable degree of freedom
  afford_dof <- ceiling(n_distinct(as.character(data[which(data[,tag_y_col]==1),cluster_col]))/15) # affordable degree of freedom
  use_dof <- 4*length(rcs5_x_cols)+3*length(rcs4_x_cols)+2*length(rcs3_x_cols)+length(linear_x_cols)+length(fct_x_cols) # using dof from fml obj
  print(paste0("affordable dof: ", afford_dof, ", using dof: ", use_dof))
  
  
  # return finalized fml_string
  return(list("fml_string" = fml_string,
              "rcs5_x_cols" = rcs5_x_cols, 
              "rcs4_x_cols" = rcs4_x_cols, 
              "rcs3_x_cols" = rcs3_x_cols, 
              "linear_x_cols" = linear_x_cols, 
              "fct_x_cols" = fct_x_cols,
              "afford_dof" = afford_dof,
              "use_dof" = use_dof
              ))
  
}
