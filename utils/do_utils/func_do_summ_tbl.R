do_summ_tbl <- function(data, 
                     dict_data,
                     num_vars=NULL, # list of numeric variables into the summary table
                     fct_vars=NULL, # list of factor variables into the summary table
                     num_denom=NULL, # denominator type for numeric variables c("avail")
                     fct_denom=NULL,  # denominator type for factor variables c("avail", "known")
                     keys=c('subjectnbr'), # key of a cohort cluster
                     y=c("primary_outcome_factor"), # stratify by y
                     overall=TRUE, 
                     test=FALSE,
                     unknown_level_list = c("Unknown","Other/Unknown", "Unknown or declined to self-identify", "Ambiguous")
                     
){
  
  # ---- remove duplicates if any ----
  if (length(y)>0){
    data <- dplyr::distinct(data[, c(keys, y, num_vars, fct_vars)]) # if y is not in data, or more than 1, error will arise
  } else {
    data = dplyr::distinct(data[, c(keys, num_vars, fct_vars)]) # if y is not in data, or more than 1, error will arise
  }
  
  # ---- check variable name lists not empty ----
  if (is.null(num_vars) & is.null(fct_vars)) {
    warning("No variable name is provided.")
    return()
  }
  
  # ---- fix variable data type ----
  # save attributes in to a dictionary whenever we make change to the dataframe
  # numeric
  for (num_var in num_vars) {
    data[,num_var] <- as.numeric(as.character(data[, num_var]))
  }
  # factor
  for (fct_var in fct_vars) {
    data[,fct_var] <- as.factor( as.character(data[,fct_var]) )
    # reset "No" as lowest order of levels
    if ("No" %in% levels(data[,fct_var])){
      tryCatch({
        stopifnot(!fct_var=="steroids_admin_factor")
        data[,fct_var] <- relevel(data[,fct_var],"No")
      },error=function(e){
        print(paste0("skip reset No as the lowest level for factor var ",fct_var))
      })
    }
  }
  # assign dictionary back to dataframe object
  data <- assign.dict(data, dict_data)
  
  # ---- add denominator info by data type ----
  dict_data <- get.dict(data)
  if(!is.null(num_denom)){
    for (num_var in num_vars){
      # setup valid label to variable
      label(data[,num_var]) <- attr(data[,num_var], "label")
      # denominator : available answer 
      if (num_denom == "avail"){
        data[,paste0(num_var,"_avail")] <- factor(ifelse(!is.na(data[,num_var]), "Yes", "No"), c("No", "Yes"))
        label(data[, paste0(num_var,"_avail")]) <- paste0(ifelse(attr(data[,num_var], "label")!="", attr(data[,num_var], "label"), num_var)," (N: Available Number)")
      }
      # finally print out NA rows
      print(paste0("--- find ", nrow(data[which(is.na(data[,num_var])), c(keys, num_var)]) ," NA rows in ", num_var, " (label:", label(data[,num_var]), ") ---"))
    }
  }else{
    warning("invalid num_denom string option(s), msut be 'avail'.")
  }
  
  # factor
  includeNA = TRUE # default denominator: all of cohort (Only effective for categorical variables)
  if(!is.null(fct_denom)){
    for (fct_var in fct_vars){
      # keep the raw column
      data[,paste0(fct_var,"_raw")] <- data[,fct_var]
      # setup valid label to variable
      label(data[,fct_var]) <- attr(data[,fct_var], "label")
      #  denominator : available answer
      if (fct_denom == "avail"){ 
        # reset includeNA indicator
        includeNA = FALSE 
        # add _avail column
        data[,paste0(fct_var,"_avail")] <- factor(ifelse(!is.na(data[,fct_var]), "Yes", "No"), c("No", "Yes"))
        # add label attribute
        label(data[,paste0(fct_var,"_avail")]) <- paste0(ifelse(label(data[,fct_var])!="", label(data[,fct_var]), fct_var)," (N: Available Levels)")
      } else if (fct_denom == "known"){#  denominator : known answers 
        # reset includeNA indicator
        includeNA = FALSE 
        # prepare the global unknown level list
        # if there is unknown level in current column
        if ( any(unknown_level_list %in% levels(data[,fct_var])) ){
          # print rows that are filter out
          print(paste0("reset info-unknown level(s) ", levels(data[,fct_var])[which(levels(data[,fct_var]) %in% unknown_level_list)] ," in ", fct_var, " to NA"))
          levels(data[,fct_var])[which(levels(data[,fct_var]) %in% unknown_level_list)] <- NA # reset "Unknown" level as NA if any in factor variable
        }
        data[,paste0(fct_var,"_known")] <- factor(ifelse(!is.na(data[,fct_var]), "Yes", "No"), c("No", "Yes"))
        label(data[,paste0(fct_var,"_known")]) <- paste0( ifelse(label(data[,fct_var])!="", label(data[,fct_var]), fct_var)," (N: Known-info Levels)")
      }
      # finally print out NA rows
      print(paste0("--- final NA rows in ", fct_var, " (label:", label(data[,fct_var]), ") ---"))
      print(nrow(data[which(is.na(data[,fct_var])), c(keys, paste0(fct_var,"_raw"))]))
    } 
  }else{
    warning("invalid fct_denom string option(s), must be 'avail' or 'known'.")
  }
  # assign dictionary back to dataframe object
  data <- assign.dict(data, dict_data)
  
  
  
  # ---- organize variable name list ----
  vars <- c(num_vars, fct_vars)
  for (var in vars){
    if (paste0(var, "_avail") %in% colnames(data)){
      vars <- c(vars, paste0(var,"_avail"))
    }
    if (paste0(var, "_known") %in% colnames(data)){
      vars <- c(vars, paste0(var,"_known"))
    }
  }
  # sort variable name by label alphabetically (if input variables count more than one!!)
  try({
    label_obj <- sort(label(data[,vars]))
    stopifnot(!is.null(attributes(label_obj)))
    vars <- attributes(label_obj)$names
  },TRUE) # error Only one variable given.
  
  
  # ---- create Tableone objects ----
  # note tbl obj has 3 elements 
  if(length(y)>0){
    tbl <- tableone::CreateTableOne(vars, # numeric and factor vars are automatically seperated
                                    strata=c(y), # if y is not in data, or more than 1, error will arise
                                    data=data,
                                    includeNA=includeNA, # Only effective for categorical variables.
                                    test=test,
                                    addOverall=overall
    )
  } else {
    tbl <- tableone::CreateTableOne(vars, # numeric and factor vars are automatically seperated
                                    data=data,
                                    includeNA=includeNA, # Only effective for categorical variables.
                                    test=test,
                                    addOverall=overall
    )
  }
  
  #print(tbl_obj$tbl, varLabels = TRUE)
  
  ## To further examine the variables, use the summary.ContTable method, which will show more details.
  # summary(tbl$ContTable)
  ## To further examine the variables, use the summary.CatTable method, which will show more details.
  # summary(tbl$CatTable)
  # convert detailed summary information per type of variables in dataframe and return
  fct_detail_df <- data.frame()
  try({
    for(i in names(tbl$CatTable) ){
      df <- as.data.frame(do.call(rbind, tbl$CatTable[[i]]))
      df <- df[which((!grepl("_known",rownames(df))) & (!grepl("_avail",rownames(df))) ), ]
      df$group <- i
      df$varname <- rownames(df)
      fct_detail_df <- bind_rows(fct_detail_df, df)
    }
    rownames(fct_detail_df) <- 1:nrow(fct_detail_df)
    fct_detail_df <- fct_detail_df[,c("group","varname", setdiff(colnames(fct_detail_df),c("group","varname")))]
  },TRUE)
  
  
  num_detail_df <- data.frame()
  try({
    for(i in names(tbl$ContTable) ){
      df <- as.data.frame(tbl$ContTable[[i]])
      df$group <- i
      df$varname <- rownames(df)
      num_detail_df <- bind_rows(num_detail_df, df)
    }
    rownames(num_detail_df) <- 1:nrow(num_detail_df)
    num_detail_df <- num_detail_df[,c("group","varname", setdiff(colnames(num_detail_df),c("group","varname")))]
  },TRUE)
  
  return(list("tbl" = tbl,
              "fct_detail_df" = fct_detail_df,
              "num_detail_df" = num_detail_df))
}

