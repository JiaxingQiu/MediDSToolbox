summ_tbl <- function(data, num_vars=NULL, fct_vars=NULL, num_denom=NULL, fct_denom=NULL, keys, y, overall=TRUE, test=FALSE)  {
  
  # ---- remove duplicates if any ----
  data = dplyr::distinct(data[, c(keys, y, num_vars, fct_vars)]) # if y is not in data, or more than 1, error will arise
  
  # ---- check variable name lists not empty ----
  if (is.null(num_vars) & is.null(fct_vars)) {
    warning("No variable name is provided.")
    return()
  }
  
  # ---- clean variables by type ----
  # numeric
  for (num_var in num_vars) {
    if ( !is.numeric(data[[num_var]]) ) { # check all num_vars are numeric, if not, coerce to numeric and raise warning
      warning(paste0("Coerce ",num_var," to numeric."))
      data[[num_var]] <- lapply(data[[num_var]], as.numeric)#as.numeric(as.character(data[, num_var]))
    }
  }
  # factor
  for (fct_var in fct_vars) {
    if ( !is.factor(data[[fct_var]]) ) { # check all fct_vars are factor, if not, coerce to factor and raise warning
      warning(paste0("Coerce ",fct_var," to factor."))
      data[[fct_var]] <- lapply(data[[fct_var]], as.factor) #as.factor(as.character( data[, fct_var]))
    }
    # reset "No" as lowest order of levels
    if ("No" %in% levels(data[[fct_var]])){
      try({
        if (fct_var=="steroids_admin.factor") next
        data[[fct_var]] <- relevel(data[[fct_var]],"No")
        },TRUE)
    }
  }
  
  # ---- add denominator variables by data type ----
  # numeric
  if(!is.null(num_denom)){
    if (num_denom == "avail"){ # denominator : available answer 
      for (num_var in num_vars){
        data[[paste0(num_var,"_avail")]] <- factor(ifelse(!is.na(data[[num_var]]), "Yes", "No"), c("No", "Yes"))
        label(data[[paste0(num_var,"_avail")]]) <- paste0(label(data[[num_var]])," (N: Available Number)")
      }
    }else{
      warning("Not in valid num_denom string option(s): 'avail' .")
    }
  }
  
  # factor
  includeNA = TRUE # default denominator: all of cohort (Only effective for categorical variables)
  if(!is.null(fct_denom)){
    if (fct_denom == "avail"){ #  denominator : available answer
      includeNA = FALSE # reset includeNA indicator
      for (fct_var in fct_vars){
        data[[paste0(fct_var,"_avail")]] <- factor(ifelse(!is.na(data[[fct_var]]), "Yes", "No"), c("No", "Yes"))
        label(data[[paste0(fct_var,"_avail")]]) <- paste0(label(data[[fct_var]])," (N: Available Levels)")
      }
    } else if (fct_denom == "known"){ #  denominator : known answers 
      includeNA = FALSE # reset includeNA indicator
      for (fct_var in fct_vars){
        unknown_level_list <-  c("Unknown","Other/Unknown", "Unknown or declined to self-identify")
        if ( any(unknown_level_list %in% levels(data[[fct_var]])) ){
          levels(data[[fct_var]])[levels(data[[fct_var]]) %in% unknown_level_list] <- NA # reset "Unknown" level as NA if any in factor variable
          data[,paste0(fct_var,"_known")] <- factor(ifelse(!is.na(data[[fct_var]]), "Yes", "No"), c("No", "Yes"))
          label(data[,paste0(fct_var,"_known")]) <- paste0(label(data[[fct_var]])," (N: Known-info Levels)")
        } else {
          data[[paste0(fct_var,"_avail")]] <- factor(ifelse(!is.na(data[[fct_var]]), "Yes", "No"), c("No", "Yes"))
          label(data[[paste0(fct_var,"_avail")]]) <- paste0(label(data[[fct_var]])," (N: Available Levels)")
        }
      }
    } else {
      warning("Not in valid fct_denom string option(s): 'avail' / 'known' .")
    }
  }
  
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
  try(
    {
      label_obj <- sort(label(data[,vars]))
      stopifnot(!is.null(attributes(label_obj)))
      vars <- attributes(label_obj)$names
    },TRUE # error Only one variable given.
  )
  
  
  
  # ---- create Tableone object ----
  tbl <- tableone::CreateTableOne(vars, # numeric and factor vars are automatically seperated
                                  strata=c(y), # if y is not in data, or more than 1, error will arise
                                  data=data,
                                  includeNA=includeNA, # Only effective for categorical variables.
                                  test=test,
                                  addOverall=overall
  )
  
  return(tbl)
}

