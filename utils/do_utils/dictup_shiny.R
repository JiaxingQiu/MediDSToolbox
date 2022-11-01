dictup_shiny <- function(data_org, 
                         dict_org,
                         time_over_labels){
  # ---- [don't change] engineer global variables ----
  # reformat varnames and labels
  colnames(data_org) <- gsub("[^[:alnum:]]","_",colnames(data_org))
  dict_org$varname <- gsub("[^[:alnum:]]","_",dict_org$varname)
  dict_org$label <- gsub("_v[0-9]", "", dict_org$label)
  if(!"unit"%in%colnames(dict_org)){
    dict_org$unit <- ""
  }
  if(!"source_file"%in%colnames(dict_org)){
    dict_org$source_file <- "source"
  }
  # key / cluster name and labels
  key_var_name <- as.character(dict_org$varname[which(dict_org$type=="key")])
  key_var_label <- as.character(dict_org$label[which(dict_org$type=="key")])
  # numeric variable name and labels 
  num_var_name <- as.character(dict_org$varname[which(dict_org$type=="num")])
  num_var_label <- as.character(dict_org$label[which(dict_org$type=="num")])
  # tag (one hot binary) var names and labels
  tag_var_name <- as.character(dict_org$varname[which(dict_org$type=="fct"&dict_org$unit=="tag01")])
  tag_var_label <- as.character(dict_org$label[which(dict_org$varname%in%tag_var_name)])
  for(var in tag_var_name){
    data_org[which(is.na(data_org[,var])),var] <- 0
  }
  # decoded factor(>1 levels) var names and labels
  fct_var_name <- as.character(dict_org$varname[which(dict_org$type=="fct"&dict_org$unit!="tag01")])
  fct_var_label <- as.character(dict_org$label[which(dict_org$varname%in%fct_var_name)])
  for (fct_col in fct_var_name){
    data_org[,fct_col] <- paste0("_level__", data_org[,fct_col] )
  }
  # initiate data_ml (the ultimate dataframe variable that will be used everywhere)
  data_ml <- data_org
  if(length(fct_var_name)>0){ # dummy variables from factor variables
    data_ml <- dummy_cols(data_org, select_columns = fct_var_name, ignore_na=TRUE) %>% as.data.frame()
    colnames(data_ml) <- gsub("[^[:alnum:]]","_",colnames(data_ml))
  }
  fct2tag_var_name <- sort(setdiff(colnames(data_ml), colnames(data_org))) # derive new tag variable name list dummied by fct columns
  fct2tag_var_label <- c()
  fct2tag_var_unique <- c()
  fct2tag_var_source_file <- c()
  for (var in fct2tag_var_name){
    var_dict <- strsplit(var,"__level__")[[1]][1] # variable name in the dict_org
    var_level <- strsplit(var,"__level__")[[1]][2]
    var_label <- paste0(as.character(dict_org$label[dict_org$varname==var_dict]),"___",var_level)
    var_unique <- dict_org$unique_per_sbj[dict_org$varname==var_dict]
    var_source_file <- dict_org$source_file[dict_org$varname==var_dict]
    fct2tag_var_label <- c(fct2tag_var_label, var_label)
    fct2tag_var_unique <- c(fct2tag_var_unique, var_unique)
    fct2tag_var_source_file <- c(fct2tag_var_source_file, var_source_file)
  }
  # initiate dict_ml (the ultimate dictionary variable that will be used everywhere)
  dict_ml <- data.frame()
  if(length(key_var_name)>0){
    dict_ml <- bind_rows(dict_ml, data.frame(varname=key_var_name,
                                             label=key_var_label,
                                             type="key",
                                             unit=dict_org$unit[which(dict_org$varname%in%key_var_name)],
                                             unique_per_sbj=dict_org$unique_per_sbj[which(dict_org$varname%in%key_var_name)],
                                             source_file=dict_org$source_file[which(dict_org$varname%in%key_var_name)],
                                             stringsAsFactors = FALSE))
  }
  if(length(num_var_name)>0){
    dict_ml <- bind_rows(dict_ml, data.frame(varname=num_var_name,
                                             label=num_var_label,
                                             type="num",
                                             unit=dict_org$unit[which(dict_org$varname%in%num_var_name)],
                                             unique_per_sbj=dict_org$unique_per_sbj[which(dict_org$varname%in%num_var_name)],
                                             source_file=dict_org$source_file[which(dict_org$varname%in%num_var_name)],
                                             stringsAsFactors = FALSE))
  }
  if(length(tag_var_name)>0){
    dict_ml <- bind_rows(dict_ml, data.frame(varname=tag_var_name,
                                             label=tag_var_label,
                                             type="fct",
                                             unit="tag01", 
                                             unique_per_sbj=dict_org$unique_per_sbj[which(dict_org$varname%in%tag_var_name)],
                                             source_file=dict_org$source_file[which(dict_org$varname%in%tag_var_name)],
                                             stringsAsFactors = FALSE))
  }
  
  if(length(fct_var_name)>0){
    dict_ml <- bind_rows(dict_ml, data.frame(varname=fct_var_name,
                                             label=fct_var_label,
                                             type="fct",
                                             unit=dict_org$unit[which(dict_org$varname%in%fct_var_name)],
                                             unique_per_sbj=dict_org$unique_per_sbj[which(dict_org$varname%in%fct_var_name)],
                                             source_file=dict_org$source_file[which(dict_org$varname%in%fct_var_name)],
                                             stringsAsFactors = FALSE))
  }
  
  if(length(fct2tag_var_name)>0){
    dict_ml <- bind_rows(dict_ml,  data.frame(varname=fct2tag_var_name,
                                              label=fct2tag_var_label,
                                              type="fct",
                                              unit="tag01", 
                                              unique_per_sbj=fct2tag_var_unique,
                                              source_file=fct2tag_var_source_file,
                                              stringsAsFactors = FALSE))
  }
  
  dict_ml <- dplyr::distinct(dict_ml)
  data_ml <- assign.dict(data_ml, dict_ml)
  dict_ml <- get.dict(data_ml)
  dict_ml$varname_dict <- stringr::str_split_fixed(stringr::str_split_fixed(dict_ml$varname, "_factor_",2)[,1], "___", 2)[,1]
  data_ml <- assign.dict(data_ml, dict_ml)
  
  
  # ---- [don't change] finalize global variables ----
  # remove bad columns
  rm_c <- c()
  for(c in colnames(data_ml)){
    if(n_distinct( data_ml[,c] )<2){
      rm_c <- c(rm_c, c)
    }
  }
  data_ml <- data_ml[,setdiff(colnames(data_ml),rm_c)]
  dict_ml <- get.dict(data_ml)
  data_ml <- assign.dict(data_ml, dict_ml)
  data_ml <- assign.dict(data_ml, dict_org, overwrite = FALSE)
  dict_ml <- get.dict(data_ml)
  stopifnot(all(!dict_ml$source_file==""))
  
  return(list(data_ml = data_ml,
              dict_ml = dict_ml,
              data_org = data_org,
              dict_org = dict_org))
}


