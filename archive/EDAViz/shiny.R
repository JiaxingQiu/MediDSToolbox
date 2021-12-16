rm(list = ls())


library(dplyr)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(rms)
library(fastDummies)
library(ggalluvial)
library(MLmetrics)
library(caret)
library(pROC)
library(tableone)


# ---- source global functions -----
path = paste0("./prj_utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
path = paste0("./utils/do_utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
path = paste0("./utils/viz_utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
path = paste0("./utils/front_utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

dict_org <- read.csv("./data/dict_deid_data_final.csv", stringsAsFactors = FALSE)
data_org <- read.csv("./data/deid_data_final.csv", stringsAsFactors = FALSE)


# key / cluster name and labels
key_var_name <- as.character(dict_org$varname[which(dict_org$type=="key")])
key_var_label <- as.character(dict_org$label[which(dict_org$type=="key")])

# numeric variable name and labels 
num_var_name <- as.character(dict_org$varname[which(dict_org$type=="num")])
num_var_label <- as.character(dict_org$label[which(dict_org$type=="num")])
drop_num_lst <- c(num_var_name[endsWith(num_var_name,"_avail_dur_sec")],
                  num_var_name[endsWith(num_var_name,"_event_dur_min")])
# remove numeric vars that are too descrite
for (var in num_var_name){
  if (n_distinct(round(data_org[,var],2)) <= 10){
    drop_num_lst <- c(drop_num_lst, var)
  }
}
num_var_name <- setdiff(num_var_name, drop_num_lst)
num_var_label <- as.character(dict_org$label[which(dict_org$varname%in%num_var_name)])

# tag (onehot binary) var names and labels
tag_var_name <- as.character(dict_org$varname[which(dict_org$type=="fct"&dict_org$unit=="tag01"& !endsWith(dict_org$varname,"_factor") )])
tag_var_label <- as.character(dict_org$label[which(dict_org$varname%in%tag_var_name)])

# decoded factor(>1 levels) var names and labels
fct_var_name <- as.character(dict_org$varname[which(dict_org$type=="fct"&dict_org$unit!="tag01"&grepl("_factor",dict_org$varname))])
fct_var_label <- as.character(dict_org$label[which(dict_org$type=="fct"&dict_org$unit!="tag01"&grepl("_factor",dict_org$varname))])

# final data for viz tool
data_viz <- dummy_cols(data_org, select_columns = fct_var_name, ignore_na=TRUE) %>% 
  select(-c(contains("_Un",ignore.case=FALSE)&!contains("primary_outcome_factor_Unfavorable",ignore.case=FALSE),
            contains("_No",ignore.case=FALSE)&!contains("_North",ignore.case = FALSE) )) %>% 
  as.data.frame()
colnames(data_viz) <- gsub("[^[:alnum:]]","_",colnames(data_viz))

# derive new tag variable name list dummied by fct columns
fct2tag_var_name <- sort(setdiff(colnames(data_viz), colnames(data_org)))
fct2tag_var_label <- c()
fct2tag_var_unique <- c()
fct2tag_var_source_file <- c()
for (var in fct2tag_var_name){
  var_dict <- paste0(strsplit(var,"_factor_")[[1]][1],"_factor") # variable name in the dict_org
  var_level <- strsplit(var,"_factor_")[[1]][2]
  var_label <- paste0(as.character(dict_org$label[dict_org$varname==var_dict]),"___",var_level)
  var_unique <- dict_org$unique_per_sbj[dict_org$varname==var_dict]
  var_source_file <- dict_org$source_file[dict_org$varname==var_dict]
  fct2tag_var_label <- c(fct2tag_var_label, var_label)
  fct2tag_var_unique <- c(fct2tag_var_unique, var_unique)
  fct2tag_var_source_file <- c(fct2tag_var_source_file, var_source_file)
}

# final dictionary for viz tool
dict_viz <- bind_rows(  data.frame(varname=key_var_name,
                                   label=key_var_label,
                                   type="key",
                                   unit=dict_org$unit[which(dict_org$varname%in%key_var_name)],
                                   unique_per_sbj=dict_org$unique_per_sbj[which(dict_org$varname%in%key_var_name)],
                                   source_file=dict_org$source_file[which(dict_org$varname%in%key_var_name)],
                                   stringsAsFactors = FALSE), 
                        data.frame(varname=num_var_name,
                                   label=num_var_label,
                                   type="num",
                                   unit=dict_org$unit[which(dict_org$varname%in%num_var_name)],
                                   unique_per_sbj=dict_org$unique_per_sbj[which(dict_org$varname%in%num_var_name)],
                                   source_file=dict_org$source_file[which(dict_org$varname%in%num_var_name)],
                                   stringsAsFactors = FALSE), 
                        data.frame(varname=fct_var_name,
                                   label=fct_var_label,
                                   type="fct",
                                   unit=dict_org$unit[which(dict_org$varname%in%fct_var_name)],
                                   unique_per_sbj=dict_org$unique_per_sbj[which(dict_org$varname%in%fct_var_name)],
                                   source_file=dict_org$source_file[which(dict_org$varname%in%fct_var_name)],
                                   stringsAsFactors = FALSE), 
                        data.frame(varname=tag_var_name,
                                   label=tag_var_label,
                                   type="fct",
                                   unit="tag01", 
                                   unique_per_sbj=dict_org$unique_per_sbj[which(dict_org$varname%in%tag_var_name)],
                                   source_file=dict_org$source_file[which(dict_org$varname%in%tag_var_name)],
                                   stringsAsFactors = FALSE),
                        data.frame(varname=fct2tag_var_name,
                                   label=fct2tag_var_label,
                                   type="fct",
                                   unit="tag01", 
                                   unique_per_sbj=fct2tag_var_unique,
                                   source_file=fct2tag_var_source_file,
                                   stringsAsFactors = FALSE)
)



data_viz$GA_bins <- floor(data_viz$ga_days/7)
dict_viz <- bind_rows(dict_viz,
                      data.frame(varname="GA_bins", 
                                 label="GA weeks", 
                                 type="fct", 
                                 unit="", 
                                 unique_per_sbj=TRUE, 
                                 source_file="base",
                                 stringsAsFactors = FALSE))

# data_viz$pma_weeks <- data_viz$pma_days/7
# dict_viz <- bind_rows(dict_viz,
#                       data.frame(varname="pma_weeks",
#                                  label="Post-menstrual Age (week)",
#                                  type="num",
#                                  unit="",
#                                  unique_per_sbj=FALSE,
#                                  source_file="base",
#                                  stringsAsFactors = FALSE))
# 
# data_viz$ca_weeks <- data_viz$ca_days/7
# dict_viz <- bind_rows(dict_viz,
#                       data.frame(varname="ca_weeks",
#                                  label="Chronological Age (week)",
#                                  type="num",
#                                  unit="",
#                                  unique_per_sbj=FALSE,
#                                  source_file="base",
#                                  stringsAsFactors = FALSE))
# data_viz$ga_weeks <- data_viz$ga_days/7
# dict_viz <- bind_rows(dict_viz,
#                       data.frame(varname="ga_weeks",
#                                  label="Gestational Age (week)",
#                                  type="num",
#                                  unit="",
#                                  unique_per_sbj=FALSE,
#                                  source_file="base",
#                                  stringsAsFactors = FALSE))

data_viz <- assign.dict(data_viz, dict_viz)
dict_viz <- get.dict(data_viz)
dict_viz$label_front <- dict_viz$label
