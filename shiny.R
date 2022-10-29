# ---- [don't change] clean up working environment ----
rm(list = ls())


# ---- [don't change] prepare packages ----
# if (!require(dplyr)) install.packages('dplyr')
# if (!require(ggplot2)) install.packages('ggplot2')
# if (!require(ggpubr)) install.packages('ggpubr')
# if (!require(rms)) install.packages('rms')
# if (!require(fastDummies)) install.packages('fastDummies')
# if (!require(ggalluvial)) install.packages('ggalluvial')
# if (!require(ggsignif)) install.packages('ggsignif')
# if (!require(broom)) install.packages('broom')
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
library(ggfortify)


# ---- [don't change] source global functions -----
# logsitic regression modeling utilities
path = paste0("./utils/lrm_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
# ordinary least squqres (linear regression) utilities
path = paste0("./utils/ols_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
# uni-variate analysis utilities
path = paste0("./utils/uni_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
# lasso modeling utilities
path = paste0("./utils/lss_utils") # lasso
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
# dictionary-oriented engineering utilities
path = paste0("./utils/do_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
# exploratory visualization utilities
path = paste0("./utils/viz_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
# unsupervised learning utilities
path = paste0("./utils/uns_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
# front page "wrapper" utilities
path = paste0("./utils/front_utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
# customized project-specific utilities
path = paste0("./prj_utils")
flst = list.files(path)
if(length(flst)>0){
  sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
}



# ---- [if you are a data person, you might make changes here :) ] global constants ----
# [optional] "prj_name" is the name of your project, it's optional and will be shown in "Project Info" page if given.
prj_name <- "Demo"
# [optional] "prj_link" is a hyperlink of your project if any, it's optional and will be shown in "Project Info" page if given.
prj_link <- "https://github.com/JiaxingQiu/MediDSToolbox"
# [required (if longitudinal)] "time_over_labels" is a list of the labels of time-indicator variables of your data. 
### If your data is "longitudinal" / "time-series" data (there are repeated measures from each subject in your study),  
### you are required to specify their label here, otherwise a "fake time index" (value of 333) will be assign to your dataframe, 
### which will make your study no different from subject-wise analysis.
time_over_labels <- c("Hour since birth")
# [required] load .csv format dictionary file from "data" folder, it must have the following columns:
### "varname": machine-friendly variable names
### "label": human-friendly variable labels
### "type": type of the variable, one of: 
###         ["num": numeric, fct": factor, "tim": time-indicator, "key": subject/cluster-indicator]
### "source_file": source group of the variable, where does a variable come from / how is a variable generated
### "unique_per_sbj": TRUE/FALSE, whether or not this variable is unique per subject (i.e. patient)
dict_org <- read.csv("./data/dict_data_demo.csv", stringsAsFactors = FALSE)
# [required] load .csv format data file from "data" folder
data_org <- read.csv("./data/data_demo.csv", stringsAsFactors = FALSE) 


# ---- [don't change] engineer global variables ----
# reformat varnames and labels
colnames(data_org) <- gsub("[^[:alnum:]]","_",colnames(data_org))
dict_org$varname <- gsub("[^[:alnum:]]","_",dict_org$varname)
dict_org$label <- gsub("_v[0-9]", "", dict_org$label)
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



# ---- [if you are a data person, you might make changes here :) ] specify machine learning roles ----
# output / response variables
output_varname_list <- c("eos", "died")
# cluster / study unit variables 
cluster_varname_list <- c("id")
# input / predictors / explanatory variables
input_varname_list <- c(setdiff(dict_ml$varname[which(dict_ml$type=="num")], c(dict_ml$varname[which(dict_ml$label%in%time_over_labels)])), 
                        setdiff(dict_ml$varname[which(dict_ml$type=="fct"&dict_ml$unit!="tag01")],output_varname_list),
                        setdiff(dict_ml$varname[which(dict_ml$unit=="tag01")], output_varname_list) )

# ---- [don't change] create mlrole column for dict_ml ----
dict_ml$mlrole <- ""
dict_ml[which(dict_ml$varname %in% input_varname_list), "mlrole"] <- "input"
dict_ml[which(dict_ml$varname %in% output_varname_list), "mlrole"] <- "output"
dict_ml[which(dict_ml$varname %in% cluster_varname_list), "mlrole"] <- "cluster"
dict_ml$varname_dict <- stringr::str_split_fixed(stringr::str_split_fixed(dict_ml$varname, "_factor_",2)[,1], "___", 2)[,1]
data_ml <- assign.dict(data_ml, dict_ml)

# ---- [if you are a data person, you might make changes here :)] derive new variables ----
# # example 1
# if(("ga_days" %in%colnames(data_ml)) & (!"GA_bins"%in%colnames(data_ml)) ){
#   data_ml$GA_bins <- floor(data_ml$ga_days/7)
#   data_ml$GA_bins[which(data_ml$GA_bins <=23)] <- 23
#   data_ml$GA_bins <- as.factor(as.character(data_ml$GA_bins))
#   attr(data_ml$GA_bins, "varname") <- "GA_bins"
#   attr(data_ml$GA_bins, "label") <- "GA weeks binned"
#   attr(data_ml$GA_bins, "type") <- "fct"
#   attr(data_ml$GA_bins, "unit") <- ""
#   attr(data_ml$GA_bins, "mlrole") <- ""
#   attr(data_ml$GA_bins, "unique_per_sbj") <- TRUE
#   attr(data_ml$GA_bins, "source_file") <- "drvd"
# }
# 
# # example 2
# if(("bw" %in%colnames(data_ml)) & (!"baby_weight_lin"%in%colnames(data_ml)) ){
#   data_ml$baby_weight_lin <- ifelse(data_ml$bw<900,data_ml$bw, 900)
#   attr(data_ml$baby_weight_lin, "varname") <-"baby_weight_lin"
#   attr(data_ml$baby_weight_lin, "label") <-"Birth weight (<=900)"
#   attr(data_ml$baby_weight_lin, "type") <- "num"
#   attr(data_ml$baby_weight_lin, "unit") <- "grams"
#   attr(data_ml$baby_weight_lin, "unique_per_sbj") <- TRUE
#   attr(data_ml$baby_weight_lin, "source_file") <- "drvd"
#   attr(data_ml$baby_weight_lin, "mlrole") <- "input"
# }

# example 3 remove bad columns
rm_c <- c()
for(c in colnames(data_ml)){
  if(n_distinct( data_ml[,c] )<2){
    rm_c <- c(rm_c, c)
  }
}
data_ml <- data_ml[,setdiff(colnames(data_ml),rm_c)]

# ---- [don't change] finalize global variables ----
dict_ml <- get.dict(data_ml)
data_ml <- assign.dict(data_ml, dict_ml)
data_ml <- assign.dict(data_ml, dict_org, overwrite = FALSE)
dict_ml <- get.dict(data_ml)
stopifnot(all(!dict_ml$source_file==""))

