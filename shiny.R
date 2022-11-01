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

# ---- call dict up shiny function in util folder ----
shiny_obj <- dictup_shiny(data_org, dict_org, time_over_labels)
data_org <- shiny_obj$data_org
dict_org <- shiny_obj$dict_org
data_ml <- shiny_obj$data_ml
dict_ml <- shiny_obj$dict_ml

