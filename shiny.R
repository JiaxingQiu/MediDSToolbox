rm(list = ls())

# ---- prepare package ----
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

# ---- source global functions -----
path = paste0("./utils/lrm_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

path = paste0("./utils/ols_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

path = paste0("./utils/uni_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

path = paste0("./utils/lss_utils") # lasso
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

path = paste0("./utils/do_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

path = paste0("./utils/viz_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

path = paste0("./utils/uns_utils")
flst = list.files( path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

path = paste0("./utils/front_utils")
flst = list.files(path)
sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)

# project utils that takes care of project unique funcitonalities 
path = paste0("./prj_utils")
flst = list.files(path)
if(length(flst)>0){
  sapply(c(paste(path,flst,sep="/")), source, .GlobalEnv)
}

dict_org <- read.csv("./data/dict_deid_data_final.csv", stringsAsFactors = FALSE)
data_org <- read.csv("./data/deid_data_final.csv", stringsAsFactors = FALSE) 
dict_org$label <- gsub("_v[0-9]", "", dict_org$label)

# key / cluster name and labels
key_var_name <- as.character(dict_org$varname[which(dict_org$type=="key")])
key_var_label <- as.character(dict_org$label[which(dict_org$type=="key")])

# numeric variable name and labels 
num_var_name <- as.character(dict_org$varname[which(dict_org$type=="num")])
num_var_label <- as.character(dict_org$label[which(dict_org$type=="num")])
drop_num_lst <- c(num_var_name[endsWith(num_var_name,"_avail_dur_sec")],
                  num_var_name[endsWith(num_var_name,"_event_dur_min")])
# # remove numeric vars that are too descrite
# for (var in num_var_name){
#   if (n_distinct(round(data_org[,var],2)) <= 10){
#     drop_num_lst <- c(drop_num_lst, var)
#   }
# }
num_var_name <- setdiff(num_var_name, drop_num_lst)
num_var_label <- as.character(dict_org$label[which(dict_org$varname%in%num_var_name)])

# tag (onehot binary) var names and labels
tag_var_name <- as.character(dict_org$varname[which(dict_org$type=="fct"&dict_org$unit=="tag01"& !endsWith(dict_org$varname,"_factor") )])
tag_var_label <- as.character(dict_org$label[which(dict_org$varname%in%tag_var_name)])

# decoded factor(>1 levels) var names and labels
fct_var_name <- as.character(dict_org$varname[which(dict_org$type=="fct"&dict_org$unit!="tag01"&endsWith(dict_org$varname, "_factor"))])
fct_var_label <- as.character(dict_org$label[which(dict_org$varname%in%fct_var_name)])

# final data for viz tool
#vars2dummy <- intersect(dict_org$varname[which(dict_org$source_file%in%c("drvd", "base", "prnt"))], fct_var_name)
vars2dummy <- fct_var_name
data_ml <- dummy_cols(data_org, select_columns = vars2dummy, ignore_na=TRUE) %>% 
  as.data.frame()
colnames(data_ml) <- gsub("[^[:alnum:]]","_",colnames(data_ml))

# derive new tag variable name list dummied by fct columns
fct2tag_var_name <- sort(setdiff(colnames(data_ml), colnames(data_org)))
fct2tag_var_label <- c()
fct2tag_var_unique <- c()
fct2tag_var_source_file <- c()
for (var in fct2tag_var_name){
  # fill na with 0 in tag 01 type variable
  data_ml[which(is.na(data_ml[,var])),var] <- 0
  var_dict <- paste0(strsplit(var,"_factor_")[[1]][1],"_factor") # variable name in the dict_org
  var_level <- strsplit(var,"_factor_")[[1]][2]
  var_label <- paste0(as.character(dict_org$label[dict_org$varname==var_dict])," == ",var_level)
  var_unique <- dict_org$unique_per_sbj[dict_org$varname==var_dict]
  var_source_file <- dict_org$source_file[dict_org$varname==var_dict] #paste0(dict_org$source_file[dict_org$varname==var_dict]," dummy")
  fct2tag_var_label <- c(fct2tag_var_label, var_label)
  fct2tag_var_unique <- c(fct2tag_var_unique, var_unique)
  fct2tag_var_source_file <- c(fct2tag_var_source_file, var_source_file)
}

# final dictionary for viz tool
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

data_ml$GA_bins <- floor(data_ml$ga_days/7)
data_ml$GA_bins[which(data_ml$GA_bins <=23)] <- 23
data_ml$GA_bins <- as.factor(as.character(data_ml$GA_bins))
dict_ml <- bind_rows(dict_ml,
                      data.frame(varname="GA_bins", 
                                 label="GA weeks binned", 
                                 type="fct", 
                                 unit="", 
                                 unique_per_sbj=TRUE, 
                                 source_file="drvd",
                                 stringsAsFactors = FALSE))

data_ml <- assign.dict(data_ml, dict_ml)
data_ml <- assign.dict(data_ml, dict_org, overwrite = FALSE)
dict_ml <- get.dict(data_ml)
stopifnot(all(!dict_ml$source_file==""))
data_ml <- subset_df(data_ml, "40w")

# ----- for ml tool ----
tag_var_name <- union(tag_var_name, fct2tag_var_name)

# refine / select variables for ml
output_varname_list <- c("primary_outcome_factor_Unfavorable", 
                         "dod___tag",
                         "outcome_meds_tag",
                         "outcome_supp_tag",
                         tag_var_name[which(startsWith(tag_var_name, "medexit") | startsWith(tag_var_name,'techexit') | startsWith(tag_var_name,"exit_reason"))])
cluster_varname_list <- c("subjectnbr","m_id")
input_varname_list <- c(setdiff(num_var_name, c(num_var_name[which(endsWith(num_var_name,"_event_count") | 
                                                                   endsWith(num_var_name,"_event_dur_sec") |
                                                                   startsWith(num_var_name,"dc_")
                                                                   )],"death_ca_days") ), 
                        setdiff(fct_var_name,c(fct_var_name[which(startsWith(fct_var_name, "medexit") |
                                                                    startsWith(fct_var_name, "techexit") | 
                                                                    startsWith(fct_var_name, "exit_reason") | 
                                                                    startsWith(fct_var_name, "m_primary_outcome") | 
                                                                    startsWith(fct_var_name, "primary_outcome"))])),
                        setdiff(tag_var_name, 
                                c(tag_var_name[which(startsWith(tag_var_name, "medexit") | startsWith(tag_var_name,'techexit') | startsWith(tag_var_name,"exit_reason"))],
                                  "baby_weight_under_fenton_10pct",
                                        "baby_dob___tag",
                                        "exit_date___tag",
                                        "dod___tag",
                                        "resp_today_factor_Yes",
                                        "extub_ynunk_factor_Yes",
                                        "extub_plan_factor_Planned",
                                        "intub_ynunk_factor_Yes",
                                        "intub_reason___1_factor_Checked","intub_reason___2_factor_Checked","intub_reason___3_factor_Checked","intub_reason___4_factor_Checked","intub_reason___5_factor_Checked",
                                        "gasobtained_factor_Arterial","gasobtained_factor_Capillary","gasobtained_factor_Venous",
                                        "baby_timeunk_factor_Yes","method_ga_factor_Obstetric Estimate","method_ga_factor_Neonatal Estimate",
                                        "baby_lengthunk_factor_Yes","baby_headunk_factor_Yes",
                                        "apgar1min_unk_factor_Yes", "apgar5min_unk_factor_Yes",
                                        "resuscit_proc_factor_Yes", 
                                        "temp_method_factor_Axillary temperature","temp_method_factor_Skin temperature", "temp_method_factor_Core temperature (e.g. rectal)",
                                        "eteralfeeds_unk_factor_Yes", "fullpo_unk_factor_Yes",
                                        "primary_outcome_factor_Favorable", "primary_outcome_factor_Unfavorable",
                                        "exit_reason_factor_Discharged to home prior to 52 weeks",
                                        "exit_reason_factor_Reached 52 weeks age and still in hospital (same HIPAA entity)",
                                        "exit_reason_factor_Baby died in hospital (same HIPAA entity) prior to 52 weeks (Reminder: Complete Record of Death Form and Discharge from Hospital Form)",
                                        "exit_reason_factor_Transferred to another hospital(different HIPAA entity) prior to 52 weeks",
                                        "m_primary_outcome_factor_Favorable",
                                        "m_primary_outcome_factor_Unfavorable",
                                        "m_primary_outcome_factor_Multiple pregnancy: Discordant outcome")) )


# create mlrole column for dict_ml
dict_ml$mlrole <- ""
dict_ml[which(dict_ml$varname %in% input_varname_list), "mlrole"] <- "input"
dict_ml[which(dict_ml$varname %in% output_varname_list), "mlrole"] <- "output"
dict_ml[which(dict_ml$varname %in% cluster_varname_list), "mlrole"] <- "cluster"
dict_ml$varname_dict <- stringr::str_split_fixed(stringr::str_split_fixed(dict_ml$varname, "_factor_",2)[,1], "___", 2)[,1]
# update dictionary as attributes to dataframe
data_ml <- assign.dict(data_ml, dict_ml)
# reverse raw tag columns to factor type
# data_ml <- dict.tag2fct(data_ml, revlist = tag_var_name)

# derive new variable
data_ml$baby_weight_lin <- ifelse(data_ml$baby_weight<1000,data_ml$baby_weight, 1000)
attr(data_ml$baby_weight_lin, "varname") <-"baby_weight_lin"
attr(data_ml$baby_weight_lin, "label") <-"Birth weight (<=1000)"
attr(data_ml$baby_weight_lin, "type") <- "num"
attr(data_ml$baby_weight_lin, "unit") <- "grams"
attr(data_ml$baby_weight_lin, "unique_per_sbj") <- TRUE
attr(data_ml$baby_weight_lin, "source_file") <- "drvd"
attr(data_ml$baby_weight_lin, "mlrole") <- "input"

data_ml$nondod_unfav <- ifelse(data_ml$primary_outcome_factor_Unfavorable==1&data_ml$dod___tag!=1, 1, 0)
data_ml$nondod_unfav[which(data_ml$dod___tag==1)] <- NA
attr(data_ml$nondod_unfav, "varname") <-"nondod_unfav"
attr(data_ml$nondod_unfav, "label") <-"Unfavorable outcome excluding death"
attr(data_ml$nondod_unfav, "type") <-"fct"
attr(data_ml$nondod_unfav, "unit") <-"tag01"
attr(data_ml$nondod_unfav, "unique_per_sbj") <- TRUE
attr(data_ml$nondod_unfav, "source_file") <- "drvd"
attr(data_ml$nondod_unfav, "mlrole") <- "output"


dict_ml <- get.dict(data_ml)
data_ml <- assign.dict(data_ml, dict_ml)
data_ml <- assign.dict(data_ml, dict_org, overwrite = FALSE)
dict_ml <- get.dict(data_ml)
dict_ml <- merge(dict_ml, dict_org[,c("varname", "from_cols")], all.x = TRUE)
stopifnot(all(!dict_ml$source_file==""))

