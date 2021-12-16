subset_df <- function(df, paper){
  
  if (paper == "40w"){
    
    # select valid ts if exist
    if (!is.null(df$ts)){
      df_base <- prep_df_redcap("base")[,c('subjectnbr','baby_dob','ga_days')]
      df_base$ts_max <- as.Date(as.Date(df_base$baby_dob) - as.numeric(df_base$ga_days) + 40*7)
      if ('ts_max' %in% colnames(df)){
        df <- merge(df, df_base[,c("subjectnbr", "ts_max")], by=c("subjectnbr", 'ts_max'), all.x = TRUE)
      }else{
        df <- merge(df, df_base[,c("subjectnbr", "ts_max")], by=c("subjectnbr"), all.x = TRUE)
      }
      df <- df %>% filter(ts <= ts_max) %>% as.data.frame()
    } else { # use criteria for deidentified data
      df$subjectnbr <- as.character( round(as.numeric(df$subjectnbr_deid)/3) )
      df <- df %>% filter(pma_days<=40*7) %>%as.data.frame()
      
    }
    
    # select valid subjects
    exclude_list <- c("1073","1076","2008","2043","3026",
                      "4042","4053","4054","5007","5008",
                      "5014","2139","2140","3050","3059",
                      "3078","3097","3144","5004","5030",
                      "2069")
    df <- df[which(!df$subjectnbr %in% exclude_list),]
    
  }
  
  if (paper == "36w"){
    
    # select valid ts if exist
    if (!is.null(df$ts)){
      df_base <- prep_df_redcap("base")[,c('subjectnbr','baby_dob','ga_days')]
      df_base$ts_max <- as.Date(as.Date(df_base$baby_dob) - as.numeric(df_base$ga_days) + 36*7) 
      if ('ts_max' %in% colnames(df)){
        df <- merge(df, df_base[,c("subjectnbr", "ts_max")], by=c("subjectnbr", 'ts_max'), all.x = TRUE)
      }else{
        df <- merge(df, df_base[,c("subjectnbr", "ts_max")], by=c("subjectnbr"), all.x = TRUE)
      }
      df <- df %>% filter(ts <= ts_max) %>% as.data.frame()
    }
    
    # select valid subjects
  }
  
  
  return(df)
}

