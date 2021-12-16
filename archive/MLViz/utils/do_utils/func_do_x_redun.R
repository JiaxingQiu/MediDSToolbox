do_x_redun <- function(df = data_ml, 
                       dict_df = dict_ml, 
                       x_cols=c(x_num_cols, x_fct_cols) , 
                       r_abs=0.8, 
                       type=c("pearson","spearman")[1], 
                       rank=TRUE, 
                       winsorizing=FALSE,
                       r2=0.9){
  # ---- Description ----
  # dictionary oriented (do) predictor variables redundency analysis
  
  # ---- Arguments ----
  # df: a dataframe object with essential dictionary information as attributes on each column
  # dict: dictionary for corresponding data frame input
  # x_cols: pre-selected predictor column names (x)
  # r2: ordinary or adjusted R^2 cutoff for redundancy
  
  # ---- Value ----
  # df_final: a dataframe object with updated redundency attributes on each column
  # dict_final: updated dictionary for correspondingdf_final
  
  
  # --- remove redundency by correlation ---
  x_cols_num <- intersect(x_cols,rownames(dict_df[which(dict_df$mlrole=="input"&dict_df$type=="num"),]))
  dele_cols <- c()
  rmcor_obj <- NULL
  if(length(x_cols_num)>0){
    df_num <- df[,x_cols_num]
    rmcor_obj <- x_rmcor(df_num, r_abs=r_abs,type=type,rank=rank, winsorizing=winsorizing)
    dele_cols <- rmcor_obj$delete
    x_cols_num <- intersect(x_cols_num, rmcor_obj$keep)
  }
  
  # --- remove redundency by redundency analysis ---
  x_cols_fct <- intersect(x_cols,rownames(dict_df[which(dict_df$mlrole=="input"&dict_df$type=="fct"),]))
  # numeric predictors that failed rcs transformation will be forced to be linear term
  linear_cols <- c()
  for (col in x_cols_num){
    df[,col] <- as.numeric(df[,col])
    tran <- NULL
    try({
      tran <- rcs(df[,col],3)
    },TRUE)
    if (is.null(tran)){
      linear_cols <- c(linear_cols, col)
    }
  }
  nonlin_cols <- setdiff(x_cols_num, linear_cols)
  fml_num_nonlin <- ifelse(length(nonlin_cols)==0, "", paste(nonlin_cols, collapse=" + "))
  fml_num_linear <- ifelse(length(linear_cols)==0, "", paste(paste0("I(",linear_cols,")"), collapse=" + "))
  fml_fct <- ifelse(length(x_cols_fct)==0, "", paste(paste0("as.factor(",x_cols_fct,")"), collapse=" + "))
  fml_list <- c(fml_num_nonlin, fml_num_linear, fml_fct)
  fml_string <- paste0( "~ ", paste(fml_list[which(fml_list!="")],collapse ="+"))
  fml <- formula(fml_string)
  print(paste0( "redundency analysis formula : ", fml_string ))
  
  # ---- rerdundancy analysis ----
  redundancy <-NULL
  try({
    # Uses flexible parametric additive models (see areg and its use of regression splines) to determine how well each variable can be predicted from the remaining variables. 
    redundancy <- Hmisc::redun(fml, r2=r2, type="adjusted", data=df, allcat=TRUE, pr=TRUE, long=FALSE)
    keep_cols <- gsub("\\)", "",gsub("I\\(", "", redundancy$In))
    dele_cols <-  gsub("\\)", "",gsub("I\\(", "", redundancy$Out))
  },TRUE)
  
  df <- df[, setdiff(colnames(df), dele_cols)]
  df_final <- assign.dict(df, dict_df)
  
  for (col in intersect(linear_cols, colnames(df_final))){
    attr(df_final[,col],"redun")<-"linear"
  }
  dict_final <- get.dict(df_final)
  df_final <- assign.dict(df_final, dict_final)
  
  
  return(list("rmcor_obj" = rmcor_obj,
              "redun_obj" = redundancy,
              "df_final" = df_final,
              "dict_final" = dict_final))
  
}





flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    var1 = rownames(cormat)[row(cormat)[ut]],
    var2 = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p = pmat[ut],
    stringsAsFactors = FALSE
  )
}

# corr df on numeric x
x_corr <- function(df_num, r_abs=0.8, type=c("pearson","spearman")[1]){
  # ---- Usage ----
  # calculate pair-wise correlations between numeric variables and give na_frac and suggested variables to keep in a dataframe
  
  # ---- Arguments ----
  # df_num: a dataframe only has numeric variables
  # r_abs: absolute value for the correlation threshold
  # type: one of the strings "spearman" or "pearson"
 
  # ---- Value ----
  # cor_df: correlation dataframe sorted by abs(corr) with na_fraction
  # cor_df_top: the top few correlation pairs based on the threshold
  ## leave blank on no significant coefficient
  
  
  if (type %in% c("pearson","spearman")){
    
    # correlation filtering
    #Hmisc::varclus(as.matrix(df_num))
    library(Hmisc)
    cor_mtrx <- Hmisc::rcorr(as.matrix(df_num), type=type)
    cor_df <- as.data.frame(flattenCorrMatrix(cor_mtrx$r, cor_mtrx$P))
    cor_df <- cor_df[order(-abs(cor_df$cor)),]
    cor_df_top <- cor_df[which(abs(cor_df$cor)>r_abs),]
    cor_df_top <- cor_df_top[order(-abs(cor_df_top$cor)),] 
    if (nrow(cor_df_top)>=1){
      rownames(cor_df_top) <- 1:nrow(cor_df_top)
      cor_df_top$var1_nafrac <- NA
      cor_df_top$var2_nafrac <- NA
      cor_df_top$cor_keep_var <- NA
      for (i in 1:nrow(cor_df_top)){
        cor_df_top[i,'var1_nafrac'] <- mean(is.na(df_num[,cor_df_top[i,'var1']]))
        cor_df_top[i,'var2_nafrac'] <- mean(is.na(df_num[,cor_df_top[i,'var2']]))
        cor_df_top[i,'cor_keep_var'] <- ifelse(cor_df_top[i,'var1_nafrac']>cor_df_top[i,'var2_nafrac'], cor_df_top[i,'var2'], cor_df_top[i,'var1'])
      }
    }
  }
  return(list("cor_df"=cor_df,
              "cor_df_top"=cor_df_top))
}

x_rmcor <- function(df_num, r_abs=0.8, type=c("pearson","spearman")[1], rank=FALSE, winsorizing=FALSE){
  # ---- Usage ----
  # calculate variables to keep or delete interatively based on correlation and missingness fraction
  
  # ---- Arguments ----
  # df_num: a dataframe only has numeric variables
  # r_abs: absolute value for the correlation threshold
  # type: one of the strings "spearman" or "pearson"
  # rank: whether or not calculate correlation between percentiles
  # winsorizing: whether or not cut 99.99 percentile outliers
  
  # ---- Value ----
  # keep: list of columns to keep 
  # dele: list of columns to remove
  # cor_trace: dataframe of corralation results used to make decisions in each iteration
  
  if(winsorizing){
    df_num <- winsorize(df_num)
  }
  
  if (rank){
    for(var in colnames(df_num)){
      df_num[,var] <- est_pctl(df_num[,var])
    }
  }
  
  
  org_cols <- colnames(df_num)
  cor_trace <- c()
  cor_df_obj <- x_corr(df_num, r_abs=r_abs, type=type)
  cor_df_top <- cor_df_obj$cor_df_top
  cor_df_org <- cor_df_obj$cor_df
  print(cor_df_top)
  i <- 0
  keep_cols <- colnames(df_num)
  while(nrow(cor_df_top)>0){
    i <- i+1
    cor_df_top$trace_id <- i
    cor_trace <- rbind(cor_trace, cor_df_top)
    keep_cols <- setdiff(colnames(df_num), union(unique(cor_df_top$var1), unique(cor_df_top$var2)) )
    print(keep_cols)
    keep_cols <- union(unique(cor_df_top$cor_keep_var), keep_cols)
    df_num <- df_num[, keep_cols]
    cor_df_obj <- x_corr(df_num, r_abs=r_abs, type=type)
    cor_df_top <- cor_df_obj$cor_df_top
  }
  return(list("keep" = keep_cols,
              "delete" = setdiff(org_cols, keep_cols),
              "cor_trace" = cor_trace,
              "cor_df_org" = cor_df_org))
  
}





