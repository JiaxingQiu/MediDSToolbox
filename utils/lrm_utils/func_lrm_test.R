lrm_test <- function(
  test_data,
  y_col,
  mdl_obj,
  threshold=0.5
){
  
  # create score dataframe
  res_df <- data.frame(message="external validation initiated")
  if (is.null(test_data)){
    res_df <- data.frame(message="external validation test_data is NULL")
  }else{
    
    # ---- test data is engineered the same way to have valid scores ----
    tryCatch({
      test_data$y_true <- as.numeric(as.character( test_data[,y_col] ) )
      test_data$y_prob <- as.numeric(logit2prob(predictrms(mdl_obj, newdata = test_data)) )
      test_data_score <- test_data %>% 
        filter(!is.na(y_prob) & !is.na(y_true) & y_true%in%c(0,1)) %>%
        as.data.frame()
      y_prob <- as.numeric(test_data_score$y_prob)
      y_pred <- as.numeric(ifelse(y_prob<=threshold,0,1))
      y_true <- as.numeric(test_data_score$y_true)
      stopifnot(all(!is.na(y_true)))
      stopifnot(length(y_pred)==length(y_true))
      stopifnot(length(y_prob)==length(y_true))
      logloss <- NA
      tryCatch({
        logloss = MLmetrics::LogLoss(y_prob, y_true)
      },error=function(e){
        print(e)
      })
      AUROC <- NA
      tryCatch({
        AUROC = ifelse(is.na(MLmetrics::AUC(y_prob, y_true)), round(pROC::auc(pROC::roc(y_true, y_prob)),6), MLmetrics::AUC(y_prob, y_true))
      },error=function(e){
        print(e)
      })
      AUPRC <- NA
      tryCatch({
        AUPRC = MLmetrics::PRAUC(y_pred, y_true)
      },error=function(e){
        print(e)
      })
      accuracy <- NA
      tryCatch({
        accuracy = MLmetrics::Accuracy(y_pred,y_true)
      },error=function(e){
        print(e)
      })
      f1score <- NA
      tryCatch({
        f1score = F1_Score(y_true = y_true, y_pred = y_pred)
      },error=function(e){
        print(e)
      })
      res_df <- data.frame(
        data = "test data",
        logloss = logloss,
        AUROC = AUROC,
        AUPRC = AUPRC,
        accuracy = accuracy,
        f1score = f1score)
    },error=function(e){
      print("---- fail to get score df for test_data ----")
      print(e)
    })
  }
  
  test_data <- test_data[,union(c("y_prob",y_col),colnames(test_data))]
  
  return(list("res_df"=res_df,
              "test_data"=test_data))
}
