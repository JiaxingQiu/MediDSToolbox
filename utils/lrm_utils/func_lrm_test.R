lrm_test <- function(
  test_data,
  y_col,
  mdl_obj,
  threshold=0.5
){
  res_df <- NULL
  if(!is.null(test_data)){
    y_prob <- as.numeric(predict(mdl_obj, test_data))
    y_pred <- as.numeric(ifelse(y_prob<=threshold,0,1))
    y_true <- as.numeric(test_data[,y_col])
    stopifnot(length(y_pred)==length(y_true))
    res_df <- data.frame(logloss = MLmetrics::LogLoss(y_prob, y_true),
                         AUROC = ifelse(is.na(MLmetrics::AUC(y_prob, y_true)), round(pROC::auc(pROC::roc(y_true, y_prob)),6), MLmetrics::AUC(y_prob, y_true)),
                         AUPRC = MLmetrics::PRAUC(y_pred, y_true),
                         accuracy = MLmetrics::Accuracy(y_pred,y_true),
                         f1score = F1_Score(y_true = y_true, y_pred = y_pred)
    )
  }
  
  return(res_df)
}
