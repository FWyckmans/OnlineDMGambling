# Logistic Regression Metrics
ConfMat <- function(d, pred, DV){
  pred_y <<- as.numeric(pred > 0)
  true_y <<- as.numeric(d[[DV]] == 1)
  true_pos <<- (true_y == 1) & (pred_y == 1)
  true_neg <<- (true_y == 0) & (pred_y == 0)
  false_pos <<- (true_y == 0) & (pred_y == 1)
  false_neg <<- (true_y == 1) & (pred_y == 0)
  conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                       sum(false_neg), sum(true_neg)), 2, 2)
  colnames(conf_mat) <- c("PredOutcome = 1", "PredOutcome = 0")
  rownames(conf_mat) <- c("TrueOutcome = 1", "TrueOutcome = 0")
  return(conf_mat)
}

RLMetrics <- function(conf_mat, pred, printROC = F){
  # Precision = accuracy of positive predicted outcome
  Precision = conf_mat[1,1] /sum(conf_mat[,1])
  
  # Recall = Sensitivity = Strength of the model to predict a positive outcome
  Recall = conf_mat[1,1]/sum(conf_mat[1,])
  
  # Specificity = Strength of the model to predict a negative outcome
  Specificity = conf_mat[2, 2]/sum(conf_mat[2,])
  
  # DataFrame with metrics
  Metrics <- data.frame(Precision, Recall, Specificity)
  
  # ROC curve = TradeOff between recall and specificity
  idx <- order(-pred)
  recall <- cumsum(true_y[idx] == 1) / sum(true_y == 1)
  specificity <- (sum(true_y == 0) - cumsum(true_y[idx] == 0)) / sum(true_y == 0)
  roc_df <- data.frame(recall, specificity)
  
  if (isTRUE(printROC)){
    ROCPlot <- ggplot(roc_df, aes(x = specificity, y = recall)) +
      geom_line(color = 'blue')+
      scale_x_reverse() +
      scale_y_continuous() +
      geom_line(data = data.frame(x = (0:100)/100), aes(x = x, y = 1-x),
                linetype = "dotted", color = "red")
    print(ROCPlot)}
  
  # AUC = area under the ROC curve
  AUC <- sum(roc_df$recall[-1] * diff(1 - roc_df$specificity))
  Metrics <- cbind(Metrics, AUC)
  
  return(Metrics)
}
