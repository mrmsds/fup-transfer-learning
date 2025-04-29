library(randomForest)

load("fup_RFmodel_30desc_select_by_sqrt_transform.RData")
feature_names_in <- row.names(RF.fit$importance)

train_df <- read.csv("dawson_fup_orig_train.csv")
pfas_df <- read.csv("wetmore_data_processed.csv")

msd <- read.csv("Fup_mean_std_training_descriptors_DED052021.csv")
for (feat in feature_names_in) {
  pfas_df[[feat]] <- (pfas_df[[feat]] - msd[[feat]][1]) / msd[[feat]][2]
}

train_preds <- predict(RF.fit, train_df, nodes=TRUE)
pfas_preds <- predict(RF.fit, pfas_df, nodes=TRUE)

n <- nrow(pfas_df)
m <- nrow(train_df)
prox <- matrix(0, n, m)
for (i in 1:n) {
  for (j in 1:m) {
    prox[i, j] <- sum(attr(pfas_preds, "nodes")[i, ] == attr(train_preds, "nodes")[j, ]) / RF.fit$ntree
  }
}

neighbor_idx <- apply(prox, 1, which.max)
pfas_df$Fup_pred <- c(pfas_preds) ^ 2
pfas_df$neighbor_dtxsid <- train_df[neighbor_idx, 2]
pfas_df$neighbor_name <- train_df[neighbor_idx, 1]
pfas_df$neighbor_prox <- apply(prox, 1, max)
write.csv(pfas_df, "wetmore_dawson_fup_pred_with_prox.csv", row.names=FALSE)