# ========== USER GUIDE ========== #

# This script reconstitutes the random forest Fup model developed in Dawson et 
# al. (2021) in order to make predictions on new data sets. New data must be 
# preprocessed to match the expected input format; example code to assist with 
# this is included in the help files below.

# Required files:
# - fup_RFmodel_30desc_select_by_sqrt_transform.RData (RF.fit model)
# - Fup_mean_std_training_descriptors_DED052021.csv (standardization values)

# Help files:
# - dawson_fup_feature_names_in.txt (feature names expected by model)
# - dawson_fup_wetmore_prep.R (sample preprocessing code for new data sets)

# ========== USER INPUT ========== #

# Filename to read data for modeling
DATA_FILE <- "wetmore_data_processed.csv"
# Data label for output files
DATA_LABEL <- "wetmore"
# Are data already standardized to the training set?
STANDARDIZED <- FALSE

# ========== BEGIN SCRIPT ========== #

library(randomForest)

# Load the trained model from Dawson et al. (2021)
load("fup_RFmodel_30desc_select_by_sqrt_transform.RData")
feature_names_in <- row.names(RF.fit$importance)
# cat(feature_names_in, file="dawson_fup_feature_names_in.txt", sep="\n")

# Load data for modeling
df <- read.csv(DATA_FILE)

# If data are not already standardized, load the mean and SD values
# from the original training set and standardize them
if (!STANDARDIZED) {
  msd <- read.csv("Fup_mean_std_training_descriptors_DED052021.csv")
  for (feat in feature_names_in) {
    df[[feat]] <- (df[[feat]] - msd[[feat]][1]) / msd[[feat]][2]
  }
}

# Find rows with incomplete descriptors
mask_complete <- complete.cases(df[, feature_names_in])
# Write incomplete rows to file for examination
write.csv(df[!mask_complete, ], file=paste0(DATA_LABEL,"_incomplete.csv"), row.names=FALSE)
# Remove incomplete rows from modeling data
df <- df[mask_complete, ]

# Make predictions, squaring the predicted values since a 
# square-root transform was applied in model training
df$Fup_pred <- predict(RF.fit, df) ^ 2

# Calculate applicability domain using method of Roy et al. (2015)
p90 <- function(row) mean(row) + 1.28 * sd(row)
outside_ad <- function(row) if (min(row) > 3 || p90(row) > 3) 1 else 0
df$outside_ad <- apply(abs(df[, feature_names_in]), 1, outside_ad)

# Write predictions and applicability domain to file
write.csv(df, file=paste0(DATA_LABEL, "_dawson_fup_pred_with_ad.csv"), row.names=FALSE)
