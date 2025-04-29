# ========== USER GUIDE ========== #

# This is an example of the preprocessing code required to transform raw PADEL
# descriptor calculations and OPERA predicted properties into the input format
# expected by the random forest Fup model developed in Dawson et al. (2021). It
# may be adapted to suit other data sets.

# Required file:
# - dawson_fup_feature_names_in.txt (feature names expected by model)

# ========== USER INPUT ========== #

# File containing calculated PADEL descriptors
PADEL_FILE <- "Wetmore_chems_PadelDesc.csv"
# Column of PADEL file containing compound index/ID
PADEL_INDEx_COL <- "Name"
# Ditto for OPERA descriptors
OPERA_FILE <- "Wetmore_chems-txt_OPERA2.6Pred.csv"
OPERA_INDEX_COL <- "MoleculeID"
# File to write processed output
OUTPUT_FILE <- "wetmore_data_processed.csv"

# ========== BEGIN SCRIPT ========== #

library(dplyr)

# Read expected feature names from help file
feature_names_in <- readLines("dawson_fup_feature_names_in.txt")

# Separate lists of OPERA and PADEL descriptors
feature_sets <- split(
  feature_names_in, 
  ifelse(grepl("^padel_", feature_names_in), "PADEL", "OPERA")
)

# Process PADEL descriptor table
padel_index_col_sym <- sym(PADEL_INDEx_COL)
padel_df <- read.csv(PADEL_FILE) %>%
  # Add "padel_" tag to all columns except the index
  rename_with(~ paste0("padel_", .x), -!!padel_index_col_sym) %>%
  # Select only features expected by the model
  select(all_of(c(PADEL_INDEx_COL, feature_sets$PADEL)))

# Process OPERA descriptor table
opera_index_col_sym <- sym(OPERA_INDEX_COL)
opera_df <- read.csv(OPERA_FILE) %>%
  # Select only index and predicted value columns
  select(!!opera_index_col_sym | ends_with("_pred")) %>%
  # Remove "_pred" and add "Opera_" tag to all columns except the index
  rename_with(~ paste0("Opera_", gsub("_pred$", "", .x)), -!!opera_index_col_sym) %>%
  # Select only features expected by the model
  select(all_of(c(OPERA_INDEX_COL, feature_sets$OPERA)))

# Join OPERA and PADEL descriptors
final_df <- full_join(
  padel_df, 
  opera_df, 
  by=join_by(!!padel_index_col_sym == !!opera_index_col_sym)
)

# Write to output file
write.csv(final_df, file=OUTPUT_FILE, row.names=FALSE)
