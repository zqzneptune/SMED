#' Generate dummy data for SMED package using real subsets
#'
#' This script generates `dummy_elution_matrix` and `dummy_train_complexes`
#' by subsetting real data from the Havugimana et al. (2012) dataset
#' included in the package. This ensures that example data is representative
#' of real biochemical fractionation mass spectrometry (BF-MS) experiments.

# Load required libraries
library(data.table)

# 1. Load and subset elution matrix
# We use LTQ_HeLaCE_WAX.csv as a representative sample
ex_path <- "inst/exdata/Havugimana_etal_2012/LTQ_HeLaCE_WAX.csv"

# Read CSV (first column is Protein ID, others are fractions)
# We use fread for efficiency
raw_data <- fread(ex_path)

# Take a subset of 100 proteins to keep dummy data small but representative
# This prevents the package from becoming too large while maintaining complexity
dummy_elution_matrix <- as.matrix(raw_data[1:100, -1])
rownames(dummy_elution_matrix) <- raw_data[[1]][1:100] # Use the first column as protein IDs

# 2. Load and subset reference complexes
ref_path <- "inst/exdata/Havugimana_etal_2012/RefComplexes.txt"

# Read tab-separated text file
ref_lines <- readLines(ref_path)
ref_list <- strsplit(ref_lines, "\t")
names(ref_list) <- paste0("Complex_", 1:length(ref_list))

# Filter complexes to only those containing proteins in our subset
subset_proteins <- rownames(dummy_elution_matrix)
filtered_ref_list <- lapply(ref_list, function(x) intersect(x, subset_proteins))

# Keep only complexes with at least 2 proteins in the subset
filtered_ref_list <- filtered_ref_list[sapply(filtered_ref_list, length) >= 2]

# 3. Convert to PPI pairs using GetComplexPPI
# Note: we source the function if SMED isn't loaded/installed
if (!exists("GetComplexPPI")) {
  source("R/GetComplexPPI.R")
}

# RcppAlgos is required by GetComplexPPI
if (!requireNamespace("RcppAlgos", quietly = TRUE)) {
  stop("RcppAlgos package is required to generate training PPIs.")
}

# Generate the training PPI object (TP and TN)
# We take the first 10 identified complexes for the dummy training set
dummy_train_complexes <- GetComplexPPI(filtered_ref_list[1:min(10, length(filtered_ref_list))])

# 4. Save to data/ directory
# These objects will be available via data(dummy_elution_matrix) etc.
save(dummy_elution_matrix, file = "data/dummy_elution_matrix.rda")
save(dummy_train_complexes, file = "data/dummy_train_complexes.rda")

message("Execution complete: Dummy data updated using a subset of Havugimana 2012 data.")
