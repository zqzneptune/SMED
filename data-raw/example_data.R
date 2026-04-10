#' Generate dummy data for SMED package
#' 
#' This script generates `dummy_elution_matrix` and `dummy_train_complexes`
#' to be used in package examples and testing.

# 1. Generate elution matrix
# 10 proteins, 20 fractions
set.seed(42)
proteins <- paste0("Prot", 1:10)
fractions <- paste0("Frac", 1:20)
dummy_elution_matrix <- matrix(
  rpois(200, lambda = 10), 
  nrow = 10, 
  ncol = 20,
  dimnames = list(proteins, fractions)
)
# Add some co-elution patterns (Prot1 and Prot2 co-elute)
dummy_elution_matrix["Prot1", ] <- dummy_elution_matrix["Prot1", ] + c(rep(0, 5), rep(50, 5), rep(0, 10))
dummy_elution_matrix["Prot2", ] <- dummy_elution_matrix["Prot2", ] + c(rep(0, 5), rep(45, 5), rep(0, 10))

# 2. Generate training complexes
# Using the GetComplexPPI function from the package
# We need to load it or define it here if the package isn't installed.
# For simplicity, we just create the output structure directly or load all.
# Since we are in the package dir, we can source it.
source("R/GetComplexPPI.R")
# RcppAlgos is needed for GetComplexPPI
library(RcppAlgos)

example_complexes <- list(
  Cpc1 = c("Prot1", "Prot2", "Prot3"),
  Cpc2 = c("Prot4", "Prot5")
)

dummy_train_complexes <- GetComplexPPI(example_complexes)

# 3. Save to data/
save(dummy_elution_matrix, file = "data/dummy_elution_matrix.rda")
save(dummy_train_complexes, file = "data/dummy_train_complexes.rda")

message("Dummy data generated and saved to data/")
