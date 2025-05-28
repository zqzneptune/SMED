#  This script extracts and saves elution profiles and protein complex data from the supplementary materials of:

#  > **Havugimana, Pierre C., et al.** (2012). *A census of human soluble protein complexes*. **Cell**, 150(5), 1068–1081.

# **Elution profiles** are retrieved from **Table S1** (`mmc1`)
# **Reference and predicted complexes** are retrieved from **Table S3** (`mmc3.xls`)

# Function to extract and save elution data for a given pattern
# save_elution_data <- function(data, pattern, output_file) {
#   elution_matrix <- as.matrix(data[, grepl(pattern, colnames(data))])
#   rownames(elution_matrix) <- data$Accession
#   saveRDS(elution_matrix, file = output_file)
# }

# Load Table S1 (assumed to be pre-loaded as `mmc1`)
# Save elution profiles for different experimental conditions
# save_elution_data(mmc1, "LTQ_293NE12_HCW", "CoFrac_Havugimana_PC_Cell_2012_LTQ_293NE12_HCW.RDS")
# save_elution_data(mmc1, "LTQ_HeLaNE45_TCS", "CoFrac_Havugimana_PC_Cell_2012_LTQ_HeLaNE45_TCS.RDS")
# save_elution_data(mmc1, "LTQ_HeLaCE12_TCS", "CoFrac_Havugimana_PC_Cell_2012_LTQ_HeLaCE12_TCS.RDS")

# Load CORUM reference complexes from Table S3 (Sheet: CORUMreference_complexes)
# Assumed to be pre-loaded as `CORUMreference_complexes`

# corum_complexes <- unstack(CORUMreference_complexes[, c("Component-Accession ID", "Merged reference complex")])
# saveRDS(corum_complexes, file = "RefCORUM_Havugimana_PC_Cell_2012_n_324.RDS")
# Load predicted complexes from Table S3 (Sheet: Predicted_complexes)
# Assumed to be pre-loaded as `fileComplex`

# predicted_complexes <- strsplit(fileComplex$`Co-complex membership_UniProtKB AC`, ",")
# names(predicted_complexes) <- fileComplex$`Predicted complex`
# saveRDS(predicted_complexes, file = "PredCpx_Havugimana_PC_Cell_2012_n_622.RDS")
