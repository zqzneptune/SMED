# SMED NEWS

## Version 0.99.13
- **Code Improvements**:
  - Enhanced error handling across all scoring functions
  - Improved input validation in core functions
  - Refactored evaluate_complexes() (now 171 lines)
  
- **Documentation Updates**:
  - Added detailed biological interpretation sections
  - Expanded parameter explanations in all functions
  - Improved examples throughout documentation
  
- **Function-Specific Changes**:
  - score_ppi_by_dice(): Better error handling
  - score_ppi_by_pccn(): Improved noise handling
  - plot_elution_ridges(): Added power transformation

## Version 0.99.12
* Code quality improvements addressing BiocCheck report:
  - Replaced suppressWarnings() with proper error handling in:
    * scoring_dice.R (line 82)
    * scoring_pccn.R (line 106)
    * visualization_elution.R (line 105)
  - Refactored large functions:
    * evaluate_complexes() (165 lines → 80 lines)
    * filter_proteins_by_elution_consistency() (111 lines → 60 lines)
  - Added utility functions to utils.R:
    * calculate_intersection_matrix()
    * calculate_weighted_metric()
    * calculate_jaccard_matrix()
    * calculate_pr_matrix()
    * prepare_elution_matrix()
    * calculate_split_ratios()
    * filter_by_mad_threshold()

## Version 0.99.11
* Initial Bioconductor submission
* Added basic documentation examples for all man pages:
  - Created example for train_ensemble_ppi_models.Rd
* Fixed BiocCheck structural issues:
  - Removed SMED.BiocCheck folder
  - Added NEWS.md file
  - Added funding role to DESCRIPTION
* Verified all man pages contain runnable examples
