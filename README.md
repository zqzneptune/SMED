# SMED: Statistical Modelling of co-elution mass spectrometry data

## Description
The SMED package provides tools for analyzing co-elution mass spectrometry data to infer protein-protein interactions (PPIs). It implements statistical methods to assign probability scores to co-eluted proteins from biochemical fractionation mass spectrometry (BF-MS) experiments, with functionality for:

- Multiple PPI scoring methods (co-apex, Dice, mutual information, etc.)
- Protein complex evaluation and merging
- Visualization of results
- Comparison against reference datasets

## Installation
```r
# Install from GitHub using devtools
install.packages("devtools")
devtools::install_github("zqzneptune/SMED")

# Load the package
library(SMED)
```

## Basic Usage
```r
# Load example datasets
data_293NE12 <- load_cofrac_293NE12_hcw()
data_HeLaCE12 <- load_cofrac_HeLaCE12_tcs()

# Score PPIs using different methods
ppi_scores <- score_ppi_by_coapex(data_293NE12)
ppi_scores_mi <- score_ppi_by_mi(data_HeLaCE12)

# Evaluate against reference complexes
reference <- load_reference_corum_havugimana()
results <- evaluate_complexes(ppi_scores, reference)
```

## Examples
```r
# Generate and visualize PPI scores
ppi_scores <- score_ppi_by_wcc(data_293NE12)
plot_elution_ridges(ppi_scores)

# Compare multiple scoring methods
roc_results <- benchmark_ppi_score(list(
  coapex = score_ppi_by_coapex(data_HeLaCE12),
  dice = score_ppi_by_dice(data_HeLaCE12)
))
plot_multiple_roc(roc_results)
```

## Documentation
Key functions include:

### Data Loading
- `load_cofrac_293NE12_hcw()` - Load 293NE12 HCW dataset
- `load_reference_corum_havugimana()` - Load reference complexes

### PPI Scoring
- `score_ppi_by_coapex()` - Co-apex scoring
- `score_ppi_by_mi()` - Mutual information scoring
- `score_ppi_by_dice()` - Dice coefficient scoring

### Visualization
- `plot_elution_ridges()` - Elution profile visualization
- `plot_multiple_roc()` - ROC curve comparison

For complete documentation, see the package help files (`?function_name`) or browse the R documentation files.

## License
This package is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.