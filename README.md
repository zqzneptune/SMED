# SMED: Supervised Machine learning for Elution Data

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
devtools::install_github("zqzneptune/SMED", build_vignettes = TRUE)

```



## License
This package is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
