#' Plot Elution Profiles as Ridge Plot
#'
#' This function visualizes protein elution profiles using a ridge plot, where
#' each protein's intensity across fractions is shown as a density-like ridge.
#'
#' @param dat A numeric matrix or data frame of protein elution profiles.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom ggridges geom_density_ridges2
#' @importFrom stats sd
#' @export
PlotEluteRidge <- function(dat){
  # Normalize to 0-1
  # Remove columns with zero variance
  dat <-
    dat[, apply(dat, 2, stats::sd, na.rm = TRUE) != 0, drop = FALSE]
  
  # Row-wise normalization
  row_mins <- apply(dat, 1, min, na.rm = TRUE)
  dat <- dat - row_mins
  row_maxs <- apply(dat, 1, max, na.rm = TRUE)
  # Avoid division by zero
  row_maxs[row_maxs == 0] <- 1
  dat <- dat / row_maxs
  
  # Exaggerate peaks
  dat <- dat ^ 3
  
  # Convert to long format using tidyr
  df_long <-
    as.data.frame(dat) %>%
    mutate(Protein = rownames(dat)) %>%
    tidyr::pivot_longer(cols = -Protein, names_to = "Frac", values_to = "Peptide")
  
  # Ensure Fraction is a factor or numeric for correct plotting
  df_long$Frac <- factor(df_long$Frac, levels = colnames(dat))

  p <-
    ggplot2::ggplot(
      df_long,
      ggplot2::aes(
        x = `Frac`,
        y = `Protein`,
        height = `Peptide`,
        group = `Protein`
      )
    ) +
    ggridges::geom_density_ridges2(
      stat = "identity",
      scale = 0.9,
      fill = "#3182bd") +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 12, colour = "#000000"),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 10, colour = "#000000"),
          axis.ticks.y = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          legend.position = "none") +
    ggplot2::labs(x = "Eluted Fractionations", y = "Subunits", title = "")
    
  return(p)
}


