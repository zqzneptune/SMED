#' Generate Ridge Plot of Protein Elution Profiles
#'
#' Creates a ridge plot (also called joyplot) visualizing the elution profiles of 
#' proteins from fractionation experiments like SEC (Size Exclusion Chromatography)
#' or co-fractionation mass spectrometry. Each protein's profile is normalized 
#' (0-1 range) and optionally transformed before plotting.
#'
#' @param elution_data_matrix A numeric matrix where rows are proteins (must have 
#'   rownames) and columns are fractions (column names used for x-axis labels).
#' @param normalize_power Numeric, profiles are raised to this power after 
#'   0-1 normalization. Default 3 accentuates peaks while preserving relative 
#'   intensities. Use 1 for linear scaling, higher values emphasize strong peaks.
#' @param fill_color Character, color for filling the ridges. Default "#3182bd" 
#'   (blue). Can be a single color or vector matching number of proteins.
#' @param ridge_scale Numeric (0-1), scaling factor for ridge height. Lower values 
#'   create more separation between ridges. Default 0.9 provides good visibility 
#'   while preventing overlap.
#'
#' @return A `ggplot` object representing the ridge plot with:
#' \itemize{
#'   \item X-axis: Elution fractions (numeric or factor)
#'   \item Y-axis: Protein identifiers (ordered as input)
#'   \item Ridge height: Normalized intensity values
#'   \item Custom theme with clean styling
#' }
#' 
#' @section Biological Interpretation:
#' The plot visualizes co-elution patterns which can indicate:
#' \itemize{
#'   \item Protein complexes (shared elution peaks)
#'   \item Post-translational modifications (shifted peaks)
#'   \item Technical artifacts (broad/noisy profiles)
#' }
#' 
#' @section Customization Options:
#' The returned ggplot object can be modified with standard ggplot2 syntax:
#' \itemize{
#'   \item Add titles/labels with `+ labs()`
#'   \item Change colors with `+ scale_fill_*()`
#'   \item Adjust theme with `+ theme()`
#'   \item Add reference lines with `+ geom_vline()`
#' }
#'
#' @export
#' @importFrom reshape2 melt
#' @import ggplot2
#' @importFrom ggridges geom_density_ridges2
#' @seealso \code{\link{plot_elution_ridges}} (man page) for additional details
#' @examples
#' # Create example data with clear co-eluting groups
#' set.seed(123)
#' proteins <- c(paste0("ComplexA_", 1:3), 
#'               paste0("ComplexB_", 1:2),
#'               "Singleton1", "Singleton2")
#' fractions <- paste0("Frac", 1:24)
#' 
#' # Base matrix with noise
#' mat <- matrix(runif(length(proteins)*length(fractions), 0, 0.3), 
#'              nrow = length(proteins),
#'              dimnames = list(proteins, fractions))
#'              
#' # Add co-eluting peaks
#' mat[1:3, 8:12] <- mat[1:3, 8:12] + 
#'   matrix(rep(c(0.3, 0.8, 0.8, 0.3), each=3), nrow=3) # Complex A
#' mat[4:5, 15:19] <- mat[4:5, 15:19] + 
#'   matrix(rep(c(0.4, 0.9, 0.9, 0.4), each=2), nrow=2) # Complex B
#' 
#' # Plot with default parameters
#' if (requireNamespace("reshape2", quietly = TRUE) &&
#'     requireNamespace("ggridges", quietly = TRUE)) {
#'   p <- plot_elution_ridges(mat)
#'   print(p)
#'   
#'   # Customized version
#'   p + 
#'     ggplot2::labs(title = "Co-fractionation Profiles",
#'                  subtitle = "Showing two putative complexes") +
#'     ggplot2::scale_fill_manual(values = c(rep("#E41A1C",3), # Complex A red
#'                                         rep("#377EB8",2), # Complex B blue
#'                                         "#4DAF4A", "#984EA3")) # Singletons
#' }
plot_elution_ridges <- function(elution_data_matrix,
                              normalize_power = 3,
                              fill_color = "#3182bd",
                              ridge_scale = 0.9) {

  if (!is.matrix(elution_data_matrix) || !is.numeric(elution_data_matrix)) {
    stop("'elution_data_matrix' must be a numeric matrix.")
  }
  if (is.null(rownames(elution_data_matrix))) {
    stop("'elution_data_matrix' must have row names (protein identifiers).")
  }
  if (ncol(elution_data_matrix) == 0) {
    stop("'elution_data_matrix' must have at least one column (fraction).")
  }

  # Normalize each protein's profile to 0-1 range
  normalized_matrix <- t(apply(elution_data_matrix, 1, function(row_data) {
    current_min <- min(row_data, na.rm = TRUE)
    current_max <- max(row_data, na.rm = TRUE)
    range_val <- current_max - current_min
    if (is.infinite(current_min) || is.infinite(current_max) || range_val == 0) {
      return(rep(0, length(row_data)))
    }
    norm_row <- (row_data - current_min) / range_val
    norm_row[is.na(norm_row)] <- 0
    return(norm_row)
  }))
  colnames(normalized_matrix) <- colnames(elution_data_matrix)

  # Apply power transformation
  if (normalize_power != 1) {
    normalized_matrix <- normalized_matrix ^ normalize_power
  }

  # Melt data to long format for ggplot
  long_df <- reshape2::melt(as.matrix(normalized_matrix),
                          varnames = c("Protein", "Fraction_ID"),
                          value.name = "Intensity")

  # Convert fraction IDs to numeric if possible
  frac_has_numbers <- grepl("[0-9]", long_df$Fraction_ID)
  if (all(frac_has_numbers)) {
    numeric_frac_ids <- tryCatch(
      as.numeric(gsub("[^0-9.-]", "", long_df$Fraction_ID)),
      warning = function(w) {
        message("Some fraction IDs couldn't be fully converted to numbers: ",
                w$message)
        as.numeric(gsub("[^0-9.-]", "", long_df$Fraction_ID))
      }
    )
  } else {
    numeric_frac_ids <- rep(NA_real_, length(long_df$Fraction_ID))
  }
  if (!any(is.na(numeric_frac_ids))) {
    long_df$Fraction <- numeric_frac_ids
  } else {
    long_df$Fraction <- factor(long_df$Fraction_ID,
                             levels = colnames(normalized_matrix))
  }

  # Protein order for y-axis (reverse of input)
  long_df$Protein <- factor(long_df$Protein,
                          levels = rev(rownames(normalized_matrix)))

  # Create the ridge plot
  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(
      x = .data$Fraction,
      y = .data$Protein,
      height = .data$Intensity,
      group = .data$Protein,
      fill = .data$Protein
    )
  ) +
    ggridges::geom_density_ridges2(
      stat = "identity",
      scale = ridge_scale,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = rep(fill_color,
                                          nlevels(long_df$Protein))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, colour = "#000000"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size=10),
      axis.ticks.x = ggplot2::element_line(),
      axis.text.y = ggplot2::element_text(size = 10, colour = "#000000"),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.5,
                                               linetype = 'dotted'),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::labs(
      x = "Elution Fraction",
      y = "Protein",
      title = "Protein Elution Profiles"
    )

  return(p)
}
