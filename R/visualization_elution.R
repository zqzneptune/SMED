#' Generate Ridge Plot of Protein Elution Profiles
#'
#' Creates a ridge plot visualizing the elution profiles of proteins from a
#' fractionation experiment. Each protein's profile is normalized (0-1 range)
#' and optionally transformed before plotting.
#'
#' @param elution_data_matrix A numeric matrix where rows are proteins (named)
#'   and columns are fractions.
#' @param normalize_power Numeric, profiles are raised to this power after
#'   0-1 normalization. Default is 3, which can accentuate peaks. Use 1 for
#'   no power transformation.
#' @param fill_color Character, color for filling the ridges. Default "#3182bd".
#' @param ridge_scale Numeric, scaling factor for the height of the ridges.
#'   Passed to `ggridges::geom_density_ridges2`. Default 0.9.
#'
#' @return A `ggplot` object representing the ridge plot.
#' @export
#' @importFrom reshape2 melt
#' @import ggplot2
#' @importFrom ggridges geom_density_ridges2
#' @examples
#' # Create dummy elution data
#' set.seed(123)
#' prot_names <- paste0("Protein", 1:5)
#' frac_names <- paste0("Frac", 1:20)
#' mat <- matrix(runif(5 * 20, 0, 0.5), nrow = 5, ncol = 20,
#'               dimnames = list(prot_names, frac_names))
#' # Add some peaks
#' mat["Protein1", 5:8] <- mat["Protein1", 5:8] + c(0.5, 1, 1, 0.5)
#' mat["Protein3", 10:13] <- mat["Protein3", 10:13] + c(0.4, 0.8, 0.8, 0.4)
#' mat["Protein5", 1:20] <- mat["Protein5", 1:20] + runif(20,0,0.1) # noisy
#'
#' if (requireNamespace("reshape2", quietly = TRUE) &&
#'     requireNamespace("ggridges", quietly = TRUE)) {
#'   ridge_plot <- plot_elution_ridges(mat)
#'   # print(ridge_plot)
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


  # Remove columns with zero standard deviation (all same values)
  # as they don't provide information for plotting and can cause issues
  # in normalization if all values are zero after min subtraction.
  col_sds <- apply(elution_data_matrix, 2, stats::sd, na.rm = TRUE)
  # Keep columns with sd > small_epsilon or if all are NA (sd is NA)
  # This ensures we don't remove columns that are entirely NA if user wants them
  # However, geom_density_ridges2 will likely ignore them.
  # A simpler approach: if a column is problematic for normalization, skip it.
  # The normalization below is row-wise, so col_sds is not directly used there.
  # Original: dat <- dat[, apply(dat, 2, sd) != 0] - this might remove all cols.
  # Let's proceed with row-wise normalization first.

  # Normalize each protein's profile to 0-1 range
  normalized_matrix <- elution_data_matrix
  # Handle NAs by temporarily replacing with a value outside typical range (e.g., -Inf)
  # so they don't interfere with min/max, then restore. Or handle per row.

  normalized_matrix <- t(apply(elution_data_matrix, 1, function(row_data) {
    current_min <- min(row_data, na.rm = TRUE)
    current_max <- max(row_data, na.rm = TRUE)
    range_val <- current_max - current_min
    if (is.infinite(current_min) || is.infinite(current_max) || range_val == 0) {
      # If row is all NA, or all same value, or contains Inf/-Inf
      # return zeros or original data, depending on desired behavior
      # For plotting, setting to 0 makes sense if it cannot be normalized
      return(rep(0, length(row_data)))
    }
    norm_row <- (row_data - current_min) / range_val
    norm_row[is.na(norm_row)] <- 0 # Set original NAs to 0 after normalization attempt
    return(norm_row)
  }))
  colnames(normalized_matrix) <- colnames(elution_data_matrix)


  # Apply power transformation
  if (normalize_power != 1) {
    normalized_matrix <- normalized_matrix ^ normalize_power
  }

  # Melt data to long format for ggplot
  # Ensure fractions are treated as numeric/ordered for x-axis
  long_df <- reshape2::melt(as.matrix(normalized_matrix),
                            varnames = c("Protein", "Fraction_ID"),
                            value.name = "Intensity")

  # Attempt to make Fraction_ID numeric if it represents fraction numbers
  # This helps ggplot order fractions correctly on x-axis.
  # If colnames are like "Frac1", "Frac2", extract numbers.
  if (is.factor(long_df$Fraction_ID)) {
    long_df$Fraction_ID <- as.character(long_df$Fraction_ID)
  }
  numeric_frac_ids <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "",
                                                       long_df$Fraction_ID)))
  if (!any(is.na(numeric_frac_ids))) {
    long_df$Fraction <- numeric_frac_ids
  } else {
    # If conversion fails, use factor levels to maintain order
    long_df$Fraction <- factor(long_df$Fraction_ID,
                               levels = colnames(normalized_matrix))
  }

  # Protein order for y-axis (default is alphabetical, or reverse of input)
  # For ridges, often plotted with first protein at top, so reverse order
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
      fill = .data$Protein # Could be a fixed color or mapped
    )
  ) +
    ggridges::geom_density_ridges2(
      stat = "identity", # Use actual intensity values for height
      scale = ridge_scale,
      show.legend = FALSE # Typically no legend if fill is fixed or by Protein
    ) +
    ggplot2::scale_fill_manual(values = rep(fill_color,
                                            nlevels(long_df$Protein))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12, colour = "#000000"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size=10), # Make X axis readable
      axis.ticks.x = ggplot2::element_line(), # Show X ticks
      axis.text.y = ggplot2::element_text(size = 10, colour = "#000000"),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.5,
                                                 linetype = 'dotted'),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(), # Cleaner y-axis
      plot.background = ggplot2::element_blank(),
      legend.position = "none" # As fill is Protein or fixed
    ) +
    ggplot2::labs(
      x = "Elution Fraction",
      y = "Protein",
      title = "Protein Elution Profiles"
    )

  return(p)
}
