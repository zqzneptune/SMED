#' Plot Multiple ROC Curves
#'
#' Creates a composite plot of one or more Receiver Operating Characteristic (ROC) 
#' curves from the `pROC` package. The plot includes:
#' - Individual ROC curves with customizable colors
#' - Area Under the Curve (AUC) values displayed in legend
#' - Optional smoothing of ROC curves
#' - Diagonal reference line (random classifier)
#'
#' @section ROC Curve Interpretation:
#' The ROC curve plots the true positive rate (sensitivity) against the false 
#' positive rate (1-specificity) across different classification thresholds. 
#' Points above the diagonal indicate better-than-random performance. The closer 
#' the curve follows the left-hand border and top border, the more accurate the 
#' test.
#'
#' @section AUC Calculation:
#' The Area Under the Curve (AUC) is calculated using the trapezoidal rule as 
#' implemented in `pROC::auc()`. AUC ranges from 0.5 (random classifier) to 1 
#' (perfect classifier). AUC represents the probability that the classifier will 
#' rank a randomly chosen positive instance higher than a randomly chosen 
#' negative one.
#'
#' @param roc_object_list A named list of ROC curve objects (class `roc` from 
#'   `pROC` package). Names are used in the legend. Each ROC object should be 
#'   created using `pROC::roc()` with response and predictor vectors.
#' @param plot_title Character string for the main title of the plot (default: 
#'   "ROC Curves").
#' @param color_palette A vector of color strings to use for plotting the ROC 
#'   curves. If `NULL` (default), uses `RColorBrewer::brewer.pal` "Dark2" for 
#'   up to 8 curves, then falls back to `rainbow()`. Length should match number 
#'   of curves.
#' @param smooth_curves Logical indicating whether to smooth ROC curves using 
#'   `pROC::smooth()` with density method (default: TRUE). Smoothing can help 
#'   visualize the general trend when curves appear jagged.
#' @param legend_position_x Numeric x-coordinate for legend position (range 
#'   0-1, default: 0.55).
#' @param legend_position_y Numeric y-coordinate for legend position (range 
#'   0-1, default: 0.60).
#' @param line_width Numeric line width for ROC curves (default: 1.5).
#'
#' @section Parameter Effects:
#' - `smooth_curves`: When TRUE, applies kernel smoothing which can make curves 
#'   appear more regular but may obscure small-scale variations
#' - `legend_position`: Adjust these to avoid overlap with curves in crowded plots
#' - `line_width`: Thicker lines improve visibility in presentations/publications
#'
#' @return Invisibly returns `NULL`. The function is called for its side effect 
#'   of generating a plot with:
#'   - ROC curves for each input
#'   - AUC values displayed in legend
#'   - Diagonal reference line
#'   - Properly labeled axes
#'
#' @export
#' @importFrom pROC plot.roc lines.roc auc smooth
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics par text
#' @seealso \code{\link{plot_multiple_roc}} (man page) for additional examples
#' @examples
#' if (requireNamespace("pROC", quietly = TRUE) &&
#'     requireNamespace("RColorBrewer", quietly = TRUE)) {
#'   # Create example ROC objects
#'   set.seed(123)
#'   response <- rbinom(100, 1, 0.5)
#'   pred1 <- rnorm(100)
#'   pred2 <- rnorm(100, mean = ifelse(response == 1, 0.5, 0))
#'   pred3 <- rnorm(100, mean = ifelse(response == 1, 1, 0))
#'   
#'   roc1 <- pROC::roc(response, pred1, quiet = TRUE)
#'   roc2 <- pROC::roc(response, pred2, quiet = TRUE)
#'   roc3 <- pROC::roc(response, pred3, quiet = TRUE)
#'   
#'   # Basic plot with default settings
#'   plot_multiple_roc(list(Random = roc1, Moderate = roc2, Strong = roc3),
#'                    "Classifier Comparison")
#'                    
#'   # Customized plot with manual colors and no smoothing
#'   plot_multiple_roc(
#'     list(Random = roc1, Moderate = roc2, Strong = roc3),
#'     plot_title = "Customized ROC Plot",
#'     color_palette = c("blue", "green", "red"),
#'     smooth_curves = FALSE,
#'     legend_position_x = 0.6,
#'     legend_position_y = 0.5,
#'     line_width = 2
#'   )
#' }
plot_multiple_roc <- function(
    roc_object_list,
    plot_title = "ROC Curves",
    color_palette = NULL,
    smooth_curves = TRUE,
    legend_position_x = 0.55,
    legend_position_y = 0.60,
    line_width = 1.5) {

  if (!is.list(roc_object_list) || length(roc_object_list) == 0) {
    stop("'roc_object_list' must be a non-empty list of pROC::roc objects.")
  }
  if (is.null(names(roc_object_list))) {
    names(roc_object_list) <- paste0("ROC_", seq_along(roc_object_list))
    warning("ROC object list was unnamed. Generic names assigned.")
  }
  if (!all(sapply(roc_object_list, inherits, "roc"))) {
    stop("All elements in 'roc_object_list' must be of class 'roc'.")
  }


  num_roc <- length(roc_object_list)

  if (is.null(color_palette)) {
    if (num_roc <= 8) {
      color_palette <- RColorBrewer::brewer.pal(max(3, num_roc), "Dark2")
    } else {
      # Fallback for more than 8 ROCs (Dark2 maxes out)
      color_palette <- grDevices::rainbow(num_roc)
    }
  } else if (length(color_palette) < num_roc) {
    warning("Color palette is shorter than the number of ROC curves.",
            " Colors will be recycled.")
    color_palette <- rep(color_palette, length.out = num_roc)
  }

  # Plot the first ROC curve
  current_roc_obj <- roc_object_list[[1]]
  if (smooth_curves) current_roc_obj <- pROC::smooth(current_roc_obj,
                                                     method="density")

  # Store original par settings
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par)) # Restore on exit
  graphics::par(pty = "s") # Square plotting region

  pROC::plot.roc(
    current_roc_obj,
    legacy.axes = TRUE, # For 1-Specificity on x-axis
    identity = FALSE,   # Don't plot identity line by default with pROC
    grid = TRUE,        # Add a grid
    xlim = c(1, 0),     # Specificity from 0 to 1 (FPR from 1 to 0)
    xlab = "False Positive Rate (1 - Specificity)",
    ylab = "True Positive Rate (Sensitivity)",
    main = plot_title,
    col = color_palette[1],
    lwd = line_width,
    print.auc = FALSE # We'll add AUC manually for custom placement
  )
  graphics::abline(a = 1, b = -1, lty = 2, col = "grey") # Add identity line

  # Add AUC text for the first ROC
  auc_val_first <- pROC::auc(roc_object_list[[1]]) # Use original for AUC
  graphics::text(
    legend_position_x,
    legend_position_y,
    labels = sprintf("Sample (AUC)"), # Header for legend
    adj = c(0, 1), # Align text: 0 for left, 1 for top
    cex = 0.8
  )
  graphics::text(
    legend_position_x,
    legend_position_y - 0.07 * 1, # Position below header
    labels = paste0(names(roc_object_list)[1], ": ",
                    sprintf("%.3f", auc_val_first)),
    col = color_palette[1],
    adj = c(0, 1),
    cex = 0.8
  )

  # Plot subsequent ROC curves if any
  if (num_roc > 1) {
    for (i in 2:num_roc) {
      roc_to_plot <- roc_object_list[[i]]
      if (smooth_curves) roc_to_plot <- pROC::smooth(roc_to_plot,
                                                     method="density")

      pROC::lines.roc(
        roc_to_plot,
        col = color_palette[i],
        lwd = line_width
      )
      # Add AUC text for this ROC
      auc_val_current <- pROC::auc(roc_object_list[[i]])
      graphics::text(
        legend_position_x,
        legend_position_y - 0.07 * (i), # Adjust y based on loop index
        labels = paste0(names(roc_object_list)[i], ": ",
                        sprintf("%.3f", auc_val_current)),
        col = color_palette[i],
        adj = c(0, 1),
        cex = 0.8
      )
    }
  }
  return(invisible(NULL))
}
