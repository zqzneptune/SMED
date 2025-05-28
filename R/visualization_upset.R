#' Plot an UpSet Diagram from a List of Item Sets
#'
#' Generates an UpSet plot to visualize intersections of elements across
#' different named sets. This function takes a named list where names
#' represent the sets, and each list element is a vector of elements
#' belonging to that set. Particularly useful for biological data analysis
#' like gene sets, protein complexes, or pathway memberships.
#'
#' @param item_list A named list. Each name is treated as a "set ID",
#'   and the corresponding value must be a vector (typically character)
#'   of "elements" belonging to that set. For example:
#'   `list(SetA = c("gene1", "gene2"), SetB = c("gene2", "gene3"))`.
#'   In biological contexts, sets could represent:
#'   - Different experimental conditions
#'   - Various protein complex prediction methods  
#'   - Multiple pathway databases
#' @param n_intersections Integer, the maximum number of intersections to display
#'   in the UpSet plot. Passed to `ggupset::scale_x_upset()`.
#'   Defaults to 20. Lower values simplify complex plots by showing only the
#'   largest intersections. Higher values reveal more detailed patterns but
#'   may become cluttered.
#'
#' @return A `ggplot` object representing the UpSet plot. This can be further
#'   customized using standard `ggplot2` functions like:
#'   - `theme()` for styling
#'   - `labs()` for axis labels
#'   - `scale_fill_*()` for color schemes
#'
#' @details
#' The UpSet plot consists of three main components:
#' 1. Matrix view (top): Shows which sets participate in each intersection
#' 2. Intersection size bar plot (bottom): Shows size of each intersection
#' 3. Set size bar plot (left): Shows size of individual sets
#'
#' Intersection analysis:
#' - Each bar represents the count of elements present in the exact combination
#'   of sets shown in the matrix above it
#' - Elements must be present in ALL sets of the combination to be counted
#' - The intersection size is calculated by ggupset using the list-column
#'   of sets in the input data frame
#' - Biologically, large intersections may indicate:
#'   * Core components conserved across conditions/methods
#'   * Highly confident predictions shared by multiple approaches
#'   * Essential pathway members present in multiple databases
#'
#' Matrix configuration:
#' - The matrix shows presence (filled dot) or absence (empty space) of each set
#'   in the intersection
#' - Sets are ordered by size (largest first) on the left bar plot
#' - Intersections are ordered by size (largest first) from left to right
#'
#' Set size handling:
#' - Individual set sizes are shown in the left bar plot
#' - These represent counts of unique elements in each set, regardless of
#'   intersections with other sets
#' - Set sizes are calculated by ggupset from the input list-column
#' - In biological data, large sets may indicate:
#'   * Comprehensive experimental conditions
#'   * Broad prediction methods
#'   * General pathway categories
#'
#' The function first processes the input `item_list` using `utils::stack` and
#' `utils::unstack`. This step typically results in a list where individual
#' elements within each set are factors. This list of sets is then organized
#' into a data frame where one column (named 'Term' internally) is a
#' list-column containing these sets. This data frame structure is then used by
#' `ggupset` to generate the plot.
#'
#' See also \code{\link{plot_upset_from_item_list.Rd}} for additional details.
#'
#' Ensure that the `ggplot2` and `ggupset` packages are installed.
#' This function requires them to be listed in the `Imports:` field of your
#' package's DESCRIPTION file. `utils` is a base package.
#'
#' @examples
#' # Sample data: a list of gene sets for different conditions
#' sample_sets <- list(
#'   Condition_A = c("Gene1", "Gene2", "Gene3", "Gene4"),
#'   Condition_B = c("Gene3", "Gene4", "Gene5", "Gene6"),
#'   Condition_C = c("Gene1", "Gene5", "Gene7"),
#'   Condition_D = c("Gene8") # A set with unique elements
#' )
#'
#' # Generate the UpSet plot
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("ggupset", quietly = TRUE)) {
#'   upset_plot <- plot_upset_from_item_list(sample_sets, n_intersections = 10)
#'   # Customize the plot
#'   upset_plot +
#'     ggplot2::labs(title = "Gene Overlap Across Experimental Conditions") +
#'     ggplot2::theme(plot.title = element_text(hjust = 0.5))
#' }
#'
#' # Biological example with protein complexes
#' complex_sets <- list(
#'   CORUM = c("P0DP23", "P62258", "P61981"), # Reference complexes
#'   BioPlex = c("P0DP23", "P62258", "Q9Y6K9"), # Experimental data
#'   HuMAP = c("P62258", "P61981", "Q15021")   # Computational predictions
#' )
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("ggupset", quietly = TRUE)) {
#'   # Visualize overlap between complex prediction methods
#'   upset_plot <- plot_upset_from_item_list(complex_sets)
#'   # print(upset_plot)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_bar geom_text after_stat labs theme_minimal
#' @importFrom ggupset scale_x_upset
#' @importFrom utils stack unstack
#'
#' @export
plot_upset_from_item_list <- function(item_list, n_intersections = 20) {

  # --- Input Validation ---
  if (!is.list(item_list) || (length(item_list) > 0 &&
                              is.null(names(item_list)))) {
    stop(
      "'item_list' must be a named list where each element is a vector."
    )
  }
  if (length(item_list) > 0 &&
      !all(vapply(item_list, is.vector, logical(1)))) {
    stop("All elements of 'item_list' must be vectors.")
  }
  if (!is.numeric(n_intersections) || length(n_intersections) != 1 ||
      n_intersections <= 0 || is.na(n_intersections)) {
    warning(
      "'n_intersections' should be a single positive integer. ",
      "Using default of 20."
    )
    n_intersections <- 20
  } else {
    n_intersections <- as.integer(n_intersections)
  }


  # --- Data Preparation (following original logic) ---
  if (length(item_list) == 0) {
    message("Input 'item_list' is empty. Returning an empty plot frame.")
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title = "Empty Input: No Data for UpSet Plot",
          x = "Set Intersections", y = "Intersection Size"
        ) +
        ggplot2::theme_minimal()
    )
  }

  # 1. Stack the input list: creates 'values' and 'ind' columns.
  # 'values' will likely become factors if original elements were characters.
  stacked_data <- utils::stack(item_list)

  # 2. Reorder columns to 'ind', 'values' for unstack's default behavior.
  # This step is crucial if `unstack` is to correctly identify
  # which column forms the list names and which forms the list elements.
  if (!all(c("ind", "values") %in% names(stacked_data))) {
    # This case should ideally not happen with typical list inputs to stack
    stop("stack() did not produce 'ind' and 'values' columns.")
  }
  reordered_stacked_data <- stacked_data[, c("ind", "values")]

  # 3. Unstack to reconstruct the list. Elements within each list item
  #    will be factors if they originated from character vectors.
  processed_list_of_sets <- utils::unstack(reordered_stacked_data)

  # 4. Create the data frame for ggplot, with a list-column.
  # `Item` column for set names, `Term` column for the list of sets.
  # The names of `processed_list_of_sets` come from the `ind` column.
  plot_input_df <- data.frame(
    Item = names(processed_list_of_sets),
    stringsAsFactors = FALSE # For the 'Item' column
  )
  plot_input_df$Term <- processed_list_of_sets # Assigns the list-column

  # --- Plotting ---
  # The 'Term' column (the list-column of sets) is passed to aes(x=...).
  # ggupset processes this list to find intersections.
  p <- ggplot2::ggplot(plot_input_df, ggplot2::aes(x = .data$Term)) +
    ggplot2::geom_bar(fill = "steelblue", na.rm = TRUE) +
    ggplot2::geom_text(
      stat = "count",
      ggplot2::aes(label = ggplot2::after_stat(.data$count)),
      vjust = -0.5, # Position text above bars
      size = 3,
      na.rm = TRUE
    ) +
    ggupset::scale_x_upset(n_intersections = n_intersections) +
    ggplot2::labs(
      title = "UpSet Plot of Set Intersections",
      x = "Set Intersections", # As defined by ggupset
      y = "Intersection Size"  # Number of elements in intersection
    ) +
    ggplot2::theme_minimal(base_size = 12)

  return(p)
}
