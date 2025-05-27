#' Create an UpSet Plot from a List of Sets
#'
#' Generates an UpSet plot to visualize intersections of multiple sets.
#' The input is a named list where each element is a vector representing a set,
#' and names represent the items. The plot shows the sizes of intersections.
#'
#' @param item_sets_list A named list where names are items and each element
#'   is a character vector of set names that the item belongs to.
#'   Example: `list(ItemA = c("Set1", "Set2"), ItemB = c("Set2", "Set3"))`.
#' @param num_intersections Integer, the maximum number of intersections to
#'   display in the UpSet plot. Passed to `ggupset::scale_x_upset`. Default 20.
#' @param main_bar_fill_color Character, color for the main intersection bars.
#'   Default "steelblue".
#' @param text_size_count Integer, size of the text labels for intersection counts.
#'   Default 3.
#'
#' @return A `ggplot` object representing the UpSet plot.
#' @export
#' @import ggplot2
#' @importFrom ggupset scale_x_upset axis_combmatrix
#' @importFrom dplyr `%>%` group_by summarize
#' @examples
#' # Example data: list of items and the sets they belong to
#' upset_data <- list(
#'   Item1 = c("SetA", "SetB"),
#'   Item2 = c("SetB", "SetC"),
#'   Item3 = c("SetA", "SetC"),
#'   Item4 = c("SetA"),
#'   Item5 = c("SetB"),
#'   Item6 = c("SetA", "SetB", "SetC"),
#'   Item7 = c("SetD") # A set not overlapping much
#' )
#'
#' # Convert to the expected format for this function:
#' # A data frame where one column is 'Item' and another is 'Sets' (a list column)
#' if (requireNamespace("ggupset", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE) &&
#'     requireNamespace("tidyr", quietly = TRUE) ){
#'
#'   # Create data in the format ggplot(aes(x=Sets)) expects
#'   # This requires items to be observations, and sets they belong to
#'   # are in a list-column.
#'   # The original function's input `listTerms` was `list(SetA=c(I1,I2), SetB=c(I1,I3))`.
#'   # `PlotUpset(list(A=c("AA","BB","CC"), B=c("BB","CC","DD")))`
#'   # This means AA is in A, BB in A&B, CC in A&B, DD in B.
#'   # The function below expects `list(AA=c("A"), BB=c("A", "B"), ...)`
#'   # Let's adapt the example to reflect original `PlotUpset` input `listTerms`.
#'
#'   list_of_set_members <- list(
#'       SetA = c("Prot1", "Prot2", "Prot3"),
#'       SetB = c("Prot2", "Prot3", "Prot4"),
#'       SetC = c("Prot3", "Prot4", "Prot5", "Prot6")
#'   )
#'
#'   # Convert to tidy format needed by ggupset: item, list_of_sets_item_is_in
#'   # This conversion can be complex. The original code `unstack(stack(listTerms))`
#'   # followed by `datItem$Term <- rawTerms` and then `ggplot(aes(x=Term))`
#'   # implies Term is the list-column of sets.
#'   # Let's use tidyr to achieve this conversion:
#'   tidy_upset_data <- tidyr::enframe(list_of_set_members, name = "set", value = "item") %>%
#'     tidyr::unnest(item) %>%
#'     dplyr::group_by(item) %>%
#'     dplyr::summarize(sets_item_is_in = list(set), .groups = "drop")
#'
#'   # Now, `tidy_upset_data` has 'item' and 'sets_item_is_in' (list-column)
#'   # The plot function needs the list-column for `aes(x=...)`.
#'   # So, the `item_sets_list` for `plot_upset_from_item_list` should be
#'   # `list(Prot1=c("SetA"), Prot2=c("SetA", "SetB"), ...)`
#'
#'   # Reconstructing example input based on how original function worked:
#'   # Original `listTerms` example: `list(A = c("AA", "BB", "CC"), B = c("BB", "CC", "DD", "EE"))`
#'   # This implies:
#'   # AA is in A
#'   # BB is in A and B
#'   # CC is in A and B
#'   # DD is in B
#'   # EE is in B
#'   # The data frame for ggupset would then have a column `Term` which is a list,
#'   # where each element is `c("A")`, `c("A", "B")`, `c("A", "B")`, `c("B")`, `c("B")`.
#'   # The original code `rawTerms <- unstack(stack(listTerms)[, c("ind", "values")])`
#'   # `datItem <- data.frame(Item = names(rawTerms)); datItem$Term <- rawTerms`
#'   # If listTerms is `list(A=c("g1","g2"), B=c("g2","g3"))`:
#'   # stack(listTerms) -> values=c("g1","g2","g2","g3"), ind=c("A","A","B","B")
#'   # unstack(...) depends on structure. Let's assume `item_sets_list` is correct.
#'
#'   # Example directly using `item_sets_list` format
#'   item_membership <- list(
#'     Item_AA = c("SetA"),
#'     Item_BB = c("SetA", "SetB"),
#'     Item_CC = c("SetA", "SetB"),
#'     Item_DD = c("SetB"),
#'     Item_EE = c("SetB")
#'   )
#'   # Convert to data frame where the list-column is named 'Sets' for the plot func
#'   plot_df <- data.frame(Item = names(item_membership))
#'   plot_df$Observed_Sets <- item_membership # list-column
#'
#'   # upset_plot_obj <- plot_upset_from_item_list(plot_df, "Observed_Sets")
#'   # print(upset_plot_obj)
#' }
plot_upset_from_item_list <- function(
    item_membership_df,
    sets_col_name, # Name of the list-column in item_membership_df containing sets for each item
    num_intersections = 20,
    main_bar_fill_color = "steelblue",
    text_size_count = 3) {

  if (!is.data.frame(item_membership_df)) {
    stop("'item_membership_df' must be a data frame.")
  }
  if (!sets_col_name %in% names(item_membership_df)) {
    stop("Column '", sets_col_name, "' not found in item_membership_df.")
  }
  if (!is.list(item_membership_df[[sets_col_name]])) {
    stop("Column '", sets_col_name, "' must be a list-column.")
  }


  p <- ggplot2::ggplot(item_membership_df,
                       ggplot2::aes(x = .data[[sets_col_name]])) +
    ggplot2::geom_bar(fill = main_bar_fill_color) +
    ggplot2::geom_text(
      stat = "count",
      ggplot2::aes(label = ggplot2::after_stat(count)),
      vjust = -0.5, # Position text above bar
      size = text_size_count
    ) +
    ggupset::scale_x_upset(
      n_intersections = num_intersections,
      order_by = "freq", # Order intersections by frequency (size)
      decreasing = TRUE
    ) +
    # Optional: customize matrix appearance, e.g. dot size
    ggupset::axis_combmatrix(sep = "-", levels = NULL) + # Better default separator
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    ggplot2::labs(
      x = "Set Intersections",
      y = "Number of Items in Intersection",
      title = "UpSet Plot of Item-Set Intersections"
    )

  return(p)
}
