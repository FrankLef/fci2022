#' Create a table of effect measures with their CI
#' 
#' Create a table of effect measures with their CI.
#' 
#' The table will have 3 columns. The measure name, the estimates and the 
#' confidence interval.
#'
#' @param df Dataframe of results
#' @param conf Numeric, confidence interval
#' @param digits Integer, number of digits to the right of the decimal.
#' @param title The title of the table
#' @param subtitle Subtitle of the table
#'
#' @return A gt object create by the \code{gt} package
#' @export
gt_measures <- function(df, conf = df$conf[1], digits = 3, 
                        title = "Title", subtitle = "Subtitle") {
  
  # confidence interval label used in footnote
  ci_label <- sprintf("%.0f%% confidence interval", 100 * conf)
  
  df <- df %>%
    select(name, est, lci, uci) %>%
    mutate(across(.cols = where(is.numeric), .fns = round, digits)) %>%
    unite(col = "CI", lci, uci, sep = ", ") %>%
    mutate(CI = paste0("(", CI, ")"))
  
  
  gt::gt(df) %>%
    gt_basic(title, subtitle) %>%
    cols_label(
      name = "Measure",
      est = "Estimate") %>%
    tab_footnote(
      footnote = ci_label,
      locations = cells_column_labels(columns = matches("CI$"))
    )
}
