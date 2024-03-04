#'  Helper file with all simple functions -> columns to numeric where possible
#'
#' @param  x A data.frame
#' @return df
#' @export


is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}
