#' Convert a numeric vector to character with chosen significant digits
#' 
#' @details The `signif()` function applies the same number of decimal
#' to all values in a vector, but parameter tables often include very
#' different scales, like Wtlen_1_Fem_GP_1 = 2e-06 and 
#' Wtlen_2_Fem_GP_1 = 3, so this applies `signif()` separately to each 
#' value and then converts to a string.
#'
#' @param x Vector of numeric values
#' @param digits Number of significant digits
#' @author Ian G. Taylor
#' @export

signif_string <- function(x, digits = 3) {
  lapply(x, FUN = signif, digits = digits) %>% 
    as.character()
}
