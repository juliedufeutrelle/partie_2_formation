# définition de fonctions -------------------------------------------------

decennie_a_partir_annee <- function(mon_annee) {
  return(mon_annee - mon_annee %%
           10)
}

#' Title
#'
#' @param a 
#' @param b 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
# raccourci = CTRL ALT SHIFT R
calculer_stats <- function(a, b = "moyenne", ...) {
  if (b == "moyenne") {
    x <- mean(a, na.rm = TRUE, ...)
  } else if (b == "ecart-type" || b == "sd") {
    x <- sd(a, na.rm = TRUE, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = TRUE, ...)
  }
  return(x)
}
