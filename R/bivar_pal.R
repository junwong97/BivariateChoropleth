#' Obtain a color palette from the mixing process
#'
#' @description Creates a color palette from `bivar_col()`.
#'
#' @param xcolor1 First color for the x variable
#' @param xcolor2 Last color for the x variable
#' @param ycolor1 First color for the y variable
#' @param ycolor2 Last color for the y variable
#' @param xnum Number of x categories
#' @param ynum Number of y categories
#' @param sym Symmetric categories; if true, number of x categories is equal to y categories
#' @return Vector of colors
#' @export


bivar_pal <- function(xcolor1, xcolor2, ycolor1, ycolor2, xnum, ynum, sym = T) {
  g <- bivar_col(xcolor1, xcolor2, ycolor1, ycolor2, xnum, ynum, sym)

  pal <- ggplot2::ggplot_build(g)$data[[1]]
  pal <- pal[,c("x", "y", "fill")]
  pal[['xcat']] <- as.character(pal[['x']])
  pal[['ycat']] <- as.character(pal[['y']])
  return(pal)
}
