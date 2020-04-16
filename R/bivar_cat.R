#' Assign categories to continuous variables
#'
#' @description This function creates categories from x and y variables, and combines the two into a singular category for mapping.
#'
#' @param data sf or sp object
#' @param xcol x column
#' @param ycol y column
#' @param xnum number of x categories
#' @param ynum number of y categories
#' @param xstyle style of splitting x into categories; see classIntervals
#' @param ystyle style of splitting y into categories; see classIntervals
#' @param xbreaks optional breaks for x; only needed if xstyle is fixed
#' @param ybreaks optional breaks for y; only needed if ystyle is fixed
#' @param sym Symmetric categories; if true, number of x categories is equal to y categories and x style is equal to y style
#' @return sf object with categorical columns
#' @export

bivar_cat <- function(data, xcol, ycol, xnum, ynum, xstyle='quantile', ystyle='quantile', xbreaks, ybreaks, sym=T) {

  cl <- class(data)
  if ("sf" %notin% cl) {
    data <- tryCatch(sf::st_as_sf(data),
                     error = function(e) {
                       message("Cannot be coerced to sf")
                     })
  }

  if (sym == T) {
    ystyle = xstyle
    ynum = xnum
  }

  data[['xcat']] <- classInt::findCols(classInt::classIntervals(data[[xcol]], n=xnum, xstyle, fixedBreaks=xbreaks))
  data[['ycat']] <- classInt::findCols(classInt::classIntervals(data[[ycol]], n=ynum, ystyle, fixedBreaks=ybreaks))
  data[['cat']] <- data[['xcat']] + (xnum * (data[['ycat']] - 1))
  data[['cat']] <- as.factor(data[['cat']])

  return(data)
}
