#' Preview mixed color palette
#'
#' @description This creates a plot that previews your color palette of choice. This also creates part of the legend.
#'
#' @param xcolor1 First color for the x variable
#' @param xcolor2 Last color for the x variable
#' @param ycolor1 First color for the y variable
#' @param ycolor2 Last color for the y variable
#' @param xnum Number of x categories
#' @param ynum Number of y categories
#' @param sym Symmetric categories; if true, number of x categories is equal to y categories
#' @return Plot object with the mixed color palettes
#' @export


# Preview the color choice + drawing legend:
bivar_col <- function(xcolor1, xcolor2, ycolor1, ycolor2, xnum, ynum, sym = T) {
  if (sym == T) {
    ynum = xnum
  }

  x <- seq(xnum)
  y <- seq(ynum)
  grd <- expand.grid(x=x, y=y)

  # assign colors
  pal1 <- colorRampPalette(c(xcolor1, xcolor2), space = "rgb")
  col1 <- val2col(x, col=pal1(xnum))
  pal2 <- colorRampPalette(c(ycolor1, ycolor2), space = "rgb")
  col2 <- val2col(y, col=pal2(ynum))
  col3 <- NA*seq(nrow(grd))
  for(i in seq(nrow(grd))){
    xpos <- grd$x[i]
    ypos <- grd$y[i]
    coltmp <- (col2rgb(col1[xpos])/2) + (col2rgb(col2[ypos])/2)
    col3[i] <- rgb(coltmp[1], coltmp[2], coltmp[3], maxColorValue = 255)
  }

  # plot the color
  gg <- ggplot2::ggplot(grd, aes(x,y,fill=col3)) +
    geom_tile() +
    scale_fill_identity() +
    theme_void()

  return(gg)
}
