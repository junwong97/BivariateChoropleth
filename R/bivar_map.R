#' Create a bivariate choropleth map
#'
#' @description This function creates a bivariate choropleth map.
#'
#' @param data `sf` or `sp` object
#' @param xcol x column
#' @param ycol y column
#' @param xnum number of x categories
#' @param ynum number of y categories
#' @param title plot title
#' @param xtitle x axis title
#' @param ytitle y axis title
#' @param xcolor1 First color for the x variable
#' @param xcolor2 Last color for the x variable
#' @param ycolor1 First color for the y variable
#' @param ycolor2 Last color for the y variable
#' @param xstyle style of splitting x into categories; see `classIntervals()`
#' @param ystyle style of splitting y into categories; see `classIntervals()`
#' @param xbreaks optional breaks for x; only needed if xstyle is fixed
#' @param ybreaks optional breaks for y; only needed if ystyle is fixed
#' @param legend_pos vector of legend position: `c(x, y)`
#' @param legend_size vector of legend size: `c(width, height)`
#' @param map_style style of map: choropleth; line, point, etc. to come.
#' @param sym Symmetric categories; if true, number of x categories is equal to y categories and x style is equal to y style
#' @return sf object with categorical columns
#' @export

bivar_map <- function(data, xcol, ycol, xnum, ynum, title, xtitle, ytitle, xcolor1, xcolor2, ycolor1, ycolor2, xstyle='quantile', ystyle='quantile', xbreaks, ybreaks, legend_pos=c(0.2, 0.2), legend_size=c(0.3, 0.3), map_style = "choropleth", sym=T) {

  # Assign categories
  df <- bivar_cat(data, xcol, ycol, xnum, ynum, xstyle, ystyle, xbreaks, ybreaks, sym)

  # Get palette
  pals <- bivar_pal(xcolor1, xcolor2, ycolor1, ycolor2, xnum, ynum, sym)

  # Choropleth map
  if (map_style == "choropleth") {
    map <- ggplot2::ggplot() +
      geom_sf(data = df, mapping = aes(fill = cat), show.legend = FALSE) +
      scale_fill_manual(values=pals[['fill']]) +
      theme_void()
  }

  legend <- bivar_col(xcolor1, xcolor2, ycolor1, ycolor2, xnum, ynum, sym)
  legend <- legend + geom_segment(aes(x=1, xend = 3 , y=0, yend = 0), size=1.5,
                                  arrow = arrow(length = unit(0.6,"cm"))) +
    geom_segment(aes(x=0, xend = 0 , y=1, yend = 3), size=1.5,
                 arrow = arrow(length = unit(0.6,"cm"))) +
    labs(title=title, x=xtitle, y=ytitle)


  gg <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, legend_pos[1], legend_pos[2], legend_size[1], legend_size[2])

  return(gg)
}
