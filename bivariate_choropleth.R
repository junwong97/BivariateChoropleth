library(pals)
library(tidyverse)
library(sf)
library(classInt)
library(cowplot)
library(classInt)

# Assigns a color to each value
val2col<-function(z, zlim, col = heat.colors(12), breaks){
  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
  }
  if(missing(breaks) & missing(zlim)){
    zlim <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  colorlevels <- col[((as.vector(z)-breaks[1])/(range(breaks)[2]-range(breaks)[1]))*(length(breaks)-1)+1] # assign colors to heights for each point
  colorlevels
}

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
  gg <- ggplot(grd, aes(x,y,fill=col3)) +
    geom_tile() +
    scale_fill_identity() +
    theme_void()
  
  return(gg)
}

# Get palette
bivar_pal <- function(xcolor1, xcolor2, ycolor1, ycolor2, xnum, ynum, sym = T) {
  g <- bivar_col(xcolor1, xcolor2, ycolor1, ycolor2, xnum, ynum, sym)
  
  pal <- ggplot_build(g)$data[[1]] %>% 
    select(x,y,fill) %>% 
    mutate(xcat = as.character(x),
           ycat = as.character(y))
  return(pal)
}

# Assign classes
bivar_cat <- function(data, xcol, ycol, xnum, ynum, xstyle='quantile', ystyle='quantile', xbreaks, ybreaks, sym=T) {
  
  if (sym == T) {
    ystyle = xstyle
    ynum = xnum
  }
  
  data[['xcat']] <- findCols(classIntervals(data[[xcol]], n=xnum, xstyle, fixedBreaks=xbreaks))
  data[['ycat']] <- findCols(classIntervals(data[[ycol]], n=ynum, ystyle, fixedBreaks=ybreaks))
  data[['cat']] <- data[['xcat']] + (xnum * (data[['ycat']] - 1))
  data[['cat']] <- as.factor(data[['cat']])
  
  return(data)
} 

# Mapping function
bivar_map <- function(data, xcol, ycol, xnum, ynum, title, xtitle, ytitle, xcolor1, xcolor2, ycolor1, ycolor2, xstyle='quantile', ystyle='quantile', xbreaks, ybreaks, legend_pos=c(0.2, 0.2), legend_size=c(0.3, 0.3), map_style = "choropleth", sym=T) {
  
  # Assign categories
  df <- bivar_cat(data, xcol, ycol, xnum, ynum, xstyle, ystyle, xbreaks, ybreaks, sym)
  
  # Get palette
  pals <- bivar_pal(xcolor1, xcolor2, ycolor1, ycolor2, xnum, ynum, sym)
  
  # Choropleth map
  if (map_style == "choropleth") {
    map <- ggplot() +
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
