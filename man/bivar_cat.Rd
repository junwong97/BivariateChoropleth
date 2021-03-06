% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bivar_cat.R
\name{bivar_cat}
\alias{bivar_cat}
\title{Assign categories to continuous variables}
\usage{
bivar_cat(
  data,
  xcol,
  ycol,
  xnum,
  ynum,
  xstyle = "quantile",
  ystyle = "quantile",
  xbreaks,
  ybreaks,
  sym = T
)
}
\arguments{
\item{data}{sf or sp object}

\item{xcol}{x column}

\item{ycol}{y column}

\item{xnum}{number of x categories}

\item{ynum}{number of y categories}

\item{xstyle}{style of splitting x into categories; see classIntervals}

\item{ystyle}{style of splitting y into categories; see classIntervals}

\item{xbreaks}{optional breaks for x; only needed if xstyle is fixed}

\item{ybreaks}{optional breaks for y; only needed if ystyle is fixed}

\item{sym}{Symmetric categories; if true, number of x categories is equal to y categories and x style is equal to y style}
}
\value{
sf object with categorical columns
}
\description{
This function creates categories from x and y variables, and combines the two into a singular category for mapping.
}
