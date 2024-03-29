% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biweight.R
\name{biweight}
\alias{biweight}
\title{Biweight function}
\usage{
biweight(
  value,
  grid,
  radius,
  normalize = TRUE,
  output_grid_geometry = FALSE,
  var = NULL,
  id = NULL
)
}
\arguments{
\item{value}{: sf values + points}

\item{grid}{: sf points}

\item{radius}{: 20000 for 20 km}

\item{normalize}{: if TRUE, ensure sum of pond = 1 else no correction}

\item{output_grid_geometry}{: is `geometry` present in return value ?}

\item{id}{: names of id variable in grid}
}
\description{
Biweight function
}
