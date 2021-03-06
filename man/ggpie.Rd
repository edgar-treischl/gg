% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggpie.R
\name{ggpie}
\alias{ggpie}
\title{Make a pie chart}
\usage{
ggpie(data, slices)
}
\arguments{
\item{data}{A character vector with one element.}

\item{slices}{What to split on.}
}
\value{
A ggplot2 object.
}
\description{
Make a pie chart
}
\examples{
ggpie(palmerpenguins::penguins, slices = island)
}
