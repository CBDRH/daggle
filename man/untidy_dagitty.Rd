% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/untidy_dagitty.R
\name{untidy_dagitty}
\alias{untidy_dagitty}
\title{untidy_dagitty}
\usage{
untidy_dagitty(td)
}
\arguments{
\item{td}{A `tidy_dagitty` object}
}
\value{
A named list containing two dagitty objects. The first element preserves the layout for plotting with `plot.dagitty()` or on [dagitty.net](http://www.dagitty.net/dags.html). The second element preserves the layout to plot with `ggdag` functions.
}
\description{
Revert a basic `tidy_dagitty` object to a `dagitty` object while preserving node positions
}
