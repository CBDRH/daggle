% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grader.R
\name{grader}
\alias{grader}
\title{grader}
\usage{
grader(submission, dag, effect = "total")
}
\arguments{
\item{submission}{The submitted solution, a vector of nodes}

\item{dag}{A string defining the causal DAG}

\item{effect}{The desired effect, total (default) or direct}
}
\value{
A message indicating whether the submission was wrong or right
}
\description{
grader
}
