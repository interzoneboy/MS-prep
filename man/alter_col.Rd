% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import-data.R
\name{alter_col}
\alias{alter_col}
\title{Function that uniformly applies a change to a column. Intended to do things like 
converting character columns to numeric ones.}
\usage{
alter_col(d, col_name, alterFunc)
}
\arguments{
\item{d}{Input data frame needing column alteration.}

\item{col_name}{The name of the column to alter.}

\item{alterFunc}{Takes a single vector, returns a single vector that is an altered copy.}
}
\value{
A copy of the input data frame d with the given column altered.
}
\description{
Function that uniformly applies a change to a column. Intended to do things like 
converting character columns to numeric ones.
}
