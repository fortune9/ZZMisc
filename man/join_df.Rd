% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{join_df}
\alias{join_df}
\title{Join dataframes}
\usage{
join_df(dfList, setCol = TRUE, setNAtoZero = T, ...)
}
\arguments{
\item{dfList}{A list of data.frame or data.table}

\item{setCol}{Whether to add a 'set' column for each input
data as an indicator of data origin. When TRUE, a new
column (set1, set2, etc) is added to each input data;
when FALSE, nothing is added. Alternatively, one can
provide a character vector to be used as the names of
the indicator columns. The elements of the indicator columns
are all '1's.}

\item{setNAtoZero}{If TRUE, the NAs in the indicator columns
are set to '0'.}

\item{...}{The parameters passed to \code{\link[data.table:merge]{data.table::merge()}}
directly}
}
\value{
A \linkS4class{data.table} object
}
\description{
Given a list of \link{data.frame} or \link{data.table},
it outputs a \link{data.table} by merging them together
}
