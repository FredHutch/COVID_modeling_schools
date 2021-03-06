% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_V.R
\name{create_V}
\alias{create_V}
\title{Generalized V matrix creation}
\usage{
create_V(immune.list, vaccination.list, n_age)
}
\arguments{
\item{immune.list}{a nested list}

\item{vaccination.list}{a nested list}

\item{n_age}{number of age groups}
}
\value{
V matrix
}
\description{
Note that n_age is the size of the block diagonals (will typically represent age groups)
}
