% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize_vector_values.R
\name{normalize_vector_values}
\alias{normalize_vector_values}
\title{MinMax normalization}
\usage{
normalize_vector_values(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
A numeric vector, with values rescaled between 0 and 1.
}
\description{
Receives a vector of numeric values and apply the min-max
normalization (or rescaling).
}
\examples{
v = 1:10
normalize_vector_values(v)

}
