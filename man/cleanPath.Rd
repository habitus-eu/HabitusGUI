% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanPath.R
\name{cleanPath}
\alias{cleanPath}
\title{Cleans multiple consecutive file separator from file path}
\usage{
cleanPath(path)
}
\arguments{
\item{path}{Character with file path to be cleaned}
}
\value{
Character with cleaned file path
}
\description{
Cleans multiple consecutive file separator from file path
}
\examples{
cleanPath("C:/myfolder///myfile.csv")
}
