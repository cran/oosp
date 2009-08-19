\name{function}
\alias{mutate.function}
\alias{makepoly}
\title{Function Functions}
\description{A prototype function for conveniently changing the body of another function. Currently limited to one line functions. As well as a function for creating polynomials.}
\usage{
\method{mutate}{function}(f, str, \dots)
makepoly (k)
}
\arguments{
\item{f}{An R function, e.g. function (x) NULL.}
\item{str}{New body, as a character.}
\item{k}{A vector of coefficients.}
\item{\dots}{}
}

