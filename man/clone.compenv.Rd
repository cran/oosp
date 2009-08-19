\name{clone.compenv}
\alias{clone.compenv}
\alias{clone.environment}
\alias{compenv.clone}
\title{Clone Environments}
\description{Clones environments, including circular references. The compenv.clone function should not be used by users, it may be made hidden in the next release.}
\usage{
\method{clone}{compenv}(e, \dots)
\method{clone}{environment}(e, \dots)
compenv.clone(e, flags=pointer (list () ) )
}
\arguments{
\item{e}{An environment}
\item{flags}{User should ignore this.}
\item{\dots}{}
}

