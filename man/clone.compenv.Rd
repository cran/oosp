\name{clone.compenv}
\alias{clone.compenv}
\alias{clone.environment}
\alias{compenv.clone}
\title{clone environments}
\description{These functions clone environments, accounting for circular references. The compenv.clone function (not clone.compenv) should not be used by users, it may be made hidden in future releases.}
\usage{
\method{clone}{compenv}(e, \dots)
\method{clone}{environment}(e, \dots)
compenv.clone (e, flags=pointer (list () ) )
}
\arguments{
\item{e}{An environment.}
\item{flags}{User should ignore this.}
\item{\dots}{}
}


