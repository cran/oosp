\name{clone.ENVIRONMENT}
\alias{clone.ENVIRONMENT}
\alias{clone.environment}
\alias{ENVIRONMENT.clone}
\alias{clone.compenv}
\alias{compenv.clone}
\title{clone ENVIRONMENT objects}
\description{These functions clone environments, accounting for circular references. The ENVIRONMENT.clone function (not clone.ENVIRONMENT) should not be used by users, it may be made hidden in future releases.}
\usage{
\method{clone}{ENVIRONMENT}(e, \dots)
\method{clone}{environment}(e, \dots)
ENVIRONMENT.clone (e, flags=pointer (list () ) )
\method{clone}{compenv}(e, \dots)
compenv.clone (e, flags=pointer (list () ) )
}
\arguments{
\item{e}{An environment.}
\item{flags}{User should ignore this.}
\item{\dots}{.}
}

