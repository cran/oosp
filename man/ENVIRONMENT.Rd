\name{ENVIRONMENT}
\alias{ENVIRONMENT}
\alias{is.ENVIRONMENT}
\alias{is.hashed}
\alias{as.ENVIRONMENT}
\alias{==.ENVIRONMENT}
\alias{compenv}
\alias{is.compenv}
\alias{as.compenv}
\alias{==.compenv}
\title{enhanced environments}
\description{Enhanced environments, with ENVIRONMENT objects. The compenv version are deprecated, do not use. Refer to the relevant package vignette for more information.}
\usage{
ENVIRONMENT(\dots, hash=FALSE)
is.ENVIRONMENT(e)
is.hashed (e)
as.ENVIRONMENT(obj)
\method{==}{ENVIRONMENT}(e1, e2)
compenv(\dots, hash=FALSE)
is.compenv(e)
as.compenv(obj)
\method{==}{compenv}(e1, e2)
}
\arguments{
\item{hash}{Whether or not the ENVIRONMENT is hashed. Refer to help for new.env for more information.}
\item{e}{An ENVIRONMENT object.}
\item{e1}{An environment.}
\item{e2}{An environment.}
\item{obj}{A list or an environment.}
\item{\dots}{The environment's objects. Unless calling ENVIRONMENT directly, the arguments must be explicitly named, refer to is.cleancall for more information.}
}


