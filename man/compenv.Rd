\name{compenv}
\alias{compenv}
\alias{is.compenv}
\alias{is.hashed}
\alias{as.compenv}
\alias{==.compenv}
\title{component environments}
\description{Create, coerce and test compenv objects. A compenv object is merely an extended environment, however the compenv constructor and the methods available for compenv objects (such as print and clone), serve to make using environments as components easier.}
\usage{
compenv(\dots, hash=FALSE)
is.compenv(e)
is.hashed (e)
as.compenv(obj)
\method{==}{compenv}(e1, e2)
}
\arguments{
\item{hash}{Whether or not the compenv environment is hashed. Refer to help for new.env for more information.}
\item{e}{A compenv object.}
\item{e1}{An environment.}
\item{e2}{An environment.}
\item{obj}{A list or an environment.}
\item{\dots}{The environment's objects. Unless calling compenv directly, the arguments must be explicitly named, refer to is.cleancall for more information.}
}

