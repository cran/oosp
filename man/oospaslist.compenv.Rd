\name{oospaslist.compenv}
\alias{oospaslist.compenv}
\alias{oospasdataframe.compenv}
\alias{list.to.compenv}
\alias{compenv.to.list}
\title{Coercing Between list and compenv Objects.}
\description{These functions are self-explanatory, except that compenv to list will prune circular references. The compenv.to.list function should not be used by the user, it is likely to be made hidden in the next release.}
\usage{
\method{oospaslist}{compenv}(e, \dots)
\method{oospasdataframe}{compenv}(e, \dots)
list.to.compenv(obj, hash=FALSE)
compenv.to.list(e, flags=pointer (list (e) ) )
}
\arguments{
\item{obj}{}
\item{hash}{}
\item{e}{}
\item{flags}{}
\item{\dots}{}
}

