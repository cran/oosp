\name{collection}
\alias{collection}
\alias{is.collection}
\alias{as.collection}
\alias{[.collection}
\alias{[<-.collection}
\title{collection objects}
\description{A collection object extends a list object. It has two differences. One, by default it automatically resolves argument names. Two, element objects can be accessed using single brackets.}
\usage{
collection (..., resolve=TRUE, call=sys.call () )
is.collection (obj)
as.collection (obj)
\method{[}{collection}(obj, \dots)
\method{[}{collection}(obj, \dots)<-value
}
\arguments{
\item{resolve}{This should generally be true, and means that the function attempts to resolve argument names. If calling collection with dots as an argument, it is advisable to either set resolve to false, or replace the call. Refer to is.cleancall for more information.}
\item{call}{The call containing the original arguments, that is, the call used to resolve the argument names.}
\item{obj}{A collection object, or a potential collection object.}
\item{value}{New value for an element object.}
\item{\dots}{For collection, the objects to be in the collection. For the extraction methods, the indices.}
}

