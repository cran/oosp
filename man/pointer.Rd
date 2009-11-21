\name{pointer}
\alias{pointer}
\alias{pointer.image}
\alias{map.pointer}
\alias{print.pointer}
\alias{length.pointer}
\alias{[.pointer}
\alias{[<-.pointer}
\alias{[[.pointer}
\alias{[[<-.pointer}
\title{pointer objects}
\description{Here, pointer objects are extended freemethods (refer to freemethod), that are used to reference an object refered to as the pointer image. Whilst the implementation may be unorthodox, these objects has been partly modelled around C pointers, including the ability to casually reference and deference objects. One major difference, is that we use pointers to vectors (or lists) rather than pointer arrays. We create a pointer, by calling ptr = pointer (obj) and reassign it, by map (ptr, newobj). We access the image by calling the pointer as a function, ptr (). If the image is a vector or list (and even if it isn't), we can manipulate and access the elements directly, using extraction methods. Note that the map function doesn't return a value.}
\usage{
pointer (obj)
pointer.image ()
\method{map}{pointer}(ptr, obj, \dots)
\method{print}{pointer}(ptr, \dots)
\method{length}{pointer}(x)
\method{[}{pointer}(ptr, \dots)
\method{[}{pointer}(ptr, \dots) <- value
\method{[[}{pointer}(ptr, \dots)
\method{[[}{pointer}(ptr, \dots) <- value
}
\arguments{
\item{ptr}{}
\item{x}{}
\item{obj}{}
\item{value}{}
\item{\dots}{}
}

