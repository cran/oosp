\name{misc}
\alias{ifst}
\alias{if0}
\alias{if1}
\alias{iter}
\alias{itobj}
\title{if and for statement constructs}
\description{The ifst function (if something) returns true if an object is non-null and has length > 0. The if0 and if1 functions return true if an object has length 0 or 1 respectively. The iter function produces an integer vector from min to n, if n >= min, and a zero length vector otherwise. Instead of if (n > 0) for (i in 1:n) ...commands..., we can write for (i in iter (n) ) ...commands... .The itobj function is similar to iter, except that the vector is 1:length (obj).
}
\usage{
ifst (obj)
if0 (obj)
if1 (obj)
iter (n, min=1)
itobj (obj)
}
\arguments{
\item{obj}{}
\item{n}{}
\item{min}{}
}

