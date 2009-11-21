\name{method}
\alias{freemethod}
\alias{hypermethod}
\alias{print.freemethod}
\title{method objects}
\description{Here, free methods are regarded methods, that are created and invoked directly, as functions. They are created by specifying a function, along with a series of attributes. A container object is automatically created containing those attributes. The container object (which is an environment) is accessed within the function using a dot. Hypermethods (which are also free methods) are a slight exception to this, and are intended to fake object level methods. They are created from an m object and wrap an m class method (taking care of the class's context attribute).}
\usage{
freemethod (f, \dots)
hypermethod (obj, mid)
\method{print}{freemethod}(f, \dots)
}
\arguments{
\item{f}{A function, that can use a dot to reference the container object.}
\item{obj}{An m object.}
\item{mid}{Method identifier, as a character.}
\item{\dots}{A series of attributes. i.e. A series of R objects to be assigned to the container object. If using \dots as one of the arguments (i.e. to represent zero or more of the attributes), the top level call must explicitly name each value (name1=value1, name2=value2, etc).}
}


