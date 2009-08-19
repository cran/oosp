\name{oospbridges}
\alias{print.object}
\alias{format.object}
\alias{summary.object}
\alias{plot.object}
\alias{lines.object}
\alias{predict.object}
\alias{fitted.object}
\alias{residuals.object}
\alias{weights.object}
\alias{as.list.object}
\alias{as.data.frame.object}
\title{Bridge Functions}
\description{When a standard generic is applied to an object object, the copycat generic is then used, unless the oosp argument (must be named) is false, in which case normal method despatch is used. Note that if the plot y argument is missing, the missing argument is not given to an oosp function.}
\usage{
\method{print}{object}(x, \dots, oosp=TRUE)
\method{format}{object}(x, \dots, oosp=TRUE)
\method{summary}{object}(object, \dots, oosp=TRUE)
\method{plot}{object}(x, y, \dots, oosp=TRUE)
\method{lines}{object}(x, \dots, oosp=TRUE)
\method{predict}{object}(object, \dots, oosp=TRUE)
\method{fitted}{object}(object, \dots, oosp=TRUE)
\method{residuals}{object}(object, \dots, oosp=TRUE)
\method{weights}{object}(object, \dots, oosp=TRUE)
\method{as.list}{object}(x, \dots, oosp=TRUE)
\method{as.data.frame}{object}(x, row.names=NULL, optional=FALSE, \dots, oosp=TRUE)
}
\arguments{
\item{x}{}
\item{y}{}
\item{object}{}
\item{row.names}{}
\item{optional}{}
\item{\dots}{}
\item{oosp}{}
}

