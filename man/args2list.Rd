% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{args2list}
\alias{args2list}
\title{Get the Current Values of the Formal Arguments of a Function as a List}
\usage{
args2list(include_ellipsis = TRUE, simplify = TRUE,
  include_missing = FALSE, ...)
}
\arguments{
\item{include_ellipsis}{a logical value indicating if the ellipsis (...)
should be included in the result.}

\item{simplify}{a logical value indicating if the ellipsis should be included
in the result as a list with the name "..." (\code{FALSE}) or if the
elements hereof should be directly included in the list returned
(\code{TRUE}).}

\item{include_missing}{a logical value indicating if missing formal arguments
should be included in the result. An argument might be missing because it
was not provided when the function was called \emph{and} it does not have a
default value, or because it was deleted in the body of the function before
\code{args2list} was called. If \code{TRUE}, the respective elements of the
list are empty names.}

\item{...}{ignored. Just here to get rid of the R CMD check note "... may be
used in an incorrect context: 'list(...)'".}
}
\value{
a list, named for all formal arguments included and possibly unnamed
  for the ellipsis, depending on whether the additional arguments were named
  or not.
}
\description{
This function returns the \emph{current} values of the formal arguments of
the calling function as a list.
}
\examples{
f <- function(a, b, c = 3, ...) {
  a <- 7
  rm(b)
  d <- 0
  e <- 99
  args2list()
}

f(a = 1, b = 2, d = 9)  # a == 7, b not included, c == 3, d == 9 (not 0!)
f()  # a == 7, c == 3
f(a = 1, b = 2, c = 4, 5)  # result unnamed for ellipsis
}
\keyword{internal}
