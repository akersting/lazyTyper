% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertType.R
\name{assertType}
\alias{assertType}
\title{Type-Based Assertions}
\usage{
assertType(expr, type, ...)
}
\arguments{
\item{expr}{the (unquoted) expression whose return value to check.}

\item{type}{the type to check the return value of \code{expr} against (as a
character string).}

\item{...}{further properties of the return value of \code{expr} to
guarantee. The availability of additional properties depends on
\code{type}. Both positional and partial matching are used on these
arguments.}
}
\value{
expr (evaluated and invisibly).
}
\description{
Assert that the return value of an expression is of the desired type.
}
