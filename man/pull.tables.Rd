% Generated by roxygen2 (4.0.2): do not edit by hand
\name{pull.tables}
\alias{pull.tables}
\title{Pull tables}
\usage{
pull.tables(tables = c("yields", "traits", "managements_treatments",
  "managements", "treatments", "species"))

pull.tables(tables = c("yields", "traits", "managements_treatments",
  "managements", "treatments", "species"))
}
\arguments{
\item{tables}{}

\item{dbtabnames}{names of tables to pull}

\item{...}{additional arguments to \code{\link{query.bety}}}
}
\value{
list of data.frames pulled from database

list of tables from BETYdb as data.tables
}
\description{
Import tables from BETYdb as data.frames

import key tables
}
\details{
called by \code{\link{pull.data.tables}}

returns tables to the working environment
}
\author{
David LeBauer

David LeBauer
}

