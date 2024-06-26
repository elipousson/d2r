% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/special.R
\name{d2_sql_table}
\alias{d2_sql_table}
\title{Create a D2 SQL Table Diagram}
\usage{
d2_sql_table(
  data = NULL,
  visibility = NULL,
  col_types = vec_ptype_abbr,
  id = NULL,
  label = id,
  ...
)
}
\arguments{
\item{data}{A data frame used for column names and types. Ignored if
\code{col_types} is a named vector. Required if \code{col_types} is unnamed or a
function.}

\item{visibility}{One or more of: , +, -, #. Length is
recycled to match the number of columns in \code{data}.}

\item{col_types}{A named character vector, an unnamed character vector with
the same length as the number of columns in \code{data}, or a function that
returns a character vector based on the columns from \code{data}. Defaults to
\link[vctrs:vec_ptype_full]{vctrs::vec_ptype_abbr}. Other recommended options are
\link[vctrs:vec_ptype_full]{vctrs::vec_ptype_full} or \link[base:typeof]{base::typeof}. If \code{types} is a named character
vector, the column names of \code{data} are ignored the names of types used
instead.}

\item{id}{Diagram container id passed to \code{\link[=d2_container]{d2_container()}}. Defaults to
\code{NULL}. If \code{NULL}, \code{id} is set to \code{rlang::caller_arg(data)} if \code{data} is
supplied or a random string if \code{data = NULL}.}

\item{label}{Diagram label. Defaults to \code{id}. If supplied, \code{label} is
displayed instead of \code{id}.}

\item{...}{
  Arguments passed on to \code{\link[=d2_container]{d2_container}}
  \describe{
    \item{\code{lines}}{Optional diagram text. If supplied \code{lines} is recycled to match
the length of \code{id}.}
    \item{\code{class}}{Diagram class}
    \item{\code{name}}{Diagram container name}
    \item{\code{width,height}}{Diagram width and height (in pixels?)}
    \item{\code{style}}{A string or results from the \code{\link[=d2_style]{d2_style()}} helper function.}
    \item{\code{icon}}{Diagram icon}
    \item{\code{collapse}}{If \code{collapse = "\\n"}, return a string with D2 diagram code for a
container. If \code{collapse = NULL} (default), return a character vector with the same
length as id.}
  }}
}
\value{
A character string with D2 diagram code using the "sql_type"
shape.
}
\description{
Create a D2 SQL Table Diagram
}
\examples{

d2_sql_table(mtcars[, 1:4])

d2_sql_table(
  label = "col_types Example",
  col_types = c("name" = "char", "num" = "int")
)

}
