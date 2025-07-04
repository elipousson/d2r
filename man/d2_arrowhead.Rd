% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrowhead.R
\name{d2_arrowhead}
\alias{d2_arrowhead}
\alias{d2_arrowheads}
\title{Specify attributes for a single arrowhead or a pair of source and target
arrowheads}
\usage{
d2_arrowhead(
  shape = "triangle",
  type = c("target", "source"),
  label = NULL,
  filled = NULL
)

d2_arrowheads(..., target = list(), source = list(), id = NULL)
}
\arguments{
\item{shape}{See \code{d2r::keys_d2[["arrowhead"]]} for allowed shape values.}

\item{type}{Type of arrowhead: "target" or "source".}

\item{label}{Short arrowhead label.}

\item{filled}{Must be \code{NULL} or logical. Ignored if shape is \code{"arrow"} or a
crow's foot shape (any shape starting with \code{"cf-"}).}

\item{...}{Named parameters used to set values for both the source and
target arrowheads.}

\item{target, source}{Named list of arguments (excluding type) passed to
\code{\link[=d2_arrowhead]{d2_arrowhead()}}.}

\item{id}{D2 diagram map identifier. All other container or map attributes
are recycled to match the length of \code{id} using \code{\link[vctrs:vec_recycle]{vctrs::vec_recycle_common()}}}
}
\description{
\code{\link[=d2_arrowhead]{d2_arrowhead()}} specifies the label, shape, and fill for a source or target
arrowhead. \code{\link[=d2_arrowheads]{d2_arrowheads()}} allows the specification of both a source and
target arrowhead.
}
\examples{
d2_arrowhead(
  shape = "cf-many-required",
  label = "Many required"
)

example_diagram <- "A -> B"

d2_arrowheads(
  shape = "triangle",
  id = example_diagram,
  filled = FALSE
)

d2_arrowheads(
  source = list(
    shape = "triangle",
    label = "source",
    filled = FALSE
  ),
  target = list(
    shape = "circle",
    filled = TRUE
  ),
  id = example_diagram
)

}
