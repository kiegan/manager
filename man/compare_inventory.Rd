% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_extract.R
\name{compare_inventory}
\alias{compare_inventory}
\title{Compare two inventory objects}
\usage{
compare_inventory(
  inventory1,
  inventory2,
  object_types = "exported",
  report_type = c("table", "summary", "objects"),
  summary_file = ""
)
}
\arguments{
\item{inventory1}{An inventory object, a data.frame from the `take_inventory` function}

\item{inventory2}{A second inventory object, a data.frame from the `take_inventory` function}

\item{object_types}{A vector of object types to compare. Default is "exported" objects only, but can be any of c("exported", "non-exported", "data").}

\item{report_type}{A vector of reporting type preferences. "table" returns a data.frame with one row for each object that differs between the two inventories. "summary" returns concatenated summaries of identified differences. "objects" returns a data.frame with one row for each object which differs between the two inventories, but also includes the objects themselves as well as checksums.}

\item{summary_file}{A file to store inventory difference summary information. Default is "", which will return the summary in the console.}
}
\value{
a list with element `table`. A text summary will be saved to the file specified in `summary_file`, or printed in the console.
}
\description{
Compare two inventory objects
}
\examples{
\dontrun{compare_inventory(inventory1, inventory2)}
}
