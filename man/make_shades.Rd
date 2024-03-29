% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colours.R
\name{make_shades}
\alias{make_shades}
\title{Make shades}
\usage{
make_shades(colour, n, lighter = TRUE)
}
\arguments{
\item{colour}{seed colour to base the shades on}

\item{n}{amount of shades to generate}

\item{lighter}{switch wether to go to darker (\code{FALSE})
or lighter (\code{TRU}E) shades}
}
\value{
\code{n} color values (hex codes)
}
\description{
Creates different shades of an input colour. Can be either lighter or darker.
}
\examples{
#three lighter shades
make_shades("goldenrod", n=3)

#five darker shades
make_shades("goldenrod", n=5, lighter=FALSE)

}
