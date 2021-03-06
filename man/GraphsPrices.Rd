% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GraphsPrices.R
\name{GraphsPrices}
\alias{GraphsPrices}
\title{Graphs prices}
\usage{
GraphsPrices(SelogerDF)
}
\arguments{
\item{SelogerDF}{}
}
\value{
the graph with the prices per square meter vs the input variables
}
\description{
Graphs prices
}
\examples{
\dontrun{
  Link<-http://www.seloger.com/list.htm?idtt=2&naturebien=1,2,4&idtypebien=1,2&ci=750056&tri=initial&surfacemin=5&surfacemax=9
  df<-CreatesDataframe(Link)
  GraphsPrices(sf)
}
}
