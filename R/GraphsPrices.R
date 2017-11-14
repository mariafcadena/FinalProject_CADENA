#' Graphs prices
#'
#' @param SelogerDF
#'
#' @import ggplot2
#' @import reshape2
#' @import RColorBrewer
#' @return the graph with the prices per square meter vs the input variables
#' @export
#'
#' @examples
#' \dontrun{
#'   Link<-http://www.seloger.com/list.htm?idtt=2&naturebien=1,2,4&idtypebien=1,2&ci=750056&tri=initial&surfacemin=5&surfacemax=9
#'   df<-CreatesDataframe(Link)
#'   GraphsPrices(sf)
#' }
GraphsPrices<-function(SelogerDF)
{
  Seloger_melted <- melt(SelogerDF)
  head(Seloger_melted)
  ggplot(data = Seloger_melted, aes(x=CARTIER, y=AREA_GROUP, fill=PRICE)) + geom_tile()
}
