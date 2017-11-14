#' Graphs prices
#'
#' @param SelongerDF The dataframe created from the link in Selonger
#' @import ggplot
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
GraphsPrices<-function(SelongerDF)
{
  GroupedData <- SelongerDF %>% group_by(Cartier, Area) %>% summarize(Price=max(Price))
  p <- plot_ly(x = Cartier, y = Area, z = Price,colors = colorRamp(c("red","yellow", "green")), type = "heatmap")
  return(p)
}
