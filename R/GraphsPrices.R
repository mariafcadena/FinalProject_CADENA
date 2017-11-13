#' Graphs prices
#'
#' @param SelongerDF The dataframe created from the link in Selonger
#' @param Variables  The variables that wants to be compared to the prices
#'
#' @return the graph with the prices per square meter vs the input variables
#' @export
#'
#' @examples
#' \dontrun{
#'   Link<-http://www.seloger.com/list.htm?idtt=2&naturebien=1,2,4&idtypebien=1,2&ci=750056&tri=initial&surfacemin=5&surfacemax=9
#'   df<-CreatesDataframe(Link)
#'   GraphsPrices(sf, c(Area, Cartier))
#' }
GraphsPrices<-function(SelongerDF,Variables)
{

}
