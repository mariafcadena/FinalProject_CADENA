#' Create Database
#'
#' @param Cartier  Cartier Array
#' @param Property Property Array
#' @param Price    Price Array
#' @param Area     Area Array
#' @param Area_Group Grouped Area Array
#' @param Pieces Pieces Array
#' @param Chambres Chambres Array
#' @import plyr
#' @import base
#'
#' @return Data frame with the input information
#' @export
#'
#' @examples
#' \dontrun{
#' CreateData(Cartier, Property, Price, Area, Area_Group, Pieces, Chambres)
#' }
CreateDatabase <- function(Cartier, Property, Price, Area, Area_Group, Pieces, Chambres)
{
  DataBase <- do.call(rbind.data.frame, Map('c', Cartier,Property, Price, Area, Area_Group, Pieces, Chambres))
  colnames(DataBase) <- c("CARTIER","PROPERTY", "PRICE","AREA", "AREA_GROUP", "PIECES", "CHAMBRES")
  return(DataBase)
}
