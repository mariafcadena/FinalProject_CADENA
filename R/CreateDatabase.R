#' Create Database
#'
#' @param Cartier  Cartier Array
#' @param Property Property Array
#' @param Price    Price Array
#' @param Area     Area Array
#' @param Pieces Pieces Array
#' @param Chambres Chambres Array
#' @param Price_Group Price group array
#' @param Area_Group Area group array
#'
#' @import plyr
#' @return Data frame with the input information
#' @export
#'
#' @examples
#' /dontrun{
#' CreateData(Cartier, Property, Price, Area, Area_Group, Pieces, Chambres)
#' }
CreateDatabase <- function(Cartier, Property, Price, Price_Group, Area, Area_Group, Pieces, Chambres)
{
  DataBase <- do.call(rbind.data.frame, Map('c', Cartier,Property, Price,Price_Group, Area,Area_Group, Pieces, Chambres))
  colnames(DataBase) <- c("CARTIER","PROPERTY", "PRICE","PRICE_GROUP","AREA","AREA_GROUP", "PIECES", "CHAMBRES")
  return(DataBase)
}
