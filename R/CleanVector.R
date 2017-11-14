#' Clean Vector
#'
#' @param Vector Vector with the scrapped values
#' @param Type   Type of information stored in the vector
#'
#' @return A vector with all the variables in the correct format
#' @export
#'
#' @examples
#' /dontrun{
#' Area <-CleanVector(PRE_CARACT, "Area")
#' }
CleanVector<- function(Vector, Type)
{
  NewVec<-lapply(Vector,FixParameters, Type=Type)
  return(NewVec)
}
