#' Groups Prices
#'
#' @param Price Price Vector
#'
#' @return the group to which the price belongs to
#' @export
#'
#' @examples
#' /dontrun{
#'  GroupsArea(Area_Scraped)
#' }
GroupsPrice <- function(Price)
{
  m = matrix(c(0,50000,100000,500000,1000000,5000000,10000000,20000000,50000,100000,500000,1000000,5000000,10000000,20000000,500000000), nrow=8, ncol=2)
  Grupo<-""
  for (i in 1:7)
  {
    Price<-Price[1]
    if (is.na(Price)==FALSE)
    {if (m[i,1]<=Price & m[i,2]>Price){Grupo<-paste(as.character(m[i,1]),as.character(m[i,2]),sep=" - ")}}
  }
  return(Grupo)
}
