#' Groups Areas
#'
#' @param Area the value of the area
#'
#' @return the group to which the area belongs to
#' @export
#'
#' @examples
#' \dontrun{
#'  GroupsArea(Area_Scraped)
#' }
GroupsArea <- function(Area)
{
  m = matrix(c(5,20,40,80,100,150,200,20,40,80,100,150,200,401), nrow=7, ncol=2)
  Grupo<-""
  for (i in 1:7)
  {
    Area<-Area[1]
    if (is.na(Area)==FALSE)
    {if (m[i,1]<=Area & m[i,2]>Area){Grupo<-paste(as.character(m[i,1]),as.character(m[i,2]),sep=" - ")}}
  }
  return(Grupo)
}
