#' Creates Data Frame
#'
#' @param link The link of the selonger pages that is suposed to be scrap
#' @import rvest
#' @return a dataframe with the information scraped from the web page
#' @export
#'
#' @examples
#' \dontrun{
#'   Link<-http://www.seloger.com/list.htm?idtt=2&naturebien=1,2,4&idtypebien=1,2&ci=750056&tri=initial&surfacemin=5&surfacemax=9
#'   df<-CreatesDataframe(Link)
#' }
CreatesDataframe <-function(link)
{
  WEB_PAGE <- read_html(link)
  NO_PAGES <- GetPages(WEB_PAGE)
  link<-paste(link,"&LISTING-LISTpg=",sep="")
  for (i in 1:NO_PAGES)
  {
    test<-paste(link,i,sep="")
    WEB_PAGE <- read_html(test)
    PRE_CARTIER  <- WEB_PAGE %>% html_nodes(".c-pa-city") %>% html_text()
    PRE_PROPERTY <- WEB_PAGE %>% html_nodes(".c-pa-link") %>% html_text()
    PRE_PRICE    <- WEB_PAGE %>% html_nodes(".c-pa-price") %>% html_text()
    PRE_CARACT   <- WEB_PAGE %>% html_nodes(".c-pa-criterion") %>% html_text()

    CARTIER    <- CleanVector(PRE_CARTIER, "Cartier")
    PROPERTY   <- CleanVector(PRE_PROPERTY, "Property")
    PRICE      <- CleanVector(PRE_PRICE, "Price")
    PRICE_GROUP<- lapply(PRICE,GroupsPrice)
    AREA       <- CleanVector(PRE_CARACT, "Area")
    AREA_GROUP <- lapply(AREA,GroupsArea)
    PIECES     <- CleanVector(PRE_CARACT, "Piece")
    CHAMBRES   <- CleanVector(PRE_CARACT, "Chambre")

    data<-CreateDatabase(CARTIER,PROPERTY,PRICE, PRICE_GROUP,AREA, AREA_GROUP,PIECES,CHAMBRES)

    if (i==1){results<-data
    }else {results<-rbind(results, data)}
  }
  return(results)
}
