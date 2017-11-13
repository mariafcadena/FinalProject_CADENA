#' Get pages
#'
#' @param WEB_PAGE Selonger link
#' @import stringi
#' @import stringr
#' @import rvest
#' @return the number of pages the link contains
#' @export
#'
#' @examples
#' \dontrun{
#'   Link<-http://www.seloger.com/list.htm?idtt=2&naturebien=1,2,4&idtypebien=1,2&ci=750056&tri=initial&surfacemin=5&surfacemax=9
#'   WEB_PAGE<-read_html(Link)
#'   GetPages(WEB_PAGES)
#' }
GetPages <-function(WEB_PAGE)
{
  PRE_PAGES  <- WEB_PAGE %>% html_nodes(".title_nbresult") %>% html_text()
  PRE_PAGES2<- strsplit(stri_trans_general(PRE_PAGES[[1L]][1],"Latin-ASCII")," ")
  if (length(PRE_PAGES2[[1L]])>2) {
    PRE_PAGES3<-paste(PRE_PAGES2[[1L]][1],PRE_PAGES2[[1L]][2],sep="")
  } else {
    PRE_PAGES3<-PRE_PAGES2[[1L]][1]
  }
  TOTAL_PAGES<-floor(as.numeric(PRE_PAGES3)/20)
  #Selonger only shows a max of 20 properties and 100 pages per filter, this restriction is to avoid duplicates
  if (TOTAL_PAGES>100){TOTAL_PAGES<-100}
  return(TOTAL_PAGES)
}
