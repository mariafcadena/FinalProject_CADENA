#' Fix Parameters
#'
#' @param Value Array with the information scraped that needs to be fix
#' @param Type   Type of information in the array ("Price", "Piece", "Chambre", "Parameters")
#' @import stringi
#' @import stringr
#' @import rvest
#' @return a clean value of the information scraped
#' @export
#'
#' @examples
#'
#' /dontrun{
#'  FixParameters(AreaScraped, "Area")
#' }
#'
#'
FixParameters <- function(Value, Type)
{
  #Sets cartier name
  if(Type=="Cartier"){
    FinalParameter<-stri_trans_general(Value,"Latin-ASCII")
  }

  #Sets the price as a numerical variable
  if (Type=="Price")
    {
      P1<-stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", Value)),"Latin-ASCII")
      P2<-regmatches(P1,gregexpr(" ", P1),TRUE)
      FinalParameter=""
      for (i in 1:(length(P2[[1L]])-1))
      {
        if (grepl("[^a-zA-Z]", P2[[1L]][i])==TRUE){FinalParameter=paste(FinalParameter,P2[[1L]][i],sep="")}
      }
      FinalParameter<-as.numeric(FinalParameter)
    }
  #Sets the property type
  if (Type=="Property")
  {
      a<-unlist(str_locate_all(pattern ='Appartement', Value))
      if (length(a)>0){FinalParameter<-'Appartement'} else { FinalParameter<-'Maison' }
  }
  #Sets the piece as a numerical variable
  if (Type=="Piece")
  {
    P1  <-  stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", Value)),"Latin-ASCII")
    P2  <-  regmatches(P1,gregexpr("p", P1),TRUE)
    FinalParameter <- trimws(P2[[1L]][1], "both")
    if (grepl(",",FinalParameter)==TRUE){FinalParameter<-gsub(",", ".", FinalParameter)}
    options(digits=5)
    FinalParameter<-as.numeric(FinalParameter)
  }
  #Sets the number of chambres as a numerical variable
  if (Type=="Chambre")
  {
    P1  <-  stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", Value)),"Latin-ASCII")
    P2  <-  regmatches(P1,gregexpr("p", P1),TRUE)
    P3  <-  regmatches(P2[[1L]][2],gregexpr("chb", P2[[1L]][2]),TRUE)
    if (length(P3[[1L]])==1){FinalParameter<-0}
    else {FinalParameter<-stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", P3[[1L]][1])),"Latin-ASCII")}
    if (grepl(",",FinalParameter)==TRUE){FinalParameter<-gsub(",", ".", FinalParameter)}
    options(digits=5)
    FinalParameter<-as.numeric(FinalParameter)
  }
  #Sets the area as a numerical variable
  if (Type=="Area")
  {
    P1  <-  stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", Value)),"Latin-ASCII")
    P2  <-  regmatches(P1,gregexpr("chb", P1),TRUE)
    if (length(P2[[1L]])==1){P2  <-  regmatches(P1,gregexpr("p", P1),TRUE)}
    P3  <-  regmatches(P2[[1L]][2],gregexpr("m", P2[[1L]][2]),TRUE)
    FinalParameter<-stri_trans_general(trimws(gsub("^\\s*<U\\+\\w+>|-", " ", P3[[1L]][1])),"Latin-ASCII")
    if (grepl(",",FinalParameter)==TRUE){FinalParameter<-gsub(",", ".", FinalParameter)}
    options(digits=5)
    FinalParameter<-as.numeric(FinalParameter)
  }
  return(FinalParameter)
}


