#' USGS Site File Data Retrieval
#'
#' Imports data from USGS site file site. This function gets data from here: \url{http://waterservices.usgs.gov/}
#'
#' @param siteNumber vector of string USGS site number.  This is usually an 8 digit number
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @examples
#' siteINFO <- getSiteFileData('05114000')
#' sites <- c('05114000', '01646500')
#' sitesInfo <- getSiteFileData(sites)
getSiteFileData <- function(siteNumber){
  
  siteNumber <- paste(siteNumber, collapse=",")
  
  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&siteOutput=Expanded&sites=",siteNumber,sep = "")
  
  SiteFile <- read.delim(
    urlSitefile,
    header = TRUE,
    quote="\"",
    dec=".",
    sep='\t',
    colClasses=c('character'),
    fill = TRUE,
    comment.char="#")
  
  INFO <- SiteFile[-1,]
  names(INFO) <- gsub("_",".",names(INFO))
  
  INFO$queryTime <- Sys.time()
  INFO$dec.lat.va <- as.numeric(INFO$dec.lat.va)
  INFO$dec.long.va <- as.numeric(INFO$dec.long.va)
  INFO$alt.va <- as.numeric(INFO$alt.va)
  
  return(INFO)
}
