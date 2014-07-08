#'Import Data
#'
#'Reads groundwater level measurements from NWISweb.
#'
#' @param site a single USGS station identifier as a character string.
#' @param begin.date the date to use for the earliest value. If, "", then retrieve beginning 
#'with the first record in the database.
#' @param end.date the date to use for the latest value. If "", then retrieve the most recent 
#'values in the database.
#' @param convert.type Convert data to types indicated by the column type in the
#'data or as indicated in \bold{Note}? See \bold{Details}.
#' @return A data frame of the appropriate data. See
#'\url{http://waterdata.usgs.gov/usa/nwis/gw} for details about groundwater.
#' @note Column names ending in "_va" are always forced to be numeric even if
#'the column header information indicates otherwise. Other columns may need to
#'be converted depending on user needs.\cr Peak flow data do not always have
#'complete date information due to uncertainity in the exact day of the peak.
#'The column \code{peak_dt} is always type "character" but can be converted by
#'the user.
#' @seealso \code{\link{importRDB}}
#' @keywords manip IO
#' @export
#' @examples
#'\dontrun{
#'# Get the most recent groundwater levels, in feet below land surface,
#'# from a well south of Bend, Ore.
#'readNWISgwl("434400121275801", begin.date="2010-01-01")
#'}
readNWISgwl <- function(site, begin.date="", end.date="",
                        convert.type=TRUE) {
  ## Coding history:
  ##    2005Oct25 TimCohn  Original
  ##    2009Dec30 TimCohn  Revisions
  ##    2011Jul12 DLLorenz Changed output to data.frame
  ##    2012Feb07 DLLorenz Tweaks for USGS package
  ##                        basically set up to use importRDB
  ##    2012Feb08 DLLorenz Added begin and end dates for dv and measurements
  ##    2012Feb14 DLLorenz Added gw options
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Sep14 DLLorenz Force all columns ending in _va to numeric
  ##    2012Oct27 DLLorenz Add uv and suppress date conversion on peak
  ##    2012Nov08 DLLorenz Add "gage" and "well" dtypes
  ##    2012Dec03 DLLorenz Revision for date range selection for dv data
  ##                        and allow retrieval of all parameters for dv data
  ##    2012Dec04 DLLorenz Forced column ending in _nu to integer
  ##    2012Dec07 DLLorenz Bug fix for gage retrievals to get partial records
  ##    2012Dec20 DLLorenz Tweaks for unit values
  ##    2013Jan30 DLLorenz Added convert.type option to supress all type conversions
  ##    2013Jan30 DLLorenz Prep for gitHub
  ##    2013Sep04 DLLorenz Modified for waterservices
  ##    2014Jun23 DLLorenz made specific to groundwater levels
  ##
  if(begin.date == "") # Fix needed to set earliest date
    begin.date <- "1860-01-01"
  if(end.date == "") # Fix neede to set today
    end.date <- as.character(today())
  myurl <- url(paste("http://waterservices.usgs.gov/nwis/gwlevels/?format=rdb,1.0&sites=", site,
                     "&startDT=",begin.date, "&endDT=", end.date, sep=""))
  ## Use the date.format option according to the dtype
  URL <- summary(myurl)$description # keep for debugging below
  warn <- options("warn")
  options(warn=-1)
  retval <- try(importRDB(myurl, convert.type=convert.type), silent=TRUE)
  close(myurl)
  options(warn)
  if(class(retval) == "try-error") {
    stop("one of the arguments is invalid, or not valid with a default, or data service is not available--check URL:\n",
         URL)
  }
  if(convert.type) { #Do not force conversion if requested not to
    ## In some cases, columns ending in _va are not always numeric, but should be
    for(i in grep("_va$", names(retval), value=TRUE))
      retval[[i]] <- as.numeric(retval[[i]])
    ## In some cases, columns ending in _nu are not always numeric, but should be
    ##  forced to integer
    for(i in grep("_nu$", names(retval), value=TRUE))
      retval[[i]] <- as.integer(retval[[i]])
  }
  if(nrow(retval) == 0L)
    warning("No data retrieved, check arguments for validity")
  if(ncol(retval) == 1L) {
    ## No retrieval should get only a single column, must not be valid
    ## The one-column retrieval means that an HTML page was returned instead
    warning("Invalid data retrieved, check for validity of the request, URL:\n",
            URL)
    retval <- data.frame()
  }
  return(retval)
}
