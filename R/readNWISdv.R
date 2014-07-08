#'Import USGS Data
#'
#'Reads surface-water or groundwater daily value data from NWISweb.
#'
#'The value for \code{param} must match a valid 5-digit USGS parameter code or "all."
#'The default value is "all," which retrieve all available data.\cr
#'In some cases, daily values data can be tagged with additional nonnumeric
#'flags. In those cases, the data would be converted to \code{NA}, but setting
#'\code{convert.type} to \code{FALSE} will preserve all data as character and
#'the all data can be converted manually by the user.
#'
#' @param site a single USGS station identifier as a character string.
#' @param begin.date the data to use for the earliest value. Not used for
#'\code{dtype} = "peak." If, "", then retrieve beginning with the first record
#'in the database for "swdv" and "gwdv". If, "" for "uv", one week of data is returned.
#' @param end.date the data to use for the latest value. If "", then retrieve the most 
#'recent values in the database.
#' @param param a character vector of the parameter codes to retrieve. See \bold{Details}.
#' @param stat the statistic codes or name of code to retrieve or "all." If "all," 
#'then retrieve all. Otherwise, must be the 5-digit code or one of "maximum,"
#'"minimum," "mean," or "median." May be uniquely abbreviated. Note also that the statistic
#'code is not always a reliable way to filter the retrieved data.
#' @param convert.type Convert data to types indicated by the column type in the
#'data or as indicated in \bold{Note}? See \bold{Details}.
#' @return A data frame of the appropriate data. See
#'\url{http://waterdata.usgs.gov/usa/nwis/sw} for details about surface water
#'or \url{http://waterdata.usgs.gov/usa/nwis/gw} for details about groundwater.
#' @note Column names ending in "_va" are always forced to be numeric even if
#'the column header information indicates otherwise. Other columns may need to
#'be converted depending on user needs.
#' @author Original coding by Tim Cohn \email{tacohn@@usgs.gov} additional
#'capabilities added by Dave Lorenz \email{lorenz@@usgs.gov}.
#' @seealso \code{\link{importRDB}}, \code{\link{renCol}}
#' @keywords manip IO
#' @export
#' @examples
#'\dontrun{
#'# Get the first 5 days in 2010 for USGS station identifier
#'# 01578310 SUSQUEHANNA RIVER AT CONOWINGO, MD
#'readNWISdv("01578310", begin.date="2010-01-01", end.date="2010-01-05")
#'}
readNWISdv <- function(site, begin.date="", end.date="",
                     param="all", stat="all", convert.type=TRUE) {
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
  ##    2014Jun23 DLLorenz Made specific to daily values
  ##
  setStat <- function(Stat) {
    if(length(Stat) == 1L) {
      xstat <- pmatch(Stat, c("maximum", "minimum", "mean", "median"),
                      nomatch=0)
      if(xstat > 0) {
        Stat <- select(xstat, "00001", "00002", "00003", "00008")
        Stat <- paste("&statCd=", Stat, sep="")
      } else
        Stat <- ""
    } else { # Cllapse sequence
      Stat <- paste(Stat, collapse=",")
      Stat <- paste("&statCd=", Stat, sep="")
    }
    return(Stat)
  }
  if(begin.date == "") # Fix needed to set earliest date
    begin.date <- "1860-01-01"
  if(end.date == "") # Fix neede to set today
    end.date <- as.character(today())
  # Process param, stat done in call to create URL
  param <- paste(param, collapse=",")
  if(param == "all") {
    param <- ""
  } else
    param <- paste("&parameterCd=", param, sep="")
  # Construct the URL
  myurl <- url(paste("http://waterservices.usgs.gov/nwis/dv/?format=rdb,1.0&sites=", 
                     site,"&startDT=",begin.date, "&endDT=", end.date, 
                     setStat(stat), param, sep=""))
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
