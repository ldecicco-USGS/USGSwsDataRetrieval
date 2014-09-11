#'Import Data
#'
#'Reads surface-water or groundwater unit-value data from NWISweb.
#'
#'The value for \code{param} must match the valid 5-digit USGS parameter codes.\cr
#'Time zones should be specified using the Olson format to avoid ambiguity in their use
#'see \code{\link{timezones}} for details. This argument is required for correctly importing data
#'that are stored in a timezone different from the default setting on your computer.
#'
#' @param site a single USGS station identifier as a character string.
#' @param begin.date the data to use for the earliest value. If, "", then retrieve 
#'one week of data.
#' @param end.date the data to use for the latest value. If "", then retrieve the most 
#'recent values in the database.
#' @param param a character vector of the the parameter codes to retrieve. See \bold{Details}.
#'The default value, "00060," retrieves streamflow data.
#' @param tz a character string specifying the time zone to be used for the conversion.
#'See \bold{Details}. The default setting, an empty character string, will use the default 
#'setting on yur computer.
#' @param convert.type Convert data to types indicated by the column type in the
#'data or as indicated in \bold{Note}?
#' @return A data frame of the appropriate data. See
#'\url{http://waterdata.usgs.gov/usa/nwis/sw} for details about surface water
#'or \url{http://waterdata.usgs.gov/usa/nwis/gw} for details about groundwater.
#' @note Column names ending in "_va" are always forced to be numeric even if
#'the column header information indicates otherwise. Other columns may need to
#'be converted depending on user needs.
#' @seealso \code{\link{importRDB}}, \code{\link{renameNWISColumns}}
#' @keywords manip dataIO
#' @export
#' @examples
#'\dontrun{
#'# Get the streamflow data for October 1, 2010 for USGS station identifier
#'# 01578310 SUSQUEHANNA RIVER AT CONOWINGO, MD
#'# Note these data are stored in standard time, so require setting of
#'# the time zone to "America/Jamaica"
#'readNWISuv("01578310", begin.date="2010-10-01", end.date="2010-10-01",
#' tz="America/Jamaica")
#'}
readNWISuv <- function(site, begin.date="", end.date="",
                     param="00060", tz="", convert.type=TRUE) {
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
  ##    2014Jun23 DLLorenz made specific to unit values
  ##
  if(begin.date == "") # Fix needed to set earliest date
    begin.date <- "1860-01-01"
  if(end.date == "") # Fix needed to set today
    end.date <- as.character(today())
  if(is.null(param)) {
    stop("the param argument is required unit-value data")
  }
  myurl <- url(paste("http://waterservices.usgs.gov/nwis/iv/?format=rdb,1.0&sites=", site,
                     "&startDT=",begin.date, "&endDT=", end.date, "&parameterCd=", param, sep=""))
  ## Use the date.format option according to the uv data
  URL <- summary(myurl)$description # keep for debugging below
  warn <- options("warn")
  options(warn=-1)
  retval <- try(importRDB(myurl, date.format="%Y-%m-%d %H:%M", tz=tz,
                          convert.type=convert.type), silent=TRUE)
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
