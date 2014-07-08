#' Import Data
#'
#' Reads the current rating table for an active USGS streamgage.
#'
#' @param site a single USGS station identifier as a character string.
#' @param type the type of rating, must be "base" for the base rating curve,
#'"corr" for the base with corrections, or "exsa" for expanded, shift-corrected
#'rating curve. Only a single character is needed. The default is "base."
#' @return A data frame. If \code{type} is "base," then the columns are
#'INDEP, typically the gage height, in feet; DEP, typically the streamflow,
#'in cubic feet per second; and STOR, where "*" indicates that the pair are
#'a fixed point of the rating curve. If \code{type} is "exsa," then an
#'additional column, SHIFT, is included that indicates the current shift in
#'the rating for that value of INDEP. If \code{type} is "corr," then the
#'columns are INDEP, typically the gage height, in feet; CORR, the correction
#'for that value; and CORRINDEP, the corrected value for CORR.\cr
#'If \code{type} is "base," then the data frame has an attribute called "RATING"
#'that describes the rating curve is included.
#' @note Not all active USGS streamgages have traditional rating curves that
#'relate flow to stage.
#' @seealso \code{\link{importRDB}}
#' @keywords manip IO
#' @export
#' @examples
#'\dontrun{
#'# Get the current base rating curve for USGS station identifier
#'# 05331000 MISSISSIPPI RIVER AT ST. PAUL, MN
#'readNWIS("05331000")
#'}
readNWISratingt <- function(site, type="base") {
  ## Coding history:
  ##    2014Jun10 DLLorenz Original coding
  ## 
  ## Make sure type is valid and site was specified
  type <- match.arg(type, c("base", "corr", "exsa"))
  if(missing(site))
    stop("the site argument is missing")
  ## Construct the URL
  myurl <- url(paste("http://waterdata.usgs.gov/nwisweb/get_ratings?site_no=",
                     site, "&file_type=", type, sep=""))
  URL <- summary(myurl)$description # keep for debugging below
  warn <- options("warn")
  options(warn=-1)
  retval <- try(importRDB(myurl), silent=TRUE)
  close(myurl)
  options(warn)
  if(class(retval) == "try-error") {
    stop("one of the arguments is invalid, or not valid with a default, or data service is not available--check URL:\n",
         URL)
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
  # Strip the importatn rating information for base
  if(type == "base") {
    Rat <- grep("//RATING ", comment(retval), value=TRUE, fixed=TRUE)
    Rat <- sub("# //RATING ", "", Rat)
    Rat <- scan(text=Rat, sep=" ", what="")
    attr(retval, "RATING") <- Rat
  }
  return(retval)
}
