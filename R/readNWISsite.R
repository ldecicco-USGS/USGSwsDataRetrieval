#'Import Data
#'
#'Reads site data from NWISweb.
#'
#'
#' @param sites a character vector containing the USGS station identifier. 
#'Missing values are not permitted.
#' @param dtype a character string indicating the data type desired, "all" (default)
#'returns all data, "gage" returns data appropriate for a surface-water gage, and
#'"well" returns data appropraiate for a groundwater well.
#' @param convert.type Convert data to types indicated by the column type in the
#'data or as indicated in \bold{Note}? See \bold{Details}.
#' @return A data frame of the appropriate data. See
#'\url{http://waterdata.usgs.gov/usa/nwis/sw} for details about surface water
#'or \url{http://waterdata.usgs.gov/usa/nwis/gw} for details about groundwater.
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
#'# Get the site information for USGS station identifier
#'# 01578310 SUSQUEHANNA RIVER AT CONOWINGO, MD
#'readNWISsite("01578310", dtype="gage")
#'}
readNWISsite <- function(sites, dtype="swdv", begin.date="", end.date="",
                     param=NULL, stat=NULL, convert.type=TRUE) {
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
  ##    2014Jun23 DLLorenz made specific to site data
  ##
  ## Columns for gage and well:
  GAGE <- c("agency_cd","site_no","station_nm","site_tp_cd","lat_va","long_va",
         "dec_lat_va","dec_long_va","coord_meth_cd","coord_acy_cd",
         "coord_datum_cd","dec_coord_datum_cd","district_cd","state_cd",
         "county_cd","country_cd","land_net_ds","map_nm","map_scale_fc",
         "alt_va","alt_meth_cd","alt_acy_va","alt_datum_cd","huc_cd",
         "basin_cd","topo_cd","instruments_cd",
         "construction_dt","inventory_dt","drain_area_va",
         "contrib_drain_area_va","tz_cd","local_time_fg","reliability_cd",
         "project_no")
  WELL <- c("agency_cd","site_no","station_nm","site_tp_cd","lat_va","long_va",
            "dec_lat_va","dec_long_va","coord_meth_cd","coord_acy_cd",
            "coord_datum_cd","dec_coord_datum_cd","district_cd","state_cd",
            "county_cd","country_cd","land_net_ds","map_nm","map_scale_fc",
            "alt_va","alt_meth_cd","alt_acy_va","alt_datum_cd","huc_cd",
            "basin_cd","topo_cd","instruments_cd",
            "construction_dt","inventory_dt","tz_cd","local_time_fg",
            "reliability_cd","gw_file_cd","nat_aqfr_cd","aqfr_cd",
            "aqfr_type_cd","well_depth_va","hole_depth_va","depth_src_cd",
            "project_no")
  sites <- paste(sites, collapse=",")
  myurl <- url(paste("http://waterservices.usgs.gov/nwis/site/?format=rdb,1.0&sites=", sites,  
                       "&siteOutput=expanded", sep=""))
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
  if(dtype == "gage") {
    retval <- retval[, GAGE]
  } else if(dtype == "well")
    retval <- retval[, WELL]
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
