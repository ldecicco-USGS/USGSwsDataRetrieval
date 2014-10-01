#' @title Data Inventory
#'
#' @description Gets a description of unit-value, daily-value, or instantaneous 
#'water-quality data are available for a USGS station and the
#'beginning and ending dates of each parameter.
#'
#' @details The parameter group codes and corresponding NWIS descriptions
#'\tabular{ll}{
#'Code \tab Description\cr
#'INF \tab Information \cr
#'PHY \tab Physical \cr
#'INM \tab Inorganics, Major, Metals (major cations) \cr
#'INN \tab Inorganics, Major, Non-metals (major anions) \cr
#'NUT \tab Nutrient \cr
#'MBI \tab Microbiological \cr
#'BIO \tab Biological \cr
#'IMN \tab Inorganics, Minor, Non-metals \cr
#'IMM \tab Inorganics, Minor, Metals \cr
#'TOX \tab Toxicity \cr
#'OPE \tab Organics, pesticide \cr
#'OPC \tab Organics, PCBs \cr
#'OOT \tab Organics, other \cr
#'RAD \tab Radiochemical \cr
#'SED \tab Sediment \cr
#'POP \tab Population/community \cr
#'}
#' @param site a single USGS station identifier as a character string.
#' @param param.groups a character vector indicating the parameter group 
#'codes to retrun. If blank, then return all parameters. If "ALL,"
#'then return only the summary of all constituents. Otherwise any combination of
#'"INF," "PHY," "INM," "INN," "NUT," "MBI," "BIO," "IMM," "IMN," "TOX,"
#'"OPE," "OPC," "OOT," "RAD," "SED", and "POP." See \bold{Details} for
#'a decription of the parameter group codes.
#' @return A data frame containing these columns:
#'site_no, the USGS station identifier;
#'parm_cd, the parameter code; description, a desciption of the parameter code;
#'begin_date, the earliest date available for the parameter code data;
#'end_date, the latest date available for the parameter code data; and
#'count_nu, the number of values available for the parameter code.
#',EndDate, pCode, and name.

#' @keywords DataIO
#' @examples
#' \dontrun{
#'# These examples require an internet connection to run
#'UVavailable <- whatNWISuv("04027000")
#'}
#' @name whatNWIS
#' @rdname whatNWIS
#' @export whatNWISuv
whatNWISuv <- function(site) {
  ## Coding history:
  ##    2014Sep09 DLLorenz merge whatUV with getDataAvailability
  ##
  if(missing(site))
    stop("site is required")

  myurl <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=",site,sep = "")
  retval <- importRDB(myurl)
  ## Filter for service requested (uv) and remove unnecessary columns
  retval <- retval[retval$data_type_cd %in% "uv", c("parm_cd", "site_no", "stat_cd", "dd_nu", "loc_web_ds", "medium_grp_cd",
  																									"parm_grp_cd", "srs_id", "access_cd", "begin_date",
  																									"end_date", "count_nu")]
  ## Merge with pcode info to get descriptions
  pCodes <- retval$parm_cd
  pcodeINFO <- parameterCdFile[parameterCdFile$parameter_cd %in% pCodes, c("parameter_cd", "srsname", "parameter_units")]
  retval <- merge(pcodeINFO,retval,by.x="parameter_cd", by.y="parm_cd", all.y=TRUE)
  return(retval)
}

#' @rdname whatNWIS
#' @export whatNWISdv
whatNWISdv <- function(site) {
	## Coding history:
	##    2014Sep09 DLLorenz merge whatUV with getDataAvailability
	##
	if(missing(site))
		stop("site is required")
	
	myurl <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=",site,sep = "")
	retval <- importRDB(myurl)
	## Filter for service requested (uv) and remove unnecessary columns
	retval <- retval[retval$data_type_cd %in% "dv", c("parm_cd", "site_no", "stat_cd", "dd_nu", "loc_web_ds", "medium_grp_cd",
																										"parm_grp_cd", "srs_id", "access_cd", "begin_date",
																										"end_date", "count_nu")]
	## Merge with pcode info to get descriptions
	pCodes <- retval$parm_cd
	pcodeINFO <- parameterCdFile[parameterCdFile$parameter_cd %in% pCodes, c("parameter_cd", "srsname", "parameter_units")]
	retval <- merge(pcodeINFO,retval,by.x="parameter_cd", by.y="parm_cd", all.y=TRUE)
	return(retval)
}

#' @rdname whatNWIS
#' @export whatNWISqw
whatNWISqw <- function(site, param.groups="") {
	## Coding history:
	##    2014Sep09 DLLorenz merge whatUV with getDataAvailability
	##
	if(missing(site))
		stop("site is required")
	
	myurl <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=",site,sep = "")
	retval <- importRDB(myurl)
	## Filter for service requested (qw) and remove unnecessary columns
	retval <- retval[retval$data_type_cd %in% "qw", c("parm_cd", "site_no", "stat_cd", "dd_nu", "loc_web_ds", "medium_grp_cd",
																										"parm_grp_cd", "srs_id", "access_cd", "begin_date",
																										"end_date", "count_nu")]
	## Merge with pcode info to get descriptions
	pCodes <- retval$parm_cd
	pcodeINFO <- parameterCdFile[parameterCdFile$parameter_cd %in% pCodes, c("parameter_cd", "srsname", "parameter_units")]
	retval <- merge(pcodeINFO,retval,by.x="parameter_cd", by.y="parm_cd", all.y=TRUE)
	## Filter for parameter groups
	if(param.groups[1L] != "")
		retval <- retval[retval$parm_grp_cd %in% param.groups, ]
	return(retval)
}
