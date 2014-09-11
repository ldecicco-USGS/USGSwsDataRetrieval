#' @title Parameter Code Information
#'
#' @description Get information about USGS parameter codes.
#'
#' @details Valid parameter group codes for \code{params} are "All," 
#'which retrieves all parameters and these parameter group codes 
#'with the corresponding NWIS descriptions.
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
#'STI \tab Stable isotopes \cr
#'SED \tab Sediment \cr
#'POP \tab Population/community \cr
#'}

#'@param params A character string that contains the name of a parameter
#'group, or a vector of paramater codes. See \bold{Details} for parameter
#'group codes.
#'@param group include the parameter group in the output?
#'@param name include the parameter name in the output?
#'@param CASRN include the parameter Chemical Abstracts Service Registry Number
#'(CASRN) in the output?
#'@param short include the parameter short name in the output?
#'@param units include the parameter units in the output?
#'@param col.name include a column name for the parameter in the output?
#'@return A data frame with the parameter codes and the selected columns.
#'@references Lorenz, D.L., 2014, USGSqw OFR.\cr See information about discrete
#'samples at \url{http://nwis.waterdata.usgs.gov/usa/nwis/qw}.
#'@keywords datasets IO
#'@examples
#'
#'\dontrun{
#'pcodeNWISqw("00925")
#'}
#'
#'@export
readNWISpcode <- function(params="All", group=TRUE, name=TRUE, CASRN=FALSE, short=TRUE,
                        units=TRUE, col.name=FALSE) {
  ## Coding history:
  ##    2012Sep07 DLLorenz original Coding
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
  ## The index of short names to NWIS group names
  grpcd <- c("Information"="INF",
             "Physical"="PHY",
             "Inorganics, Major, Metals"="INM",
             "Inorganics, Major, Non-metals"="INN",
             "Nutrient"="NUT",
             "Microbiological"="MBI",
             "Biological"="BIO",
             "Inorganics, Minor, Metals"="IMM",
             "Inorganics, Minor, Non-metals"="IMN",
             "Toxicity"="TOX",
             "Organics, pesticide"="OPE",
             "Organics, PCBs"="OPC",
             "Organics, other" ="OOT",
             "Radiochemical"="RAD",
             "Stable Isotopes"="STI",
             "Sediment"="SED",
             "Population/Community"="POP")
  ## Continue, ckprm = 0 if pcode list
  if(length(params) > 1L) # Must be multiple pcodes, get em all
    ckprm <- -1L
  else
    ckprm <- pmatch(params, c(grpcd, "All"), nomatch=0L)
  if(ckprm == 0L) { # Get a single pcode
    myurl <- url(paste("http://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=pm_search",
                       "&pm_search=", params,
                       "&format=rdb", "&show=parameter_group_nm"[group],
                       "&show=parameter_nm", "&show=casrn"[CASRN],
                       "&show=srsname", "&show=parameter_units"[units],
                       sep=""))
    retval <- importRDB(myurl)
    close(myurl)
  }
  else {
    if(ckprm == -1L)
      prmndx <- length(grpcd) + 1L
    else
      prmndx <- ckprm
    prmgp <- c(names(grpcd), "All+--+include+all+parameter+groups")[prmndx]
    prmgp <- gsub(" ", "+", prmgp)
    prmgp <- gsub(",", "%2C", prmgp)
    ## Always retrieve name and short name to produce column name if necessary
    myurl <- url(paste("http://nwis.waterdata.usgs.gov/nwis/pmcodes/pmcodes?radio_pm_search=param_group",
                       "&pm_group=", prmgp,
                       "&format=rdb", "&show=parameter_group_nm"[group],
                       "&show=parameter_nm", "&show=casrn"[CASRN],
                       "&show=srsname", "&show=parameter_units"[units],
                       sep=""))
    retval <- importRDB(myurl)
    close(myurl)
    ## get the requested pcodes if needed
    if(ckprm == -1L)
      retval <- retval[retval$parameter_cd %in% params, ]
  }
  ## Recode to shorter names if groupcodes were requested
  if(group)
    retval$parameter_group_nm <- factor(grpcd[retval$parameter_group_nm], levels=grpcd)
  ## Add column names if requested
  if(col.name)
    retval <- merge(retval, with(retval,
                                 makeColNames(parameter_cd, parameter_nm, srsname)),
                    by.x="parameter_cd", by.y="parm_cd")
  ## remove unwanted columns if necessasry
  if(!name)
    retval$parameter_nm <- NULL
  if(!short)
    retval$srsname <- NULL
  return(retval)
}
