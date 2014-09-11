#' @title Water-Quality Data
#'
#' @description Import discrete sample water-quality data from NWIS web.
#'
#' @details Valid groups are "All," "information," "physical," "cations," "anions,"
#'"nutrients," "microbiological," "biological," "metals," "nonmetals,"
#'"toxicity," "pesticides," "pcbs," "other organics," "radio chemicals,",
#'"stable isotopes," "sediment," and "population/community."
#'The parameter group codes and corresponding NWIS descriptions
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
#' @param sites a character vector of the USGS station identifiers.
#' @param params a character string that contains the code for a parameter
#'group, or a character vector of 5-digit parameter codes. See \bold{Details}.
#' @param begin.date the earliest date for data, must be a character with the
#'format "YYYY-mm-dd."
#' @param end.date the latest date for data, must be a character with the format
#'"YYYY-mm-dd."
#' @return A data frame of columns describing water-quality samples organized by
#'parameter code.
#' @note Probably need a note.
#' @seealso \code{\link{readNWISwqp}}, \code{\link{readSTORETwqp}}
#' @references Lorenz, D.L., 2014, USGSqw OFR.\cr See information about discrete
#'samples at \url{http://nwis.waterdata.usgs.gov/usa/nwis/qw}.
#' @keywords datasets IO
#' @examples
#'
#'\dontrun{
#'readNWISqw("05330000", "00608") # Ammonia samples from the Minnesota River at Jordan.
#'}
#'
#' @export
readNWISqw <- function(sites, params="All", begin.date="", end.date="") {
  ## Coding history:
  ##    2012Sep06 DLLorenz original Coding
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2013Dec16 DLLorenz
  ##
  params <- paste(params,  collapse=",")
  pgrp <- pmatch(params, c("INF", "PHY", "INM", "INN", "NUT", "MBI", "BIO", "IMM", "IMN", "TOX",
  												 "OPE", "OPC", "OOT", "RAD", "XXX", "SED", "POP"), nomatch=0)
  if(pgrp == 0) {
    ## pack sites
    sites <- paste(sites, collapse=",")
    if(params == "All")
      myurl <- url(paste("http://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=", sites,
                         "&sort_key=site_no&group_key=NONE&inventory_output=0",
                         "&begin_date=", begin.date, "&end_date=", end.date,
                         "&TZoutput=0",
                         "&radio_parm_cds=all_parm_cds&qw_attributes=0&format=rdb",
                         "&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD",
                         "&rdb_compression=value&list_of_search_criteria=multiple_site_no",
                         sep=""))
    else {
      myurl <- url(paste("http://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=",
                         sites,
                         "&sort_key=site_no&group_key=NONE&inventory_output=0",
                         "&begin_date=", begin.date, "&end_date=", end.date,
                         "&TZoutput=0&radio_parm_cds=parm_cd_list",
                         "&radio_multiple_parm_cds=", params,
                         "&TZoutput=0",
                         "&qw_attributes=0&format=rdb",
                         "&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD",
                         "&rdb_compression=value&list_of_search_criteria=multiple_site_no",
                         sep=""))
    }
    retval <- importRDB(myurl)
    close(myurl)
  }
  else { # must be request for a group
    grp <- c("INF", "PHY", "INM", "INN", "NUT", "MBI", "BIO", "IMM", "IMN", "TOX",
             "OPE", "OPC", "OOT", "RAD", "XXX", "SED", "POP")[pgrp]
    retval <- lapply(as.list(sites), function(i, grp) {
      myurl <- url(paste("http://nwis.waterdata.usgs.gov/nwis/qwdata/?site_no=", i,
                         "&agency_cd=USGS&param_group=", grp,
                         "&format=rdb", sep=""))
      retdf <- importRDB(myurl)
      close(myurl)
      return(retdf)
    }, grp=grp)
    retval <- do.call(rbind, retval)
    if(begin.date != "") {
      begin.date <- as.Date(begin.date)
      retval <-  retval[retval$sample_dt >= begin.date, ]
    }
    if(end.date != "") {
      end.date <- as.Date(end.date)
      retval <-  retval[retval$sample_dt <= end.date, ]
    }
  } # end of else stmt
  ## Well, for some peculiar reason, columns ending in _va are character but
  ## should be numeric--change them
  for(i in grep("_va$", names(retval), value=TRUE))
      retval[[i]] <- as.numeric(retval[[i]])
  ## Stop if empty data
  if(nrow(retval) == 0L)
    stop("No data retrieved, check for valid arguments")
  return(retval)
}
