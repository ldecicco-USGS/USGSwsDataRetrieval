#' @title Water-Quality Data
#'
#' @description Import discrete sample water-quality data from STORET or NWIS
#'databases in the Water Quality Portal web service. 
#'This function gets the data from \url{http://www.waterqualitydata.us}. This 
#'function is more general than \code{readNWISwqp} because it allows for 
#'agencies other than the USGS.  
#'
#' @details This function uses Characteristic Name to identify water-quality constituents
#'instead of parameter codes. A complete list of the names can be found at 
#'\url{http://www.waterqualitydata.us/Codes/Characteristicname?mimeType=xml}.
#'
#' @param siteid a character string of the site identifier consiting of the
#'agency code, a hyphen ("-"), and the site-identification number. See \bold{Examples}.
#' @param charName a chracter vector of valid Characteristic Names. See \bold{Details}.
#' @param begin.date the earliest date for data, must be a character with the
#'format "YYYY-mm-dd." If blank, then retrieve data from the earliest record.
#' @param end.date the latest date for data, must be a character with the format
#'"YYYY-mm-dd." If blank, then retrieve data to the most recent record.
#' @keywords data import WQP web service
#' @return A data frame with these columns:
#' 
#' \tabular{ll}{
#'Column Name \tab Definition \cr
#'OrganizationIdentifier \tab A designator used to uniquely identify a unique business establishment within a context. \cr
#'OrganizationFormalName \tab The legal designator (i.e. formal name) of an organization. \cr
#'ActivityIdentifier \tab Designator that uniquely identifies an activity within an organization. \cr
#'ActivityTypeCode \tab The text describing the type of activity. \cr
#'ActivityMediaName \tab Name or code indicating the environmental medium where the sample was taken. \cr
#'ActivityMediaSubdivisionName \tab Name or code indicating the environmental matrix as a subdivision of the sample media. \cr
#'ActivityStartDate \tab The calendar date on which the field activity is started. \cr
#'ActivityStartTime/Time \tab The time of day that is reported when the field activity began, based on a 24-hour timescale. \cr
#'ActivityStartTime/TimeZoneCode \tab The time zone for which the time of day is reported. Any of the longitudinal divisions of the earth's surface in which a standard time is kept. \cr
#'ActivityEndDate \tab The calendar date when the field activity is completed. \cr
#'ActivityEndTime/Time \tab The time of day that is reported when the field activity ended, based on a 24-hour timescale. \cr
#'ActivityEndTime/TimeZoneCode \tab The time zone for which the time of day is reported. Any of the longitudinal divisions of the earth's surface in which a standard time is kept. \cr
#'ActivityDepthHeightMeasure/MeasureValue \tab A measurement of the vertical location (measured from a reference point) at which an activity occurred. Measure value is given in the units stored in ActivityDepthHeightMeasure/MeasureUnitCode. \cr
#'ActivityDepthHeightMeasure/MeasureUnitCode \tab The code that represents the unit for measuring the item. \cr
#'ActivityDepthAltitudeReferencePointText \tab The reference used to indicate the datum or reference used to establish the depth/altitude of an activity. \cr
#'ActivityTopDepthHeightMeasure/MeasureValue \tab A measurement of the upper vertical location of a vertical location range (measured from a reference point) at which an activity occurred. Measure value is given in the units stored in ActivityTopDepthHeightMeasure/MeasureUnitCode. \cr
#'ActivityTopDepthHeightMeasure/MeasureUnitCode \tab The code that represents the unit for measuring the item. \cr
#'ActivityBottomDepthHeightMeasure/MeasureValue \tab A measurement of the lower vertical location of a vertical location range (measured from a reference point) at which an activity occurred. Measure value is given in the units stored in ActivityBottomDepthHeightMeasure/MeasureUnitCode. \cr
#'ActivityBottomDepthHeightMeasure/MeasureUnitCode \tab The code that represents the unit for measuring the item. \cr
#'ProjectIdentifier \tab A designator used to uniquely identify a data collection project within a context of an organization. \cr
#'ActivityConductingOrganizationText \tab A name of the Organization conducting an activity. \cr
#'MonitoringLocationIdentifier \tab A designator used to describe the unique name, number, or code assigned to identify the monitoring location. \cr
#'ActivityCommentText \tab General comments concerning the activity. \cr
#'SampleAquifer \tab  A code that designates the aquifer associated with groundwater samples. \cr
#'HydrologicCondition \tab  Hydrologic condition is the hydrologic condition that is represented by the sample collected (i.e. ? normal, falling, rising, peak stage). \cr
#'HydrologicEvent \tab  A hydrologic event that is represented by the sample collected (i.e. - storm, drought, snowmelt). \cr
#'SampleCollectionMethod/MethodIdentifier \tab The identification number or code assigned by the method publisher. \cr
#'SampleCollectionMethod/MethodIdentifierContext \tab Identifies the source or data system that created or defined the identifier. \cr
#'SampleCollectionMethod/MethodName \tab The title that appears on the method from the method publisher. \cr
#'SampleCollectionEquipmentName \tab The name for the equipment used in collecting the sample. \cr
#'ResultDetectionConditionText \tab The textual descriptor of a result. \cr
#'CharacteristicName \tab The object, property, or substance which is evaluated or enumerated by either a direct field measurement, a direct field observation, or by laboratory analysis of material collected in the field. \cr
#'ResultSampleFractionText \tab The text name of the portion of the sample associated with results obtained from a physically-partitioned sample. \cr
#'ResultMeasureValue \tab The reportable measure of the result for the chemical, microbiological or other characteristic being analyzed. Measure value is given in the units stored in ResultMeasure/MeasureUnitCode. \cr
#'ResultMeasure/MeasureUnitCode \tab The code that represents the unit for measuring the item. \cr
#'ResultStatusIdentifier \tab Indicates the acceptability of the result with respect to QA/QC criteria. \cr
#'StatisticalBaseCode \tab The code for the method used to calculate derived results. \cr
#'ResultValueTypeName \tab A name that qualifies the process which was used in the determination of the result value (e.g., actual, estimated, calculated). \cr
#'ResultWeightBasisText \tab The name that represents the form of the sample or portion of the sample which is associated with the result value (e.g., wet weight, dry weight, ash-free dry weight). \cr
#'ResultTimeBasisText \tab The period of time (in days) over which a measurement was made. For example, BOD can be measured as 5 day or 20 day BOD. \cr
#'ResultTemperatureBasisText \tab The name that represents the controlled temperature at which the sample was maintained during analysis, e.g. 25 deg BOD analysis. \cr
#'ResultParticleSizeBasisText \tab User defined free text describing the particle size class for which the associated result is defined. \cr
#'PrecisionValue \tab A measure of mutual agreement among individual measurements of the same property usually under prescribed similar conditions. \cr
#'ResultCommentText \tab Free text with general comments concerning the result. \cr
#'USGSPCode \tab  5-digit number used in the US Geological Survey computerized data system, National Water Information System (NWIS), to uniquely identify a specific constituent. \cr
#'ResultDepthHeightMeasure/MeasureValue \tab   A measurement of the vertical location (measured from a reference point) at which a result occurred. \cr
#'ResultDepthHeightMeasure/MeasureUnitCode \tab   The code that represents the unit for measuring the item. \cr
#'ResultDepthAltitudeReferencePointText \tab   The reference used to indicate the datum or reference used to establish the depth/altitude of a result. \cr
#'SubjectTaxonomicName \tab The name of the organism from which a tissue sample was taken. \cr
#'SampleTissueAnatomyName \tab  The name of the anatomy from which a tissue sample was taken. \cr
#'ResultAnalyticalMethod/MethodIdentifier \tab The identification number or code assigned by the method publisher. \cr
#'ResultAnalyticalMethod/MethodIdentifierContext \tab Identifies the source or data system that created or defined the identifier. \cr
#'ResultAnalyticalMethod/MethodName \tab The title that appears on the method from the method publisher. \cr
#'MethodDescriptionText \tab  A brief summary that provides general information about the method. \cr
#'LaboratoryName \tab The name of Lab responsible for the result. \cr
#'AnalysisStartDate \tab The calendar date on which the analysis began. \cr
#'ResultLaboratoryCommentText \tab Remarks which further describe the laboratory procedures which produced the result. \cr
#'DetectionQuantitationLimitTypeName \tab Text describing the type of detection or quantitation level used in the analysis of a characteristic. \cr
#'DetectionQuantitationLimitMeasure/MeasureValue \tab Constituent concentration that, when processed through the complete method, produces a signal that is statistically different from a blank. Measure value is given in the units stored in DetectionQuantitationLimitMeasure/MeasureUnitCode. \cr
#'DetectionQuantitationLimitMeasure/MeasureUnitCode \tab The code that represents the unit for measuring the item. \cr
#'PreparationStartDate \tab The calendar date when the preparation/extraction of the sample for analysis began. \cr
#'}
#' @export
#' @seealso \code{\link{readNWISwqp}}
#' @import RCurl
#' @examples
#' \dontrun{
#' # These examples require an internet connection to run
#'readSTORETwqp("USGS-01594440","Chloride", "", "")
#'readSTORETwqp("WIDNR_WQX-10032762","Specific conductance", "", "")
#' }
readSTORETwqp <- function(siteid, charName, begin.date, end.date) {
	## Coding history:
	##    2014Sep09 DLLorenz original Coding from getWQPData.r
  ##
	## Set the dates
  if (nzchar(begin.date)){
    begin.date <- format(as.Date(begin.date), format="%m-%d-%Y")
  }
  if (nzchar(end.date)){
    end.date <- format(as.Date(end.date), format="%m-%d-%Y")
  }
  ## Process the characterstic name
  charName <- paste(charName, collapse=";")
  charName <- URLencode(charName)
  
  baseURL <- "http://www.waterqualitydata.us/Result/search?siteid="
  url <- paste(baseURL,
               siteid,
               "&characteristicName=",
               charName,   # to get multi-parameters, use a semicolen 
               "&startDateLo=",
               begin.date,
               "&startDateHi=",
               end.date,
               "&countrycode=US&mimeType=tsv",sep = "")
  
  ## Set up the retrieval
  h <- basicHeaderGatherer()
  doc <- getURI(url, headerfunction = h$update)
  numToBeReturned <- as.numeric(h$value()["Total-Result-Count"])
  
  suppressWarnings(retval <- read.delim(url, header = TRUE, quote="\"", dec=".", sep='\t', colClasses=c('character'), fill = TRUE))
  
  originalLength <- nrow(retval)
  
  if (!is.na(numToBeReturned)){
  	if(originalLength != numToBeReturned) 
  		warning(numToBeReturned, " sample results were expected, ", originalLength, " were returned")
  	## Convert Dates and Values
  	for(col in c("ActivityStartDate", "ActivityEndDate", "AnalysisStartDate", "PreparationStartDate"))
  		retval[[col]] <- as.Date(retval[[col]], format="%Y-%m-%d")
  	for(col in c("ActivityDepthHeightMeasure.MeasureValue", "ActivityTopDepthHeightMeasure.MeasureValue",
  							 "ActivityBottomDepthHeightMeasure.MeasureValue", "ResultMeasureValue", "PrecisionValue",
  							 "ResultDepthHeightMeasure.MeasureValue", "DetectionQuantitationLimitMeasure.MeasureValue"))
  		retval[[col]] <- as.numeric(retval[[col]])
  } else {
  	warning("No data retrieved")
  }
  return(retval)
}
