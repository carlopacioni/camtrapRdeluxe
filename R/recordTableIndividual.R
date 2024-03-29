#' @export
recordTableIndividual <- function(inDir,
                                  cameraID,
                                  hasStationFolders,
                                  IDfrom,
                                  StationIDfrom="directory",
                                  speciesIDfrom,
                                  cameraIDfrom,
                                  camerasIndependent,
                                  individualIDfrom,
                                  exclude,
                                  minDeltaTime = 0,
                                  deltaTimeComparedTo,
                                  timeZone,
                                  stationCol,
                                  writecsv = FALSE,
                                  outDir,
                                  metadataHierarchyDelimitor = "|",
                                  metadataSpeciesTag,
                                  metadataIDTag,
                                  additionalMetadataTags,
                                  removeDuplicateRecords = TRUE,
                                  stationIDposition = NULL,
                                  speciesPosition = NULL,
                                  cameraIDposition = NULL,
                                  directoryInfoPositions,
                                  directoryInfoNames)
{
  # dealing with users with older arguments
  if (!missing(IDfrom)) {
    warning("argument IDfrom is deprecated; please use speciesIDfrom and
            individualIDfrom instead.",
            call. = FALSE)
    individualIDfrom = IDfrom
      speciesIDfrom = "directory"
  }

  if (!missing(hasStationFolders)) {
    warning("argument hasStationFolders is deprecated; please use StationIDfrom instead.",
            call. = FALSE)
    if(hasStationFolders) {
      StationIDfrom <- "directory"
    } else {
      StationIDfrom <- "filename"
    }
  }

  if (!missing(cameraID)) {
    warning("argument cameraID is deprecated; please use cameraIDfrom instead.",
            call. = FALSE)
    cameraIDfrom = cameraID
  }


  if(class(individualIDfrom) != "character"){stop("IDfrom must be of class 'character'")}
  if(individualIDfrom %in% c("metadata", "directory") == FALSE) stop("'IDfrom' must be 'metadata' or 'directory'")

 if(individualIDfrom == "metadata"){
    if(metadataHierarchyDelimitor %in% c("|", ":") == FALSE) stop("'metadataHierarchyDelimitor' must be '|' or ':'")

    if(!hasArg(metadataIDTag)) {stop("'metadataIDTag' must be defined if IDfrom = 'metadata'")}
    if(class(metadataIDTag)  != "character") {stop("metadataIDTag must be of class 'character'")}
    if(length(metadataIDTag) != 1) {stop("metadataIDTag must be of length 1")}
  }

  if(hasArg(metadataIDTag)){
    if(class(metadataIDTag) != "character"){stop("metadataIDTag must be of class 'character'", call. = FALSE)}
    if(length(metadataIDTag) != 1){stop("metadataIDTag must be of length 1", call. = FALSE)}
  }

recTable <- recordTableFUN(inDir=inDir,
                           IDfrom=IDfrom,
                           StationIDfrom=StationIDfrom,
                           speciesIDfrom=speciesIDfrom,
                           cameraIDfrom=cameraIDfrom,
                           camerasIndependent=camerasIndependent,
                           individualIDfrom=individualIDfrom,
                           exclude=exclude,
                           minDeltaTime=minDeltaTime,
                           deltaTimeComparedTo=deltaTimeComparedTo,
                           timeZone=timeZone,
                           stationCol=stationCol,
                           writecsv=writecsv,
                           outDir=outDir,
                           metadataHierarchyDelimitor=metadataHierarchyDelimitor,
                           metadataSpeciesTag=metadataSpeciesTag,
                           metadataIDTag=metadataIDTag,
                           additionalMetadataTags=additionalMetadataTags,
                           removeDuplicateRecords=removeDuplicateRecords,
                           stationIDposition=stationIDposition,
                           speciesPosition=speciesPosition,
                           cameraIDposition=cameraIDposition,
                           directoryInfoPositions=directoryInfoPositions,
                           directoryInfoNames=directoryInfoNames)

  return(recTable)
}
