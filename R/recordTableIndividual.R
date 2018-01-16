recordTableIndividual <- function(inDir,
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
                                  speciesPosition = NULL,
                                  cameraIDposition = NULL,
                                  directoryInfoPositions,
                                  directoryInfoNames,
                                  countsName

)
{


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

  if(hasArg(countsName)) stop("Counts cannot be used in recordTableIndividual")

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
                           speciesPosition=speciesPosition,
                           cameraIDposition=cameraIDposition,
                           directoryInfoPositions=directoryInfoPositions,
                           directoryInfoNames=directoryInfoNames,
                           countsName)

  return(recTable)
}
