recordTable <- function(inDir,
                        IDfrom,
                        StationIDfrom="directory",
                        speciesIDfrom,
                        cameraIDfrom,
                        camerasIndependent,
                        exclude,
                        minDeltaTime = 0,
                        deltaTimeComparedTo,
                        timeZone,
                        stationCol,
                        writecsv = FALSE,
                        outDir,
                        metadataHierarchyDelimitor = "|",
                        metadataSpeciesTag,
                        additionalMetadataTags,
                        removeDuplicateRecords = TRUE,
                        speciesPosition = NULL,
                        cameraIDposition = NULL,
                        directoryInfoPositions,
                        directoryInfoNames,
                        countsName
)
{
  if (!missing(IDfrom)) {
    warning("argument IDfrom is deprecated; please use speciesIDfrom instead.",
            call. = FALSE)
    speciesIDfrom = "directory"
  }
  recTable <- recordTableFUN(inDir=inDir,
                             IDfrom=IDfrom,
                             StationIDfrom=StationIDfrom,
                             speciesIDfrom=speciesIDfrom,
                             cameraIDfrom=cameraIDfrom,
                             camerasIndependent=camerasIndependent,
                             exclude=exclude,
                             minDeltaTime=minDeltaTime,
                             deltaTimeComparedTo=deltaTimeComparedTo,
                             timeZone=timeZone,
                             stationCol=stationCol,
                             writecsv=writecsv,
                             outDir=outDir,
                             metadataHierarchyDelimitor=metadataHierarchyDelimitor,
                             metadataSpeciesTag=metadataSpeciesTag,
                             additionalMetadataTags=additionalMetadataTags,
                             removeDuplicateRecords=removeDuplicateRecords,
                             speciesPosition=speciesPosition,
                             cameraIDposition=cameraIDposition,
                             directoryInfoPositions=directoryInfoPositions,
                             directoryInfoNames=directoryInfoNames,
                             countsName)

  return(recTable)
  }
