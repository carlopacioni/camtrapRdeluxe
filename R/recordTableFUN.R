recordTableFUN <- function( inDir,
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
                            directoryInfoNames,
                            countsName
)
{
  #################### To DO ##########################################
  # add checks for new arguments

  wd0 <- getwd()
  on.exit(setwd(wd0))

  if(hasArg(stationCol) == FALSE) stationCol <- "Station"
  stopifnot(is.character(stationCol))
  speciesCol <- "Species"

  individualCol <- "Individual"

  checkForSpacesInColumnNames(stationCol = stationCol)

  if(class(StationIDfrom) != "character"){stop("StationIDfrom must be of class 'character'", call. = FALSE)}
  if(StationIDfrom %in% c("filename", "directory") == FALSE) {stop("StationIDfrom can only be 'filename', 'directory'", call. = FALSE)}
  if(StationIDfrom == "directory") hasStationFolders <- TRUE else hasStationFolders <- FALSE

  if(class(speciesIDfrom) != "character"){stop("speciesIDfrom must be of class 'character'")}
  if(speciesIDfrom %in% c("metadata", "directory") == FALSE) stop("'speciesIDfrom' must be 'metadata' or 'directory'")

 if(speciesIDfrom == "metadata"){
    if(metadataHierarchyDelimitor %in% c("|", ":") == FALSE) stop("'metadataHierarchyDelimitor' must be '|' or ':'")

    if(!hasArg(metadataSpeciesTag)) {stop("'metadataSpeciesTag' must be defined if speciesIDfrom = 'metadata'")}
    if(class(metadataSpeciesTag) != "character"){stop("metadataSpeciesTag must be of class 'character'")}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of length 1")}
  }

  multiple_tag_separator <- "_&_"

  # check input
  if(hasArg(timeZone) == FALSE) {
    warning("timeZone is not specified. Assuming UTC", call. = FALSE,  immediate. = TRUE)
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()", call. = FALSE)
  }
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)

  if(hasArg(metadataSpeciesTag)){
    if(class(metadataSpeciesTag) != "character"){stop("metadataSpeciesTag must be of class 'character'", call. = FALSE)}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of length 1", call. = FALSE)}
  }

  if(hasArg(cameraID)){
    if(class(cameraID) != "character"){stop("cameraID must be of class 'character'", call. = FALSE)}
    if(cameraID %in% c("filename", "directory") == FALSE) {stop("cameraID can only be 'filename', 'directory', or missing", call. = FALSE)}
    if(!hasArg(camerasIndependent)){stop("camerasIndependent is not defined. It must be defined if cameraID is defined", call. = FALSE)}
    if(class(camerasIndependent) != "logical"){stop("camerasIndependent must be of class 'logical'", call. = FALSE)}
  } else { camerasIndependent <- FALSE}

  cameraCol <- "Camera"


  if(hasArg(outDir)){
    if(class(outDir) != "character"){stop("outDir must be of class 'character'", call. = FALSE)}
    if(file.exists(outDir) == FALSE) stop("outDir does not exist", call. = FALSE)
  }

  if(hasArg(exclude)){
    if(class(exclude) != "character"){stop("exclude must be of class 'character'", call. = FALSE)}
  }

  stopifnot(is.logical(removeDuplicateRecords))

  metadata.tagname <- "HierarchicalSubject"    # for extracting metadata assigned in tagging software

  if(hasArg(additionalMetadataTags)){
    if(class(additionalMetadataTags) != "character"){stop("additionalMetadataTags must be of class 'character'", call. = FALSE)}
    if(any(grep(pattern = " ", x = additionalMetadataTags, fixed = TRUE))) stop("In argument additionalMetadataTags, spaces are not allowed")
    if("HierarchicalSubject" %in% additionalMetadataTags & speciesIDfrom == "metadata")  {
      warning("'HierarchicalSubject' may not be in 'additionalMetadataTags' if speciesIDfrom = 'metadata'. It will be ignored because the function returns it anyway.", call. = FALSE)
      additionalMetadataTags <- additionalMetadataTags[-grep(pattern = "HierarchicalSubject", x = additionalMetadataTags)]  # remove it
    }
  }

  minDeltaTime <- as.integer(minDeltaTime)
  stopifnot(class(minDeltaTime) == "integer")

  if(minDeltaTime != 0){
    stopifnot(hasArg(deltaTimeComparedTo))
    stopifnot(class(deltaTimeComparedTo) == "character")
    stopifnot(deltaTimeComparedTo %in% c("lastRecord", "lastIndependentRecord"))
    if(!hasArg(deltaTimeComparedTo)) stop(paste("minDeltaTime is not 0. deltaTimeComparedTo must be defined"), call. = FALSE)
  } else {
    if(hasArg(deltaTimeComparedTo)) {warning(paste("minDeltaTime is 0. deltaTimeComparedTo = '", deltaTimeComparedTo, "' will have no effect", sep = ""), call. = FALSE, immediate. = TRUE)
    } else {
      deltaTimeComparedTo <- "lastRecord"
      }
  }


  stopifnot(class(writecsv) == "logical")

#  if(!hasArg(inDir)){stop("inDir must be defined", call. = FALSE)}
  if(class(inDir) != "character"){stop("inDir must be of class 'character'", call. = FALSE)}
  if(length(inDir) != 1){stop("inDir may only consist of 1 element only", call. = FALSE)}
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)


  # find image directories
  if(hasStationFolders == TRUE){
    dirs       <- list.dirs(inDir, full.names = TRUE,  recursive = FALSE)
    dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
  } else {
    dirs       <- inDir
    dirs_short <- inDir
  }

  record.table.list <- vector("list", length = length(dirs))

  if(hasArg(additionalMetadataTags)){
    command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject', paste(" -",additionalMetadataTags,  collapse = "", sep = ""), ' -ext JPG "', dirs, '"', sep = "")
    colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject", additionalMetadataTags)
  } else {
    command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject -ext JPG "',dirs, '"', sep = "")
    colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject")
  }

  for(i in 1:length(dirs)){   # loop through station directories

    # execute exiftool
    metadata.tmp <- runExiftool(command.tmp = command.tmp[i], colnames.tmp = colnames.tmp)

    if(class(metadata.tmp) == "NULL"){            # omit station if no images found

      length.tmp <- length(list.files(dirs[i], pattern = ".jpg$|JPG$", ignore.case = TRUE, recursive = TRUE))
      warning(paste(dirs_short[i], "contains no images;", " found", length.tmp, "JPEGs"), call. = FALSE,  immediate. = TRUE)

    } else {

      message(paste(dirs_short[i], ":", nrow(metadata.tmp), "images"))

      # check presence / consistency of DateTimeOriginal column, go to next station or remove records if necessary
      metadata.tmp <- checkDateTimeOriginal (intable    = metadata.tmp,
                                             dirs_short = dirs_short,
                                             i          = i)
      if(is.null(metadata.tmp)) next

      # now split HierarchicalSubject tags and add as columns to table
      metadata.tmp <- addMetadataAsColumns (intable                    = metadata.tmp,
                                            metadata.tagname           = metadata.tagname,
                                            metadataHierarchyDelimitor = metadataHierarchyDelimitor,
                                            multiple_tag_separator     = multiple_tag_separator)

      # add species names to metadata table (from folders or metadata, otherwise NA)
      metadata.tmp <- assignSpeciesID (intable                = metadata.tmp,
                                       IDfrom                 = speciesIDfrom,
                                       metadataSpeciesTag     = metadataSpeciesTag,
                                       speciesCol             = speciesCol,
                                       dirs_short             = dirs_short,
                                       i_tmp                  = i,
                                       multiple_tag_separator = multiple_tag_separator,
                                       speciesPosition = speciesPosition
      )

      # add individual ID to metadata table (from folders or metadata, otherwise NA) if relevant
      if(hasArg(individualIDfrom)) {
        metadata.tmp <- assignSpeciesID (intable                = metadata.tmp,            # also works for individual IDs assuming that there is 1 species only
                                         IDfrom                 = individualIDfrom,
                                         metadataSpeciesTag     = metadataIDTag,
                                         speciesCol             = individualCol,
                                         dirs_short             = dirs_short,
                                         i_tmp                  = i,
                                         multiple_tag_separator = multiple_tag_separator
        )
      }

      # if no tagged images in current station, go to next one
      if(class(metadata.tmp) != "data.frame")       next

      # remove empty metadata columns (if HierarchicalSubject is all empty or if additionalMetadataTags were not found)
      empty_cols <- which(apply(metadata.tmp, MARGIN = 2, FUN = function(X){all(X == "-")}))
      if(length(empty_cols) >= 1){
        metadata.tmp <-  metadata.tmp[, -empty_cols]
      }

      # add station and camera id to metadata table
      arg.list0 <- list(intable=metadata.tmp, dirs_short=dirs_short, stationCol=stationCol,
                        hasStationFolders=hasStationFolders, cameraCol=cameraCol,
                        i=i, IDfrom=speciesIDfrom, stationIDposition=stationIDposition)

      if(!hasArg(cameraID)) metadata.tmp <- do.call(addStationCameraID, arg.list0)
      if( hasArg(cameraID)) metadata.tmp <- do.call(addStationCameraID,
                                                    c(arg.list0, cameraID=cameraID,
                                                      cameraIDposition=cameraIDposition))

      # remove species in argument "excluded"
      if(hasArg(exclude)){
        if(any(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude))) {  # if there is anything to remove
          metadata.tmp <- metadata.tmp[-which(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude)),]
        }
      }

      if(nrow(metadata.tmp) >= 1) {   # if anything left after excluding species, do

        # convert character vector extracted from images to time object and format for outfilename
        metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata.tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = timeZone))

        # sort by (camera), species and time
        if(hasArg(individualIDfrom)) {
          if(camerasIndependent == TRUE) {
            metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,individualCol], metadata.tmp[,cameraCol], metadata.tmp$DateTimeOriginal),]
          } else {
            metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,individualCol], metadata.tmp$DateTimeOriginal),]
          }
        } else {
          if(camerasIndependent == TRUE) {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,speciesCol], metadata.tmp[,cameraCol], metadata.tmp$DateTimeOriginal),]
        } else {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,speciesCol], metadata.tmp$DateTimeOriginal),]
        }
        }

        #remove duplicate records of same species taken in same second at the same station (by the same camera, if relevant)
        metadata.tmp2 <- removeDuplicatesOfRecords(metadata.tmp           = metadata.tmp,
                                                  removeDuplicateRecords = removeDuplicateRecords,
                                                  camerasIndependent     = camerasIndependent,
                                                  stationCol             = stationCol,
                                                  speciesCol             = speciesCol,
                                                  cameraCol              = cameraCol)

        # extact info from path if relevant
        if(hasArg(directoryInfoPositions))  {
          dirPath <- parseDir(metadata.tmp2, directoryInfoPositions = directoryInfoPositions)
          names(dirPath) <- directoryInfoNames
          metadata.tmp2 <- cbind(metadata.tmp2, dirPath)
        }

        if(hasArg(countsName))  {
        metadata.tmp2[, countsName] <- as.numeric(metadata.tmp2[, countsName])
        }

        # assess independence between records and calculate time differences
        record.table.list[[i]] <- assessTemporalIndependence(intable= metadata.tmp2,
                                          deltaTimeComparedTo = deltaTimeComparedTo,
                                          camerasIndependent  = camerasIndependent,
                                          columnOfInterest    = if(hasArg(individualIDfrom)) individualCol else speciesCol,
                                          cameraCol           = cameraCol,
                                          minDeltaTime        = minDeltaTime,
                                          stationCol          = stationCol,
                                          countsName          = countsName)
      }  # end      if(nrow(metadata.tmp) >= 1){} else {...}   # i.e. not all species were excluded
    }    # end      if(nrow(metadata.tmp) == 0){} else {...}   # i.e. directory i contained images
  }      # end      for(i in 1:length(dirs)){   # loop through station directories

  record.table <- rbindlist(record.table.list, use.names=TRUE, fill=TRUE)
  if(nrow(record.table) == 0){
    stop(paste("something went wrong. I looked through all those", length(dirs)  ,"folders and now your table is empty. Did you exclude too many species? Or were date/time information not readable?"), call. = FALSE)
  }

  record.table[, Date := as.Date(DateTimeOriginal, format = "%Y/%M/%d", tz = timeZone)]
  record.table[, Time := strftime(DateTimeOriginal, format = "%H:%M:%S", tz = timeZone)]

  # compute delta time in hours and days
  record.table[, delta.time.secs := as.numeric(round(delta.time.secs, digits = 0))]
  record.table[, delta.time.mins := round(delta.time.secs / 60, digits = 0)]
  record.table[, delta.time.hours := round(delta.time.mins  / 60, digits = 1)]
  record.table[, delta.time.days  := round(delta.time.hours / 24, digits = 1)]
  record.table[, independent := NULL]
  record.table[, rn := NULL]

  # warning if additionalMetadataTags were not found
  if(hasArg(additionalMetadataTags)){
    additionalMetadataTagsNew <- gsub(additionalMetadataTags, pattern = ":", replacement = ".")
    whichadditionalMetadataTagswithcol <- which(additionalMetadataTags %in% colnames(record.table))
    if(length(whichadditionalMetadataTagswithcol) > 0)
      setnames(record.table, additionalMetadataTags[whichadditionalMetadataTagswithcol],
               additionalMetadataTagsNew[whichadditionalMetadataTagswithcol])

    whichadditionalMetadataTagsFound <- which(additionalMetadataTagsNew %in% colnames(record.table))   # replace : in additionalMetadataTags (if specifying tag groups) with . as found in column names
    if(length(whichadditionalMetadataTagsFound) < length(additionalMetadataTagsNew)){
      if(length(whichadditionalMetadataTagsFound) == 0) {  # if none of the additionalMetadataTags was found
        warning(paste("metadata tag(s)  not found in image metadata:  ", paste(additionalMetadataTags, collapse = ", ")), call. = FALSE)
        } else {                                                            # if only some of the additionalMetadataTags was found
        warning(paste("metadata tag(s)  not found in image metadata:  ", paste(additionalMetadataTags[-whichadditionalMetadataTagsFound], collapse = ", ")), call. = FALSE)
      }
    }
  }

  # set columns order
  new.order <- c(stationCol, if(hasArg(cameraID)) cameraCol, speciesCol,
                 if(hasArg(individualIDfrom)) individualCol,
                 "Directory", "FileName", "DateTimeOriginal", "Date", "Time", "delta.time.secs",
                 "delta.time.mins", "delta.time.hours", "delta.time.days",
                 if(hasArg(countsName)) countsName,
                 if(hasArg(additionalMetadataTags))
                   if("HierarchicalSubject" %in% names(record.table)) "HierarchicalSubject")
  metadata.cols <- names(record.table)[-which(names(record.table) %in% new.order)]

  setcolorder(record.table,
              if(hasArg(additionalMetadataTags)) {
                if("HierarchicalSubject" %in% names(record.table)) {
                  c(head(new.order, -1), metadata.cols, "HierarchicalSubject")
                } else {
                  c(new.order, metadata.cols)
                }
              } else {
                new.order
              })

  # Sort by station, species and camera if relevant
  setkeyv(record.table, c(stationCol, speciesCol,
                          if(camerasIndependent) cameraCol, "DateTimeOriginal"))

  # save table
  if(writecsv == TRUE){
    outtable_filename <- paste("record_table_", if(hasArg(individualIDfrom)) "individuals",
                               minDeltaTime, "min_deltaT_", Sys.Date(), ".csv", sep = "")
    if(hasArg(outDir) == FALSE){
      setwd(inDir)
    } else {
      setwd(outDir)
    }
  write.csv(record.table, file = outtable_filename)
  }
  return(record.table)
}
