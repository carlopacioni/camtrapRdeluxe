#'Generate a record table of species detections from camera trap images
#'
#'Generates a record table from camera trap images and user defined metadata
#'tags and/or directory folders, or spreadsheet data, and can also report
#'maximum abundance counts within independent event intervals.
#'
#'The \code{recordTable} function generates a record table from camera trap
#'images. Images must be sorted into station directories at least. A station is
#'typically considered the location of a single camera, or the location of
#'multiple, possibly non-independent, cameras (e.g. two cameras opposite each
#'other on road).
#'
#'The function can handle a number of different ways of storing images, and
#'supports sorting/describing of images by moving images into directory folders,
#'as well as metadata tagging. In every case, images need to be stored into
#'station directories (i.e.folders). If images are sorted by moving them into
#'species directories, abundance counts directories, etc., a camera directory is
#'optional (e.g. "Station/Species/XY.JPG" or
#'"Station/Camera/Species/Counts/XY.JPG"). Likewise, if images are identified
#'using metadata tagging, a camera directory can be used optionally (e.g.
#'"Station/XY.JPG" or "Station/Camera/XY.JPG").
#'
#'The function can read mutliple descriptors, such as species identification,
#'abundance counts, etc., from the directory structure (e.g.
#'"Station/Species/XY.JPG" or "Station/Camera/Species/Counts/.../XY.JPG") or
#'from image metadata tags. For the sake of workflow efficiency, it is suggested
#'that metadata tagging, using an appropriate image management software, be used
#'if multiple descriptors beyond species level are to be implemented. Camera
#'metadata, as well as additional image tags, can also be retrieved and included
#'in the generated record table.
#'
#'Users can use mutiple directory levels before station level (e.g. site, survey
#'period, etc.) and after station or species level directories (e.g. species,
#'abundance, sex, age, behaviour, etc.). However, as mentioned previously, for
#'efficiency sake it is advised that metadata tagging be used instead of
#'multiple directories where the need for copying and pasting of images into
#'several descriptive folders is increased.
#'
#'This function allows for multiple directory folders, including abundance, and
#'enables the determination of relative abundance indices by calculating
#'independent events using the abundance counts information. The following
#'arguments allow users to create columns populated with information contained
#'within directories/folders in the filepath/directory structure above station
#'level and below species level folders: \code{stationIDposition},
#'\code{speciesPosition}, \code{cameraIDposition},
#'\code{directoryInfoPositions}, \code{directoryInfoNames}, \code{countsName}.
#'
#'Use the following commands to display the directory folder names and their
#'level / numeric position within the filepath: ldir <- list.dirs(dir.in), or
#'folderIndices(inDir), or by counting the position of the desired directory
#'folder after the name of the storage drive within the filepath (e.g.
#'"C:/Documents/CamProject/Region/Site/Transect/Station/Species/...", the
#''Station' directory ID position = 6).
#'
#'The arguments \code{IDfrom} and \code{cameraID} will present an 'argument
#'deprecated' warning if used and are only included to allow backwards
#'compatability for users of the previous iteration of camtrapR. These arguments
#'have been replaced by \code{speciesIDfrom} and \code{cameraIDfrom}.
#'
#'If images are identified by metadata tagging, \code{metadataSpeciesTag}
#'specifies the metadata tag group name that contains species identification
#'tags. \code{metadataHierarchyDelimitor} is "|" for images tagged in DigiKam
#'and images tagged in Adobe Bridge / Lightroom with the default settings. It is
#'only necessary to change it if the default was changed in these programs.
#'
#'\code{minDeltaTime} is a criterion for temporal independence of species
#'recorded at the same station. Setting it to 0 will make the function return
#'all records. Setting it to a higher value will remove records that were taken
#'less than \code{minDeltaTime} minutes after the last record
#'(\code{deltaTimeComparedTo = "lastRecord"}) or the last independent record
#'(\code{deltaTimeComparedTo = "lastIndependentRecord"}). For two records to be
#'considered independent, the second record must be at least \code{minDeltaTime}
#'minutes after the last independent record of the same species
#'(\code{"lastIndependentRecord"}), or \code{minDeltaTime} minutes after the
#'last record (\code{"lastRecord"}). For example, if a sequence of records of
#'the same species from the same station were observed, with a difference in
#'time between each record being 0 (first image), 5, 10, 7, 25 and 40 minutes
#'and \code{minDeltaTime} defined as 30 minutes, \code{"lastIndependentRecord"}
#'would return 3 (0, 25 and 40) independent events due to the cumulative
#'calculation of time difference between earlier and later records, whereas
#'\code{"lastRecord"} would return 2 (0 and 40) independent events because this
#'argument simply uses the time difference between successive records.
#'
#'\code{camerasIndependent} defines if the cameras at a station are to be
#'considered independent. If \code{TRUE}, records of the same species taken by
#'different cameras are considered independent (e.g. if they face different
#'trails). Use \code{FALSE} if both cameras face each other and possibly
#'\code{TRUE} ).
#'
#'\code{exclude} can be used to exclude "species" directories containing
#'irrelevant images (e.g. c("team", "blank", "unidentified")). \code{stationCol}
#'can be set to match the station column name in the camera trap station table
#'(see \code{\link{camtraps}}).
#'
#'Many digital images contain Exif metadata tags such as "AmbientTemperature" or
#'"MoonPhase" that can be extracted if specified in \code{metadataTags}. Because
#'these are manufacturer-specific and not standardized, function
#'\code{\link{exifTagNames}} provides a vector of all available tag names.
#'Multiple names can be specified as a character vector as: \code{c(Tag1, Tag2,
#'...)}. The metadata tags thus extracted may be used as covariates in modelling
#'species distributions.
#'
#'@param inDir character. Directory filepath containing station directories. It
#'  must either contain images in species subdirectories (e.g.
#'  "inDir/StationA/SpeciesA") or images with species metadata tags instead of
#'  species directories (e.g. "inDir/StationA"). Only include path up to
#'  root/parent folder in which the station/site subfolders are located (e.g.
#'  "C:/Documents/CamProject" ...). Station folders would be next in the file
#'  path but are not included when defining \code{inDir}.
#'
#'@param IDfrom This argument is deprecated. Use \code{speciesIDfrom}.
#'
#'@param cameraID This argument is deprecated. Use \code{cameraIDfrom}.
#'
#'@param StationIDfrom character. Read station ID from image "filename" or
#'  station "directory" names. "filename" requires images renamed with
#'  \code{\link{imageRename}}.
#'
#'@param speciesIDfrom character. Read species ID from image "metadata" or from
#'  species "directory" names.
#'
#'@param cameraIDfrom character. Read camera ID from image "filename" or camera
#'  "directory" names. "filename" requires images renamed with
#'  \code{\link{imageRename}}. "directory" requires a camera subdirectory within
#'  station directories (e.g. "Station/Camera/...").
#'
#'@param camerasIndependent logical. If \code{TRUE}, species records are
#'  considered independent between cameras at same station.
#'
#'@param exclude character. Vector of species names to be excluded from the
#'  record table (e.g. c("Dog", "Cat", ...)).
#'
#'@param minDeltaTime integer. The minimum specified time difference (in
#'  minutes) between records of the same species, at the same station, that are
#'  to be considered independent.
#'
#'@param deltaTimeComparedTo character. For two records to be considered
#'  independent, the second record must be at least \code{minDeltaTime} minutes
#'  after the last independent record of the same species
#'  (\code{"lastIndependentRecord"}), or \code{minDeltaTime} minutes after the
#'  last record (\code{"lastRecord"}).
#'
#'@param timeZone character. Must be an argument of
#'  \code{\link[base]{OlsonNames}}.
#'
#'@param stationCol character. Name of the camera trap station column. Assumes
#'  "Station" if undefined.
#'
#'@param writecsv logical. If \code{TRUE}, a .csv file of the record table will
#'  be saved.
#'
#'@param outDir character. Directory to save .csv file to. If NULL and
#'  \code{writecsv = TRUE}, recordTable will be written to \code{inDir}.
#'
#'@param metadataHierarchyDelimitor character. The character (either "|" or ":")
#'  that delimits hierarchy levels of image metadata tags contained in the image
#'  management software field called "HierarchicalSubject".
#'
#'@param metadataSpeciesTag character. In custom image metadata, the species ID
#'  tag name. Only one species name can be provided (e.g. "dog").
#'
#'@param additionalMetadataTags character. Additional camera model-specific
#'  metadata tags to be extracted. If possible, specify tag groups as returned
#'  by \code{\link{exifTagNames}}, (e.g.
#'  c("MakerNotes:AmbientTemperature","MakerNotes:MoonPhase").
#'
#'@param removeDuplicateRecords logical. If \code{TRUE}, only one record will be
#'  shown in the record table when there are several records of the same species
#'  at the same station (also same camera if cameraID is defined) at exactly the
#'  same time.
#'
#'@param stationIDposition integer. The numeric position within the
#'  filepath/directory structure of the 'Station' directories (e.g.
#'  "C:/Documents/CamProject/Region/Site/Transect/Station/Camera/Species/...",
#'  the 'Station' directory position = 6).Only need to use
#'  this argument when information from one or more directories (i.e. folders)
#'  before the 'Station' level directory are to be used to populate columns
#'  within the record table (e.g.
#'  "SurveyPeriod/Region/Site/Transect/Station/..."). If this argument is
#'  missing, the function assumes the station directory is next in the filepath
#'  as specified in \code{inDir}.
#'
#'@param speciesPosition integer. The numeric position within the
#'  filepath/directory structure of the 'Species' directories (e.g.
#'  "C:/Documents/CamProject/Region/Site/Transect/Station/Camera/Species/...",
#'  the species directory position = 8). Only need to use this argument when
#'  \code{speciesIDfrom} = "directory" and there are one or more directories
#'  (i.e. folders) after the 'Species' level directory (e.g.
#'  "SurveyPeriod/Region/Site/Transect/Station/Species/Counts/...").
#'
#'@param cameraIDposition integer. The numeric position of the 'Camera'
#'  directories within the filepath/directory structure (e.g.
#'  "C:/Documents/CamProject/Region/Site/Transect/Station/Camera/Species,...",
#'  the camera ID directory position = 7). Only need to use this argument when
#'  \code{cameraIDfrom} = "directory" and there are one or more directories
#'  (i.e. folders) after the 'Species' level directory (e.g.
#'  "SurveyPeriod/Region/Site/Transect/Station/Species/Counts/...").
#'
#'@param directoryInfoPositions integer. Vector of the numeric positions within
#'  the filepath/directory structure of the directories containing extra
#'  information that the user wishes to have itemised in columns within the
#'  record table. Can take a vector of length >1. This argument must be used if
#'  the directory filepath contains extra directories beyond 'Species' (e.g. the
#'  directories "Site", "Transect" and "Counts" within the filepath of
#'  "C:/Documents/CamProject/Site/Transect/Station/Camera/Species/Counts/...",
#'  would be specified as c(3:4, 8)).
#'
#'@param directoryInfoNames character. The names of the directories for the
#'  additional information coming from the directory as specified in
#'  \code{directoryInfoPositions}. If the length is >1, then the names need to
#'  be concatonated and listed in the same order as the numbers specified in
#'  \code{directoryInfoPositions} (e.g. the \code{directoryInfoPositions} of
#'  c(3:4, 8) within the filepath of
#'  "C:/Documents/CamProject/Site/Transect/Station/Camera/Species/Counts/...",
#'  would be specified as c("Site", "Transect","Counts")). This argument must be
#'  used if the directory filepath contains extra directories beyond 'Species'.
#'
#'@param countsName character. Vector of the metadata tag name (as per image
#'  management/tagging software) containing count/abundance information, or the
#'  vector of the name given to the count/abundance folder position within
#'  \code{directoryInfoNames} (e.g. "Counts").
#'
#'@return A data frame containing species records and additional information
#'  about stations, date, time and (optionally) relative abundance and further
#'  metadata.
#'
#'@section {Warning }{Custom image metadata must be organised hierarchically
#'  (tag group - tag; e.g. "Species" - "Leopard Cat"). Detailed information on
#'  how to set up and use metadata tags can be found in
#'  \href{https://CRAN.R-project.org/package=camtrapR/vignettes/SpeciesIndividualIdentification.html#metadata-tagging}{vignette
#'   2: Species and Individual Identification}.
#'
#'  Custom image metadata tags must be written to the images. The function
#'  cannot read tags from .xmp sidecar files. Make sure you set the preferences
#'  accordingly. In DigiKam, go to Settings/Configure digiKam/Metadata. There,
#'  make sure "Write to sidecar files" is unchecked.
#'
#'  Please note the section about defining argument \code{timeZone} in the
#'  vignette on data extraction (accessible via
#'  \code{vignette("DataExtraction")} or online
#'  (\url{https://cran.r-project.org/package=camtrapR/vignettes/DataExtraction.html})).
#'   }
#'
#'@note The results of a number of other functions will depend on the output of
#'  this function (namely on the arguments exclude for excluding species and
#'  minDeltaTime/ deltaTimeComparedTo for temporal independence):
#'
#'  \tabular{l}{ \code{\link{detectionMaps}} \cr \code{\link{detectionHistory}}
#'  \cr \code{\link{activityHistogram}} \cr \code{\link{activityDensity}} \cr
#'  \code{\link{activityRadial}} \cr \code{\link{activityOverlap}} \cr
#'  \code{\link{activityHistogram}} \cr \code{\link{surveyReport}} \cr }
#'
#'@author Juergen Niedbella, Luke Emerson, Carlo Pacioni
#'
#'@references Phil Harvey's ExifTool
#'  http://www.sno.phy.queensu.ca/~phil/exiftool/
#'
#'@examples {
#'wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
#'
#'if (Sys.which("exiftool") != ""){        # only run these examples if ExifTool is available
#'
#'
#'  rec.db1 <- recordTable(inDir                  = wd_images_ID,
#'                         IDfrom                 = "directory",
#'                         minDeltaTime           = 60,
#'                         deltaTimeComparedTo    = "lastRecord",
#'                         writecsv               = FALSE,
#'                         additionalMetadataTags = c("EXIF:Model", "EXIF:Make")
#'  )
#'  # note argument additionalMetadataTags: it contains tag names as returned by function exifTagNames
#'
#'  rec.db2 <- recordTable(inDir                  = wd_images_ID,
#'                         IDfrom                 = "directory",
#'                         minDeltaTime           = 60,
#'                         deltaTimeComparedTo    = "lastRecord",
#'                         exclude                = "NO_ID",
#'                         writecsv               = FALSE,
#'                         timeZone               = "Asia/Kuala_Lumpur",
#'                         additionalMetadataTags = c("EXIF:Model", "EXIF:Make", "NonExistingTag")
#'  )
#'  # note the warning that the last tag in "additionalMetadataTags" was not found
#'
#'
#'  any(rec.db1$Species == "NO_ID")
#'  any(rec.db2$Species == "NO_ID")
#'
#'
#'  #############
#'  # here's how the removeDuplicateRecords argument works
#'
#'  \dontrun{   # this is because otherwise the test would run too long to pass CRAN tests
#'
#'    rec.db3a <- recordTable(inDir                 = wd_images_ID,
#'                            IDfrom                 = "directory",
#'                            minDeltaTime           = 0,
#'                            exclude                = "NO_ID",
#'                            timeZone               = "Asia/Kuala_Lumpur",
#'                            removeDuplicateRecords = FALSE
#'    )
#'
#'    rec.db3b <- recordTable(inDir                 = wd_images_ID,
#'                            IDfrom                 = "directory",
#'                            minDeltaTime           = 0,
#'                            exclude                = "NO_ID",
#'                            timeZone               = "Asia/Kuala_Lumpur",
#'                            removeDuplicateRecords = TRUE
#'    )
#'
#'
#'    anyDuplicated(rec.db3a[, c("Station", "Species", "DateTimeOriginal")])   # got duplicates
#'    anyDuplicated(rec.db3b[, c("Station", "Species", "DateTimeOriginal")])   # no duplicates
#'
#'    # after removing duplicates, both are identical:
#'    whichAreDuplicated <- which(duplicated(rec.db3a[, c("Station", "Species", "DateTimeOriginal")]))
#'    all(rec.db3a[-whichAreDuplicated,] == rec.db3b)
#'
#'  }
#'
#'} else {                                # show function output if ExifTool is not available
#'  message("ExifTool is not available. Cannot test function")
#'  data(recordTableSample)
#'}
#'
#'}
#'
#'@export
recordTable <- function(inDir,
                        IDfrom,
                        cameraID,
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
                        stationIDposition = NULL,
                        speciesPosition = NULL,
                        cameraIDposition = NULL,
                        directoryInfoPositions,
                        directoryInfoNames,
                        countsName)

{  if (!missing(IDfrom)) {
    warning("argument IDfrom is deprecated; please use speciesIDfrom instead.",
            call. = FALSE)
    speciesIDfrom = IDfrom}

  if (!missing(cameraID)) {
    warning("argument cameraID is deprecated; please use cameraIDfrom instead.",
            call. = FALSE)
    cameraIDfrom = cameraID}

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
                             stationIDposition=stationIDposition,
                             speciesPosition=speciesPosition,
                             cameraIDposition=cameraIDposition,
                             directoryInfoPositions=directoryInfoPositions,
                             directoryInfoNames=directoryInfoNames,
                             countsName=countsName)

  return(recTable)}
