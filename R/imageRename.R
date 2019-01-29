#'Copy and rename images based on camera trap station ID and creation date
#'
#'The function renames and copies raw camera trap images into a new location
#'where they can be identified. Images are renamed with camera trap station ID,
#'camera ID (optional), creation date and a numeric identifier for images taken
#'within one minute of each other at a given station. Station ID and camera ID
#'are derived from the raw image directory structure. The creation date is
#'extracted from image metadata using ExifTool.
#'
#'Setting up the correct raw image directory structure is necessary for running
#'the function successfully. \code{inDir} is the main directory that contains
#'camera trap station subdirectories (e.g. inDir/StationA). If one camera was
#'deployed per station and no camera subdirectories are used within station
#'directories, \code{hasCameraFolders} can be set to \code{FALSE}. If more than
#'one camera was deployed at stations, there must be subdirectories for the
#'individual camera traps within the station directories (e.g.
#'"inDir/StationA/CameraA1" and "inDir/StationA/CameraA2"). Even if only some
#'stations had multiple cameras, all station will need camera subdirectories.
#'The argument \code{hasCameraFolders} must be \code{TRUE}. Within the camera
#'subdirectories, the directory structure is irrelevant.
#'
#'Renaming of images follows the following pattern: If \code{hasCameraFolders}
#'is TRUE, it is: "StationID__CameraID__Date__Time(Number).JPG", e.g.
#'"StationA__CameraA1__2015-01-31__18-59-59(1).JPG". If \code{hasCameraFolders}
#'is FALSE, it is:  "StationID__Date__Time(Number).JPG", e.g.
#'"StationA__2015-01-31__18-59-59(1).JPG".
#'
#'The purpose of the number in parentheses is to prevent assigning identical
#'file names to images taken at the same station (and camera) in the same
#'second, as can happen if cameras take sequences of images. It is a consecutive
#'number given to all images taken at the same station by the same camera within
#'one minute. The double underscore "__" in the image file names is for
#'splitting and extracting information from file names in other functions (e.g.
#'for retrieving camera IDs in \code{\link{recordTable}} if camera
#'subdirectories are not preserved (\code{keepCameraSubfolders = FALSE})).
#'
#'The function finds all JPEG images and extracts the image timestamp from the
#'image metadata using ExifTool and copies the images (with new file names) into
#'\code{outDir}, where it will set up a directory structure based on the station
#'IDs and, if required by \code{keepCameraSubfolders = TRUE}, camera IDs (e.g.
#'outDir/StationA/ or outDir/StationA/CameraA1).
#'
#'\code{copyImages} can be set to FALSE to simulate the renaming and check the
#'file names of the renamed images without copying. If you are handling large
#'number of images (>e.g., 100,000), the function may take some time to run.
#'
#'\strong{NOTE}: This function assumes that camera folders, if present, are
#'within the station folders.
#'@inheritParams recordTable
#'@param hasCameraFolders Do the station directories in \code{inDir} have camera
#'  subdirectories (e.g. "inDir/StationA/Camera1")?
#'@param keepCameraSubfolders logical. Should camera directories be preserved as
#'  subdirectories of \code{outDir} (e.g. "outDir/StationA/CameraA1")?
#'@param createEmptyDirectories  logical. If station or camera directories are
#'  empty, should they be copied nevertheless (causing empty directories in
#'  \code{inDir}, but preserving the whole directory structure)?
#'@param copyImages logical. Copy images to \code{outDir}?
#'@param writecsv logical. Save a data frame with a summary as a .csv?
#'@return A \code{data.frame} with original directory and file names, new
#'  directory and file names and an indicator for whether images were copied
#'  successfully.
#'@references Phil Harvey's ExifTool
#'  \url{http://www.sno.phy.queensu.ca/~phil/exiftool/ }
#'  @export
#' @examples
#'
#' ### "trial" run. create a table with file names after renaming, but don't copy images.
#'
#' # first, find sample image directory in package directory:
#' wd_images_raw <- system.file("pictures/raw_images", package = "camtrapRdeluxe")
#'
#'if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available
#'
#'  # because copyImages = FALSE, outDir does not need to be defined
#'  renaming.table <- imageRename(inDir               = wd_images_raw,
#'                                hasCameraFolders = FALSE,
#'                                copyImages          = FALSE,
#'                                writecsv            = FALSE
#'  )
#'} else {
#'  message("ExifTool is not available. Cannot test function")
#'}
#'
#'\dontrun{
#'
#'  # define image directories
#'
#'  # raw image location
#'  wd_images_raw <- system.file("pictures/raw_images", package = "camtrapRdeluxe")
#'  # destination for renamed images
#'  wd_images_raw_renamed <- file.path(tempdir(), "raw_images_renamed")
#'
#'
#'  if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available
#'
#'    # now we have to set outDir because copyImages = TRUE
#'    renaming.table2 <- imageRename(inDir               = wd_images_raw,
#'                                   outDir              = wd_images_raw_renamed,
#'                                   hasCameraFolders    = FALSE,
#'                                   copyImages          = TRUE,
#'                                   writecsv            = FALSE
#'    )
#'  }
#'
#'  list.files(wd_images_raw_renamed, recursive = TRUE)
#'
#'}
#'}
imageRename <- function(inDir,
                        outDir,
                        hasCameraFolders,
                        keepCameraSubfolders,
                        stationIDposition = NULL,
                        cameraIDposition = NULL,
                        createEmptyDirectories = FALSE,
                        copyImages = FALSE,
                        writecsv = FALSE) {

  wd0 <- getwd()
  on.exit(setwd(wd0))

  file.sep <- .Platform$file.sep

  stationCol <- "Station"
  cameraCol  <- "Camera"

  # check call for consistency
  stopifnot(is.logical(copyImages))
  stopifnot(is.logical(writecsv))
  stopifnot(is.logical(hasCameraFolders))

    if(isTRUE(hasCameraFolders)){
      stopifnot(hasArg(keepCameraSubfolders))
      stopifnot(is.logical(keepCameraSubfolders))
   } else {
    keepCameraSubfolders <- FALSE
   }
   if(hasArg(hasCameraFolders) & hasArg(keepCameraSubfolders)){
    if(keepCameraSubfolders == TRUE & hasCameraFolders == FALSE){
      stop("If hasCameraFolders is FALSE, keepCameraSubfolders must be FALSE too", call. = FALSE)
      }
   }


  stopifnot(length(inDir) == 1)
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)

  if(hasArg(outDir)){
    stopifnot(length(outDir) == 1)
    if(isTRUE(all(unlist(strsplit(tolower(inDir), split = file.sep)) %in%
                  unlist(strsplit(tolower(outDir), split = file.sep)))))
      stop("outDir may not be identical to or a subdirectory of inDir", call. = FALSE)
    }
  if(copyImages == TRUE){

    if(any(c(grep("/$", inDir) == 1, grep("/$", outDir) == 1))) stop("inDir and outDir may not end with /", call. = FALSE)

    # Ensure all paths use the file.sep
    inDir <- gsub(pattern = "[\\]", replacement = file.sep, x = inDir)
    outDir <- gsub(pattern = "[\\]", replacement = file.sep, x = outDir)

  } else {
    if(isTRUE(grepl("/$", inDir))) stop("inDir may not end with /", call. = FALSE)
  }
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)
  if(isTRUE(writecsv) & hasArg(outDir) == FALSE) stop("writecsv is TRUE. Please specify outDir", call. = FALSE)




  # list of station directories
  if(is.null(stationIDposition)) {
    dirs <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
    dirs_short <- list.dirs(inDir, full.names = FALSE , recursive = FALSE)
  } else {
    dirs_all <- list.dirs(inDir, full.names = TRUE, recursive = TRUE)
    l<-strsplit(dirs_all, split = file.sep, fixed = TRUE)
    slength <- sapply(l, length)
    dirs <- dirs_all[slength == stationIDposition]
    dirs_short <- basename(dirs)
  }

  # make sure none is empty
  list_n_files <- lapply(dirs, list.files, pattern = ".jpg$|.JPG$", recursive = TRUE)

  if(any(unlist(lapply(list_n_files, length)) == 0)){
    warning("at least one station directory contains no JPEGs:  ", paste(dirs_short[which(lapply(list_n_files, length) == 0)], collapse = "; "), call. = FALSE, immediate. = TRUE)
  }

  # remove dirs and dirs_short if they contain no images (and if user overrides default createEmptyDirectories = FALSE)
  if(!isTRUE(createEmptyDirectories)){
    stations2remove  <- which(lapply(list_n_files, length) == 0)
    if(length(stations2remove) >= 1){
      dirs          <- dirs[-stations2remove]
      dirs_short  <- dirs_short[-stations2remove]
    }
  }

  # function body

  copy.info.table <- data.frame()

  for(i in 1:length(dirs_short)){     # for all non-empty directories

    # check if there are any images not in camera trap subfolders
    if(hasCameraFolders == TRUE && length(list.files(dirs[i], pattern = ".jpg$|.JPG$", recursive = FALSE)) >= 1){
      stop(paste("Directory ", dirs[i], " contains images not sorted into Camera Trap subfolders. Check argument 'hasCameraFolders'"), call. = FALSE)
    }

    command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -ext JPG "', dirs[i], '"', sep = "")
    colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal")

    # run exiftool to get image date and time
    metadata.tmp <- runExiftool(command.tmp = command.tmp, colnames.tmp = colnames.tmp)


    if(length(metadata.tmp) == 0){
      length.tmp <- length(list.files(dirs[i], pattern = ".jpg$|JPG$", ignore.case = TRUE, recursive = TRUE))
      warning(paste(dirs_short[i], "seems to contain no images;", " found", length.tmp, "jpgs"), call. = FALSE, immediate. = TRUE)    # give message if station directory contains no jpgs
    } else {

      message(paste(dirs_short[i], ":", nrow(metadata.tmp), "images"))

      if(isTRUE(hasCameraFolders)){
        if(is.null(cameraIDposition)) {
          cam_dirs <- list.dirs(dirs[i], full.names =TRUE, recursive = FALSE)
          cam_dirs_short <- list.dirs(dirs[i], full.names = FALSE, recursive = FALSE)
        } else {
          # Identify camera folders if position is passed
          subfolders_all <- list.dirs(dirs[i], full.names = TRUE, recursive = TRUE)
          l<-strsplit(subfolders_all, split = file.sep, fixed = TRUE)
          slength <- sapply(l, length)
          cam_dirs <- subfolders_all[slength == cameraIDposition]
          cam_dirs_short <- basename(cam_dirs)
        }
        filenames_by_subfolder <- lapply(as.list(cam_dirs),
                                         FUN = list.files, pattern = ".jpg$|.JPG$", recursive = TRUE, ignore.case = TRUE)
        metadata.tmp$CameraID <- rep(cam_dirs_short,
                                     times = unlist(lapply(filenames_by_subfolder, length)))
        metadata.tmp$Station <- rep(dirs_short[i], times = nrow(metadata.tmp))
        colnames(metadata.tmp)[grep("CameraID", colnames(metadata.tmp))] <- cameraCol
        colnames(metadata.tmp)[grep("Station", colnames(metadata.tmp))] <- stationCol
      } else {
        metadata.tmp$CameraID <- "NA"                                                    # "camera" name if no camera id available (only station ids)
        metadata.tmp$Station <- rep(dirs_short[i], times = nrow(metadata.tmp))
        colnames(metadata.tmp)[grep("CameraID", colnames(metadata.tmp))] <- cameraCol
        colnames(metadata.tmp)[grep("Station", colnames(metadata.tmp))] <- stationCol
      }

      # make time readable
      metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = as.character(metadata.tmp$DateTimeOriginal),
                                                           format = "%Y:%m:%d %H:%M:%S", tz = "UTC"))
      metadata.tmp$DateTimeOriginal2 <- as.POSIXlt(metadata.tmp$DateTimeOriginal)

      # exclude images for which no DateTimeOriginal was found
      suppressWarnings(na.date.rows <- which(is.na(metadata.tmp$DateTimeOriginal)))
      if(length(na.date.rows) != 0){
        #metadata.tmp.na.date <- metadata.tmp[na.date.rows,]
        warning(paste("could not read DateTimeOriginal tag of: \n",
                    paste(paste(metadata.tmp$Directory,  metadata.tmp$FileName, sep = file.sep)[na.date.rows], collapse = "\n")),
                call. = FALSE, immediate. = TRUE)
        metadata.tmp <- data.frame(metadata.tmp, DateReadable = NA)
        metadata.tmp$DateReadable[-na.date.rows] <- TRUE
        metadata.tmp$DateReadable[na.date.rows]  <- FALSE
      } else {
        metadata.tmp$DateReadable <- TRUE
      }
      rm(na.date.rows)

      # rearrange column order
      metadata.tmp <- metadata.tmp[,c("Directory",  "FileName", stationCol, cameraCol,
                                      "DateTimeOriginal", "DateTimeOriginal2", "DateReadable")]

      # find images taken within 1 minute of one another (to append number)
      metadata.tmp$DateTimeOriginal2$sec <- 0

      if(isTRUE(hasCameraFolders)){
        metadata.tmp.split <- split(x = metadata.tmp, f = list(metadata.tmp[,cameraCol],
                                                               as.POSIXct(metadata.tmp$DateTimeOriginal2)
        ), drop = TRUE)
      } else {
        metadata.tmp.split <- split(x = metadata.tmp, f = list(metadata.tmp[,stationCol],
                                                               as.POSIXct(metadata.tmp$DateTimeOriginal2)
        ), drop = TRUE)
      }

      metadata.tmp.split2 <- lapply(metadata.tmp.split, FUN = function(X){
        X2 <- X[with(X, order(DateTimeOriginal)), ]
        cbind(X2, minute.append = seq(from = 1, to = nrow(X2), by = 1))
      })

      metadata.tmp2 <- do.call("rbind", metadata.tmp.split2)    # reassemble
      if(any(metadata.tmp$DateReadable == FALSE)){
        metadata.tmp2 <- rbind(cbind(metadata.tmp[metadata.tmp$DateReadable == FALSE,], minute.append = NA) ,metadata.tmp2)
      }
      rm(metadata.tmp.split, metadata.tmp.split2)

      # convert time object to character vector and format for outfilename
      time.tmp <- gsub(pattern = ":", replacement = "-", metadata.tmp2$DateTimeOriginal)
      time.tmp2 <- gsub(pattern = " ", replacement = "__", time.tmp)
      metadata.tmp2$DateTime_for_filename <- time.tmp2
      rm(time.tmp, time.tmp2)

      # create outDir and filename_new
      if(hasArg(outDir)){
        metadata.tmp2$outDir <- sub(pattern=inDir, replacement=outDir, x=metadata.tmp2$Directory)

        if(keepCameraSubfolders == FALSE){
          metadata.tmp2$outDir <- mapply(FUN=sub, pattern=metadata.tmp2[, cameraCol],
                                        x=metadata.tmp2$outDir,
                                        MoreArgs = list(replacement=""))
          metadata.tmp2$outDir <- sub(pattern = "//", replacement = "/", x = metadata.tmp2$outDir)
        }
      }

      if(isTRUE(hasCameraFolders)){
        metadata.tmp2$filename_new <- paste(paste(metadata.tmp2[,stationCol],
                                                  metadata.tmp2[,cameraCol],
                                                  paste(metadata.tmp2$DateTime_for_filename, "(",metadata.tmp2$minute.append, ")", sep = ""),
                                                  sep = "__"),
                                            ".JPG", sep = "")
      } else {
        metadata.tmp2$filename_new <- paste(paste(metadata.tmp2[,stationCol],
                                                  paste(metadata.tmp2$DateTime_for_filename, "(",metadata.tmp2$minute.append, ")", sep = ""),
                                                  sep = "__"),
                                            ".JPG", sep = "")
      }

      copy.info.table <- rbind(copy.info.table, metadata.tmp2)
      rm(metadata.tmp2)
    }
  }

  if(!any(copy.info.table$DateReadable)) stop("could not read DateTimeOriginal tag of any image. Check if the DateTimeOriginal tag is present in metadata with exifTagNames(..., returnMetadata = TRUE). If not, try fixing it with fixDateTimeOriginal()",
                                                      call. = FALSE)

    # create directory structure in outDir
      if(isTRUE(copyImages)){
        if(isTRUE(createEmptyDirectories)) {
          dirs_all <- list.dirs(inDir, full.names = TRUE, recursive = TRUE)
          dir2create <- sub(pattern=inDir, replacement=outDir, x=dirs_all)
          if(isFALSE(keepCameraSubfolders)) {
             cams <- unique(copy.info.table[, cameraCol])
             cams <- paste(cams, collapse = "|")
             dir2create <- sapply(dir2create, sub, pattern=cams, replacement="")
             dir2create <- sub(pattern = "//", replacement = "/", x = dir2create)
          }
          sapply(unique(dir2create), dir.create, recursive = TRUE, showWarnings = FALSE)
        } else {
          sapply(unique(copy.info.table$outDir), dir.create, recursive = TRUE, showWarnings = FALSE)
          }
        }

        # check if renamed images exist already
        if(hasArg(outDir)) {
          copy.info.table$fileExistsAlready <- file.exists(file.path(copy.info.table$outDir, copy.info.table$filename_new))
        } else {
          copy.info.table$fileExistsAlready <- FALSE
        }

          if(any(copy.info.table$fileExistsAlready)) {
          message(paste(sum(copy.info.table$fileExistsAlready), "out of", nrow(copy.info.table), "images existed already in outDir. They will not be copied"))
          }


      # copy images
      if(isTRUE(copyImages)){

         if(any(copy.info.table$fileExistsAlready)) {
          switch(menu(choices = c("Copy only images that are not in outDir", "Copy nothing"), title =  "outDir is not empty. What should I do?"),
                 proceed <- TRUE ,
                 proceed <- FALSE)
             } else {
                 proceed <- TRUE
             }

      if(isTRUE(proceed)){
      # find items to copy
          items_to_copy <- which(copy.info.table$DateReadable == TRUE & copy.info.table$fileExistsAlready == FALSE)

          message(paste("copying", length(items_to_copy), "images to", outDir, " ... This may take some time."))

          copy.info.table$CopyStatus[items_to_copy] <- file.copy(from      = apply(copy.info.table[items_to_copy, c("Directory", "FileName")],  MARGIN = 1, FUN = paste, collapse = file.sep),
                                                                                       to        = apply(copy.info.table[items_to_copy, c("outDir", "filename_new")], MARGIN = 1, FUN = paste, collapse = file.sep),
                                                                                       overwrite = FALSE)
          copy.info.table$CopyStatus[-items_to_copy] <- FALSE
        } else {
          copy.info.table$CopyStatus <- FALSE
        }
      } else {
        copy.info.table$CopyStatus <- FALSE
      }

  rownames(copy.info.table) <- NULL
  copy.info.table <- copy.info.table[,-which(names(copy.info.table) %in%
                                               c("DateTimeOriginal2", "DateTime_for_filename", "minute.append"))]
  # save table
  if(writecsv == TRUE){
    dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
    setwd(outDir)
    write.csv(copy.info.table, file = paste("_renaming_table_", Sys.Date(), ".csv", sep = ""),
              row.names = FALSE)
  }
  return(copy.info.table)
}
