#' Folder indices
#'
#' Return the indices of the folder structure to identify information needed to
#' populate \code{recordTable}'s or \code{recordTableIndividual}'s arguments
#'
#' Depending on your set up, you may have multiple information stored in your
#' folder structure. \code{camtrapRdeluze} can now extract information from
#' multiple folders (i.e. not only Station and Species), but it needs to know
#' their position in the directory path when using \code{recordTable} or
#' \code{recordTableIndividual}. This function aims to facilitate the position
#' of the folders for the following arguments:  \itemize{ \item
#' stationIDposition \item speciesPosition \item cameraIDposition }
#'
#' If there are multiple paths within \code{inDir} - as it is likely - the
#' function will look for  the (first) longest and use that as an example.
#'
#' @inheritParams recordTable
#' @export
folderIndices <- function(inDir) {
  dirs <- list.dirs(inDir, full.names = TRUE,  recursive = TRUE)
  splitPaths <- strsplit(dirs, split = "/", fixed = TRUE)
  pathLength <- sapply(splitPaths, length)
  tmp <- unlist(strsplit(dirs[[which.max(pathLength)]], split = "/", fixed = TRUE))
  names(tmp) <- 1:length(tmp)
  return(tmp)
  }
