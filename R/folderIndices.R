#' @export
folderIndices <- function(inDir) {
  dirs <- list.dirs(inDir, full.names = TRUE,  recursive = FALSE)
  tmp <- unlist(strsplit(dirs[[1]], split = "/", fixed = TRUE))
  names(tmp) <- 1:length(tmp)
  return(tmp)
  }
