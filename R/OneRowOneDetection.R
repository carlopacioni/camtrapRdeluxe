#' Expand a record table to have one row for each detection
#'
#' If a record table has a count column, this functon expand the record table to
#' have a row for each detection based on the count column.
#'
#' \strong{NOTE}: This function is currently possibly slow (a few mins) for
#' recortable that are more than 1E4, so be patient!
#' @inheritParams recordTable,assassessTemporalIndependence
#' @return record table (data.table)
#' @import data.table
#' @export
#' @examples
#' # create a test recordTable
#' nc<-12
#' nr<-1E4
#' m <- matrix(sample(1:100, replace = T, size = nr*nc), ncol = nc)
#' recTabTest <- data.table(m)
#' recTabTest[, Count:=sample(1:3, replace = T, size = nr)]
#'
#' system.time(
#'   test<-oneRowOneDetection(intable=recTabTest, countsName="Count")
#' )
oneRowOneDetection <- function(intable, countsName) {
  if(!is.data.table(intable)) intable <- data.table(intable)
  tempDT <- data.table()
  for(r in seq_len(nrow(intable))) {
    if(intable[r, countsName, with=FALSE] > 1)
      for(n in seq_len(intable[r, get(countsName)] - 1)) {
        tempDT <- rbindlist(list(tempDT, intable[r,]))
      }
  }
  intable <- rbindlist(list(intable, tempDT))
  return(intable)
}
