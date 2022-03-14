#' Expand a record table to have one row for each detection
#'
#' If a record table has a count column, this function expand the record table to
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
#' # Make up a test recTable
#' recTest <- data.table(fileName=LETTERS[1:5],
#' Group1Count=c(rep(2,4), 1), # The count of individuals in the first group (G1)
#' Group1Species= c(rep("sambar", 4), "Fallow"), # The species of G1
#' Group1Sex=sample(c("Male", "Female", NA), 5, replace = T) # The sex of G1
#' )
#' recTest
#'
#' # Replicate rows
#' OROD <- oneRowOneDetection(W2L, countsName = "Group1Count")
#' OROD
#'
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
