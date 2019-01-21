#'Convert a record table from a wide to long formatting
#'
#'If there are multiple columns in a record table that provide the same
#'information, for example when metatags are used to provide informaiton on
#'multiple individuals in the same image (e.g. Ind1Species, Ind1Age,
#'Ind2Species, Ind2Age, etc.), the user may want to collate all these in one
#'column (e.g. Species and Age). This function fecilitate this process and
#'removes lines where all the information being removed are missing (e.g. when
#'there are no additional individuals).
#'
#'If there is need to use both wide2long and oneRowOneDetection, the latter
#'should be used first.
#'
#'@inheritParams assassessTemporalIndependence
#'@param pattern Character. The pattern to identify the columns names that need
#'  to be collated (e.g. c("Species$", "Age$"))
#'@param valuesNames Character. The vector of names to apply to the conmbines
#'  variables
#'@return A record table
#'@import data.table
#'@export
#'
wide2long <- function(intable, pattern, valueNames) {
  nms <- names(intable)
  pat <- paste(pattern, collapse = "|")
  message(paste("The pattern provided matched these columns:\n",
                paste(nms[grep(pattern = pat, x = nms)], collapse = ", ")))
  dt<-data.table::melt.data.table(intable,
                                  measure.vars = patterns(pattern),
                                  value.name = valueNames)
  dt<-na.omit(dt, cols = valueNames)
  return(dt)
}


