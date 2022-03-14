#'Convert a record table from a wide to long formatting
#'
#'If there are multiple columns in a record table that provide the same
#'information, for example when metatags are used to provide information on
#'multiple individuals in the same image (e.g. Ind1Species, Ind1Age,
#'Ind2Species, Ind2Age, etc.), the user may want to collate all these in one
#'column (e.g. Species and Age). This function facilitate this process and
#'removes lines where all the information being collated is missing (e.g. when
#'there are no additional individuals) if \code{rm.allNA=TRUE} [default]. See the
#'first example for this situation.
#'
#'If there is need to use both wide2long and oneRowOneDetection, the latter
#'should be used first.
#'
#'@inheritParams assassessTemporalIndependence
#'@param pattern Character. The pattern to identify the columns names that need
#'  to be collated (e.g. c("Species$", "Age$"))
#'@param valuesNames Character. The vector of names to apply to the combined
#'  variables
#'@param  rm.allNA whether rows where the values for the columns
#'  \code{valueNames} are all NA should be removed
#'@return A record table
#'@import data.table
#'@export
#'@examples # make up a recordTable where multiple individuals in the same image
#' # are identified with the prefix Ind1 or Ind2
#' recTest <- data.table(fileName=LETTERS[1:5],
#' Count=c(rep(2,4), 1),
#' Ind1Species= c(rep("sambar", 4), "Fallow"),
#' Ind1Sex=sample(c("Male", "Female", NA), 5, replace = T),
#' Ind2Species= c(rep("sambar", 4), NA),
#' Ind2Sex=c(sample(c("M", "F", NA), 4, replace = T), NA) )
#' recTest
#' wide2long(recTest, pattern = c("Species$", "Sex$"), variableName="Individual",
#' valueNames = c("Species", "Sex"))
#'
#' # Metadata refer to groups of individuals and need to be collated and then
#' # repeated to have one entry for each detection
#'
#' # Make up a test recTable
#' recTest <- data.table(fileName=LETTERS[1:5],
#' Group1Count=c(rep(2,4), 1), # The count of individuals in the first group (G1)
#' Group2Count=c(rep(1,4), NA), # The count of individuals in the second group (G2)
#' Group1Species= c(rep("sambar", 4), "Fallow"), # The species of G1
#' Group1Sex=sample(c("Male", "Female", NA), 5, replace = T), # The sex of G1
#' Group2Species= c(rep("sambar", 4), NA), # The species of G2
#' Group2Sex=c(sample(c("M", "F", NA), 4, replace = T), NA) # The sex of G2
#' )
#' recTest
#'
#' # Apply wide2 long first
#' W2L <- wide2long(recTest, pattern = c("Count$", "Species$", "Sex$"),
#' valueNames = c("Count", "Species", "Sex"))
#' # Replicate rows
#' OROD <- oneRowOneDetection(W2L, countsName = "Count")
#' OROD
#'
wide2long <- function(intable, pattern, variableName="variable", valueNames, rm.allNA=TRUE) {
  nms <- names(intable)
  pat <- paste(pattern, collapse = "|")
  message(paste("The pattern provided matched these columns:\n",
                paste(nms[grep(pattern = pat, x = nms)], collapse = ", ")))
  dt<-data.table::melt.data.table(intable,
                                  measure.vars = patterns(pattern),
                                  variable.name = variableName,
                                  value.name = valueNames,
                                  na.rm = FALSE)
  if(rm.allNA) {
    dt <- dt[!apply(dt[, valueNames, with=FALSE], MARGIN = 1, FUN = function(x) all(is.na(x))),]
  }

  return(dt)
}


