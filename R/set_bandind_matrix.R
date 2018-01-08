#' @title Helper function to determine the bands needed to compute SIs and QIs
#' @description FUNCTION_DESCRIPTION
#' @inheritParams MODIStsp_process
#' @return `matrix` containing info on which bands are needed for computing
#'  each available QI or SI
#' @rdname set_bandind_matrix
#' @author Lorenzo Busetto, phD (2017) \email{lbusett@@gmail.com}
set_bandind_matrix <- function(bandnames, 
                               bandsel,
                               indexes_bandnames,
                               indexes_bandsel, 
                               indexes_formula, 
                               quality_bandnames, 
                               quality_bandsel, 
                               quality_source) {
  
  # matrix which associates, to each couple of index or quality band (col)
  # - original band (row), info on wether that band is required to build that
  # index
  
  bands_indexes <- matrix(
    0,
    nrow     = length(bandsel),
    ncol     = length(indexes_bandsel) + length(quality_bandsel),
    dimnames = list(bandnames, c(indexes_bandnames, quality_bandnames))
  )
  
  # cycle on selected indexes to force processing of all bands needed to
  # compute the index
  for (band in which(indexes_bandsel == 1)) {
    # If an index is selected retrieve its formula
    formula <- indexes_formula[band]
    # cycle on original bands
    for (bandorig in seq(along.with = bandnames)) {
      # check if the original band is needed for the index
      if (length(grep(bandnames[bandorig], formula)) > 0) {
        # if yes and band not yet set to be processed, set it to be processed
        if (bandsel[bandorig] == 0) {
          bands_indexes[bandorig, band] <- 1
        }
      }
    }
  }
  
  # cycle on selected QIs to force processing of all bands needed to
  # compute the Quality indicators
  for (band in which(quality_bandsel == 1)) {
    bandorig <- which(bandnames == quality_source[band])
    if (bandsel[bandorig] == 0) {
      bands_indexes[bandorig, length(indexes_bandsel) + band] <- 1
    }
  }
  return(bands_indexes)
}