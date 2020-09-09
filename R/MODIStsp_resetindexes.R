#' @title Remove custom spectral indexes
#' @description Function used to remove all user-defined Spectral Indexes from
#'  MODIStsp, thus resetting the list of available indexes
#'  to the default ones.
#' @importFrom jsonlite write_json
#' @return The function is called for its side effects. On success, the
#'  MODIStsp_indexes.json file is modified so to
#'  remove all previously custom-specified Spectral Indexes.
#' @author Lorenzo Busetto, phD (2014-2017) \email{busetto.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @seealso [MODIStsp_addindex]
#' @rdname MODIStsp_resetindexes
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove all custom-defined spectral indexes from an options file
#'
#' # Add a custom index for testing purposes
#' library(jsonlite)
#' opts_jsfile = system.file("testdata/test_addindex.json",
#'                              package = "MODIStsp")
#'  MODIStsp_addindex(
#'    opts_jsfile = opts_jsfile,
#'    gui = FALSE,
#'    new_indexbandname = paste0("Index_", as.character(sample(10000, 1))),
#'    new_indexformula = "b1_Red - b2_NIR",
#'    new_indexfullname = paste0("Index_", as.character(sample(10000, 1)))
#'    )
#'
#'  opts <- jsonlite::fromJSON(indexes_file)
#'  opts$custom_indexes[1]
#'
#'  # Now remove all custom indexes
#'  MODIStsp_resetindexes()
#'  opts <- jsonlite::fromJSON(opts_jsfile)
#'  opts$custom_indexes[1]
#'  }
#'
MODIStsp_resetindexes <- function() {

  indexes_file <- system.file("ExtData",
                              "MODIStsp_indexes.json",
                              package = "MODIStsp")

  # Re-save the json file
  jsonlite::write_json("",
                       indexes_file,
                       pretty = TRUE,
                       auto_unbox = TRUE)

}
