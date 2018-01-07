#' @title Remove custom spectral indexes
#' @description Function used to remove all user-defined Spectral Indexes from 
#'  a MODIStsp json options file, thus resetting the list of available indexes
#'  to the default ones.
#' @param opts_jsfile `character` full path of a JSON file
#'  containing the processing options in which the new indexes has to be saved
#'  (default: MODIStsp_Previous.JSON in subfolder Previous).
#' @importFrom jsonlite fromJSON write_json
#' @return The function is called for its side effects. On success, the
#'  MODIStsp_Previous.json file or the specified options file is modified so to
#'  remove all previously custom-specified Spectral Indexes.
#' @author Lorenzo Busetto, phD (2014-2017) \email{busetto.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @seealso [MODIStsp_addindex]
#' @rdname MODIStsp_resetindexes
#' @export
#'
#' @examples
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
#'  opts <- jsonlite::fromJSON(opts_jsfile)
#'  opts$custom_indexes[1]
#'  
#'  # Now remove all custom indexes
#'  MODIStsp_resetindexes(opts_jsfile)
#'  opts <- jsonlite::fromJSON(opts_jsfile)
#'  opts$custom_indexes[1]
#'  
MODIStsp_resetindexes <- function(opts_jsfile = NULL) {
  
  
  previous_jsfile <- ifelse(is.null(opts_jsfile),
                            system.file("ExtData/Previous", 
                                        "MODIStsp_Previous.json",
                                        package = "MODIStsp"),
                            opts_jsfile)
  
  general_opts    <- load_opts(previous_jsfile)
  
  message("Removing all custom Spectral Indexes from ",
          basename(previous_jsfile))
  general_opts$custom_indexes <- list()
  
  # Re-save the json file
  jsonlite::write_json(general_opts,
                       previous_jsfile,
                       pretty = TRUE,
                       auto_unbox = TRUE)
  
} 
