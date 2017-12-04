#'  MODIStest_check_md5
#' @description internal function which tests if the output of a test is corresponds to the expected one.
#' @details When running in test mode, MODIStsp is expected to produce the same files
#' @param test integer number of the performed test
#' @return NULL
#'
#' @author Luigi Ranghetti, phD (2017) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' license GPL 3.0
#' @importFrom tools md5sum
#' @importFrom RJSONIO fromJSON

MODIStest_check_md5 <- function(test) {

  # charge expected values
  exp_md5 <- as.data.frame(
    fromJSON(system.file("Test_files", "tests_md5.json", package = "MODIStsp")),
    stringsAsFactors = FALSE)
  exp_md5 <- exp_md5[exp_md5$test == test, ]

  n_err <- 0 # counter of the number of files which do not pass the test
  for (i in seq_len(nrow(exp_md5))) {
    sel_file <- file.path(tempdir(), exp_md5[i, "filename"])

    # in case of vrt files, delete the line with the reference of the filename
    # (hdr files are not checked, since the corresponding dat is checked)
    if (length(grep("\\.vrt$", sel_file)) > 0) {
      vrt_content    <- readLines(sel_file)
      lines_todelete <- grep(" *<SourceFilename*", vrt_content)
      writeLines(vrt_content[!c(seq_along(vrt_content)) %in% lines_todelete],
                 sel_file)
    }

    sel_md5 <- exp_md5[i, "md5"]
    if (!file.exists(sel_file)) {
      warning(paste0("Error: file '", sel_file,
                     "' was not created in the current temporary directory."))
      n_err <- n_err + 1
    } else if (sel_md5 != md5sum(sel_file)) {
      warning(paste0("Error: file '", sel_file,
                     "' is different from the expected one."))
      n_err <- n_err + 1
    }
  }

  # summary
  if (n_err == 0) {
    message(paste0("Test ", test, " runned succesfully.\n\n"))
  } else {
    message(paste0("Test ", test, " failed on ", n_err, "/", nrow(exp_md5),
                   " files.\n\n"))
  }

}
