#' @title MODIStsp download function
#' @description Internal function dealing with download of MODIS hdfs from the
#'  NASA Earthdata Cloud for a given date. Granules are downloaded from the URLs
#'  discovered through the CMR (see `get_mod_filenames`), authenticating with an
#'  Earthdata Login bearer token.
#' @param modislist `character array` List of MODIS images to be downloaded for
#'  the selected date (as returned from `get_mod_filenames`). Can be a single
#'  image, or a list of images in case different tiles are needed!
#' @param out_folder_mod `character` Folder where the hdfs are to be stored
#' @param download_server `character ["http"]` Server to be used.
#' @param urls `named character` mapping each HDF file name in `modislist` to its
#'  Earthdata Cloud download URL (the `"urls"` attribute returned by
#'  `get_mod_filenames`).
#' @param n_retries `numeric` Max number of retry attempts on download. If
#'  download fails more that n_retries times consecutively, abort
#' @param date_dir `character array` Sub-folder where the different images
#'  can be found (element of the list returned from `get_mod_dirs`).
#' @param use_aria `logical` if TRUE, download using aria2c
#' @param year `character` Acquisition year of the images to be downloaded
#' @param DOY `character array` Acquisition doys of the images to be downloaded
#' @param token `character` Earthdata Login bearer token used to authenticate
#'  downloads from the NASA Earthdata Cloud.
#' @param sens_sel `character ["terra" | "aqua"]` Selected sensor.
#' @param date_name `character` Date of acquisition of the images to be downloaded.
#' @param gui `logical` Indicates if on an interactive or non-interactive execution
#'  (only influences where the log messages are sent).
#' @param verbose `logical` If FALSE, suppress processing messages, Default: TRUE
#' @return The function is called for its side effects
#' @rdname MODIStsp_download
#' @author Lorenzo Busetto, phD (2014-2017)
#' @author Luigi Ranghetti, phD (2015)
#' @importFrom httr2 request req_headers req_method req_retry req_perform
#'  req_error resp_header resp_status

MODIStsp_download <- function(modislist,
                              out_folder_mod,
                              download_server,
                              urls,
                              n_retries,
                              use_aria,
                              date_dir,
                              year,
                              DOY,
                              token,
                              sens_sel,
                              date_name,
                              gui,
                              verbose) {

  # Cycle on the different files to download for the current date
  for (file in seq_along(modislist)) {
    modisname <- modislist[file]

    #   ________________________________________________________________________
    # Try to retrieve the file size of the remote HDF so that if a local    ####
    # file exists but size is different it can be redownloaded
    #
    local_filename  <- file.path(out_folder_mod, modisname)
    if (file.exists(local_filename))  {
      local_filesize <- file.info(local_filename)$size
    } else {
      local_filesize <- 0
    }

    if (download_server == "http") {
      remote_filename <- unname(urls[modisname])
      if (is.na(remote_filename) || is.null(remote_filename)) {
        stop("[", date(), "] Error: no download URL found for ", modisname,
             ". Aborting!", call. = FALSE)
      }
    }

    # On http download, retrieve the remote file size (Content-Length) so we ----
    # can skip already-complete files and detect incomplete downloads.
    if (download_server == "http") {
      remote_filesize <- get_remote_filesize(remote_filename, token, n_retries)
    } else {
      # On offline mode, don't perform file size check ----
      remote_filesize <- local_filesize
    }

    #   ________________________________________________________________________
    #   Download required HDF images                                        ####
    #   (If HDF not existing locally, or existing with different size)
    #
    # remote_filesize may be NA if the server did not report Content-Length;
    # in that case we always (re)download.

    need_download <- !file.exists(local_filename) ||
      is.na(remote_filesize) || local_filesize != remote_filesize

    if (need_download) {

      # update messages
      mess_text <- paste("Downloading", sens_sel, "Files for date:",
                         date_name, ":", which(modislist == modisname),
                         "of: ", length(modislist))
      # Update progress window
      process_message(mess_text, verbose)
      success <- FALSE
      attempt <- 0
      #  _______________________________________________________________________
      #  while loop: try to download n_retries times  ####
      while (attempt < n_retries) {

        if (download_server == "http") {
          # http download - aria
          if (use_aria == TRUE) {
            aria_string <- paste0(
              Sys.which("aria2c"), " -x 6 -d ",
              dirname(local_filename),
              " -o ", basename(local_filename),
              " ", remote_filename,
              " --allow-overwrite --file-allocation=none --retry-wait=2",
              " --header=\"Authorization: Bearer ", token, "\"")

            # intern=TRUE for Windows, FALSE for Unix
            download <- try(system(aria_string,
                                   intern = Sys.info()["sysname"] == "Windows"))
          } else {
            # http download - httr2 with bearer token. libcurl strips the
            # Authorization header on the cross-host redirect to S3/CloudFront,
            # so the pre-signed URL is honoured without leaking credentials.
            req <- httr2::request(remote_filename)
            req <- httr2::req_headers(
              req, Authorization = paste("Bearer", token))
            req <- httr2::req_retry(req, max_tries = n_retries)
            req <- httr2::req_error(req, is_error = function(resp) FALSE)
            download <- try(
              httr2::req_perform(req, path = local_filename), silent = TRUE)

            # An auth error is fatal (no point retrying with a bad token)
            if (!inherits(download, "try-error")) {
              status <- httr2::resp_status(download)
              if (status == 401 || status == 403) {
                unlink(local_filename)
                stop("Earthdata authentication failed (HTTP ", status,
                     "). Please check that your Earthdata Login token is ",
                     "valid and that the 'LP DAAC' application is authorized ",
                     "in your Earthdata profile.", call. = FALSE)
              }
            }
          }
        }

        # Check for errors on download try (network / aria2c failures -> retry)
        aria_failed <- use_aria && !is.null(attr(download, "status")) &&
          !is.na(attr(download, "status")) && attr(download, "status") != 0
        if (inherits(download, "try-error") | aria_failed) {
          attempt <- attempt + 1
          if (verbose) message("[", date(), "] Download Error - Retrying...")
          unlink(local_filename)  # On download error, delete incomplete files
          Sys.sleep(1)    # sleep for a while....
        }

        # final check on local file size: Only exit if local file size equals
        # remote filesize to  prevent problems on incomplete download!
        local_filesize <- file.info(local_filename)$size
        if (!is.na(local_filesize) && local_filesize > 0 &&
            (is.na(remote_filesize) || local_filesize == remote_filesize)) {
          # on success, bump attempt number so to exit the while cycle
          attempt <- n_retries + 1
          success <- TRUE
        } else {
          attempt <- attempt + 1
        }
      }
      if (success == FALSE) {
          unlink(local_filename)
          stop("[", date(), "] Error: server seems to be down or the file ",
               "could not be downloaded! Please retry later!", call. = FALSE)

      }
    } else {
      mess_text <- paste("HDF File:", modisname,
                         "already exists on your system. Skipping download!")
      process_message(mess_text, verbose)
    }
  }
}

#' @title Retrieve the remote size of an Earthdata Cloud granule
#' @description Helper issuing a HEAD request (bearer token, following the
#'  redirect to S3/CloudFront) to obtain the `Content-Length` of a granule.
#' @param url `character` granule download URL.
#' @param token `character` Earthdata Login bearer token.
#' @param n_retries `numeric` max retry attempts.
#' @return `integer` remote file size in bytes, or `NA` if not reported.
#' @keywords internal
#' @importFrom httr2 request req_headers req_method req_retry req_error
#'  req_perform resp_header resp_status
get_remote_filesize <- function(url, token, n_retries) {
  resp <- try({
    req <- httr2::request(url)
    req <- httr2::req_headers(req, Authorization = paste("Bearer", token))
    req <- httr2::req_method(req, "HEAD")
    req <- httr2::req_retry(req, max_tries = n_retries)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    httr2::req_perform(req)
  }, silent = TRUE)

  if (inherits(resp, "try-error")) return(NA_integer_)

  # If the size probe cannot authenticate/authorize, don't abort here: return
  # NA (forcing a download attempt) and let the GET report the definitive error.
  status <- httr2::resp_status(resp)
  if (status == 401 || status == 403) return(NA_integer_)

  cl <- httr2::resp_header(resp, "Content-Length")
  if (is.null(cl)) return(NA_integer_)
  suppressWarnings(as.integer(cl))
}
