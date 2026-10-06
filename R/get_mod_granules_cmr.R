#' @title Query NASA CMR for MODIS granules
#' @description Accessory function that queries the NASA Common Metadata
#'  Repository (CMR) for the MODIS granules of a given product / version,
#'  temporal range and (optionally) set of MODIS tiles, and returns their
#'  acquisition dates, HDF file names and Earthdata Cloud download URLs.
#'
#'  This replaces the historical approach of scraping HTML directory listings
#'  from the LP DAAC Data Pool (`e4ftl01.cr.usgs.gov`), which was retired for
#'  MODIS on 2025-06-30. MODIS Collection 6.1 is now distributed through the
#'  NASA Earthdata Cloud and discoverable via CMR (provider `LPCLOUD`).
#' @param short_name `character` MODIS product short name (e.g. "MOD13Q1").
#' @param version `character` product version/collection (e.g. "061").
#' @param date_from `character` start date, format "YYYY.MM.DD" or "YYYY-MM-DD".
#' @param date_to `character` end date, format "YYYY.MM.DD" or "YYYY-MM-DD".
#' @param v `integer array` vertical tiles of interest (e.g. `c(4, 5)`). Ignored
#'  when `tiled == 0`. Default: NULL.
#' @param h `integer array` horizontal tiles of interest (e.g. `c(17, 18)`).
#'  Ignored when `tiled == 0`. Default: NULL.
#' @param tiled `numeric [0/1]` 1 = tiled product (filter by `hXXvYY`); 0 =
#'  non-tiled/global product. Default: 1.
#' @param n_retries `numeric` number of times a failing CMR request is retried
#'  before aborting. Default: 20.
#' @return `data.frame` with columns `title`, `date` ("YYYY.MM.DD"), `hdf_name`
#'  and `url`. Empty (0-row) data.frame if no granule matches.
#' @details Server-side tile filtering uses CMR's `readable_granule_name`
#'  pattern option (`*.hXXvYY.*`), which keeps queries small. Results larger
#'  than one page are retrieved following the `CMR-Search-After` header.
#' @author Lorenzo Busetto, phD (2014-2017)
#' @author Luigi Ranghetti, phD (2016-2017)
#' @note License: GPL 3.0
#' @importFrom httr2 request req_url_query req_headers req_retry req_perform
#'  resp_header resp_body_json
#' @keywords internal

get_mod_granules_cmr <- function(short_name,
                                 version,
                                 date_from,
                                 date_to,
                                 v = NULL,
                                 h = NULL,
                                 tiled = 1,
                                 n_retries = 20) {

  cmr_url  <- "https://cmr.earthdata.nasa.gov/search/granules.json"
  page_size <- 2000

  # CMR temporal filter expects ISO dates; accept "YYYY.MM.DD" or "YYYY-MM-DD"
  iso <- function(x) gsub("\\.", "-", x)
  temporal <- paste0(iso(date_from), "T00:00:00Z,", iso(date_to), "T23:59:59Z")

  # Base query parameters
  query <- list(
    short_name = short_name,
    version    = version,
    provider   = "LPCLOUD",
    temporal   = temporal,
    page_size  = page_size
  )

  # Server-side tile filter (only for tiled products with tiles requested) ----
  # For each hXXvYY combination add a `readable_granule_name[]` pattern, all
  # OR-ed together, enabling the pattern option. The pattern is anchored on the
  # product short name (e.g. "MOD13Q1.A*.h18v04.*") rather than a leading
  # wildcard ("*.h18v04.*"): CMR rejects queries with more than 5 leading-
  # wildcard conditions, so a leading wildcard would cap tile filtering at ~2
  # tiles and 400 on larger extents. Anchoring on the (fixed) short name avoids
  # the leading wildcard entirely, so any number of tiles works.
  if (tiled == 1 && length(v) > 0 && length(h) > 0) {
    patterns <- character()
    for (vv in v) {
      for (hh in h) {
        vc <- formatC(vv, width = 2, flag = "0")
        hc <- formatC(hh, width = 2, flag = "0")
        patterns <- c(patterns,
                      paste0(short_name, ".A*.h", hc, "v", vc, ".*"))
      }
    }
    # httr2 repeats the parameter when a value is a length>1 vector
    query[["readable_granule_name[]"]] <- patterns
    query[["options[readable_granule_name][pattern]"]] <- "true"
  }

  #   __________________________________________________________________________
  #   Retrieve all granules, following CMR-Search-After pagination          ####

  entries      <- list()
  search_after <- NULL

  repeat {
    req <- httr2::request(cmr_url)
    req <- do.call(httr2::req_url_query,
                   c(list(req), query, list(.multi = "explode")))
    if (!is.null(search_after)) {
      req <- httr2::req_headers(req, `CMR-Search-After` = search_after)
    }
    req <- httr2::req_retry(req, max_tries = n_retries)

    resp <- try(httr2::req_perform(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
      stop("[", date(), "] Error: unable to reach the NASA CMR server. ",
           "Please try again later.", call. = FALSE)
    }

    body       <- httr2::resp_body_json(resp)
    page_items <- body[["feed"]][["entry"]]
    if (length(page_items) == 0) break
    entries <- c(entries, page_items)

    # Continue only if the page was full and CMR handed us a search-after token
    search_after <- httr2::resp_header(resp, "CMR-Search-After")
    if (length(page_items) < page_size || is.null(search_after)) break
  }

  if (length(entries) == 0) {
    return(data.frame(title = character(), date = character(),
                      hdf_name = character(), url = character(),
                      stringsAsFactors = FALSE))
  }

  #   __________________________________________________________________________
  #   Extract title, date, hdf name and download URL for each granule       ####

  titles <- vapply(entries, function(e) e[["title"]] %||% NA_character_,
                   character(1))

  # Download URL: the `data#` link whose href ends in `.hdf` (the same rel also
  # exposes BROWSE .jpg and .cmr.xml, which must be excluded).
  urls <- vapply(entries, function(e) {
    hrefs <- vapply(e[["links"]], function(l) {
      rel  <- l[["rel"]] %||% ""
      href <- l[["href"]] %||% ""
      if (grepl("/data#$", rel) && grepl("\\.hdf$", href)) href else NA_character_
    }, character(1))
    hrefs <- hrefs[!is.na(hrefs)]
    if (length(hrefs) >= 1) hrefs[1] else NA_character_
  }, character(1))

  hdf_name <- basename(urls)
  # For granules missing a usable URL, fall back to `<title>.hdf` for the name
  hdf_name[is.na(urls)] <- paste0(titles[is.na(urls)], ".hdf")

  # Acquisition date from the title (MODIS naming: <prod>.A<YYYYDOY>.<...>)
  doy   <- sub("^[A-Z0-9]+\\.A([0-9]{7})\\..*", "\\1", titles)
  dates <- ifelse(grepl("^[0-9]{7}$", doy),
                  strftime(as.Date(doy, format = "%Y%j"), "%Y.%m.%d"),
                  NA_character_)

  out <- data.frame(title = titles, date = dates, hdf_name = hdf_name,
                    url = urls, stringsAsFactors = FALSE)
  # Drop granules for which no protected .hdf URL was found
  out <- out[!is.na(out$url), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Null-coalescing helper (avoids a hard dependency on rlang's %||%)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
