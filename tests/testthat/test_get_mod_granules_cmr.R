message("MODIStsp Test CMR: granule discovery via NASA CMR")
test_that(
  "get_mod_granules_cmr returns granules for a tiled product", {
    skip_on_cran()
    skip_if_offline("cmr.earthdata.nasa.gov")

    res <- get_mod_granules_cmr(
      short_name = "MOD13Q1", version = "061",
      date_from = "2020.06.01", date_to = "2020.06.30",
      v = 4, h = c(17, 18), tiled = 1, n_retries = 3
    )

    expect_s3_class(res, "data.frame")
    expect_true(all(c("title", "date", "hdf_name", "url") %in% names(res)))
    expect_gt(nrow(res), 0)
    # only the requested tiles are returned
    expect_true(all(grepl("h1[78]v04", res$title)))
    # download URLs point to the Earthdata Cloud and are HDF granules
    expect_true(all(grepl("\\.hdf$", res$url)))
    expect_true(all(grepl("earthdatacloud.nasa.gov", res$url)))
    # dates are formatted as YYYY.MM.DD
    expect_true(all(grepl("^[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}$", res$date)))
  })

test_that(
  "parse_prod_version extracts short_name and version", {
    pv <- parse_prod_version("https://e4ftl01.cr.usgs.gov/MOLT/MOD13Q1.061/")
    expect_equal(unname(pv["short_name"]), "MOD13Q1")
    expect_equal(unname(pv["version"]), "061")
  })
