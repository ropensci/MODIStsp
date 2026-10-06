#' @title Get an Earthdata Login bearer token from username/password
#' @description Internal function that fetches (or, if none exists, creates) an
#'  Earthdata Login user token via the URS API, using HTTP Basic authentication
#'  with the user's Earthdata credentials. The returned bearer token is then
#'  used to authenticate downloads from the NASA Earthdata Cloud.
#' @param user `character` Earthdata Login username.
#' @param password `character` Earthdata Login password.
#' @return `character` a bearer access token.
#' @author Pasi Autio (2024)
#' @note License: GPL 3.0
#' @importFrom httr2 request req_auth_basic req_method req_error req_perform
#'  resp_status resp_body_json
#' @keywords internal
get_earthdata_token <- function(user, password) {

  # List existing tokens for the user
  list_resp <- httr2::request("https://urs.earthdata.nasa.gov/api/users/tokens") |>
    httr2::req_auth_basic(user, password) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(list_resp) == 401) {
    stop("Earthdata authentication failed: the username/password provided are ",
         "not valid. Please check your Earthdata Login credentials.",
         call. = FALSE)
  }

  body <- httr2::resp_body_json(list_resp)
  if (length(body) >= 1 && !is.null(body[[1]][["access_token"]])) {
    return(body[[1]][["access_token"]])
  }

  # No token yet: create one
  new_resp <- httr2::request("https://urs.earthdata.nasa.gov/api/users/token") |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(user, password) |>
    httr2::req_perform()

  httr2::resp_body_json(new_resp)[["access_token"]]
}

#' @title Resolve an Earthdata Login bearer token
#' @description Internal helper returning a usable Earthdata Login bearer token
#'  from whatever credentials are available, in order of preference:
#'  (1) an explicit `token`; (2) the `EARTHDATA_TOKEN` environment variable;
#'  (3) a token generated on the fly from `user`/`password` via
#'  [get_earthdata_token()].
#' @param token `character` an Earthdata Login bearer token, or NULL.
#' @param user `character` Earthdata Login username, or NULL.
#' @param password `character` Earthdata Login password, or NULL.
#' @return `character` a bearer access token.
#' @note License: GPL 3.0
#' @keywords internal
resolve_earthdata_token <- function(token = NULL, user = NULL, password = NULL) {

  if (is.null(token) || !nzchar(token)) {
    env_token <- Sys.getenv("EARTHDATA_TOKEN")
    if (nzchar(env_token)) token <- env_token
  }
  if (!is.null(token) && nzchar(token)) {
    return(token)
  }
  if (!is.null(user) && nzchar(user) &&
      !is.null(password) && nzchar(password)) {
    return(get_earthdata_token(user, password))
  }

  stop("No Earthdata Login credentials found. Provide a `token` (or set the ",
       "`EARTHDATA_TOKEN` environment variable), or a `user` and `password`. ",
       "Register and generate a token at https://urs.earthdata.nasa.gov/.",
       call. = FALSE)
}
