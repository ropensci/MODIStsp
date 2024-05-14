#' @title Get Earthdata Bearer access token function
#' @description Internal function to fetch Earthdata access token
#' @details The function is used to:
#'  - Fetch Bearer token if there is one already defined;
#'  - Request new token if no token is defined
#'  - Update token if the token is expired
#'  - Bearer token is used for authentication by other functions
#' @param user `character` Username for Earthdata servers
#' @param password `character` Password for Earthdata servers
#' @return The function is called for its side effects
#' @rdname get_earthdata_token
#' @importFrom httr2 request req_perform req_auth_basic req_headers resp_body_xml

  get_token <- function(user, password) {
    endpoint = "https://urs.earthdata.nasa.gov/api/users/tokens"
    resp <- httr2::request(endpoint) |> httr2::req_auth_basic(user, password) |> httr2::req_perform()
    token_one <- httr2::resp_body_json(resp)[[1]]
    
    # Check if no token available; if not, request one
    if(length(token_one) < 1) 
    {
      endpoint = "https://urs.earthdata.nasa.gov/api/users/token"
      resp <- httr2::request(endpoint) |> req_method("PUT") |> httr2::req_auth_basic(user, password) |> httr2::req_perform()
    } 
    token_one <- httr2::resp_body_json(resp)[[1]]
    access_token <- token_one$access_token
    # Return token
    access_token
  }
  
