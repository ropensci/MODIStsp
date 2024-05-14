





# Function to fetch Earthdata access token
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
    access_token
  }
  
