#'
#'@param output_format
#' @export

# devtools::document()
check_paramter_errors <- function(output_format, accessKey, secretKey){
  if(!is.null(output_format)){
    if(!(output_format %in% c('jsonRow', 'jsonColumn', 'matrix'))){
      stop("Output format must be jsonRow, jsonColumn or matrix or not used. Default is jsonRow")
    }
  }

  if ((is.null(accessKey) & !is.null(secretKey)) | (is.null(secretKey) & !is.null(accessKey))){
    stop("Both access and secret key parameters must have values or not be used")
  }
}

#'
#' @export

return_data <- function(output_format, jsonResponse){
  if (output_format %in% c("matrix", "jsonRow")){
    return(jsonResponse[[1]])
  }
  else{
    return(jsonResponse)
  }
}

#'
#' @export

data <- function(file_id, output_format=NULL, accessKey=NULL, secretKey=NULL){
  base_url <- "https://api-woyera.com/api/data/"

  check_paramter_errors(output_format, accessKey, secretKey)

  if (is.null(output_format)){
    output = "jsonRow"
  }
  else{
    output = output_format
  }

  url <- paste(base_url, as.character(file_id), "/", output, "/", sep="")

  if (is.null(accessKey) & is.null(secretKey)){
    r <- httr::GET(url=url)
    get_status_code <- httr::status_code(r)

    if (get_status_code == 200){
      jsonResponse <- jsonlite::fromJSON(httr::content(r, "text"))
      return (return_data(output, jsonResponse))
    }
    else if (get_status_code == 400){
      stop("The file is private. Please provide access and secret keys")
    }
    else if (get_status_code == 404){
      stop("This file ID does not exist. Please find the correct ID from your dashboard")
    }
    else{
      stop("There was something wrong on our server. Try again or contact us at info@apispreadsheets.com if the problem persists")
    }

  }
  else{
    headers <- c('accessKey' = accessKey, 'secretKey' = secretKey)
    r <- httr::POST(url=url, add_headers(headers), encode="json")

    post_status_code = httr::status_code(r)

    if (post_status_code == 200){
      jsonResponse <- jsonlite::fromJSON(httr::content(r, "text"))
      return (return_data(output,jsonResponse))
    }
    else if (post_status_code == 401){
      stop("The Access or Secret key is invalid")
    }
    else if (post_status_code == 404){
      stop("This file ID does not exist or is not your file. Please find the correct ID from your dashboard")
    }
    else{
      stop("There was something wrong on our server. Try again or contact us at info@apispreadsheets.com if the problem persists")
    }
  }
}

## df <- jsonResponse[[1]]

### RESPONSE WITH PARAM: rows
# when getting a 2D response, it turns into a list of 1 element where each element is a matrix
# in the library get the matrix outside of the list
# code to access the first element which is a matrix and the [1,1] element with the matrix
# print(jsonResponse[[1]][1,1])

### RESPONSE WITH PARAM: rowJSON
# same as above, when getting a resposnse the JSON converts to a list and the first element of that is a dataframe

### RESPONSE WITH PARAM: jsonColumn
# nothing to do, response is a list with vectors of all column
