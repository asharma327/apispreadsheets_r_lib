#'
#' @param output_format
#' @export

# devtools::document()

#'
#' @export

get_base_url <- function(){
  # return("http://127.0.0.1:8000/data/")
  return("https://api.apispreadsheets.com/data/")
}

#'
#' @export

get_output_format <- function(output_format){
  if (is.null(output_format)){
    return("jsonRow")
  }
  else{
    return(output_format)
  }
}

#'
#' @export

create_get_url <- function(url, output_format, access_key, secret_key, rows){
  if (!is.null(access_key) & !is.null(secret_key)){
    new_url = paste(url, "?accessKey=", access_key, "&secretKey=", secret_key, sep="")
    data_prefix = "&"
  }
  else{
    new_url = url
    data_prefix = "?"
  }
    
  data_query_param = paste(data_prefix, "dataFormat=", output_format, sep="")
  output_url = paste(new_url, data_query_param, sep="")
  
  final_url = output_url
  
  if (!is.null(rows)){
    if (grepl('?', url)){
      final_url = paste(output_url, "&rows=", as.character(rows), sep="")
    }
    else{
      final_url = paste(output_url, "?rows=", as.character(rows), sep="")
    }
  }
    
  return(final_url)
}

#'
#' @export

create_post_headers <- function(access_key, secret_key){
  headers <- c()
  
  if (!is.null(access_key) & !is.null(secret_key)){
    headers <- c('accessKey' = access_key, 'secretKey' = secret_key)
  }
  
  return(headers)
}

#'
#' @export

check_paramter_errors <- function(output_format, accessKey, secretKey, rows=NULL, data=NULL){
  if(!is.null(output_format)){
    if(!(output_format %in% c('jsonRow', 'jsonColumn', 'matrix'))){
      stop("Output format must be jsonRow, jsonColumn or matrix or not used. Default is jsonRow")
    }
  }
  
  if ((is.null(accessKey) & !is.null(secretKey)) | (is.null(secretKey) & !is.null(accessKey))){
    stop("Both access and secret key parameters must have values or not be used")
  }
  
  if (!is.null(rows)){
    if(is.na(as.numeric(rows))){
      stop("Rows parameter must be an integer")
    }
  }
  
  if (!is.null(data)){
    if (nrow(data) == 0){
      stop("Data must not be empty")
    }
  }
}

#'
#' @export

post_data <- function(file_id, data_format=NULL, access_key=NULL, secret_key=NULL, data=NULL){
  base_url <- get_base_url()
  
  check_paramter_errors(data_format, access_key, secret_key, data=data)
  
  output <- get_output_format(data_format)
  
  url <- paste(base_url, as.character(file_id), "/", sep="")
  headers <- create_post_headers(access_key, secret_key)

  r <- httr::POST(url=url, httr::add_headers(headers), body=list(format = output, data=data), encode="json")
  
  post_status_code <- httr::status_code(r)
  
  return(post_status_code)
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

get_data <- function(file_id, data_format=NULL, access_key=NULL, secret_key=NULL, rows=NULL){
  base_url <- get_base_url()

  check_paramter_errors(data_format, access_key, secret_key, rows=rows)
  
  output <- get_output_format(data_format)
  
  id_url <- paste(base_url, as.character(file_id), "/", sep="")
  url <- create_get_url(id_url, output, access_key, secret_key, rows)
  
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
    stop("There was something wrong on our server. Try again or contact us at support@apispreadsheets.com if the problem persists")
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
