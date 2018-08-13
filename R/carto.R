#' Get data from the City's Carto API
#'
#' Philadelphia's open data is published to carto, which allows you to query the
#' data using SQL (specifically the PostgreSQL flavor with the PostGIS
#' extension). Rather than executing the queries in a database program, they are
#' executed through HTTP (ie. the web browser) via Carto's SQL API.
#'
#' @param query character, SQL query
#' @param format character, desired response format, either "csv" (returned as
#'   data.frame) or "json" (parsed into list by jsonlite::fromJSON)
#' @param base_url character, API endpoint, defaults to Philadelphia's Carto SQL
#'   endpoint
#' @param stringsAsFactors logical, defaults to FALSE, applies to result if
#'   format = "csv"
#' @param ... other arguments passed either to read.csv or fromJSON, depending
#'   on format
#'
#' @examples
#'
#' query <- paste("SELECT service_name, COUNT(*) AS n",
#'                "FROM public_cases_fc",
#'                "GROUP BY service_name",
#'                "ORDER BY n DESC")
#'
#' get_carto(query)
#' get_carto(query, format = "json")
#'
#' @export

get_carto <- function(query,
                      format = "csv",
                      base_url = "https://phl.carto.com/api/v2/sql",
                      stringsAsFactors = F,
                      ...) {
  stopifnot(format %in% c("csv", "json"))
  url <- httr::parse_url(base_url)
  url$query <- list(q = query, format = format)
  if(format == "csv"){
    read.csv(httr::build_url(url),
             header = T,
             stringsAsFactors = stringsAsFactors,
             ...)
  } else {
    jsonlite::fromJSON(httr::build_url(url), ...)
  }
}
