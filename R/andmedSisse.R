#' andmete downloadi abifunktsioon
#' @export
andmedSisse=function(url="https://www.riigiteenused.ee/api/et/all") {
  library(rjson)
  library(jsonlite)
  #loeme andmed sisse
  fromJSON(url, flatten=T)
}


