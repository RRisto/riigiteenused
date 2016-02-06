#' andmete downloadi abifunktsioon
#' @export
andmedSisse=function(url) {
  #loeme andmed sisse
  library(rjson)
  library(jsonlite)
  fromJSON(url, flatten=T)
}


