#andmete downloadi abifunktsioon
#' @export
andmedSisse=function(url) {
  #loeme andmed sisse
  library(jsonlite)
  fromJSON(readLines(url), flatten=T)
}


