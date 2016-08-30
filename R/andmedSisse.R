#' andmete downloadi abifunktsioon
#' @export
andmedSisse=function(url="https://www.riigiteenused.ee/api/et/all") {
  #library(rjson)
  library(jsonlite)
  #loeme andmed sisse
  #fromJSON(url, flatten=T) ei funka
  lai=fromJSON(url, flatten=T)
  # lai=lai["teenuste_kanalid_ja_moodikud"!="list()",]#kus on tühi sealt eemaldab
  # moodik <- rbindlist(lapply(lai[["teenuste_kanalid_ja_moodikud"]], function(x) {
  #   as.list(unlist(x))
  # }), fill=TRUE)
  # lai$teenuste_kanalid_ja_moodikud=NULL
  # lai=cbind(lai,moodik)
  lai
}


