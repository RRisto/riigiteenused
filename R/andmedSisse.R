#andmete downloadi abifunktsioon
andmedSisse=function(url) {
  #loeme andmed sisse
  library(jsonlite)
  library(data.table)
  andmed=fromJSON(readLines(url), flatten=T)
  andmed=andmed["teenuste_kanalid_ja_moodikud"!="list()"]
  andmedMoodik <- rbindlist(lapply(andmed[["teenuste_kanalid_ja_moodikud"]], function(x) {
    as.list(unlist(x))
  }), fill=TRUE)
  andmed
}


