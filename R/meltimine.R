#abifunktsioon, mis teeb laiast pika formaadi
#' @export
meltimine=function(kanal, data) {
  library(reshape2)
  #leiame ainult seda kanalit puudutavad muutujad
  sub=data[, grepl(paste(kanal, "|identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse", sep=""), names(data))]
  #määran id-d, mis meltimisel meltimata jäävad
  id=grep(c("identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse|link"), names(sub), value=T)
  #kui selle kanali kohta stati pole, anna vastuseks null
  if(length(id)<7) {
    tulem=NULL
  } else {
    #meldime andmed kitsaks
    tulem=melt(sub, id=id)
    #muudan variable nime ära, mis on kanalispets, muidu ei saa rbindida
    names(tulem)=c("identifikaator", "tegevusvaldkond", "tyyp", "ministeerium", "allasutus",
                   "makse", "link",  "variable",
                   "value")
  }
  tulem
}

#sama mis eelmine funktsioon, kuid töötab data.table' formaadiga
#' @export
meltimineDT=function(kanal, data) {
  library(data.table)
  sub=data[,grepl(paste(kanal, "|identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse", sep=""),
                  names(data)), with=F]

  id=grep(c("identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse|link"), names(sub), value=T)
  if(length(id)<7) {
    tulem=NULL
  } else {
    #meldime andmed kitsaks
    tulem=melt(sub, id=id)
    #muudan variable nime ära, mis on kanalispets, muidu ei saa rbindida
    lingiNimi=names(tulem)[7]
    setnames(tulem, old=lingiNimi, new=c("link"))
  }
  tulem
}
