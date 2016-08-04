#abifunktsioon, mis teeb laiast pika formaadi
#töötab data.table' formaadiga
#' @export
meltimine=function(kanal, data) {
  sub=data[,grepl(paste(kanal, "|identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse", sep=""),
                  names(data)), with=F]

  id=grep(c("identifikaator|ministeerium|allasutus|tegevusvaldkond|teenusetyyp|makse|link"), names(sub), value=T)
  if(length(id)<7) {
    tulem=NULL
  } else {
    #meldime andmed kitsaks
    tulem=reshape2::melt(sub, id=id)
    #muudan variable nime ära, mis on kanalispets, muidu ei saa rbindida
    lingiNimi=names(tulem)[7]
    setnames(tulem, old=lingiNimi, new=c("link"))
  }
  tulem
}
