#andmed pikaks, võtab eelenvad funktsioonid kokku, iga
#aasta andmed teeb pikaks, kasutab data.table'i formaati
#' @export
andmedPikaks=function(andmedLai) {
  andmed=data.table(andmedLai)
  #mõõtmiste aastate väärtused
  aastad=c("2011", "2012","2013","2014","2015", "empty")
  #col nimede list, kus iga mõõtmise aasta kohta on vajalikud andmed
  colNimed=list()
  for(i in 1:length(aastad)) {
    colNimed[[i]]=c(grep(paste(paste0(aastad[-i],"."), collapse = "|"),
                                   names(andmed), value=T, invert=T))
  }
  #mapply eri aastate andmed listi
  andmedLaiList=mapply(function(colNimed, andmed)
    andmed[, colNimed, names(andmed), with=F],colNimed,
    MoreArgs=list(andmed=andmed))
  names(andmedLaiList)=aastad#lisit elementidele nimed
  #lisa mõõtmise aasta muutuja
  andmedLaiList=Map(cbind, andmedLaiList, year = as.list(aastad))
  #lapply läbi korrastaja
  andmedLaiList <- lapply(andmedLaiList,
                          function(df) {
                            korrastaja(andmed=df, mootmiseAasta=df$year[1])
                          })
  #tee üheks data frame/table-ks
  andmedPikk=rbindlist(andmedLaiList, fill=TRUE)
  andmedPikk[, value:=as.numeric(as.character(value))]#tee characteriks
  andmedPikk[, MootmiseAasta:=gsub("empty", "pole moodetud", MootmiseAasta)]
}
