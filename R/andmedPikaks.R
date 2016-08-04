#andmed pikaks, võtab eelenvad funktsioonid kokku, iga
#aasta andmed teeb pikaks, kasutab data.table'i formaati
#' @export
andmedPikaks=function(andmedLai) {
  andmed=data.table(andmedLai)
  #tee mõõtmise aasta järgi listiks
  andmedLaiList=list(
    andmedLai.2015=andmed[, !grepl("empty.|2011.|2013.|2012.|2014.",
                                names(andmed)), with=F],
  andmedLai.2014=andmed[, !grepl("empty.|2011.|2013.|2012.|2015.",
                                names(andmed)), with=F],
  andmedLai.2013=andmed[, !grepl("empty.|2011.|2012.|2014.|2015.",
                                names(andmed)), with=F],
  andmedLai.2012=andmed[, !grepl("empty.|2011.|2013.|2014.|2015.",
                                names(andmed)), with=F],
  andmedLai.2011=andmed[, !grepl("empty.|2014.|2013.|2012.|2015.",
                                names(andmed)), with=F],
  andmedLai.Empty=andmed[, !grepl("2014.|2011.|2013.|2012.|2015.",
                                 names(andmed)), with=F])

  years=gsub("[^0-9]","",names(andmedLaiList))#leiab aastad
  years=ifelse(nchar(years)==0, "empty", years)#tühi element asenda "empty"ga
  years=as.list(years)#tee listiks

  andmedLaiList=Map(cbind, andmedLaiList, year = years)#lisa mõõtmise aasta
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
