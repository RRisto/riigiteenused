#andmed pikaks, võtab eelenvad funktsioonid kokku, iga
#aasta andmed teeb pikaks

andmedPikaks=function(andmedLai) {
  vars=names(andmedLai) %in% c("regulatsioon", "objectId", "eluarisyndmus", "kirjeldus",
                               "eeltingimus", "jareltingimus", "createdAt",
                               "updatedAt", "keel", "osakondyksus", "omanikunimi",
                               "omanikutelefon","omanikuemail",
                               "konfinfo", "seotuddokumendid", "seisund",
                               "muudatustvajav", "aegumisekpv", "funktsioon",
                               "veebiaadress")
  #eemaldame muutujad
  andmedLai=andmedLai[,!vars]
  #aastate põhjal teeme andmed 2-ks (kui aastaid rohkem siis vastavalt sellele
  #arv muutub)
  andmedLai2015=andmedLai[, !grepl("empty.|2011.|2013.|2012.|2014.",
                                   names(andmedLai))]
  andmedLai2014=andmedLai[, !grepl("empty.|2011.|2013.|2012.|2015.",
                                   names(andmedLai))]
  andmedLai2013=andmedLai[, !grepl("empty.|2011.|2012.|2014.|2015.",
                                   names(andmedLai))]
  andmedLai2012=andmedLai[, !grepl("empty.|2011.|2013.|2014.|2015.",
                                   names(andmedLai))]
  andmedLai2011=andmedLai[, !grepl("empty.|2014.|2013.|2012.|2015.",
                                   names(andmedLai))]
  andmedLaiEmpty=andmedLai[, !grepl("2014.|2011.|2013.|2012.|2015.",
                                    names(andmedLai))]
  puhas2015=korrastaja(andmedLai2015, "2015.", "2015")
  puhas2014=korrastaja(andmedLai2014, "2014.", "2014")
  puhas2013=korrastaja(andmedLai2013, "2013.", "2013")
  puhas2012=korrastaja(andmedLai2012, "2012.", "2012")
  puhas2011=korrastaja(andmedLai2011, "2011.", "2011")
  puhasEmpty=korrastaja(andmedLaiEmpty, "empty.", "pole moodetud")
  #paneme andmed kokku
  andmedPikk=rbind(puhas2015, puhas2014,puhas2013,puhas2012, puhas2011,puhasEmpty)
  #eemaldame read, kus pole linki
  andmedPikk=andmedPikk[!is.na(andmedPikk$link),]
  andmedPikk
}