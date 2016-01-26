#andmete downloadi abifunktsioon
andmedSisse=function(url) {
  #loeme andmed sisse
  library(jsonlite)
  #library(data.table)
  andmed=fromJSON(readLines(url), flatten=T)
  andmed=andmed["teenuste_kanalid_ja_moodikud"!="list()"]
  andmedMoodik <- rbindlist(lapply(andmed[["teenuste_kanalid_ja_moodikud"]], function(x) {
    as.list(unlist(x))
  }), fill=TRUE)
  andmed
}

#abifunktsioon, mis teeb laiast pika formaadi
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

#abifunktsioon kõikide kanalite pikaks tegemiseks
korrastaja=function(andmed, eemalda, mootmiseAasta) {
  library(reshape2)
  #eemalda - mis osa columnite nimedest tuleb eemdalda
  names(andmed)=gsub(pattern=eemalda,"" ,names(andmed))
  #kanalite lõikes meldime
  veeb=meltimine("Veebileht / portaal.", data=andmed)
  iseteen=meltimine("E-iseteenindus.", data=andmed)
  eesti=meltimine("Eesti.ee.", data=andmed)
  nuti=meltimine("Nutirakendus.", data=andmed)
  digitv=meltimine("Digitelevisioon.", data=andmed)
  epost=meltimine("E-post.", data=andmed)
  sms=meltimine("Tekstisõnum.", data=andmed) #shiny jaoks vaja
  telefon=meltimine("Telefon.", data=andmed)
  faks=meltimine("Faks.", data=andmed)
  post=meltimine("Post.", data=andmed)
  lett=meltimine("Letiteenus büroos.", data=andmed) #shiny jaoks vaja
  kodus=meltimine("Kliendi juures.", data=andmed)
  #rbindime
  koos=rbind(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks,
             post, lett, kodus)
  #leiame kanali ja näitaja
  #stati saamiseks eemaldame punktid kanali nimedest
  koos$variable=gsub(".ee.", ".", as.character(koos$variable), fixed=T)
  koos$variable=gsub("Letiteenus büroos", "Letiteenus", as.character(koos$variable), fixed=T)
  koos$variable=gsub("E-iseteenindus", "Eiseteenindus", as.character(koos$variable), fixed=T)
  koos$variable=gsub("E-post", "Epost", as.character(koos$variable), fixed=T)
  koos$variable=gsub("Veebileht / portaal", "Veebileht", as.character(koos$variable), fixed=T)
  stat=gsub("Kliendi juures", "Kliendijuures", as.character(koos$variable), fixed=T)

  #lõikame punktini asja maha
  stat <- strsplit(stat, split ="\\.")
  #teeme df-ks
  df=as.data.frame(stat)
  #transponeerime
  df=as.data.frame(t(df))
  #lisame algsesse andmestikku
  koos$kanal=df[,2]
  koos$naitaja=df[,3]
  #teeme numbriliseks
  koos$value=as.numeric(as.character(koos$value))
  koos$MootmiseAasta=mootmiseAasta
  koos
}

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

###näide
#andmedLai=andmedSisse("https://www.riigiteenused.ee/api/et/all")
#andmedPikk=andmedPikaks(andmedLai)

