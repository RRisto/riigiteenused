#abifunktsioon kõikide kanalite pikaks tegemiseks
#' @export
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
  sms=meltimine("Tekstis", data=andmed) #shiny jaoks vaja
  #sms=meltimine("Tekstisõnum.", data=andmed) #shiny jaoks vaja
  #sms=meltimine(enc2utf8("Tekstisõnum."), data=andmed)
  telefon=meltimine("Telefon.", data=andmed)
  faks=meltimine("Faks.", data=andmed)
  post=meltimine("Post.", data=andmed)
  lett=meltimine("Letiteenus b", data=andmed) #shiny jaoks vaja
  #lett=meltimine("Letiteenus büroos.", data=andmed) #shiny jaoks vaja
  #lett=meltimine(enc2utf8("Letiteenus büroos."), data=andmed)
  kodus=meltimine("Kliendi juures.", data=andmed)
  #rbindime
  koos=rbind(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks,
             post, lett, kodus)
  #leiame kanali ja näitaja
  #stati saamiseks eemaldame punktid kanali nimedest
  koos$variable=gsub(".ee.", ".", as.character(koos$variable), fixed=T)
  koos$variable=gsub("Letiteenus büroos", "Letiteenus", as.character(koos$variable), fixed=T)
  #koos$variable=gsub(enc2utf8("Letiteenus büroos"), "Letiteenus", as.character(koos$variable), fixed=T)
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

#sama, mis eelmine funktsioon, kuid kasutab data.table'i fomraati
korrastajaDT=function(andmed, eemalda, mootmiseAasta) {
  library(data.table)
  #eemalda - mis osa columnite nimedest tuleb eemdalda
  setnames(andmed, names(andmed), gsub(eemalda, "", names(andmed)))
  #kanalite lõikes meldime
  veeb=meltimineDT("Veebileht / portaal.", data=andmed)
  iseteen=meltimineDT("E-iseteenindus.", data=andmed)
  eesti=meltimineDT("Eesti.ee.", data=andmed)
  nuti=meltimineDT("Nutirakendus.", data=andmed)
  digitv=meltimineDT("Digitelevisioon.", data=andmed)
  epost=meltimineDT("E-post.", data=andmed)
  sms=meltimineDT("Tekstisõnum.", data=andmed)
  telefon=meltimineDT("Telefon.", data=andmed)
  faks=meltimineDT("Faks.", data=andmed)
  post=meltimineDT("Post.", data=andmed)
  lett=meltimineDT("Letiteenus.", data=andmed)
  kodus=meltimineDT("Kliendi juures.", data=andmed)

  #rbindime
  koos=rbindlist(list(veeb, iseteen, eesti, nuti, digitv, epost, sms, telefon, faks,
                      post, lett, kodus))

  #eemaldame kanali ja näitaja ning paneme eraldi veergu
  if (length(koos)==0) {
    return(NULL)
  } else {
    koos[,variable:=gsub(".ee.", ".", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Letiteenus büroos", "Letiteenus", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("E-iseteenindus", "Eiseteenindus", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("E-post", "Epost", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Veebileht / portaal", "Veebileht", as.character(koos[,variable]), fixed=T)]
    koos[,variable:=gsub("Kliendi juures", "Kliendijuures", as.character(koos[,variable]), fixed=T)]

    koos[, c("kanal", "naitaja") := tstrsplit(as.character(koos[["variable"]]), "\\.(?=[^\\.]+$)", perl=T)]
    koos[,kanal:=gsub("^.*\\.", "", koos[, kanal])]
    #viskame välja tühjad read, kus pole linki
    koos=koos[link!="NA"]
    koos[,MootmiseAasta:=mootmiseAasta]
    koos
  }
}
