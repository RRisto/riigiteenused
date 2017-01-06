#andmed pikaks, võtab eelnevad funktsioonid kokku, iga
#aasta andmed teeb pikaks
#' @export
andmedPikaks=function(andmedLai) {
  tulem=data.frame(NULL)
  for (i in 1:nrow(andmedLai)) {
    sub=andmedLai$serviceStatistics[i]
    aastad=sub[[1]][2]
    teenustulem=data.frame(NULL)
    for (j in 1:nrow(aastad)) {
      abikas=sub[[1]][3]$availableChannel[[j]]
      moodikud=abikas$channelStatistics
      taisrida=cbind(moodikud, abikas[, c("url", "additionalType", "payment")])
      taisrida=cbind(taisrida, andmedLai[i, c("domain", "subdomain", "serviceType",
                                              "provider.name", "provider.memberOf.name")])
      taisrida$aasta=aastad$value[j]
      teenustulem=rbind(teenustulem, taisrida)
    }
    teenustulem$id=andmedLai$identifier[i]
    tulem=rbind(tulem, teenustulem)
  }

  library(reshape2)
  tulem$'@type'=NULL
  #nimetame veerud ümber
  names(tulem)[names(tulem)=="aasta"] <- "MootmiseAasta"
  names(tulem)[names(tulem)=="payment"] <- "makse"
  names(tulem)[names(tulem)=="id"] <- "identifikaator"
  names(tulem)[names(tulem)=="domain"] <- "valdkond"
  names(tulem)[names(tulem)=="subdomain"] <- "alamvaldkond"
  names(tulem)[names(tulem)=="serviceType"] <- "teenusetyyp"
  names(tulem)[names(tulem)=="provider.memberOf.name"] <- "ministeerium"
  names(tulem)[names(tulem)=="provider.name"] <- "allasutus"
  names(tulem)[names(tulem)=="additionalType"] <- "kanal"
  names(tulem)[names(tulem)=="transaction"] <- "osutamistearv"
  names(tulem)[names(tulem)=="satisfaction"] <- "rahulolu"
  names(tulem)[names(tulem)=="cost"] <- "halduskulu"
  names(tulem)[names(tulem)=="activeTimeSpent"] <- "ajakulu"
  names(tulem)[names(tulem)=="totalTimeSpent"] <- "ajakuluBruto"

  pikk=melt(tulem, measure.vars=c("osutamistearv", "rahulolu", "halduskulu",
                                  "ajakulu", "ajakuluBruto"))
  names(pikk)[names(pikk)=="variable"] <- "naitaja"
  #kus on müüdik puudu paneme NA
  pikk$value=ifelse(pikk$value=="", NA, pikk$value)
  pikk
}
