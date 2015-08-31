##Mandatberegning 
#St. Laguës modifiserte metode
# En kolonne med partier, en kolonne med resultater
# Bruk .xslx-filer for å få rett æ, ø og å

setwd("~/Documents/Valg 15")
library(gdata)
source('bokstavfix.r')

lague <- function(antall.mandater) {
  ting <- read.xls('liste.xlsx', header=FALSE, stringsAsFactors=FALSE)
  ting[,1] <- fix(ting, 1)
  ting[,2] <- as.numeric(ting[,2])
  ting <- ting[,1:2]
  ant <- antall.mandater

  man <- matrix(nrow=length(ting[,2]), ncol=ant)
  man[,1] <- ting[,2]/1.4
  o <- 3
    for (i in 2:ant) {
    man[,i] <- ting[,2]/o
    o <- o+2
    }
  man <- data.frame(man)
  ting <- as.data.frame(append(ting,man))

  # col1 lager en liste med repeterende partinavn, like lang som antall tall i matrisen.
  # col2 fylles med tall fra matrisen. 
    col1 <- rep(ting[,1],ncol(ting)-2)
    col2 <- c()


    for(i in 3:ncol(ting)) {
        col2 <- c(col2,ting[,i])
        }

  # man er en data frame satt sammen av listene col1 og col2.
  # deretter sorterer vi 'man' i synkende rekkefølge og klipper den etter antall 
  # forekomster bestemt av dman.
  # dman = distriktsmandater, mandater minus utjevningsmandater

  man <- data.frame(parti=col1,deltall=col2, stringsAsFactors=FALSE)
  man <- man[order(as.numeric(man[,2]),decreasing=TRUE),]
  neste <- man[ant+1,]
  siste <- man[ant,]
  man <- man[1:ant,]
  rownames(man) <- 1:ant
  
  ting <- table(t(man))

  mandater <- function(x) { tail(ting, n = x) }

  #print('mandater(x), der x er antall partier, gir mandatlisten.')

  tabell <- tapply(man$deltall, man$parti, length)
  tabell <- as.data.frame(tabell)
  tabell[is.na(tabell)] <- 0
  
  #print(man)
  print(tabell)	
  cat('Siste: ')
  print(siste)
  cat('Neste: ')
  print(neste)
}