### Fiks æ, ø og å
fix <- function(tabell,kol) {
  tabell[,kol] <- gsub("\xf8", "ø", tabell[,kol])
  tabell[,kol] <- gsub("\xd8", "Ø", tabell[,kol])
  tabell[,kol] <- gsub("\xe6", "æ", tabell[,kol])
  tabell[,kol] <- gsub("\xe5", "å", tabell[,kol])
  tabell[,kol] <- gsub("\xc5", "Å", tabell[,kol])
  }