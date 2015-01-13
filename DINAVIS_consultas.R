setwd("C:/IAvH/DINAVIS datos/DINAVIS/")

(tables <- list.files(pattern = "RData"))
lapply(tables, load)

sp <- c("Ara macao","Ateles hybridus")

buscarSP <- function(sp){
  sp_ <- gsub(" ","",sp)
  pos <- c(which(TAX$speciesBlank%in%sp_, TAX$scientificName%in%sp))
  (pos.2 <- sapply(sp, FUN = function(x){
    pos <- grep(x,TAX$scientificName)
    return(pos)
  }))
  pos <- c(pos,unlist(pos.2))
  (pos.3 <- sapply(sp_, FUN = function(x){
    pos <- grep(x,TAX$speciesBlank)
    return(pos)
  }))
  pos <- unique(c(pos,unlist(pos.2),unlist(pos.3)))
  IDs <- TAX$InternalID[pos]
  return(IDs)
}


SP <- c("a","b","bb","c","cc","d","dd","e","ee","f","ff","g","gg","h","hh","i","ii","j","jj","k","kk","l","ll","cc")
sp <- c("c","d")

a <- Sys.time()
(c1 <- sapply(sp, FUN = function(x){
  pos <- grep(x,SP)
  return(pos)
}))
Sys.time() - a

a <- Sys.time()
pos <- NULL
for (sp1 in 1:length(sp)){
  pos <- c(pos,grep(sp[sp1],SP))
}
Sys.time() - a
