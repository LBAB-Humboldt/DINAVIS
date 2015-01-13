setwd("C:/IAvH/DINAVIS datos/DINAVIS/")
load("GEO_2014-06-05.RData")
load("MAP_2014-06-05.RData")
load("MED_2014-06-05.RData")
load("TAX_2014-06-05.RData")

buscarSP <- function(sp, out.dir = NULL){
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
  indexTAX <- which(TAX$InternalID%in%IDs)
  indexGEO <- which(GEO$InternalID%in%IDs)
  indexMED <- which(MED$InternalID%in%IDs)
  indexMAP <- which(MAP$InternalID%in%IDs)
  out <- list(IDs,indexTAX,indexGEO,indexMED,indexMAP); names(out) <- c("InternalID","indexTAX","indexGEO","indexMED","indexMAP")
  if (!is.null(out.dir)){
    try(write.csv(TAX[indexTAX,], paste0(out.dir,"/Taxonomic.csv"), row.names = FALSE))
    try(write.csv(GEO[indexGEO,], paste0(out.dir,"/Records.csv"), row.names = FALSE))
    try(write.csv(MED[indexMED,], paste0(out.dir,"/Measurements.csv"), row.names = FALSE))
    try(write.csv(MAP[indexMAP,], paste0(out.dir,"/Maps.csv"), row.names = FALSE))
  }
  return(out)
}

busqueda <- buscarSP(c("Ara macao","Ateles hybridus","Ara ararauna","Carollia perspicillata"), out.dir ="C:/IAvH/DINAVIS datos/Pruebas")

str(busqueda)
TAX[busqueda$indexTAX,]
GEO[busqueda$indexGEO,]
MED[busqueda$indexMED,]
MAP[busqueda$indexMAP,]