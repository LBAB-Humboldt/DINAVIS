library(maptools)
library(sp)

#Unir tablas taxonomicas
setwd("C:/IAvH/DINAVIS datos/Tax")

load("BIOTA_Tax_2014-05-30.RData")
tax.biota <- full.tax; rm(full.tax)

load("IUCN_Tax_2014-06-03.RData")
tax.IUCN <- tax.table; rm(tax.table)

load("set16_Tax_2014-06-05.RData")
tax.set16 <- tax.table; rm(tax.table) 
  
colnames(tax.biota)
colnames(tax.IUCN)
colnames(tax.set16)

TAX <- rbind(tax.biota, tax.IUCN, tax.set16)
rm(tax.biota, tax.IUCN, tax.set16)

TAX$speciesBlank <- gsub(" ","",TAX$scientificName)
TAX$InternalID <- 1:nrow(TAX)


n.sp.inicial <- nrow(TAX)
uniquesp <- duplicated(TAX$speciesBlank)
TAX <- TAX[!uniquesp,]; rm(uniquesp)

TAXID <- TAX[,c("InternalID","speciesBlank")]

# Geo
setwd("C:/IAvH/DINAVIS datos/Geo")

load("BIOTA_Geo_2014-05-30.RData")
geo.biota <- full.geo; rm(full.geo)

load("set16_Tax_2014-06-04.RData")
geo.set16 <- geo.table; rm(geo.table)

GEO <- rbind(geo.set16)
GEO$speciesBlank <- gsub(" ","",GEO$scientificName)
GEO$IDGeo <- 1:nrow(GEO)
rm(geo.biota,geo.set16)
coords <- GEO[,c("decimalLongitude","decimalLatitude", "scientificName")]; head(coords)
coordinates(GEO)
eco <- readShapePoly("C:/IAvH/Chicharrones/EspeciesEndemicasMotoresReporte/Vias/am_ecosistemaMAGNAS.shp")
ws <- readShapePoly()
ANP <- readShapePoly()
AICAS <- readShapePoly()
dep <- readShapePoly()
mun <- readShapePoly()

# Medidas
setwd("C:/IAvH/DINAVIS datos/Med/")
load("BIOTA_Med_2014-05-30.RData")
MED <- full.med; rm(full.med)
MED$speciesBlank <- gsub(" ","",MED$species)
MED$IDMed <- 1:nrow(MED)
head(MED)
#Mapas
setwd("C:/IAvH/DINAVIS datos/Map/")
load("BioModelos&IUCN_Map_2014-05-30.RData")
MAP$speciesBlank <- gsub(" ","",MAP$scientificName)
MAP$IDMap <- 1:nrow(MAP)
#MAP <- read.csv("BioModelos&IUCN_Map_2014-05-30.csv");MAP <- MAP[-c(1),];fi <- sapply(MAP, is.factor);MAP[fi] <- lapply(MAP[fi], as.character);MAP$scientificName <- paste0(MAP$scientificName,"__");MAP$scientificName <- gsub("__","",gsub(" __","",gsub("  __","",MAP$scientificName)));save(MAP, file = "BioModelos&IUCN_Map_2014-05-30.RData")

# Asignar InternalID
head(MAP);tail(MAP)
MED <- merge(MED, TAXID, by = "speciesBlank", all.x = T, sort = FALSE)
GEO <- merge(GEO, TAXID, by = "speciesBlank", all.x = T, sort = FALSE)
MAP <- merge(MAP, TAXID, by = "speciesBlank", all.x = T, sort = FALSE)
head(GEO);tail(MAP)


# Guardar versiones de las tablas
setwd("C:/IAvH/DINAVIS datos/DINAVIS/")
save(GEO, file=paste0("GEO_",as.Date(Sys.Date()),".RData"))
save(MED, file=paste0("MED_",as.Date(Sys.Date()),".RData"))
save(TAX, file=paste0("TAX_",as.Date(Sys.Date()),".RData"))
save(MAP, file=paste0("MAP_",as.Date(Sys.Date()),".RData"))