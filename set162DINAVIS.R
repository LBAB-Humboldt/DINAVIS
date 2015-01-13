library(dismo)
library(maptools)
library(sp)
library(maps)

rm(list=ls())  # Borra  todo loq ue etse en memoria
gc()
memory.limit(size = 1000000) 

setwd("C:/IAvH/DINAVIS datos/Fuentes/set16")
load("W:/Ocurrencias/Registros_2014-07-30.RData")
source("C:/IAvH/DINAVIS_set16/scripts/tools.R")
set16 <- set16[which(set16$enCol == 1),]


#Cargar datos
names.format <- read.csv("Columnas.csv")
names.format.geo <- read.csv("Columnas_geo.csv")
formato.tax <- read.csv("Formato_Taxonom.csv")
formato.geo <- read.csv("Formato_Geo.csv")

#Definr campos para uno y otro
colnames(set16)
tax.col <- colnames(set16)%in%c("ID","source","species","speciesOriginal","occurrenceID","Nombre","genero",
                                "epiteto_especifico","es_aceptadoCoL","id_registro_CoL","id_familia_CoL","id_nombre_CoL",
                                "id_nombre_aceptado","nombre_aceptado","autor_nombre_aceptado","genero_aceptado","genero","epiteto_especifico",
                                "epiteto_aceptado","familia_CoL","orden_CoL","clase_CoL","phylum_CoL","reino_CoL","especie_aceptada")

nogeo.col <- colnames(set16)%in%c("Nombre","genero","epiteto_especifico","es_aceptadoCoL","id_registro_CoL","id_familia_CoL","id_nombre_CoL",
                                "id_nombre_aceptado","nombre_aceptado","autor_nombre_aceptado","genero_aceptado","genero","epiteto_especifico",
                                "epiteto_aceptado","familia_CoL","orden_CoL","clase_CoL","phylum_CoL","reino_CoL")

tax <- set16[,tax.col]
head(tax);unique(tax$source)
unique.sp2 <- paste(tax$species,tax$speciesOriginal,tax$especie_aceptada);head(unique.sp2)
unique.sp.ID <- duplicated(unique.sp2,fromLast=TRUE); sum(!unique.sp.ID)
tax <- tax[which(!unique.sp.ID),]
rm(unique.sp2, unique.sp.ID)

geo <- set16[,-which(nogeo.col)];head(geo)
rm(set16)

# Preparar datos taxonomicos
colnames.original <- colnames(tax)
colnames.final <- NULL

# Volver columnas de factor a texto
fi <- sapply(tax, is.factor);tax[fi] <- lapply(tax[fi], as.character)

# Homologacion de nombres de campos
c <- 23
for (c in 1:length(colnames.original)){
  (name.c <- colnames.original[c])
  (pos.c <- which(names.format$Original==name.c)[1])
  (name.f <- names.format$Equivalente[pos.c][1])
  colnames.final <- c(colnames.final,as.character(name.f))
};colnames.final

head(tax)
unique(tax$source)
colnames(tax) <- colnames.final
tax.table <- orig2set16(tax, format = formato.tax, fuente = tax$source, occID = tax$occurrenceID, cleanscinames = F); head(tax.table)
unique(tax.table$source)
save(tax.table, file = paste0("set16_Tax_",as.Date(Sys.Date()),".RData"))
file.copy(paste0("set16_Tax_",as.Date(Sys.Date()),".RData"),paste0("C:/IAvH/DINAVIS datos/Tax/set16_Tax_",as.Date(Sys.Date()),".RData"))
#write.csv(tax.table, paste0("set16_Tax_",as.Date(Sys.Date()),".csv"), row.names = FALSE)


# Generar el nivel de validación geográfico del LBAB
geo$bienPais[is.na(geo$bienPais)] <- 0;geo$bien_depto[is.na(geo$bien_depto)] <- 0; geo$bien_muni[is.na(geo$bien_muni)] <- 0
georeferenceVerificationStatus <- (geo$bienPais+geo$bien_depto+geo$bien_muni)
colnames.original <- colnames(geo)
colnames.final <- NULL

# Volver columnas de factor a texto
fi <- sapply(geo, is.factor)
geo[fi] <- lapply(geo[fi], as.character)

# Homologaci?n de nombres de campos
c <- 5
for (c in 1:length(colnames.original)){
  (name.c <- colnames.original[c])
  (pos.c <- which(names.format.geo$Original==name.c)[1])
  (name.f <- names.format.geo$Equivalente[pos.c][1])
  colnames.final <- c(colnames.final,as.character(name.f))
};colnames.final

head(geo)
colnames(geo) <- colnames.final
geo$geoValidationLBAB <- georeferenceVerificationStatus; rm(georeferenceVerificationStatus)
geo$eventDate <- paste0(geo$eventDate,"/",geo$eventDate2)
geo.table <- orig2set16(geo, format = formato.geo, fuente = geo$source, occID = geo$occurrenceID, cleanscinames = F); head(geo.table)
geo.table$originalNameUsage <- geo$originalNameUsage

colnames(geo.table)
save(geo.table, file = paste0("set16_Geo_",as.Date(Sys.Date()),".RData"))
file.copy(paste0("set16_Geo_",as.Date(Sys.Date()),".RData"),paste0("C:/IAvH/DINAVIS datos/Geo/set16_Tax_",as.Date(Sys.Date()),".RData"))
#write.csv(geo.table, paste0("set16_Geo_",as.Date(Sys.Date()),".csv"), row.names = FALSE)