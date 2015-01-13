library(maptools)
library(sp)
library(rgdal)
library(raster)

rm(list = ls())  # Borra  todo lo que este en memoria
gc()
memory.limit(size = 1000000) 

#Unir tablas taxonomicas
setwd("C:/IAvH/DINAVIS datos/Tax")

load("BIOTA_Tax_2014-05-30.RData")
tax.biota <- full.tax; rm(full.tax)

load("IUCN_Tax_2014-06-03.RData")
tax.IUCN <- tax.table; rm(tax.table)
F
load("set16_Tax_2014-08-08.RData")
tax.set16 <- tax.table; rm(tax.table) 
  
colnames(tax.biota)
colnames(tax.IUCN)
colnames(tax.set16)

TAX <- rbind(tax.biota, tax.IUCN, tax.set16)
rm(tax.biota, tax.IUCN, tax.set16)


TAX$speciesBlank <- gsub(" ","",TAX$scientificName)
TAX$InternalID <- 1:nrow(TAX)

TAX.FULL <- TAX

n.sp.inicial <- nrow(TAX)
uniquesp <- duplicated(TAX$speciesBlank)
TAX <- TAX[!uniquesp,]; rm(uniquesp);dim(TAX);dim(TAX.FULL)

fi <- sapply(TAX, is.factor);TAX[fi] <- lapply(TAX[fi], as.character)

unique.sp <- unique(TAX.FULL$speciesBlank); length(unique.sp)
# unique <- c("occurrenceID","lsid","scientificNameID","scientificNameAuthorship","kingdom","phylum","class","order","family","genus","subgenus","specificEpithet","infraspecificEpithet","taxonRank","taxonConceptId")
# several <- c("occurrenceID","source","taxonRemarks","taxonomicStatus","vernacularName","reference","asocciatedReferences")
# 
# ej.taxa.for <- ej.taxa.app <- TAX.FULL[1:1000,]
# unique.sp2 <- unique(ej.taxa.app$speciesBlank); length(unique.sp2)
# 
# t <- Sys.time()
# for (u in 1:length(unique.sp2)){
#   (pos.full <- which(TAX.FULL$speciesBlank == unique.sp[u]))
#   (pos.u <- which(TAX$speciesBlank == unique.sp[u]))
#   if (length(pos.full)>1) {
#     set.TAX <- TAX.FULL[pos.full,]
#     uniqueValues <- sapply(set.TAX[,unique], 
#                            FUN = function(x){
#                              y <- unique(as.character(subset(x,!is.na(x)&x!="")))
#                              if (length(y)!=0){
#                                return (gsub("  "," ",gsub("  "," ",sort(y)[1])))
#                             } else {
#                               return ("")
#                             }
#                            }
#     )    
#     severalValues <- sapply(set.TAX[,several], 
#                             FUN = function(x){
#                               y <- unique(as.character(subset(x,!is.na(x)&x!="")))
#                               return (paste(unique(y), collapse = ", "))
#                             }
#     )
#     ej.taxa.for[pos.u,unique] <- t(uniqueValues)
#     ej.taxa.for[pos.u,several] <- t(severalValues)
#     cat (u, "de",length(unique.sp), "-", round(u/length(unique.sp),2),"% \n")
#   }
# }
# Sys.time() - t
# 
# 
# dif <- ej.taxa.for != ej.taxa.app
# index.dif <- which(dif,arr.ind= T)
# sum(ej.taxa.for != ej.taxa.app, na.rm = T)
# 
# app <- ej.taxa.app[unique(index.dif[,1]),unique(index.dif[,2])]
# for1 <- ej.taxa.for[unique(index.dif[,1]),unique(index.dif[,2])]
# 
# getwd()
# write.csv(ej.taxa.app, "PruebaAPP.csv")
# write.csv(ej.taxa.for, "PruebaFOR.csv")
# write.csv(app, "malPruebaAPP.csv")
# write.csv(for1, "malPruebaFOR.csv")

TAXID <- TAX[,c("InternalID","speciesBlank")]

# Geo
setwd("C:/IAvH/DINAVIS datos/Geo")

load("BIOTA_Geo_2014-05-30.RData")
geo.biota <- full.geo; rm(full.geo)

load("set16_Geo_2014-08-08.RData")
geo.set16 <- geo.table; rm(geo.table)

GEO <- rbind(geo.set16)
GEO$speciesBlank <- gsub(" ","",GEO$scientificName)
GEO$IDGeo <- 1:nrow(GEO)
rm(geo.biota,geo.set16)

fi <- sapply(GEO, is.factor);GEO[fi] <- lapply(GEO[fi], as.character)

coords <- GEO[,c("IDGeo","decimalLongitude","decimalLatitude", "scientificName")]; head(coords)
coords[, c("decimalLongitude","decimalLatitude")] <- lapply(coords[, c("decimalLongitude","decimalLatitude")], as.numeric)
coordinates(coords) =~ decimalLongitude+decimalLatitude
pr <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
pr2 <- "+proj=longlat +ellps=WGS84 +no_defs"
coords@proj4string@projargs <- pr2

# Arreglar capas, proyectarlas y renombrarlas

(capas <- list.files(path = "C:/IAvH/DINAVIS datos/Fuentes/Layers/Originales", pattern = ".shp$"))
c <- capas[1]
for (c in capas){
  layer.c <- readOGR(dsn = "C:/IAvH/DINAVIS datos/Fuentes/Layers/Originales", layer = gsub(".shp","",c))
  writeOGR(obj = spTransform(layer.c,CRSobj = CRS(pr)), dsn = "C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84",
           driver = "ESRI Shapefile", layer = gsub(".shp","",c),  overwrite_layer = TRUE)
}


#col <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers","Colombia_buffer_10km")
#eco <- readOGR("C:/IAvH/Chicharrones/EspeciesEndemicasMotoresReporte/Vias","am_ecosistemaMAGNAS")
pnn <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","pnn")
reg <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","reg")
mun <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","mun")
car <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","car")
hum <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","hum")
par <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","par")

# over.assignation
vars <- c("reg","mun","car","hum","par")
for (v in vars){
  assign(paste0("over.",v),eval(parse(text = paste0("over(coords,",v,")"))))
}


vars2 <- rbind (c("sigRegion", "reg$NOM_REGIÃ³N"),
       c("sigRegionSub", "reg$NOM_SUBREG"),
       c("sigDep", "mun$DEPARTAMEN"),
       c("sigMun", "mun$NOMBRE_ENT"),
       c("sigCAR", "car$NOMCAR"),
       c("sigHUM", "hum$HUNOMBRE"),
       c("sigPARSect", "par$PASECTOR"),
       c("sigPARDist", "par$PADISTRITO"),
       c("sigPARComp", "par$PACOMPLEJO"))
for (v2 in 1:nrow(vars2)){
  assign(paste0(vars2[v2,1]),eval(parse(text = paste0("as.character(over.",vars2[v2,2],")"))))
}
assign("GEO",eval(parse(text = paste0("cbind(GEO,",paste(ls(pattern = "sig"), collapse = ", "),")"))))

#GEO$sigRegion <- as.character(over.reg$NOM_REGIóN)

# Genero listados unicos
for (v3 in vars2[,1]){
  assign(paste0("u.",v3), eval(parse(text = paste0("as.character(unique(",v3,"))"))))
}


for (v4 in 1:nrow(vars4)){
  assign("richness", eval(parse(text = paste0("rep(0,dim(", vars4[1,v4],")[1])"))))
  s <- 1
  for (s in 1:length(eval(parse(text = paste0("u.",vars2[1,v4]))))){
    assign("pos.GEO", eval(parse(text = paste0("which(GEO$",vars2[v4, 1], " == u.", vars2[v4,1],"[s])"))))
    assign("pos.s", eval(parse(text = paste0("which(",vars2[v4, 2], " == u.", vars2[v4,1],"[s])"))))
    species <- length(unique(GEO$speciesBlank[pos.GEO]))
    richness[pos.s] <- species
  }
  assign(paste0("richnessOcc",vars4[v4, 2]), richness)
  assign(paste0("data@",vars4[v4, 1]),eval(parse(text = paste0("cbind(",vars4[v4, 1],"@data, richnessOcc",vars4[v4, 2],")"))))
}


  
  vars4 <- rbind(c("reg", "Reg", vars2[1, 1], "NOM_REGIÃ³N"),
                 c("reg", "Reg", vars2[1, 1], "NOM_SUBREG"),
                 c("mun", "Mun", vars2[1, 1], "DEPARTAMEN"),
                  c("", "", vars2[1, 1], ""),
#reg$NOM_REGIóN, reg$NOM_SUBREG, mun$DEPARTAMEN, mun$NOMBRE_ENT, car$NOMCAR, hum$HUNOMBRE, par$PASECTOR, par$PADISTRITO, par$PACOMPLEJO

reg$richnesOccReg <- 0 
for (s in 1:length(u.sigRegion)){
  pos.GEO <- which(GEO$sigRegion == u.sigRegion[s])
  pos.s <- which(reg$NOM_REGIÃ³N == u.sigRegion[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  reg$richnesOccReg[pos.s] <- species
}
reg@data$richnesOccReg

p <- reg$richnesOccReg

reg$richnesOccSubreg <- 0 
for (s in 1:length(unique.Region)){
  pos.GEO <- which(GEO$sigRegionSub == unique.Region[s])
  pos.s <- which(reg$NOM_SUBREG == unique.Region[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  reg$richnesOccSubreg[pos.s] <- species
}

mun$richnesOccDep <- 0 
for (s in 1:length(unique.Dep)){
  pos.GEO <- which(GEO$sigDep == unique.Dep[s])
  pos.s <- which(mun$DEPARTAMEN == unique.Dep[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  mun$richnesOccDep[pos.s] <- species
}

mun$richnesOccMun <- 0 
for (s in 1:length(unique.Mun)){
  pos.GEO <- which(GEO$sigDep == unique.Dep[s])
  pos.s <- which(mun$NOMBRE_ENT == unique.Dep[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  mun$richnesOccMun[pos.s] <- species
}

car$richnesOcc <- 0 
for (s in 1:length(unique.CAR)){
  pos.GEO <- which(GEO$sigDep == unique.CAR[s])
  pos.s <- which(car$NOMCAR == unique.CAR[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  car$richnesOcc[pos.s] <- species
}

hum$richnesOcc <- 0 
for (s in 1:length(unique.HUM)){
  pos.GEO <- which(GEO$sigHUM == unique.HUM[s])
  pos.s <- which(hum$HUNOMBRE == unique.HUM[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  hum$richnesOcc[pos.s] <- species
}

par$richnesOccSect <- 0 
for (s in 1:length(unique.PARSect)){
  pos.GEO <- which(GEO$sigDep == unique.PARSect[s])
  pos.s <- which(par$PASECTOR == unique.PARSect[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  par$richnesOccSect[pos.s] <- species
}

par$richnesOccDist <- 0
for (s in 1:length(unique.PARDist)){
  pos.GEO <- which(GEO$sigDep == unique.PARDist[s])
  pos.s <- which(par$PADISTRITO == unique.PARDist[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  par$richnesOccDist[pos.s] <- species
}

par$richnesOccComp <- 0
for (s in 1:length(unique.PARComp)){
  pos.GEO <- which(GEO$sigDep == unique.PARComp[s])
  pos.s <- which(par$PACOMPLEJO == unique.PARComp[s])
  species <- length(unique(GEO$speciesBlank[pos.GEO]))
  par$richnesOccComp[pos.s] <- species
}



# Especies de Biomodelos

## Cargar máscara de Colombia con los ID's del DF de Beta
colombia.raster <- raster("W:/DINAVIS/raster_col_5km_wgs.tif")

## Rasterizar las capas deseadas
pnn.r <- rasterize(ANP, colombia.raster, field = ANP$OBJECTID)
mun.r <- rasterize(mun, colombia.raster)
reg.r <- rasterize(reg, colombia.raster)
par.r <- rasterize(par, colombia.raster)
hum.r <- rasterize(hum, colombia.raster)
#plot(pnn.r, add = T)

#Cargar DF de Beta
load("W:/DINAVIS/BetaDF_151434_2014-08.RData")
col5km <- 
DFID <- DF$ID
DF <- DF[,-c(1,2)]


id <- ANP$OBJECTID
target <- as.character(ANP$PNNOMBRE)
u.target <- unique(target)
Rich <- rep(0, times = length(target)); list.spp <- NULL
for (b in 1:length(u.target)){
  pos.shape <- which(target == u.target[b])
  raster.b <- Which(pnn.r == id[pos.shape])# plot(raster.b)
  pos.b <- which(raster.b[]!=0)
  beta.row <-  which(DFID %in% pos.b) # beta.row <- 10200:10300
  if (length(beta.row)>0){  
    (sum.row <- rowSums(DF[beta.row,], na.rm = T)); sum(sum.row)
    pos.row <- which(sum.row>0)
    if (length(pos.row)>0){  
      sum.col <- colSums(DF[beta.row[pos.row],], na.rm = T)
      spp <- gsub("_"," ",names(which(sum.col>0)))
      Rich[pos.shape] <- length(unique(spp))
      list.spp <- rbind(list.spp, cbind(u.target[b], spp))
      cat(u.target[b])
      }
  cat(b, "de", length(id), "- celdas:",pos.row , "\n")
  }
}
ANP$BioModRich <- BetaRich
list.spp.pnn <- list.spp
t1 <-table(list.spp[,1], list.spp[,2])


# Escribir los shapes con los campos
writeOGR(obj, folder, layer:name, driver = "ESRI Shapefile", overwrite_layer=FALSE)

# Medidas

setwd("C:/IAvH/DINAVIS datos/Med/")
load("BIOTA_Med_2014-05-30.RData")
MED <- full.med; rm(full.med)
MED$speciesBlank <- gsub(" ","",MED$species)
MED$IDMed <- 1:nrow(MED)
head(MED)
fi <- sapply(MED, is.factor);MED[fi] <- lapply(MED[fi], as.character)

#Mapas
setwd("C:/IAvH/DINAVIS datos/Map/")
load("BioModelos&IUCN_Map_2014-05-30.RData")
MAP$speciesBlank <- gsub(" ","",MAP$scientificName)
MAP$IDMap <- 1:nrow(MAP)
#MAP <- read.csv("BioModelos&IUCN_Map_2014-05-30.csv");MAP <- MAP[-c(1),];fi <- sapply(MAP, is.factor);MAP[fi] <- lapply(MAP[fi], as.character);MAP$scientificName <- paste0(MAP$scientificName,"__");MAP$scientificName <- gsub("__","",gsub(" __","",gsub("  __","",MAP$scientificName)));save(MAP, file = "BioModelos&IUCN_Map_2014-05-30.RData")
fi <- sapply(MAP, is.factor);MAP[fi] <- lapply(MAP[fi], as.character)

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
  pos4 <- sapply(sp,FUN =  function(x){
    (y <- strsplit(as.character(x)," ")[[1]])
    a <- y[1]
    b <- y[2]
    a.index <- unique(c(grep(a,TAX$speciesBlank),grep(a,TAX$scientificName)))
    b.index <- unique(c(grep(b,TAX$speciesBlank),grep(b,TAX$scientificName)))
    return(intersect(a.index,b.index))
  })
  pos <- unique(c(pos,unlist(pos.2),unlist(pos.3),unlist(pos4)))
  
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


save.image(file="DINAVIS.RData")
file.copy("DINAVIS.RData","W:/DINAVIS/DINAVIS.RData")
