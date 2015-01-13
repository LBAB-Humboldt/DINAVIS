library(maptools)
library(sp)
library(rgdal)
library(raster)
library(foreign)

source("C:/IAvH/DINAVIS datos/shiny/shinyRData.R")
rm(list = ls())  # Borra  todo lo que este en memoria
gc()
memory.limit(size = 1000000) 

#Unir tablas taxonomicas
setwd("C:/IAvH/DINAVIS datos/Tax")

load("BIOTA_Tax_2014-05-30.RData")
tax.biota <- full.tax; rm(full.tax)

load("IUCN_Tax_2014-06-03.RData")
tax.IUCN <- tax.table; rm(tax.table)

load("set16_Tax_2014-08-08.RData")
tax.set16 <- tax.table; rm(tax.table) 
  
colnames(tax.biota)
colnames(tax.IUCN)
colnames(tax.set16)

TAX <- rbind(tax.biota, tax.IUCN, tax.set16)
rm(tax.biota, tax.IUCN, tax.set16)
head(TAX)

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

#####
#### Riqueza segzn registros

coords <- GEO[,c("IDGeo","decimalLongitude","decimalLatitude", "scientificName")]; head(coords)
coords[, c("decimalLongitude","decimalLatitude")] <- lapply(coords[, c("decimalLongitude","decimalLatitude")], as.numeric)
coordinates(coords) =~ decimalLongitude+decimalLatitude
pr <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
pr2 <- "+proj=longlat +ellps=WGS84 +no_defs"
coords@proj4string@projargs <- pr2
# GEO$decimalLongitude <- as.numeric(GEO$decimalLongitude)
# GEO$decimalLatitude <- as.numeric(GEO$decimalLatitude)
# 
# coordinates(GEO)=~ decimalLongitude+decimalLatitude


# Arreglar capas, proyectarlas y renombrarlas

# (capas <- list.files(path = "C:/IAvH/DINAVIS datos/Fuentes/Layers/Originales", pattern = ".shp$"))
# c <- capas[2]
# for (c in capas){
#   layer.c <- readOGR(dsn = "C:/IAvH/DINAVIS datos/Fuentes/Layers/Originales", layer = gsub(".shp","",c))
#   trans <- spTransform(layer.c, CRSobj = CRS(pr2))
#   writeOGR(obj = trans, dsn = "C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84", layer = gsub(".shp","",c), driver = "ESRI Shapefile", overwrite_layer = TRUE)
# }
# rm(layer.c)

#col <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers","Colombia_buffer_10km")
#eco <- readOGR("C:/IAvH/Chicharrones/EspeciesEndemicasMotoresReporte/Vias","am_ecosistemaMAGNAS")
pnn <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","pnn")
reg <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","reg"); reg$ID <- as.numeric(reg$NOM_REGIC3N); reg$ID_SUBREG <- as.numeric(reg$NOM_SUBREG); colnames(reg@data)[5] <- "NOM_REGION"
dep <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","dep"); dep$ID <- 1:nrow(dep@data)
mun <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","mun")
car <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","car"); car$ID <- 1:nrow(car@data)
hum <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","hum")
par <- readOGR("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84","par"); par$ID_DIS <- as.numeric(par$PADISTRITO); par$ID_SEC <- as.numeric(par$PASECTOR)


# over.assignation
vars <- c("reg","mun","dep","car","hum","par","pnn")
for (v in 1:length(vars)){
  assign(paste0("over.",vars[v]),eval(parse(text = paste0("over(coords,",vars[v],")"))))
}

vars2 <- rbind (c("sigRegion", "reg$NOM_REGION"),
       c("sigRegionSub", "reg$NOM_SUBREG"),
       c("sigDep", "dep$NOM_DPTO"),
       c("sigMun", "mun$NOMBRE_ENT"),
       c("sigCAR", "car$NOMCAR"),
       c("sigHUM", "hum$HUNOMBRE"),
       c("sigPARSect", "par$PASECTOR"),
       c("sigPARDist", "par$PADISTRITO"),
       c("sigPARComp", "par$PACOMPLEJO"),
       c("sigPNN", "pnn$PNNOMBRE"))

for (v2 in 1:nrow(vars2)){
  assign(paste0(vars2[v2,1]),eval(parse(text = paste0("as.character(over.",vars2[v2,2],")"))))
}
assign("GEO",eval(parse(text = paste0("cbind(GEO,",paste(ls(pattern = "sig"), collapse = ", "),")"))))
head(GEO)
rm(list = c(ls(pattern = "^over"),"coords"))

# Genero listados unicos
for (v3 in vars2[,1]){
  assign(paste0("u.",v3), eval(parse(text = paste0("as.character(unique(",v3,"))"))))
}
rm(list = ls(pattern = "^sig"))

# Adiciono a los shape@data la informacisn de riqueza 
vars4 <- rbind(c("reg", "Reg", vars2[1, 1], "NOM_REGION"),
               c("reg", "Subre", vars2[2, 1], "NOM_SUBREG"),
               c("dep", "Dep", vars2[3, 1], "NOM_DPTO"),
               c("mun", "Mun", vars2[4, 1], "NOMBRE_ENT"),
               c("car", "Car", vars2[5, 1], "NOMCAR"),
               c("hum", "Hum", vars2[6, 1], "HUNOMBRE"),
               c("par", "Sect", vars2[7, 1], "PASECTOR"),
               c("par", "Dist", vars2[8, 1], "PADISTRITO"),
               c("par", "Comp", vars2[9, 1], "PACOMPLEJO"),
               c("pnn", "PNN", vars2[10, 1], "PNNOMBRE"))

for (v4 in 1:nrow(vars4)){
  assign("richness", eval(parse(text = paste0("rep(0,dim(", vars4[v4,1],")[1])"))))
  spp.list <- NULL
  for (s in 1:length(eval(parse(text = paste0("u.",vars2[v4,1]))))){
    assign("pos.GEO", eval(parse(text = paste0("which(GEO$",vars2[v4, 1], " == u.", vars2[v4,1],"[s])"))))
    assign("pos.s", eval(parse(text = paste0("which(",vars2[v4, 2], " == u.", vars2[v4,1],"[s])"))))
    spp <- unique(GEO$scientificName[pos.GEO])
    if (length(grep("NA NA", spp))>0){
      spp <- spp[which(spp != "NA NA")]
    }
    richness[pos.s] <- length(spp)
    if (length(spp)>0){
      assign("spp.list", eval(parse(text = paste0("rbind(spp.list, cbind(u.",vars2[v4,1],"[s], spp))"))))
    }
    #rbind(spp.list, cbind(u.sigPNN[s], spp))
  }
  assign(paste0("rO", vars4[v4, 2]), richness)
  assign(paste0("data_",vars4[v4, 1]), eval(parse(text = paste0("cbind(",vars4[v4, 1],"@data, rO",vars4[v4, 2],")"))))
  write.csv(table(spp.list[,2], spp.list[,1]), paste0("C:/IAvH/DINAVIS datos/shiny/tables/Tabla_occur_",gsub("\\.r","",vars4[v4, 2]),".csv"))
}
rm(list = ls(pattern = "^u.sig"))
rm(list = ls(pattern = "^rO"))

#####

#####
##### Riqueza segzn Biomodelos

## Cargar mascara de Colombia con los ID's del DF de Beta
colombia.raster <- raster("W:/DINAVIS/raster_col_5km_wgs.tif")
plot(colombia.raster)
load("W:/DINAVIS/BetaDF_151434_2014-08.RData")
DF <- DF[,-c(1,2)]

## Rasterizar las capas deseadas
vars5 <- rbind(c("reg.r","reg","ID","NOM_REGION"),
               c("reg.r.subre","reg", "ID_SUBREG","NOM_SUBREG"),
               c("dep.r","dep","ID","NOM_DPTO"), 
               c("mun.r","mun","COD_MUNICI","NOMBRE_ENT"),
               c("car.r","car","ID","IDCAR"),
               c("hum.r","hum","HUCODIGO","HUNOMBRE"),
               c("par.r.sec","par","ID_SEC","PASECTOR"),
               c("par.r.dis","par","ID_DIS","PADISTRITO"),
               c("par.r.com","par","PACODIGO","PACOMPLEJO"),
               c("pnn.r","pnn","OBJECTID","PNNOMBRE"))
v5 <- 2
for (v5 in 1:nrow(vars5)){
  assign(vars5[v5, 1], eval(parse(text = paste0("rasterize(",vars5[v5, 2],", colombia.raster, field = ", vars5[v5, 2],"$",vars5[v5, 3],")"))))  
  assign("id", eval(parse(text = paste0(vars5[v5, 2],"$",vars5[v5, 3]))))
  assign("target", eval(parse(text = paste0("as.character(",vars5[v5, 2],"$",vars5[v5, 4],")"))))
  u.target <- unique(target)    
  BioModRich <- rep(0, times = length(target)); list.spp <- NULL
  b <- 1
  list.spp <- NULL
  for (b in 1:length(u.target)){
    #sum(target != target2); sum(target == target2)
    pos.shape <- which(target == u.target[b])
    assign("beta.row", eval(parse(text = paste0("which(",vars5[v5, 1],"[] == id[pos.shape][1])"))))
    if (length(beta.row)>0){  
      (sum.row <- rowSums(DF[beta.row,], na.rm = T))
      pos.row <- which(sum.row>0)
      if (length(pos.row)>0){  
        sum.col <- colSums(DF[beta.row[pos.row],], na.rm = T)
        spp <- gsub("_"," ",names(which(sum.col>0)))
        BioModRich[pos.shape] <- length(unique(spp))
        list.spp <- rbind(list.spp, cbind(u.target[b], spp))
        cat(u.target[b]," ")
      }
      cat(b, "de", length(id), "- celdas:",length(pos.row) , "\n")
    }
  }
  assign(paste0("rM",vars4[v5, 2]), BioModRich)
  assign(paste0("data_", vars5[v5, 2]), eval(parse(text = paste0("cbind(data_",vars5[v5, 2],",  rM",vars4[v5, 2],")"))))
  write.csv(table(list.spp[,2], list.spp[,1]), paste0("C:/IAvH/DINAVIS datos/shiny/tables/Tabla_biomod_",gsub("\\.r","",vars4[v5, 2]),".csv"))
}
rm(list = c(ls(pattern = "^rM"),DF))

del <- eapply(.GlobalEnv, class)
rm(list = names(del[grep("Raster",del)]))


##### Riqueza segzn mapas experto

dbfs <- list.files(path = "C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84", pattern = "int")
(dbfs <- dbfs[grep("dbf",dbfs)])
v6 <- 4
for (v6 in 1:nrow(vars5)){
  if (length(grep(vars5[v6,2],dbfs))==0) {
    next
  }
  dbf <- read.dbf(paste0("C:/IAvH/DINAVIS datos/Fuentes/Layers/wgs84/int_",vars5[v6,2],".dbf"))
  i <- sapply(dbf, is.factor)
  dbf[i] <- lapply(dbf[i], as.character)
  assign("richUICN", eval(parse(text = paste0("rep(0, times = nrow(",vars5[v6,2],"@data))"))))
  assign("u.vals", eval(parse(text = paste0("as.character(unique(", vars5[v6, 2], "$", vars5[v6,4], "))"))))
  assign("pos.col", eval(parse(text = paste0("which(colnames(", vars5[v6, 2], "@data) == '",vars5[v6,4],"')"))))
  list.spp <- NULL
  u <- 5
  for (u in 1:length(u.vals)){
    assign("pos.u", eval(parse(text = paste0("which(", vars5[v6, 2], "$", vars5[v6, 4], " == u.vals[u])"))))
    spp <- unique(dbf$binomial[which(dbf[, paste0(vars5[v6,4])] == u.vals[u])])
    #cat(length(spp), "\n")
    richUICN[pos.u] <- length(spp)
    if (length(spp)>0){
      list.spp <- rbind(list.spp, cbind(u.vals[u], spp))
    }
  }
  assign(paste0("rE",vars4[v6, 2]), richUICN)
  assign(paste0("data_", vars5[v6, 2]), eval(parse(text = paste0("cbind(data_",vars5[v6, 2],", rE", vars4[v6, 2],")"))))
  write.csv(table(list.spp[,2], list.spp[,1]), paste0("C:/IAvH/DINAVIS datos/shiny/tables/Tabla_uicn_",vars4[v6, 2],".csv"))
}
rm(list = ls(pattern = "^rE"))


# Escribir los shapes con los campos
car@data <- data_car
hum@data <- data_hum
pnn@data <- data_pnn
mun@data <- data_mun
par@data <- data_par
reg@data <- data_reg

rm(list = c(ls(pattern = "data_"),dbf))

for (v in vars){
  assign(paste0("rep.names"), eval(parse(text = paste0("which(duplicated(colnames(",v,"@data)))"))))
  if (length(rep.names)>0){
    eval(parse(text = paste0("writeOGR(",v,"[,-rep.names], dsn = 'C:/IAvH/DINAVIS datos/shiny/maps', layer = 'dinavis_",v,"', driver = 'ESRI Shapefile', overwrite_layer=TRUE)")))
  } else {
    eval(parse(text = paste0("writeOGR(",v,", dsn = 'C:/IAvH/DINAVIS datos/shiny/maps', layer = 'dinavis_",v,"', driver = 'ESRI Shapefile', overwrite_layer=TRUE)")))
  }
}

# Compilar la informacion y dejarla guardada en el archivo shinyRData.RData para Shiny
shinyRData()




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
  out <- list(IDs,indexTAX,indexGEO,indexMED,indexMAP)
  names(out) <- c("InternalID","indexTAX","indexGEO","indexMED","indexMAP")
  
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