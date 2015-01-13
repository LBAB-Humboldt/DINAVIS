library(xlsx)
setwd("C:/IAvH/DINAVIS datos/Fuentes/Biota")
source("C:/IAvH/DINAVIS_set16/scripts/gbif3.R")

raros2vocales <- function(x){
  (x <- gsub("ÃƒÂ³","o",x))# � por �
  (x <- gsub("Ãƒ.","i",x))
  (x <- gsub("ÃƒÂº","u",x))
  (x <- gsub("Ã\\.","i",x))
  (x <- gsub("Ã³","o",x))
  (x <- gsub("ó","o",x))
  (x <- gsub("�\\.","i",x))
  (x <- gsub("ú","u",x))  
  (x <- gsub("Ginero","Genero",x))
  return(x)
}

archivos <- list.files(pattern = "Listado_Tax",recursive = TRUE)
if (length(grep("~",archivos))>0){archivos <- archivos[-grep("~",archivos)]}


formato.tax <- read.csv("Formato_Taxonom.csv")
formato.geo <- read.csv("Formato_Geo.csv")
campos.geo <- read.csv("geoCampos.csv", header=F, colClasses = "character")

campos.med <- read.csv("Formato_Measurement.csv", header=F,colClasses = "character")
formato.med <- campos.med[-(1),]; colnames(formato.med) <- as.character(campos.med)
#formato.med.v <- unique(strsplit(x=paste(unique(t(campos.med)),collapse = " "),split=" ")[[1]])
names.format2 <- cbind(campos.med,rbind(campos.med[1,],campos.med[1,]))

#names.format <- read.xlsx("Columnas.xlsx",sheetIndex=1);colnames(names.format)[2] <- "Original";colnames(names.format)[3] <- "Equivalente"
names.format <- read.csv("Columnas.csv"); colnames(names.format)[3] <- "Original";colnames(names.format)[4] <- "Equivalente"
#names.format$Original <- raros2vocales(names.format$Original)

campos.usados <- columnas <- resumen <- unused.fields <- tax.table <- geo.table <- med.table <- NULL
full.geo <- full.tax <- full.med <- NULL

i <- 111
for (i in 1:length(archivos)){ #111 116 118 43 50
  pos <- gregexpr("/",archivos[i]) 
  (fil <- substr(archivos[i],0,pos[[1]][1]-1))
  (vol <- substr(archivos[i],pos[[1]][1]+1,pos[[1]][2]-1))
  (no <- substr(archivos[i],pos[[1]][2]+1,pos[[1]][3]-1))
  (art <- substr(archivos[i],pos[[1]][3]+1,pos[[1]][4]-1))
  (exc <-  substr(archivos[i],pos[[1]][4]+1,nchar(archivos[i])))
  
  (index.source <- gsub(" ","",paste(vol,no,art, sep = "-")))
  (index.source.ID <- paste("Biota",gsub("Volumen ","",vol),gsub("No. ","",gsub("N. ","",no)),substr(art,0,2), sep = "-"))
  
  archivo.i <- read.xlsx(archivos[i],sheetIndex=1);  head(archivo.i)
  
  blankIndex <- rowSums(!(archivo.i == "NA" | is.na(archivo.i)))
  archivo.i <- archivo.i[blankIndex>0,]
  
  (colnames.orginal.mal <- colnames(archivo.i))
  (colnames.orginal <- raros2vocales(colnames(archivo.i)))
  colnames.final <- NULL

  # Volver columnas de factor a texto
  fi <- sapply(archivo.i, is.factor)
  archivo.i[fi] <- lapply(archivo.i[fi], as.character)
  
  # Homologaci?n de nombres de campos 
  c <- 1
  for (c in 1:length(colnames.orginal)){
    (name.c <- colnames.orginal[c])
    (name.f <- names.format$Equivalente[which(names.format$Original==name.c)][1])
    colnames.final <- c(colnames.final,as.character(name.f))
  }
  colnames(archivo.i) <- colnames.final
  archivo.i$scientificName <- paste0(archivo.i$scientificName,"__")
  archivo.i$scientificName <- gsub("__","",gsub(" __","",gsub("  __","",archivo.i$scientificName)))
  
  #Identificar campos de tipo 'atributo'
  (med <- which(colnames.final%in%campos.med))
  used.names2 <- med
  m <- 1
  used.names3 <- used.names2
  if (length(med)>0){
    used.names3 <- c(used.names2)
    archivo.m <- archivo.i[,med]
    if (is.null(dim(archivo.m))){archivo.m <- as.data.frame(archivo.m); colnames(archivo.m) <- colnames(archivo.i)[med]}
    archivo.med <- cbind(species = archivo.i$scientificName, orig2set16(archivo.m, format = formato.med, fuente = "Biota-", occID = index.source.ID, cleanSciNames = F)); head(archivo.med)
    index2 <- archivo.med[,!colnames(archivo.med)%in%c("species","source","occurrenceID")]
    blankIndex2 <- rowSums(!(index2 == "NA" |index2 == "" | is.na(index2)))
    med.table <- archivo.med[blankIndex2>0,]
    head(med.table)
#     for (m in 1:length(med)){
#       (med.m <- med[m])
#       (med.m2 <- which(colnames.final[med.m+1]%in%formato.med.v))
#       (med.m3 <- which(colnames.final[med.m+2]%in%formato.med.v))
#       (med.m4 <- which(colnames.final[med.m+3]%in%formato.med.v))
#       
#       list.m <- c(med.m, med.m2*(med.m+1), med.m3*(med.m+2), med.m4*(med.m+3))
#       (colnames.med.orig <- colnames(archivo.i[,list.m]))
#       
#       colnames.med.final <- NULL
#       c2 <- 1
#       for (c2 in 1:length(colnames.orginal)){
#         (name.f <- names.format2[1,which(names.format2[2,]==colnames.med.orig[c2])])
#         colnames.med.final <- c(colnames.med.final,as.character(name.f))
#       }
#       
#       used.names3 <- unique(c(used.names2, list.m))
#       archivo.m <- archivo.i[,list.m]
#       colnames(archivo.m) <- colnames.med.final
#       med.table.m <- orig2set16(archivo.m, format = formato.med, fuente = paste0("Biota-",index.source), occID = index.source.ID, cleanSciNames = F)
#       med.table.m <- cbind(archivo.i$scientificName, med.table.m)
#       
#       head(med.table.m)
#       med.table <- NULL
#       na.m <- which(is.na(med.table.m$measurementType))
#       med.table <- rbind(med.table,med.table.m[-na.m,])
#       head(med.table)
     #}
  }
  
  used.names4 <- used.names3
  #Identificar campos de tipo 'registro'
  (geo <- which(colnames.final%in% colnames(formato.geo)))
  if (length(geo)>0){
    used.names4 <- unique(c(used.names3, geo))
    archivo.g <- archivo.i[,geo]
    if (is.null(dim(archivo.g))){archivo.g <- as.data.frame(archivo.g); colnames(archivo.g) <- colnames(archivo.i)[geo]}
    archivo.geo <- cbind(species = archivo.i$scientificName, orig2set16(archivo.g, format = formato.geo, fuente = paste0("Biota-",index.source), occID = index.source.ID, cleanSciNames = F)); head(archivo.geo)
    mal.geo <- unique(which(archivo.geo$locality == "" & archivo.geo$localityID == "" & is.na(archivo.geo$occurrenceRemarks)))
    geo.table <- archivo.geo[-mal.geo,]
  }

  # Campos taxon?micos
  if (length(used.names4) == 0){
    archivo.tax <- archivo.i
  } else {
    archivo.tax <- archivo.i[,-c(used.names4)]
  }

  tax.table <- orig2set16(archivo.tax, format = formato.tax, fuente = "Biota", occID = paste0(index.source.ID,"-",1:nrow(archivo.i)), cleanSciNames = F); head(tax.table) #fuente = paste0("Biota-",index.source)
  head(tax.table)
  head(archivo.i)
  #Guardar registro de los campos usados o no
  used.names <- unique(c(used.names4,which(colnames(archivo.i)%in%colnames(tax.table))))  
  unused.names <- (1:length(colnames(archivo.i)))[-used.names]
  campos.usados <- rbind(campos.usados,c(archivos[i],paste0(colnames(archivo.i)[used.names],sep=",", collapse=""),paste0(colnames(archivo.i)[unused.names],sep=",", collapse="")))
  
  #unir tablas
  full.geo <- rbind(full.geo, geo.table)
  full.med <- rbind(full.med, med.table)
  full.tax <- rbind(full.tax, tax.table)
  cat(i, "de",length(archivos),"-",archivos[i] ,"\n")
}

full.geo2 <-full.geo[,-which(colnames(full.geo)=="X")] 

save(full.geo2, file=paste0("BIOTA_Geo_",as.Date(Sys.Date()),".RData"))
save(full.med, file=paste0("BIOTA_Med_",as.Date(Sys.Date()),".RData"))
save(full.tax, file=paste0("BIOTA_Tax_",as.Date(Sys.Date()),".RData"))
write.csv(full.geo2, paste0("BIOTA_Geo_",as.Date(Sys.Date()),".csv"), row.names = FALSE)
write.csv(full.med, paste0("BIOTA_Med_",as.Date(Sys.Date()),".csv"), row.names = FALSE)
write.csv(full.tax, paste0("BIOTA_Tax_",as.Date(Sys.Date()),".csv"), row.names = FALSE)
write.csv(campos.usados, paste0("Campos_usados_",as.Date(Sys.Date()),".csv"), row.names = FALSE)

