setwd("C:/IAvH/DINAVIS datos/Fuentes/I2D")
source("C:/IAvH/DINAVIS_set16/scripts/gbif3.R")

raros2vocales <- function(x){
  (x2 <- gsub("ÃƒÂ³","o",x))# � por �
  (x3 <- gsub("Ãƒ.","i",x2))
  (x4 <- gsub("ÃƒÂº","u",x3))
  (x5 <- gsub("Ã\\.","i",x4))
  (x6 <- gsub("Ã³","o",x5))
  (x7 <- gsub("Ginero","Genero",x6))
  return(x7)
}

# #Arreglar archivos originales
# (zip <- list.files(pattern = "zip"))
# zip.names <- gsub("\\.zip","",zip)
# z <- 1
# for (z in 1:length(zip)){
#   dir.create(path=zip.names[z],showWarnings=F)
#   unzip(zipfile=zip[z],exdir=zip.names[z])
# }

divipola <- read.csv("C:/IAvH/DINAVIS datos/Formatos/DIVIPOLA_20140628.csv", stringsAsFactors = FALSE)
len <- sapply(divipola[,1:3],nchar)
unique(len[,1])
unique(len[,2])
unique(len[,3])

head(divipola)

txt <- list.files(pattern = "txt", recursive=T)

archivos <- t(sapply(txt,FUN = function(y){
  (pos <- regexpr("/",y)[[1]][1])
  (genus <- substr(y,0,pos-1))
  (sp <- substr(y,pos+1,nchar(y)))
  (file <-cbind(genus,sp))
  return(t(file))
}))
row.names(archivos) <- 1:nrow(archivos)


tipo.archivos <- unique(archivos[,2])
fuentes <- unique(archivos[,1])
(tipo.archivos <- gsub("\\.txt","",tipo.archivos))


# # Se extraen los nombres de los campos
# c <- 5
# nombres.todos <- NULL
# for (c in 1:length(tipo.archivos)){
#   (tipo.c <- tipo.archivos[c])
#   archivos.c <- txt[grep(tipo.c,txt)]
#   nombres <- c(tipo.c,tipo.c)
#   for (a in 1:length(archivos.c)){
#     txt.a <- read.delim(archivos.c[a]);head(txt.a)
#     nombres <- cbind(nombres,txt.a[1:2,]) # taxon  # occ  # distribution # reference  # description  # typesandspecimen  # measurementorfact
#     cat(c,"-",a," ", archivos.c[a], "\n")
#   }
#   dup <- duplicated(colnames(nombres))
#   nombres <- nombres[,!dup]
#   write.csv(t(nombres), paste0("Nombres_",tipo.c,".csv"))
#   i <- sapply(nombres, is.factor); nombres[i] <- lapply(nombres[i], as.character)
#   nombres.todos <- rbind(nombres.todos, cbind(tipo.c,t(nombres)))
# }
# col.n <- colnames(nombres.todos)
# rownames(nombres.todos) <- nombres[1,]; nombres[1,] <- col.n
# write.csv(unique(nombres.todos),"NOMBRES_TODOS.csv")

# tax <- read.delim(txt[grep("taxon.txt",archivos[,2])[1]])
# occ <- read.delim(txt[grep("occurrence.txt",archivos[,2])[1]])
# dis <- read.delim(txt[grep("distribution.txt",archivos[,2])[1]]) #
# ref <- read.delim(txt[grep("reference.txt",archivos[,2])[1]])
# des <- read.delim(txt[grep("description.txt",archivos[,2])[1]])
# typ <- read.delim(txt[grep("typesandspecimen.txt",archivos[,2])[1]])
# mea <- read.delim(txt[grep("measurementorfact.txt",archivos[,2])[1]])

t(t(archivos[archivos[,1]%in%archivos[grep("taxon",archivos[,2]),1],]))
t(t(archivos[archivos[,1]%in%archivos[grep("occurrence",archivos[,2]),1],]))

t(t(unique(archivos[archivos[,1]%in%archivos[grep("taxon",archivos[,2]),1],2])))
t(t(unique(archivos[archivos[,1]%in%archivos[grep("occurrence",archivos[,2]),1],2])))

full.geo <- full.tax <- full.med <- NULL

i <- 7
for (i in 1:length(fuentes)){ #71
  fuente.i <- fuentes[i]
  # archivos[archivos[,1] == fuente.i,]
  archivos.i <- archivos[archivos[,1] == fuente.i,2]
  tax <- med <- geo <- NULL

  if (length(grep("taxon",archivos.i))>0){
    tax.i <- read.delim(paste0(fuente.i,"/",archivos.i[grep("taxon",archivos.i)]), stringsAsFactors=FALSE)
    dim(tax.i)
    if (length(grep("reference",archivos.i))>0){
      ref.i <- read.delim(paste0(fuente.i,"/reference.txt"))
    }
    if (length(grep("typesandspecimen",archivos.i))>0){
      typ.i <- read.delim(paste0(fuente.i,"/typesandspecimen.txt"))
      tax <- cbind(tax.i, typ.i)
    }  
    if (length(grep("distribution",archivos.i))>0){
      dis.i <- read.delim(paste0(fuente.i,"/distribution.txt"), stringsAsFactors=FALSE)
      if (length(grep("location$", colnames(dis.i))) == 0){
        
      }
      
      
#       dis <- archivos[grep("distribution", archivos[,2]),]
#       u.col <- NULL 
#       d.col <- NULL
#       u <- 3
#       for(u in 1:nrow(dis)){
#         file.u <- read.delim(paste0(dis[u,1], "/", dis[u,2])); head(file.u)
#         u.col <- c(u.col, colnames(file.u))  
#         d.col <- c(d.col, ncol(file.u))
#       }
# 
#       unique.dis <- gsub(" ", "", unique(unlist(strsplit(un, ";"))))
#       pos.co <- grep("CO", unique.dis)
#       co.dis <- gsub("CO:", "", unique.dis[pos.co])
#       
#       dim(dis.i)
#       head(dis.i)
#       head(divipola)
#       length(unique(dis.i$id))
      geo <- rbind(geo,)
    }
    if (length(grep("description",archivos.i))>0){
      des.i <- read.delim(paste0(fuente.i,"/description.txt"))
      t.des.i <- table(des.i)
      head(des.i)
      dim(des.i)
      med <- rbind(med, )
      
    }
    if (length(grep("measurementorfact",archivos.i))>0){
      full.med <- rbind(full.med,){
      }
    
    full.tax <- rbind(full.tax,)
  }
  
  if (length(grep("occurrence",archivos.i))>0){
    full.geo <- rbind(full.geo,)
    if (length(grep("measurementorfact",archivos.i))>0){
      full.med <- rbind(full.med,){        
      }
  }
  
    
  }

  
  # (taxon == typesandspecimen) != distribution != description
  
  # occurrence == measurementorfact
  measurementType  measurementValue	measurementUnit
  
  
  colnames.orginal.mal <- colnames(archivo.i)
  colnames.orginal <- raros2vocales(colnames(archivo.i))
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
  
  full.geo <- rbind(full.geo, geo.table)
  full.med <- rbind(full.med, med.table)
  full.tax <- rbind(full.tax, tax.table)
  cat(i, "de",length(archivos),"-",archivos[i] ,"\n")
}

save(full.geo[,-which(colnames(full.geo)=="X")], file=paste0("BIOTA_Geo_",as.Date(Sys.Date()),".RData"))
save(full.med, file=paste0("BIOTA_Med_",as.Date(Sys.Date()),".RData"))
save(full.tax, file=paste0("BIOTA_Tax_",as.Date(Sys.Date()),".RData"))

write.csv(full.geo[,-which(colnames(full.geo)=="X")], paste0("BIOTA_Geo_",as.Date(Sys.Date()),".csv"), row.names = FALSE)
write.csv(full.med, paste0("BIOTA_Med_",as.Date(Sys.Date()),".csv"), row.names = FALSE)
write.csv(full.tax, paste0("BIOTA_Tax_",as.Date(Sys.Date()),".csv"), row.names = FALSE)
write.csv(campos.usados, paste0("Campos_usados_",as.Date(Sys.Date()),".csv"), row.names = FALSE)
