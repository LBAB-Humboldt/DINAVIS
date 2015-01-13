library(xlsx)
setwd("C:/IAvH/DINAVIS datos/Datos spatial iucn/")
source("C:/Google Drive/Descarga20140303/script/gbif2_IG.R")

raros2vocales <- function(x){
  (x2 <- gsub("ÃƒÂ³","o",x))
  (x3 <- gsub("Ãƒ.","i",x2))
  (x4 <- gsub("ÃƒÂº","u",x3))
  (x5 <- gsub("Ã\\.","i",x4))
  (x6 <- gsub("Ã³","o",x5))
  (x7 <- gsub("Ginero","Genero",x6))
  return(x7)
}

formato.tax <- read.csv("Formato_Taxonom.csv")

names.format <- read.csv("Columnas.csv");colnames(names.format)[2] <- "Original";colnames(names.format)[3] <- "Equivalente"

campos.usados <- columnas <- resumen <- unused.fields <- tax.table <- geo.table <- med.table <- NULL
full.geo <- full.tax <- full.med <- NULL

archivo.i <- read.xlsx("Listado&Tax_IUCN_unique_sp.xlsx",sheetIndex=1);  head(archivo.i)
colnames.original <- colnames(archivo.i)
colnames.final <- NULL

# Volver columnas de factor a texto
fi <- sapply(archivo.i, is.factor)
archivo.i[fi] <- lapply(archivo.i[fi], as.character)

# Homologaci?n de nombres de campos
c <- 2
for (c in 1:length(colnames.original)){
  (name.c <- colnames.original[c])
  (name.f <- names.format$Equivalente[which(names.format$Original==name.c)][1])
  colnames.final <- c(colnames.final,as.character(name.f))
};colnames.final
colnames(archivo.i) <- colnames.final

# Campos taxon?micos
tax.table <- orig2set16(archivo.i, format = formato.tax, fuente = archivo.i$source, occID = paste0(archivo.i$source,"-",archivo.i$taxonConceptId), cleanSciNames = F); head(tax.table)


save(tax.table, file=paste0("IUCN_Tax_",as.Date(Sys.Date()),".RData"))
write.csv(tax.table, paste0("IUCN_Tax_",as.Date(Sys.Date()),".csv"), row.names = FALSE)
