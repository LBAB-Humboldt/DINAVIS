setwd("C:/IAvH/DINAVIS datos")
load("Tax/set16_Tax_2014-08-08.RData")
load("Map/BioModelos&IUCN_Map_2014-05-30.RData")

tax.table$blank <- gsub(" ", "", tax.table$acceptedNameUsage)
MAP$blank <- gsub(" ", "", MAP$scientificName)
head(tax.table)

mer <- merge(tax.table[, c("scientificName", "scientificNameAuthorship", "kingdom", "phylum", "class", "order", "family", "genus", "blank")], MAP, by = "blank", all.y = TRUE)

head(mer)
write.csv(mer, "Map/BioModelos&IUCN_Map_2014-05-30_HELE&Tax.CSV")
