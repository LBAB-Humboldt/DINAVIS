library(snowfall)
lista <- paste0("L",1:6962)

datos <- paste0("L",1:1000000)
columna <- datos[sample(1:length(datos), 60000, replace = TRUE)]

times <- 7
size <- length(lista)/times
if (round(size)-size<0) {size <- round(size) +1} else {size <- round(size)}; size
times_index <- sort(rep(1:times,size))[1:length(lista)]
tail(cbind(lista, times_index))


parallel.list <- list()
for (s in 1:times){
  list.s <- lista[which(times_index == s)]
  parallel.list[[s]] <- list.s
}


Sys.time()
sfInit(parallel = TRUE, cpus = 7)
sfExport(list = c("parallel.list","columna"))

paralelizada <- 

  sfClusterApplyLB(parallel.list, function(x) {
  pos <- which(columna == "" | columna == " " | is.na(columna))
  columna[pos] <- NA
  opt <- parallel.list[[x]]
  for (c in 1:length(opt)){
    pos.c <- grep(tolower(opt[c]), tolower(columna[pos]))
    columna[pos[pos.c]] <- opt[c] 
    cat(c)
  }
}
)
sfStop()
