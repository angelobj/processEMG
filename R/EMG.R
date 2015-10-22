#' Pasos para el análisis EMG
#' 1.- Cargar todos los archivos. Ver la configuración, carpetas para mov, base, max
#' 2.- Convertir a emg
#' 3.- Remover offset
#' 4.- Rectificar
#' 5.- Media Móvil
#' 6.- Normalizar
#' 7.- On/off
#' Frecuencia de muestreo es por defecto samplingrate=1925.93
#' emg<-cargar_emg(nombre="Sujeto",n=2,carpetas=c("mov","max"),extension=".csv",sep=",",header=T)
#' emg.dc<-llaply_pr(emg,FUN=dcbiasremoval)
#' emg.r<-llaply_pr(emg.dc,FUN=rectification,type="fullwave")
#' emg.ma<-llaply_pr(emg.r,FUN=movingaverage,wsize=50)
#' emg.max<-llaply_max(emg.ma)
#' emg.n<-llaply_norm(emg.ma,max=emg.max)
#' plot(emg.n[[1]][[2]][[1]])
#' Frecuencia de muestreo Delsys bagnoli
#' Variables a introducir en funcion principal
#names, samplingrate,type="fullwave",w.ma=,units="time"
#wsize=samplingrate*w.ma

#1º Nivel <-Sujeto
#2º Nivel <-Condición
#3º Nivel <- Procesado

require("biosignalEMG")

#1
#Ingresar canales para cargar a partir del archivo delsys channel=c(1)

######### Para cargar archivos de emg, deben estar almacenados en carpetas y con los mismos nombres ######
######### Por defecto están en carpetas=c("mov","max) para poder normalizar, la extensión es ".csv" ######
cargar_emg<-function(nombre="Sujeto",n = 1, samplingrate=1925.93,carpetas=c("mov","max"),
                     extension=".csv",sep=",",header=T,...) #Revisar channels
{
  if (missing(n))
    warning("Se cargará el primer archivo de la carpeta")
  if (missing(samplingrate)) 
    warning("Se utiliza una frecuencia de 1925.93 hz")
  x<- list()
  nombres<-c(NA)
  for(i in 1:n){
    x[[length(x)+1]]<-as.list(carpetas)
    names(x[[i]])<-carpetas
    nombres[i]<-(if(i<10){paste(nombre,"_",0,i,sep="")}
    else{paste(nombre,"_",i,sep="") })
  }
  names(x)<-nombres
emg<-sapply(names(x), USE.NAMES = TRUE, simplify = FALSE,function(i){
  sapply(names(x[[i]]), USE.NAMES = TRUE, simplify = FALSE, function(c){
    read.table((paste(paste("./",c,"/",sep=""),paste(i,extension,sep=""),sep="")),sep=sep,header=header,...)})})
x<-emg
sapply(names(x), USE.NAMES = TRUE, simplify = FALSE,function(i){
  sapply(names(x[[i]]), USE.NAMES = TRUE, simplify = FALSE, function(c){
    apply(x[[i]][[c]],function(x){as.emg(as.numeric(x),samplingrate=samplingrate)},MARGIN=2)})})
}


######## Para convertir en caso de que sea necesario, función está anidada al cargar #############
#2
llaply_emg<-function(x,samplingrate=1925.93){
sapply(names(x), USE.NAMES = TRUE, simplify = FALSE,function(i){
  sapply(names(x[[i]]), USE.NAMES = TRUE, simplify = FALSE, function(c){
    apply(x[[i]][[c]],function(x){as.emg(as.numeric(x),samplingrate=samplingrate)},MARGIN=2)})})
}
###################################################################################################


######## Para eliminar offset (dcbiasremoval), rectificación y media móvil ####################################
#3- 3, 4 y 5
llaply_pr<-function(x,FUN,...){
  sapply(names(x), USE.NAMES = TRUE, simplify = FALSE,function(i){
    sapply(names(x[[i]]), USE.NAMES = TRUE, simplify = FALSE, function(c){
       lapply(x[[i]][[c]],FUN,...)})})
}
#4
#5 ##### Con la función de 3 también sale
llaply_ma<-function(x,wsize,...){
  sapply(names(x), USE.NAMES = TRUE, simplify = FALSE,function(i){
    sapply(names(x[[i]]), USE.NAMES = TRUE, simplify = FALSE, function(c){
      lapply(x[[i]][[c]],FUN=movingaverage,wsize=wsize,...)})})
}

# Normalizar
# Para obtener el max
llaply_max<-function(x,n=10){
  sapply(names(x), USE.NAMES = TRUE, simplify = FALSE,function(i){
    sapply(names(x[[i]]), USE.NAMES = TRUE, simplify = FALSE, function(c){
      sapply(names(x[[i]][[c]]), USE.NAMES = TRUE, simplify = FALSE, function(m){
        mean(sort(x[[i]][[c]][[m]]$values,decreasing=T)[1:n])})})})
}
# Para normalizar
llaply_norm<-function(x,max,samplingrate=1925.93){
  sapply(names(x), USE.NAMES = TRUE, simplify = FALSE,function(i){
    sapply(names(x[[i]]), USE.NAMES = TRUE, simplify = FALSE, function(c){
      sapply(names(x[[i]][[c]]), USE.NAMES = TRUE, simplify = FALSE, function(m){
        as.emg(as.numeric(x[[i]][[c]][[m]]$values*100/max[[i]][[c]][[m]]),samplingrate=samplingrate)})})})
}