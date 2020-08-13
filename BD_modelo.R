#script que toma la BD Donaciones_Total y agrega columnas necesarias para generar el modelo 

#### Montos en UTM ####
#cargar BD
ScriptsPath_ETL.consolidacion<-paste("/home/",usuario,"/workspace/SxC_ETL/Donaciones/DonacionesFM/Consolidacion/",sep="")
load(paste(ScriptsPath_ETL.consolidacion,"Donaciones_total_compilado_070820.Rdata",sep=""))

#conservar solo donaciones con rut 
Donaciones_total<-Donaciones_total[!is.na(Donaciones_total$RUT),] #1.415.469

library(readxl)
valorUTM <- read_excel("~/workspace/donationstax/valorUTM.xlsx")
colnames(valorUTM)<-c("Año","Mes","UTM")

sum(is.na(Donaciones_total$Año))#12 observaciones sin año
sum(is.na(Donaciones_total$Mes))#19 observaciones sin mes

#se quitan donaciones sin año
Donaciones_total<-Donaciones_total[!is.na(Donaciones_total$Año),] #1.415.457
sum(is.na(Donaciones_total$Mes))#7 observaciones sin mes
#para aquellas donaciones con Mes=NA se imputa por Mes=01
Donaciones_total$Mes<-ifelse(is.na(Donaciones_total$Mes),"01",Donaciones_total$Mes)

Donaciones_total$Año<-as.numeric(Donaciones_total$Año)
Donaciones_total$Mes<-as.numeric(Donaciones_total$Mes)
valorUTM$Año<-as.numeric(valorUTM$Año)
valorUTM$Mes<-as.numeric(valorUTM$Mes)

#Monto en pesos corrientes a UTM
Donaciones_total<-merge(Donaciones_total,valorUTM,by=c("Año","Mes"),all.x = TRUE)
sum(is.na(Donaciones_total$UTM)) #43 observaciones con año 2020
Donaciones_total<-Donaciones_total[!is.na(Donaciones_total$UTM),] #1.415.414

Donaciones_total$MONTO.FONDO.MIXTO<-as.numeric(Donaciones_total$MONTO.FONDO.MIXTO)
Donaciones_total$MONTO.INSTITUCIÓN<-as.numeric(Donaciones_total$MONTO.INSTITUCIÓN)
Donaciones_total$monto.total.donacion.num<-as.numeric(Donaciones_total$monto.total.donacion.num)

Donaciones_total$monto_FM_UTM<-Donaciones_total$MONTO.FONDO.MIXTO/Donaciones_total$UTM
Donaciones_total$monto_institucion_UTM<-Donaciones_total$MONTO.INSTITUCIÓN/Donaciones_total$UTM
Donaciones_total$monto_total_UTM<-Donaciones_total$monto.total.donacion.num/Donaciones_total$UTM

#### Fecha en formato date ####
sum(is.na(Donaciones_total$Día)) #868 observaciones con dia NA, se imputa con Dia=1
Donaciones_total$Día<-ifelse(is.na(Donaciones_total$Día),1,Donaciones_total$Día)
sum(is.na(Donaciones_total$Día)) #0 observaciones con dia NA

library(lubridate)
library(stringr)
Donaciones_total$Fecha<-NA
Donaciones_total$Fecha<-str_c(Donaciones_total$Día,Donaciones_total$Mes,Donaciones_total$Año,sep ="-")
Donaciones_total$Fecha<-dmy(Donaciones_total$Fecha)
Donaciones_total[which(is.na(Donaciones_total$Fecha)),17]<-dmy("30-11-2015")

#extraer datos pertenecientes a la viegencia de la leyes 20.565 y 20.900 (1)
#("2011-01-01" y "2016-04-13")

Donaciones_total<-subset(Donaciones_total,Fecha>="2011-01-01" & Fecha<="2016-04-13") #1.392.873

#seleccion de variables
datos.modelo<-subset(Donaciones_total,select = c(RUT,DONATARIO,Fecha,monto_FM_UTM,monto_total_UTM,monto_institucion_UTM))

#### Generacion de variables ####
datos.modelo$ID<-seq(1,nrow(datos.modelo))
datos.modelo$Limite.donacion<-NA
datos.modelo$Ley.20900<-NA
datos.modelo$Orden.donacion<-NA
datos.modelo$Monto_acum_donante<-NA
datos.modelo$Monto_acum_donatario<-NA
datos.modelo$Monto_acum_FM<-NA
datos.modelo$Contribuyente_1ra<-NA
datos.modelo$Cert_digital<-NA

library(lubridate)
fecha_limite=ymd("2012-02-08") #fecha ley 20900

#http://www.desarrollosocialyfamilia.gob.cl/pdf/upload/IDS_INAL_FCM_3.pdf pag 205
# Reglamento marzo 2014 https://www.bcn.cl/leychile/navegar?idNorma=1060093
# http://www.lasegunda.com/Noticias/Impreso/2013/02/819781/hogar-de-cristo-y-sii-firman-convenio-que-podria-duplicar-aportes-de-socios
fecha_cert.digital=ymd("2014-03-07") #fecha puesta en marcha certificado digital 

for(j in  1:nrow(datos.modelo)){ #
  print(j)
  donante<-as.data.frame(matrix(ncol=14))
  donatario<-as.data.frame(matrix(ncol=14))
  
  donante<-datos.modelo[datos.modelo$RUT==datos.modelo[j,1],] #selección de df solo con las donaciones del donante
  donatario<-datos.modelo[datos.modelo$DONATARIO==datos.modelo[j,2],] #selección de df solo con donaciones a donatario
  
  #Limite_donacion
  #suma de las donaciones del año del donante mayor a 1000
  #extraer año de la donación 
  año.don<-year(datos.modelo[j,3])
  donante$año<-year(donante$Fecha)
  datos.modelo[j,8]<-ifelse(sum(subset(donante,año==año.don)$monto_total_UTM,na.rm = TRUE)>1000 ,1,0) #si el monto total es mayor a 1000 entonces 1
  #ley 20900
  datos.modelo[j,9]<-ifelse(datos.modelo[j,3]>=fecha_limite,1,0) #si la fecha de la donación es dentro del periodo de la ley entonces 1
  #orden donacion 
  donante<-subset(donante,año==año.don)
  donante<-donante[order(donante$Fecha),]
  rownames(donante)<-NULL
  datos.modelo[j,10]<- which(donante$ID == datos.modelo[j,7]) #lugar/posición de la donación
  #monto acumulado del donante
  datos.modelo[j,11]<-sum(donante[1:datos.modelo[j,10],]$monto_total_UTM,na.rm = TRUE)
  #monto acumulado donatario
  donatario$año<-year(donatario$Fecha)
  donatario<-subset(donatario,año==año.don)
  donatario<-donatario[order(donatario$Fecha),]
  rownames(donatario)<-NULL
  orden.don<-which(donatario$ID == datos.modelo[j,7])
  datos.modelo[j,12]<-sum(donatario[1:orden.don,]$monto_institucion_UTM,na.rm = TRUE)
  #monto acumulado FM (por ese donante en ese año)
  datos.modelo[j,13]<-sum(donante[1:datos.modelo[j,10],]$monto_FM_UTM,na.rm = TRUE)
  #contribuyente primera categoria
  #quitar digito verificador
  rut<-as.numeric(substr(datos.modelo[j,1],1,nchar(datos.modelo[j,1])-1))
  datos.modelo[j,14]<-ifelse(rut>49000000,1,0) #1 si es de primera categoria 
  #vigencia certificado digital 
  datos.modelo[j,15]<-ifelse(datos.modelo[j,3]>=fecha_cert.digital,1,0) #si la fecha de la donación 
  #es dentro del periodo de vigencia del cert digital entonces 1
}

#cambiar tipo de variables 
datos.modelo$Limite.donacion<-as.factor(datos.modelo$Limite.donacion)
datos.modelo$Ley.20900<-as.factor(datos.modelo$Ley.20900)
datos.modelo$Orden.donacion<-as.integer(datos.modelo$Orden.donacion)
datos.modelo$Monto_acum_donante<-as.double(datos.modelo$Monto_acum_donante)
datos.modelo$Monto_acum_donatario<-as.double(datos.modelo$Monto_acum_donatario)
datos.modelo$Monto_acum_FM<-as.double(datos.modelo$Monto_acum_FM)
datos.modelo$Contribuyente_1ra<-as.factor(datos.modelo$Contribuyente_1ra)
datos.modelo$Cert_digital<-as.factor(datos.modelo$Cert_digital)

#se genera BD
load("~/workspace/donationstax/Datos_Modelo_120820.Rdata")

summary(datos.modelo.final)


