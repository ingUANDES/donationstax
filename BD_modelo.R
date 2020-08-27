#script que toma la BD Donaciones_Total y agrega columnas necesarias para generar el modelo 

#### Montos en UTM ####
#cargar BD
ScriptsPath_ETL.consolidacion<-paste("/home/",usuario,"/workspace/SxC_ETL/Donaciones/DonacionesFM/Consolidacion/",sep="")
load(paste(ScriptsPath_ETL.consolidacion,"Donaciones_total_compilado_070820.Rdata",sep=""))

Donaciones_total<-df2
#conservar solo donaciones con rut 
Donaciones_total<-Donaciones_total[!is.na(Donaciones_total$RUT),] #1.540.636

library(readxl)
valorUTM <- read_excel("~/workspace/donationstax/valorUTM.xlsx")
colnames(valorUTM)<-c("Año","Mes","UTM")

sum(is.na(Donaciones_total$Año))#12 observaciones sin año
sum(is.na(Donaciones_total$Mes))#20 observaciones sin mes

#se quitan donaciones sin año
Donaciones_total<-Donaciones_total[!is.na(Donaciones_total$Año),] #1.540.624
sum(is.na(Donaciones_total$Mes))#8 observaciones sin mes
#para aquellas donaciones con Mes=NA se imputa por Mes=01,son de año 2014 y 2015, no influyen en cambio ley
Donaciones_total$Mes<-ifelse(is.na(Donaciones_total$Mes),"01",Donaciones_total$Mes)

Donaciones_total$Año<-as.numeric(Donaciones_total$Año)
Donaciones_total$Mes<-as.numeric(Donaciones_total$Mes)
valorUTM$Año<-as.numeric(valorUTM$Año)
valorUTM$Mes<-as.numeric(valorUTM$Mes)

#Monto en pesos corrientes a UTM
Donaciones_total<-merge(Donaciones_total,valorUTM,by=c("Año","Mes"),all.x = TRUE)
sum(is.na(Donaciones_total$UTM)) #43 observaciones con año 2020
Donaciones_total<-Donaciones_total[!is.na(Donaciones_total$UTM),] #1.540.581

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

save(Donaciones_total,file="Donaciones_total_graficos.Rdata")

Donaciones_total.seleccion<-subset(Donaciones_total,Fecha>="2011-01-01" & Fecha<="2016-04-13") #1.517.981

#seleccion de variables
datos.modelo<-subset(Donaciones_total.seleccion,select = c(RUT,DONATARIO,Fecha,monto_FM_UTM,monto_total_UTM,monto_institucion_UTM))

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

save(datos.modelo,file = "datos_modelo_140820.Rdata")



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

#unir distintas BD generadas 
#5 BD generadas en MacBook Claudia
load("~/workspace/donationstax/iteraciones/Datosmodelo1_750000_200820.Rdata")
#5 BD generadas en servidor 
load("~/workspace/donationstax/iteraciones/modelo1.Rdata")
datos.modelo.comp<-rbind(datos.modelo.comp,datos.modelo[750001:900000,])
load("~/workspace/donationstax/iteraciones/modelo2.Rdata")
datos.modelo.comp<-rbind(datos.modelo.comp,datos.modelo[900001:1050000,])
load("~/workspace/donationstax/iteraciones/modelo3.Rdata")
datos.modelo.comp<-rbind(datos.modelo.comp,datos.modelo[1050001:1200000,])
load("~/workspace/donationstax/iteraciones/modelo4.Rdata")
datos.modelo.comp<-rbind(datos.modelo.comp,datos.modelo[1200001:1350000,])
load("~/workspace/donationstax/iteraciones/modelo5.Rdata")
datos.modelo.comp<-rbind(datos.modelo.comp,datos.modelo[1350001:1517981,])
datos.modelo<-datos.modelo.comp

#cambiar tipo de variables 
datos.modelo$Limite.donacion<-as.factor(datos.modelo$Limite.donacion)
datos.modelo$Ley.20900<-as.factor(datos.modelo$Ley.20900)
datos.modelo$Contribuyente_1ra<-as.factor(datos.modelo$Contribuyente_1ra)
datos.modelo$Cert_digital<-as.factor(datos.modelo$Cert_digital)

#se genera BD
save(datos.modelo,file = "Datos_modelo_final_200820_compilacion.Rdata")

load("~/workspace/donationstax/iteraciones/Datos_modelo_final_200820_compilacion.Rdata")
a=which(is.na(datos.modelo$Contribuyente_1ra))
datos.modelo$RUT<-str_replace_all(datos.modelo$RUT,"[\\s]+", " ")
summary(datos.modelo)

for (i in a) {
  print(i)
  rut<-as.numeric(substr(datos.modelo[i,1],1,nchar(datos.modelo[i,1])-1))
  datos.modelo[i,14]<-ifelse(rut>49000000,1,0) #1 si es de primera categoria 
}

a=which(is.na(datos.modelo$Contribuyente_1ra)) #quedan donantes con rut erroneo tipo 9900kksjkd00-0
datos.modelo$RUT_err<-NA
for (i in a) {
  print(i)
  rut<-as.numeric(substr(datos.modelo[i,1],1,nchar(datos.modelo[i,1])-1))
  datos.modelo[i,16]<-ifelse(is.na(rut),"quitar","correcto") #1 si es de primera categoria 
}
datos.modelo$RUT_err<-ifelse(is.na(datos.modelo$RUT_err),"correcto","quitar")

datos.modelo.final<-datos.modelo[datos.modelo$RUT_err!="quitar",] #de 1517981 a  1517839
datos.modelo.final<-datos.modelo.final[,1:15]

#quitar observaciones que no tienen ningun monto 
#quitar<-subset(datos.modelo.final,is.na(datos.modelo.final$monto_FM_UTM) & is.na(datos.modelo.final$monto_total_UTM) & is.na(datos.modelo.final$monto_institucion_UTM))

datos.modelo.final<-datos.modelo.final[!is.na(datos.modelo.final$monto_institucion_UTM),] #de 1517839 a 1517769

sum(is.na(datos.modelo.final$monto_FM_UTM)) #1407223

datos.modelo.final$monto_FM_UTM<-ifelse(is.na(datos.modelo.final$monto_FM_UTM),0,datos.modelo.final$monto_FM_UTM)
sum(is.na(datos.modelo.final$monto_FM_UTM)) #0
sum(is.na(datos.modelo.final$monto_institucion_UTM)) #0

a=which(is.na(datos.modelo.final$monto_total_UTM))
for (i in a) {
  datos.modelo.final[i,5]<-datos.modelo.final[i,4]+datos.modelo.final[i,6]
}

summary(datos.modelo.final)
datos.modelo.final <- na.omit(datos.modelo.final) #de 1517769 a 1517769

#### GENERAR PANEL DE DATOS ####

library(panelr)
library(lubridate)

datos.modelo.final$año<-NA
datos.modelo.final$año<-year(datos.modelo.final$Fecha)
sum(is.na(datos.modelo.final$año))

datos.seleccion<-subset(datos.modelo.final,select =c("RUT","monto_FM_UTM","monto_institucion_UTM",
                                                     "Limite.donacion","Ley.20900","Orden.donacion","Monto_acum_donante",
                                                     "Monto_acum_donatario" ,"Monto_acum_FM","Contribuyente_1ra",
                                                     "Cert_digital","año"))
datos.seleccion$RUT<-as.factor(datos.seleccion$RUT)
str(datos.seleccion)
summary(datos.seleccion)
#save(datos.seleccion,file="Datos_seleccion_modelo.Rdata")

regresion <- panel_data(datos.seleccion, id = RUT, wave = año)

#dv ~           varying_variables | 
#               invariant_variables |
#                 cross_level_interactions/random effects

model <- wbm(monto_institucion_UTM ~ monto_FM_UTM+Monto_acum_FM+Limite.donacion+Orden.donacion+Monto_acum_donante+Monto_acum_donatario |
               Ley.20900 + Contribuyente_1ra + Cert_digital | 
               ( año | RUT), data = regresion)
#boundary (singular) fit: see ?isSingular https://rdrr.io/cran/lme4/man/isSingular.html
#https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models/379068

saveRDS(model, "model.RDS")
#save(model,file="modeloPR200820.Rdata")
summary(model)


library(summarytools)
st_css()

print(dfSummary(datos.seleccion, plain.ascii = FALSE, style = "grid", 
                graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp"), method = 'render',na.col=FALSE)

descr(datos.seleccion)

