
#inicializar ubicacion
setwd("~/CLIMA UPCH/R codes/ENAHO/bases")
#librerias
library(haven) #leer archivos .sav
install.packages('dplyr') #arrange dataframes
library(dplyr)
install.packages('tidyverse') #arrange dataframes part II
library(tidyverse)

library(readxl)

#-2010-------------------------------------------------------------
#-2010-mod100-----------------------------------------------------------

b10100<-read_sav("Enaho01-2010-100.sav") #se almacena la base de datos a trabajar, importado a partir de un archivo .spss

ma10100<-data.frame(b10100$CONGLOME) #creo una base de datos donde almacenaré las variables que me interesan extraer, iniciando a partir de los datos de la columna conglomerado de la base original
colnames(ma10100)[1]<-'HO1CONG' #cambio de nombre a la columna

ma10100$HO1VIVI<-b10100$VIVIENDA #extracción de la variable
ma10100$HO1HOGA<-b10100$HOGAR #extracción de la variable
ma10100$HO1UBIG<-b10100$UBIGEO #extracción de la variable
ma10100$HO1DOMI<-b10100$DOMINIO #extracción de la variable

ma10100$HOINTER<-b10100$P1144 #extracción de la variable
ma10100$HOELEC<-b10100$P1121 #extracción de la variable
ma10100$HOWAT<-b10100$P110 #extracción de la variable
ma10100$HOSSHH<-b10100$P111 #extracción de la variable
#ma10100$HONOB<-b10100$nbi3 

#ma10100<-data.frame(HO1CONG,HO1VIVI,HO1HOGA,HO1UBIG,HO1DOMI,HOINTER,HOELEC,HOWAT,HOSSHH,HONOB)

colnames(ma10100) #muestra los nombres de las columnas almacenadas, verificar que corresponden a los nombres asignados - chekpoint

ma10100$HOID1<-with(ma10100, paste0(HO1CONG,HO1VIVI, HO1HOGA, HO1UBIG, HO1DOMI)) #crear el identificador a nivel hogar

ma10100<-ma10100[colnames(ma10100[c(10,1:9)])] #reordenar variables

colnames(ma10100) #muestra nombres de la columna - checkpoint 2
colnames(ma10100)[5]<-"ubigeo" #cambio de nombre, para implementarlo en otro código

m<-ma10100 #Almacenamiento de el df generado con las variables de interés al df final

#write_sav(m,"ma10100.sav") #Exportación parcial a formato spss

#verificar ID
#n_occur1 <- data.frame(table(ma10100$HOID1))
#n_o1<- data.frame(n_occur1[n_occur1$Freq > 0,])

#write.csv2(ma10100,"ma10100.csv",row.names = FALSE)
#-2010-mod200--------------------------------------------------------
# caract hogar modulo 200
b10200<-read_sav("Enaho01-2010-200.sav")

ma10200<-data.frame(b10200$CONGLOME)
ma10200$HO2CONG<-b10200$CONGLOME


ma10200$HO2VIVI<-b10200$VIVIENDA
ma10200$HO2HOGA<-b10200$HOGAR
ma10200$HO2UBIG<-b10200$UBIGEO
ma10200$HO2DOMI<-b10200$DOMINIO

ma10200$HO2CODPE<-b10200$CODPERSO

ma10200$HOPRELJ<-b10200$P203
ma10200$HOPJENU<-b10200$P203B
ma10200$HOSEXO<-b10200$P207
ma10200$HOEDAD<-b10200$P208A
ma10200$HONDIS<-b10200$P208A1
ma10200$HONDPR<-b10200$P208A2
ma10200$HOMARS<-b10200$P209

ma10200$b10200.CONGLOME<-NULL

#ma10200<- data.frame(HO2CONG,HO2VIVI,HO2HOGA,HO2UBIG,HO2DOMI,HO2CODPE,HOPRELJ,HOPJENU,HOSEXO,HOEDAD,HONDIS,HONDPR,HOMARS)
colnames(ma10200)

ma10200$HOID1<-with(ma10200, paste0(HO2CONG,HO2VIVI, HO2HOGA, HO2UBIG, HO2DOMI))
ma10200$HOID2<-with(ma10200, paste0(HO2CONG,HO2VIVI, HO2HOGA, HO2UBIG, HO2DOMI, HO2CODPE))
ma10200$HOID3<-with(ma10200, paste0(HO2CONG,HO2VIVI, HO2HOGA, HO2UBIG, HO2DOMI, HOPRELJ))

ma10200<-ma10200[colnames(ma10200[c(14,15,16,1:13)])]
str(ma10200)

#combinar con la base de datos generada
m<- left_join(ma10200,ma10100)
colnames(m)

#eliminando columnas que no sirven
m <- m[, -c(17:21)]
m$HOubigeo<-m$HO2UBIG
m$HOdisna<-m$HONDPR

colnames(m)
#comprobando que datos a eliminar no tienen contenido
#subset<-subset(m, is.na(m$HOPJENU))
#sum(is.na(subset$HOPJENU))


#m<-m[!is.na(m$HOPJENU), ]

#-2010-gastos-pendiente---------------------------------------------------------
b10609<-read_sav("Enaho01-2010-609.sav")

#-2010-mod300----------------------------------------------------------
#eduacion modulo 300 - #arreglar internet
b10a300<-read_sav("Enaho01A-2010-300.sav")

HO3CONG<-b10a300$CONGLOME
HO3VIVI<-b10a300$VIVIENDA
HO3HOGA<-b10a300$HOGAR
HO3UBIG<-b10a300$UBIGEO
HO3DOMI<-b10a300$DOMINIO

HO3CODPE<-b10a300$CODPERSO

HOMOTH<-b10a300$P300A
HOEDU<-b10a300$P301A
HOLIT<-b10a300$P302
HOLITC<-b10a300$P302X

HO2INT<- b10a300$P314A

ma10300 <- data.frame(HO3CONG,HO3VIVI,HO3HOGA,HO3UBIG,HO3DOMI,HO3CODPE,HOEDU,HOLIT,HOLITC,HOMOTH,HO2INT)

ma10300$HOID2<-with(ma10300, paste0(HO3CONG,HO3VIVI, HO3HOGA, HO3UBIG, HO3DOMI, HO3CODPE))
colnames(ma10300)

ma10300<-ma10300[colnames(ma10300[c(12,1:11)])]
colnames(ma10300)

m<-left_join(m,ma10300)
colnames(m)

m<-m[,-c(23:28)]

#str(ma10200)

table(ma10300$HOLIT)
table(m$HOLIT)
#-2010-?-----------------------------------------------------------
b10a300a<-read_sav("Enaho01A-2010-300A.sav")

#-2010-mod400--------------------------------------------------

b10a400<-read_sav("Enaho01a-2010-400.sav")

HO4CONG<-b10a400$CONGLOME
HO4VIVI<-b10a400$VIVIENDA
HO4HOGA<-b10a400$HOGAR
HO4UBIG<-b10a400$UBIGEO
HO4DOMI<-b10a400$DOMINIO

HO4CODPE<-b10a400$CODPERSO

HO4EDAD<-b10a400$P400A3
HO4ENF1<-b10a400$P401A

HOSEGSA<-b10a400$P4199 #aquellos que no tienen seguro, 0- pase, 1-no afiliado

P4031<-b10a400$P4031
P4032<-b10a400$P4032
P4033<-b10a400$P4033
P4034<-b10a400$P4034
P4035<-b10a400$P4035
P4036<-b10a400$P4036
P4037<-b10a400$P4037
P4038<-b10a400$P4038
P4039<-b10a400$P4039
P40310<-b10a400$P40310
P40311<-b10a400$P40311
P40312<-b10a400$P40312
P40313<-b10a400$P40313
P40314<-b10a400$P40314


P4041<-b10a400$P4041
P4042<-b10a400$P4042
P4043<-b10a400$P4043
P4044<-b10a400$P4044
P4045<-b10a400$P4045
P4046<-b10a400$P4046
P4047<-b10a400$P4047
P4062<-b10a400$P4062
P407A<-b10a400$P407A
P407B<-b10a400$P407B
P407C<-b10a400$P407C
P407D<-b10a400$P407D
P407E<-b10a400$P407E
P4091<-b10a400$P4091
P4092<-b10a400$P4092
P4093<-b10a400$P4093
P4094<-b10a400$P4094
P4095<-b10a400$P4095
P4096<-b10a400$P4096
P4097<-b10a400$P4097
P4098<-b10a400$P4098
P4099<-b10a400$P4099
P40910<-b10a400$P40910
P40911<-b10a400$P40911

ma10400<-data.frame(HO4CONG,HO4VIVI,HO4HOGA,HO4UBIG,HO4DOMI,HO4CODPE,HOSEGSA,HO4EDAD,P4031,
                    P4032,P4033,P4034,P4035,P4036,P4037,P4038,P4039,P40310,P40311,
                    P40312,P40313,P40314,P4041,P4042,P4043,P4044,P4045,P4046,P4047,P4062,
                    P407A,P407B,P407C,P407D,P407E,P4091,P4092,P4093,P4094,P4095,
                    P4096,P4097,P4098,P4099,P40910,P40911)

ma10400$HOID2<-with(ma10400, paste0(HO4CONG,HO4VIVI, HO4HOGA, HO4UBIG, HO4DOMI, HO4CODPE))
colnames(ma10400)
ma10400<-ma10400[colnames(ma10400[c(47,1:46)])]
colnames(ma10400)
ma10400<-transform(ma10400,HOEDAD4TF=2010-HO4EDAD)
#Dividir base de datos en dos segmentos: vacíos, privados y con respuesta.

com407<-ma10400[complete.cases(ma10400$P407A),]
empty407<-ma10400[is.na(ma10400$P407A),]

com404<-empty407[complete.cases(empty407$P4041),]
empty404<-empty407[is.na(empty407$P4041),]

com409<-empty404[complete.cases(empty404$P4091),]
empty409<-empty404[is.na(empty404$P4091),]

com409$HOAH<-1
empty409$HOAH<-NA
empty404<-rbind(com409,empty409)

com404$HOAH<-0
empty407<-rbind(com404,empty404)

#acceso a la salud del grupo com407 [únicos a los que se le reliza dicha encuesta]
#QAP1 establecimiento cercano
com407$P407A[which(com407$P407A < 3 )] = 0 #acceso cercano
com407$P407A[which(com407$P407A > 1 )] = 1 #acceso lejos o no sabe
#QAP2 tiempo de espera, se exluyen a los que no saben
com407$P407B[which(com407$P407B == 5 )] = NA #exclusion a los que no saben
com407$P407B[which(com407$P407B < 3 )] = 0 #esperan muy o poco
com407$P407B[which(com407$P407B > 1 )] = 1 #esperan demasiado
#QAP3 trato profesional
com407$P407D[which(com407$P407D == 5 )] = NA #exclusion a los que no saben
com407$P407D[which(com407$P407D < 3 )] = 0 #muy bueno o bueno
com407$P407D[which(com407$P407D > 1 )] = 1 #malo
#QAP4 solucion problemas
com407$P407E[which(com407$P407E < 3 )] = 0 #si se soluciono
com407$P407E[which(com407$P407E > 1 )] = 1 #no se soluciono

#QAP6 atencion personal calificado
com407<-transform(com407,P404T=P4042+P4046+P4047)
colnames(com407)
com407<-com407[c(1:29,49,30:48)]

com407$P404T[which(com407$P404T > 1 )] = 1 #no tienen acceso a personal calificado

com407$QAP <- with(com407, ifelse(is.na(P407A) & is.na(P407B) & is.na(P407D) & is.na(P407E) & is.na(P404T), NA, 
                                  rowSums(com407[,c("P407A","P407B","P407D","P407E","P404T")], na.rm=TRUE)))

#Financial = HO4SEG
#Medicinas
#MED1 - encuentra medicinas
com407$P407C[which(com407$P407C < 3 )] = 0 #encuentra todos o la mayoria de medicinas
com407$P407C[which(com407$P407C > 1 )] = 1 #no encuentra la mayoria
#MED2 - recibio servicios de medicinas en la consulta
com407$P4062[which(com407$P4062 == 1 )] = 0 #si recibio servicios de medicina
com407$P4062[which(com407$P4062 == 2 )] = 1 #no recibe

com407<-transform(com407,MED=P407C+P4062)

#ACCESO A LA SALUD
#HOAH=QAP+FIN+MED

com407$HOAH <- with(com407, ifelse(is.na(QAP) & is.na(HOSEGSA) & is.na(MED), NA,
                                   rowSums(com407[,c("QAP","HOSEGSA","MED")], na.rm=TRUE)))

com407$HOAH[which(com407$HOAH > 1 )] = 1 #no tienen acceso a un adecuado servicio de salud



#base de datos auxiliar

#aux400<- data.frame(com407$HOID2,com407$QAP,com407$MED,com407$P404T,
#com407$P407A,com407$P407B,com407$P407C,com407$P407D,
#com407$P407E,com407$P4062)
#elimina columnas [QAP, MED, P404T]
colnames(com407)
com407$QAP<-NULL
com407$MED<-NULL
com407$P404T<-NULL
colnames(empty407)

ma10400<-rbind(com407, empty407)
colnames(ma10400)

table(ma10400$HOAH)
#base de datos final para el año 2010
ma10400v2<-ma10400[,-(10:47),drop=FALSE]
colnames(ma10400v2)

m<-left_join(m,ma10400v2)

colnames(m)
m<-m[,-c(28:33)]
table(ma10400$HOAH)
table(m$HOAH)

#-2010-mod500------------------------------------------------------
##### empleo y trabajo 500
b10a500<-read_sav("Enaho01a-2010-500.sav")

HO5CONG<-b10a500$CONGLOME
HO5VIVI<-b10a500$VIVIENDA
HO5HOGA<-b10a500$HOGAR
HO5UBIG<-b10a500$UBIGEO
HO5DOMI<-b10a500$DOMINIO

HO5CODP<-b10a500$CODPERSO

HOPEA<-b10a500$OCU500

ma10500<-data.frame(HO5CONG,HO5VIVI,HO5HOGA,HO5UBIG,HO5DOMI,HO5CODP,HOPEA)

ma10500$HOID2<-with(ma10500, paste0(HO5CONG,HO5VIVI,HO5HOGA,HO5UBIG,HO5DOMI,HO5CODP))
colnames(ma10500)
ma10500<-ma10500[colnames(ma10500[c(8,1:7)])]

ma10500<-ma10500[,-c(2:7)]

m<-left_join(m,ma10500)
colnames(m)
table(ma10500$HOPEA)
table(m$HOPEA)
#-2010-Corruption perception index-----------------------------------
#no problem with NA
b10b11<-read_sav("Enaho01B-2010-1.sav")

ma101b1<-data.frame(b10b11$CONGLOME)
ma101b1$HO1bCONG<-b10b11$CONGLOME
ma101b1$HO1bVIVI<-b10b11$VIVIENDA
ma101b1$HO1bHOGA<-b10b11$HOGAR
ma101b1$HO1bUBIG<-b10b11$UBIGEO
ma101b1$HO1bDOMI<-b10b11$DOMINIO

ma101b1$HO1bCODP<-b10b11$CODPERSO

ma101b1$HOID2<-with(ma101b1,paste0(HO1bCONG,HO1bVIVI,HO1bHOGA,HO1bUBIG,HO1bDOMI,HO1bCODP))

colnames(ma101b1)
ma101b1$b10b11.CONGLOME<-NULL
ma101b1<-ma101b1[colnames(ma101b1[c(7,1:6)])]

#cálculo de percepcion corrupcion

ma101b1$COR1<-b10b11$`P2_1$01`

#COR1<-as.numeric(unlist(COR1))

ma101b1$COR2<-b10b11$`P11$5`
#COR2<-as.numeric(unlist(COR2))

ma101b1$COR2[ma101b1$COR2 == 5] <- 1

ma101b1$COR3<-b10b11$P22

#COR3<-as.numeric(unlist(COR3))

ma101b1$COR3[ma101b1$COR3 == 2] <- 0
ma101b1$COR3[ma101b1$COR3 == 4] <- 0
ma101b1$COR3[ma101b1$COR3 == 3] <- 1

ma101b1$HOCOR <- with(ma101b1, ifelse(is.na(COR1) & is.na(COR2) & is.na(COR3), NA, 
                                      rowSums(ma101b1[,c("COR1","COR2", "COR3")], na.rm=TRUE)))

table(ma101b1$HOCOR)

ma101b1$HOCOR[which(ma101b1$HOCOR > 1 )] = 1
colnames(ma101b1)
ma101b1<-ma101b1[,-c(2:10)]

m<-left_join(m,ma101b1)
colnames(m)
table(ma101b1$HOCOR)
table(m$HOCOR)

###-2010-proxy jefe de hogar -----------------------------------------
b10b12<-read_sav("Enaho01B-2010-2.sav")

HO1b2CONG<-b10b12$CONGLOME
HO1b2VIVI<-b10b12$VIVIENDA
HO1b2HOGA<-b10b12$HOGAR
HO1b2UBIG<-b10b12$UBIGEO
HO1b2DOMI<-b10b12$DOMINIO

HO1b2CODP<-b10b12$CODPERSO

HOJF2<-b10b12$P46 #proxy del hogar - podria limitarse según edad
HOJF3<-b10b12$P47 #proxy del hogar

ma101b12v1<-data.frame(HO1b2CONG,HO1b2VIVI,HO1b2HOGA,HO1b2UBIG,HO1b2DOMI,HO1b2CODP,HOJF2,HOJF3)
ma101b12v2<-data.frame(HO1b2CONG,HO1b2VIVI,HO1b2HOGA,HO1b2UBIG,HO1b2DOMI,HO1b2CODP,HOJF2,HOJF3)

ma101b12v1$HOJF<-ma101b12v1$HOJF2
ma101b12v2$HOJF<-ma101b12v2$HOJF3

ma101b12v1$HOP203g<-1
ma101b12v2$HOP203g<-2

ma101b12<-rbind(ma101b12v1,ma101b12v2)
#view(ma101v12)
ma101b12$HOID3<-with(ma101b12,paste0(HO1b2CONG,HO1b2VIVI,HO1b2HOGA,HO1b2UBIG,HO1b2DOMI,HOP203g))

colnames(ma101b12)
ma101b12<-ma101b12[colnames(ma101b12[c(11,1:10)])]

ma101b12<-ma101b12[,-c(2:9)]
ma101b12$HOJF[which(ma101b12$HOJF == 8)] = NA
ma101b12$HOJF[which(ma101b12$HOJF == 0)] = NA

# ma101b12$HOID3_copy<-ma101b12$HOID3
# ma101b12$HOID_copy<-NULL

m<-left_join(m,ma101b12)
colnames(m)

table(ma101b12$HOJF)
table(m$HOJF)

# mcom<-m[complete.cases(m$HOID3_copy),]
# ma100com<-ma101b12[complete.cases(ma101b12$HOJF),]
#  
# #A-B, base general - base maestra
# setdiff(ma100com$HOID3,mcom$HOID3_copy)
# #[1] "01270551102010152"
# which(m$HOID3 == "01270551102010152", arr.ind = TRUE)
# which(ma101b12$HOID3 == "01270551102010152", arr.ind = TRUE)
# 
# ma101b12[22355,]
# #Persona en la base de datos madre (ma10200), no se encuentra... la persona tiene un estado civil separado, por lo que no se sabe de dónde salió el cónyuge. Tal vez el dia que los entrevisto se encontró
# #Mestizo
# #[2]"11780341111020122"
# which(m$HOID3 == "11780341111020122", arr.ind = TRUE)
# which(ma101b12$HOID3 == "11780341111020122", arr.ind = TRUE)
# 
# ma101b12[29249,]
# #Persona en la base de datos madre (ma10200), no se encuentra... la persona tiene un estado civil separado, por lo que no se sabe de dónde salió el cónyuge. Tal vez el dia que los entrevisto se encontró
# #Mestizo
# #[3]"16650261107010182"
# which(m$HOID3 == "16650261107010182", arr.ind = TRUE)
# which(ma101b12$HOID3 == "16650261107010182", arr.ind = TRUE)
# 
# ma101b12[32351,]
# #Persona en la base de datos madre (ma10200), no se encuentra... la persona tiene un estado civil separado, por lo que no se sabe de dónde salió el cónyuge. Tal vez el dia que los entrevisto se encontró
# #Es un caso vacío
# 
# #[4]"27271051120060112"
# which(m$HOID3 == "27271051120060112", arr.ind = TRUE)
# which(ma101b12$HOID3 == "27271051120060112", arr.ind = TRUE)
# 
# ma101b12[38490,]
# #Persona en la base de datos madre (ma10200), no se encuentra... la persona tiene un estado civil separado, por lo que no se sabe de dónde salió el cónyuge. Tal vez el dia que los entrevisto se encontró
# #Mestizo
# 
# #[5] "31361081123011032"
# which(m$HOID3 == "31361081123011032", arr.ind = TRUE)
# which(ma101b12$HOID3 == "31361081123011032", arr.ind = TRUE)
# 
# ma101b12[38490,]
# 
# #Persona en la base de datos madre (ma10200), no se encuentra... la persona tiene un estado civil casado, por lo que no se sabe de dónde salió el cónyuge. Tal vez el dia que los entrevisto se encontró
# #Mestizo
# 
# #[6]"32020071124010112"
# which(m$HOID3 == "32020071124010112", arr.ind = TRUE)
# which(ma101b12$HOID3 == "32020071124010112", arr.ind = TRUE)
# 
# ma101b12[41493,]
# #Persona en la base de datos madre (ma10200), no se encuentra... la persona tiene un estado civil separado, por lo que no se sabe de dónde salió el cónyuge. Tal vez el dia que los entrevisto se encontró
# #Mestizo
# 
# #[7]"32381281124010112"
# which(m$HOID3 == "32381281124010112", arr.ind = TRUE)
# which(ma101b12$HOID3 == "32381281124010112", arr.ind = TRUE)
# 
# ma101b12[41713,]
# #Persona en la base de datos madre (ma10200), no se encuentra... la persona tiene un estado civil separado, por lo que no se sabe de dónde salió el cónyuge. Tal vez el dia que los entrevisto se encontró
# #Mestizo

#-2010- labels -----------------------------
m$HOID1 = labelled(m$HOID1,
                   label="Household Identification")

m$HOID2 = labelled(m$HOID2,
                   label="Individual Identification by line number")

m$HOID3 = labelled(m$HOID3,
                   label="Individual Identification by household chief")

m$HOCOR = labelled(m$HOCOR, 
                   #the value labels
                   c(Corrupto= 1, No_corrupto = 0), 
                   # we can also assign a Variable Label in SPSS style
                   label="Corruption index")

m$HOAH = labelled(m$HOAH,
                  c(No_Acceso=1, Acceso=0),
                  label="Acceso a la salud")

m$HO4EDAD = labelled(m$HO4EDAD, 
                     label = "Año de nacimiento")

m$HOEDAD4TF = labelled(m$HOEDAD4TF,
                       label = "Edad en años")

m$HOJF = labelled (m$HOJF, 
                   c(Quechua=1,Aymara=2,De_la_Amazonia=3,Negro_mulato_zambo=4,Blanco=5,Mestizo=6,Otro=7),
                   label = "Por sus antepasados, Ud. se considera (solo para el jefe del hogar o el cónyuge)")

m$HOP203g = labelled(m$HOP203g,
                     c(Jefe_Hogar=1, Cónyuge=2),
                     label = "Parentesco con el jefe del hogar generado")
#-2010- generated variables  ----------------------
HO2010<-m
colnames(HO2010)

table(HO2010$HOJF)

# % of indigenous population1 [Amazonic, Andean, local communities]

HO2010$HOINDG1 <- HO2010$HOJF
HO2010$HOINDG1[which(HO2010$HOINDG1  == 1)] = 1
HO2010$HOINDG1[which(HO2010$HOINDG1  == 2)] = 1
HO2010$HOINDG1[which(HO2010$HOINDG1  == 3)] = 1
HO2010$HOINDG1[which(HO2010$HOINDG1  == 4)] = 0
HO2010$HOINDG1[which(HO2010$HOINDG1  == 5)] = 0
HO2010$HOINDG1[which(HO2010$HOINDG1  == 6)] = 0
HO2010$HOINDG1[which(HO2010$HOINDG1  == 7)] = 0

HO2010$HOINDG1 = labelled(HO2010$HOINDG1,
                          c(Si=1, No=0),
                          label = "Población indígena compuesta por Quechua, Aymara y de la Amazonía")
# % of indigenous population2 [Amazonic, local communities]


HO2010$HOINDG2 <- HO2010$HOJF
HO2010$HOINDG2[which(HO2010$HOINDG2  == 1)] = 0
HO2010$HOINDG2[which(HO2010$HOINDG2  == 2)] = 0
HO2010$HOINDG2[which(HO2010$HOINDG2  == 3)] = 1
HO2010$HOINDG2[which(HO2010$HOINDG2  == 4)] = 0
HO2010$HOINDG2[which(HO2010$HOINDG2  == 5)] = 0
HO2010$HOINDG2[which(HO2010$HOINDG2  == 6)] = 0
HO2010$HOINDG2[which(HO2010$HOINDG2  == 7)] = 0

HO2010$HOINDG2 = labelled(HO2010$HOINDG2,
                          c(Si=1, No=0),
                          label = "Población indígena compuesta por solo de la Amazonía")
# % of minority and indigenous comunities1 [afroperuvians, indigenous population]

HO2010$HOINDG3 <- HO2010$HOJF
HO2010$HOINDG3[which(HO2010$HOINDG3  == 1)] = 0
HO2010$HOINDG3[which(HO2010$HOINDG3  == 2)] = 0
HO2010$HOINDG3[which(HO2010$HOINDG3  == 3)] = 1
HO2010$HOINDG3[which(HO2010$HOINDG3  == 4)] = 1
HO2010$HOINDG3[which(HO2010$HOINDG3  == 5)] = 0
HO2010$HOINDG3[which(HO2010$HOINDG3  == 6)] = 0
HO2010$HOINDG3[which(HO2010$HOINDG3  == 7)] = 0

HO2010$HOINDG3 = labelled(HO2010$HOINDG3,
                          c(Si=1, No=0),
                          label = "Población minoritaria (afroperuana) y de pob indígena de la Amazonía")

# % of minority and indigenous comunities2 [afroperuvians, indigenous population]

HO2010$HOINDG4 <- HO2010$HOJF
HO2010$HOINDG4[which(HO2010$HOINDG4  == 1)] = 1
HO2010$HOINDG4[which(HO2010$HOINDG4  == 2)] = 1
HO2010$HOINDG4[which(HO2010$HOINDG4  == 3)] = 1
HO2010$HOINDG4[which(HO2010$HOINDG4  == 4)] = 1
HO2010$HOINDG4[which(HO2010$HOINDG4  == 5)] = 0
HO2010$HOINDG4[which(HO2010$HOINDG4  == 6)] = 0
HO2010$HOINDG4[which(HO2010$HOINDG4  == 7)] = 0

HO2010$HOINDG4 = labelled(HO2010$HOINDG4,
                          c(Si=1, No=0),
                          label = "Población minoritaria (afroperuana) y pob indigena (aymara, quechua y de la amazonia)")

# % of other racial/ethnic minotiries [afroperuvians]

HO2010$HOINDG5 <- HO2010$HOJF
HO2010$HOINDG5[which(HO2010$HOINDG5  == 1)] = 0
HO2010$HOINDG5[which(HO2010$HOINDG5  == 2)] = 0
HO2010$HOINDG5[which(HO2010$HOINDG5  == 3)] = 0
HO2010$HOINDG5[which(HO2010$HOINDG5  == 4)] = 1
HO2010$HOINDG5[which(HO2010$HOINDG5  == 5)] = 0
HO2010$HOINDG5[which(HO2010$HOINDG5  == 6)] = 0
HO2010$HOINDG5[which(HO2010$HOINDG5  == 7)] = 0

HO2010$HOINDG5 = labelled(HO2010$HOINDG5,
                          c(Si=1, No=0),
                          label = "Población minoritaria afroperuana")

# personas mayores a 15
HO2010$HOABOVE15 <- HO2010$HOEDAD 
HO2010$HOABOVE15[which(HO2010$HOABOVE15  < 15)] = 0
HO2010$HOABOVE15[which(HO2010$HOABOVE15  >= 15)] = 1

HO2010$HOABOVE15 = labelled(HO2010$HOABOVE15,
                            c(Menor_15=0, Mayor_igual_15=1),
                            label = "Pob. mayor a 15 años")

# personas mayores a 25
HO2010$HOABOVE25 <- HO2010$HOEDAD 
HO2010$HOABOVE25[which(HO2010$HOABOVE25  < 25)] = 0
HO2010$HOABOVE25[which(HO2010$HOABOVE25  >= 25)] = 1

HO2010$HOABOVE25 = labelled(HO2010$HOABOVE25,
                            c(Menor_25=0, Mayor_igual_25=1),
                            label = "Pob. mayor a 25 años")

#niños menores a 5 años
HO2010$HOmenor5<- HO2010$HOEDAD
HO2010$HOmenor5[which(HO2010$HOmenor5 < 5)] = 1
HO2010$HOmenor5[which(HO2010$HOmenor5 > 1)] = 0

HO2010$HOmenor5 = labelled(HO2010$HOmenor5, 
                           #the value labels
                           c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                           # we can also assign a Variable Label in SPSS style
                           label="Child below 5 years old")
#adultos mayores a 65 años
HO2010$HOmayor65<- HO2010$HOEDAD
HO2010$HOmayor65[which(HO2010$HOmayor65 <= 65)] = 0
HO2010$HOmayor65[which(HO2010$HOmayor65 > 1)] = 1

HO2010$HOmayor65 = labelled(HO2010$HOmayor65, 
                            #the value labels
                            c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                            # we can also assign a Variable Label in SPSS style
                            label="Adult older than 65 years old")

#adultos menores a 15 años y mayores a 65 años
HO2010$HO1565<- HO2010$HOEDAD
HO2010$HO1565[which(HO2010$HO1565 < 15)] = 2
HO2010$HO1565[which(HO2010$HO1565 > 65)] = 1
HO2010$HO1565[which(HO2010$HO1565 > 4)] = 0
HO2010$HO1565[which(HO2010$HO1565 == 2)] = 1

HO2010$HO1565 = labelled(HO2010$HO1565, 
                         #the value labels
                         c( Entre_15y65_años= 1,  Fuera_de_rango= 0), 
                         # we can also assign a Variable Label in SPSS style
                         label="Adultos mayores a 15 y a 65")

#aquellos que no hablan castellano, ingles, portugues u otra lengua extranjera. 
HO2010$HONOCAST<-HO2010$HOMOTH
HO2010$HONOCAST[which(HO2010$HONOCAST  == 8)] = NA
HO2010$HONOCAST[which(HO2010$HONOCAST  < 4 )] = 1
HO2010$HONOCAST[which(HO2010$HONOCAST  > 1)] = 0

HO2010$HONOCAST = labelled(HO2010$HONOCAST, 
                           #the value labels
                           c(castellano_otros = 0, quechua_aymara_lgnativa = 1), 
                           # we can also assign a Variable Label in SPSS style
                           label="Población cuya lengua materna no es castellano")
#datos discapacitados
HO2010$HODISC1<-HO2010$HOMOTH
HO2010$HODISC1[which(HO2010$HODISC1  < 8 )] = 0
HO2010$HODISC1[which(HO2010$HODISC1  == 8 )] = 1

HO2010$HODISC1 = labelled(HO2010$HODISC1, 
                          #the value labels
                          c(no_sordomudo = 0, sordomudo = 1), 
                          # we can also assign a Variable Label in SPSS style
                          label="Dificultad para comunicarse_sordomudo")

#jefehogar
HO2010$HOJFx<-HO2010$HOPRELJ 
HO2010$HOJFx[which(HO2010$HOJFx  > 1 )] = 0

HO2010$HOJFx = labelled(HO2010$HOJFx,
                        c(Jefe_Hogar = 1, Otro = 0),
                        label = "Solo Jefe del Hogar")

#HOEDU2 - pob. con al menos ed. secundaria y mayor a 25 años
HO2010$HOEDU2<- HO2010$HOEDU
HO2010$HOEDU2[which(HO2010$HOEDU2 == 1 )] = 0 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 2 )] = 0 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 3 )] = 0 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 4 )] = 0 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 5 )] = 0 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 6 )] = 1 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 7 )] = 1 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 8 )] = 1 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 9 )] = 1 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 10 )] = 1 #sin nivel
HO2010$HOEDU2[which(HO2010$HOEDU2 == 11 )] = 1 #sin nivel

HO2010$HOEDU2 = labelled(HO2010$HOEDU2,
                         c(No = 0, Si =1),
                         label = " Pob. con al menos ed. secundaria completa (cambio de etiquetas a HOEDU2)")
HO2010$HOEDU2x <- with(HO2010, ifelse(is.na(HOEDU2) & is.na(HOABOVE25), NA, 
                                      rowSums(HO2010[,c("HOEDU2","HOABOVE25")], na.rm=TRUE)))

HO2010$HOEDU2x[which(HO2010$HOEDU2x == 1 )] = 0 #NO CUMPLE
HO2010$HOEDU2x[which(HO2010$HOEDU2x == 2 )] = 1 #si CUMPLE

HO2010$HOEDU2x <- labelled(HO2010$HOEDU2x, 
                           c(No = 0, Si = 1),
                           label = "pob. con al menos ed. secundaria completa y mayor a 25 años")

#HOLIT2 - pob. que sabe leer y escribir mayor = a 15 años
HO2010$HOLITE<-HO2010$HOLIT
HO2010$HOLITE[which(HO2010$HOLITE == 2 )] = 0 #cambio de etiqueta.  

HO2010$HOLITE <- labelled(HO2010$HOLITE,
                          c(No = 0, Si = 1), 
                          label = " Sabe leer y escribir (cambio de etiqueta de HOLIT)")

HO2010$HOLIT2 <- with(HO2010, ifelse(is.na(HOLITE) & is.na(HOABOVE15), NA, 
                                     rowSums(HO2010[,c("HOLITE","HOABOVE15")], na.rm=TRUE)))

HO2010$HOLIT2[which(HO2010$HOLIT2 == 1 )] = 0 #NO CUMPLE
HO2010$HOLIT2[which(HO2010$HOLIT2 == 2 )] = 1 #si CUMPLE

HO2010$HOLIT2 = labelled(HO2010$HOLIT2, 
                         #the value labels
                         c(no = 0, si = 1), 
                         # we can also assign a Variable Label in SPSS style
                         label="Pob. que sabe leer y escribir Y tiene mayor igual 15 años de edad")

#HOLIT2x - pob. sabe leer y escribir mayor = a 15 años - prueba con cartilla
HO2010$HOLITCE<-HO2010$HOLITC
HO2010$HOLITCE[which(HO2010$HOLITCE == 2 )] = 0 #cambio de etiqueta.  
HO2010$HOLITCE[which(HO2010$HOLITCE == 3 )] = NA #cambio de etiqueta.  

HO2010$HOLITCE <- labelled (HO2010$HOLITCE,
                            c(No = 0, Si = 1),
                            label = "saber leer y escribir (preg. con cartilla), cambio de etiqueta de HOLITC")

HO2010$HOLIT2x <- with(HO2010, ifelse(is.na(HOLITCE) & is.na(HOABOVE15), NA, 
                                      rowSums(HO2010[,c("HOLITCE","HOABOVE15")], na.rm=TRUE)))

HO2010$HOLIT2x[which(HO2010$HOLIT2x == 1 )] = 0 #NO CUMPLE
HO2010$HOLIT2x[which(HO2010$HOLIT2x == 2 )] = 1 #si CUMPLE

HO2010$HOLIT2x = labelled(HO2010$HOLIT2x, 
                          #the value labels
                          c(no = 0, si = 1), 
                          # we can also assign a Variable Label in SPSS style
                          label="Pob. que sabe leer y escribir (preg. con cartilla) Y tiene mayor igual 15 años de edad")

#-2010- optional variables ---------------------------------

#-2010- cambiar ubigeo -------------------------------------
HO2010_copy<-HO2010
cd<- HO2010 #copia de la base para ingresar a la funcion
ubigeo()

HO2010<-cd2

#HOMIGRA - migración interna
#no puede ser definida antes, pues la función anterior permite realizar la limpieza de datos
HO2010$HOMIGRA <-with(HO2010, ifelse(is.na(HOubigeo) | is.na(HOdisna), NA,ifelse(HO2010$HOubigeo == HO2010$HOdisna,1,0))) 

HO2010$HOMIGRA = labelled(HO2010$HOMIGRA, 
                          c(No = 0, Si = 1),
                          label = "¿Vive en el mismo distrito de nacimiento actualmente?")

#-2010- reordenar columnas------------------------------------
colnames(HO2010)
HO2010<-HO2010[colnames(HO2010[c(1:6,8,7,14,15,21,22,9:11,
                                 34,35,48,12,13,29,30,41:45,
                                 16,23:25,51:54,26,36:40,46:47,
                                 49,50,55,17:20,27,28,31,32,33)])] 

#-2010- complete base --------------------------------------
write_sav(HO2010,"HO2010.sav")
