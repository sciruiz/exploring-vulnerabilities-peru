#inicializar
setwd("~/CLIMA UPCH/R codes/ALLSAV")
library(haven)
#install.packages('xlsx')
library(xlsx)
library(dplyr)
#install.packages('tidyverse') #arrange dataframes part II
library(tidyverse)

#regex for view view[(].[0-9]*[)]
#regex for view[(].[0-9]*[_][0-9][)]

#****2010 beggin -------------------------------------------------
#2010 base general - unknown level--------------------------
a010<-read_sav("2010-64-260_1-CH_RECH0.SAV")
ma10<-data.frame(a010$HHID) #copy of original data to master #RECH0

ma10$ENDHHID<-a010$HHID
ma10$ENDHOMEN<-a010$HV002
ma10$ENDLINEN<-a010$HV003
ma10$ENDHOMEQ<-a010$HV009
ma10$ENDWOMENEL<-a010$HV010
ma10$ENDCHILD5<-a010$HV014
ma10$ENDDOMAIN<-a010$HV023
ma10$ENDREGION<-a010$HV024
ma10$ENDURBRU<-a010$HV025
ma10$ENDRES<-a010$HV026

ma10 <- ma10[, -c(1)]

#n_occur1 <- data.frame(table(ma10$ENDHHID))
#n_o1<- data.frame(n_occur1[n_occur1$Freq > 0,])

#colnames(ma10)
## setwd("~/CLIMA UPCH/R codes")
#write.csv2(ma10,"ma10.csv",row.names = FALSE)
## setwd("~/CLIMA UPCH/R codes/ALLSAV")

#2010 1-CH base general - population level -------------
#2010 1-CH (1)
a10<-read_sav("2010-64-260_1-CH_RECH1.SAV")
copy<-data.frame(a10$HHID)

copy$ENDHHID<-a10$HHID
copy$ENDIDX<-a10$HVIDX
copy$ENDRPJEFE<-a10$HV101
copy$ENDSEXO<-a10$HV104
copy$ENDEDAD<-a10$HV105
copy$ENDEDU<-a10$HV109
copy$ENDMSTA<-a10$HV115 #marital status
copy$ENDEMAR<-a10$HV116

copy$ECASEID<-paste(copy$ENDHHID,"",copy$ENDIDX)

copy <- copy[, -c(1)]

copy<-copy[colnames(copy[c(1,9,2:8)])]

#n_occur1 <- data.frame(table(copy$ECASEID))
#n_o1<- data.frame(n_occur1[n_occur1$Freq > 0,])

copy$ENDMSTA1<-a10$HV115 #marital status #nunca casado-0
copy$ENDMSTA1[which(copy$ENDMSTA1 > 1)] = 1

copy$ENDMSTA2<-a10$HV115 #marital status #nunca casado + viudo =0
copy$ENDMSTA2[which(copy$ENDMSTA2 == 2)] = 1
copy$ENDMSTA2[which(copy$ENDMSTA2 == 3)] = 0
copy$ENDMSTA2[which(copy$ENDMSTA2 == 4)] = 1
copy$ENDMSTA2[which(copy$ENDMSTA2 == 5)] = 1

copy$ENDMSTA3<-a10$HV115 #marital status #nunca casado + viudo + separado =0
copy$ENDMSTA3[which(copy$ENDMSTA3 == 2)] = 1
copy$ENDMSTA3[which(copy$ENDMSTA3 == 3)] = 0
copy$ENDMSTA3[which(copy$ENDMSTA3 == 4)] = 1
copy$ENDMSTA3[which(copy$ENDMSTA3 == 5)] = 0

copy$ENDEDAD1<-a10$HV105 #teenagers who are mother 15-19
copy$ENDEDAD1[which(copy$ENDEDAD1 >19)] = 0
copy$ENDEDAD1[which(copy$ENDEDAD1 <15)] = 0
copy$ENDEDAD1[which(copy$ENDEDAD1 >0)] = 1

copy$ENDEDAD2<-a10$HV105 #teen who are mother 12-19
copy$ENDEDAD2[which(copy$ENDEDAD2 >19)] = 0
copy$ENDEDAD2[which(copy$ENDEDAD2 <12)] = 0
copy$ENDEDAD2[which(copy$ENDEDAD2 >0)] = 1

endesf2010<-left_join(copy,ma10)
colnames(endesf2010)

endesf2010<-endesf2010[colnames(endesf2010[c(2,1,3:23)])]

#2010 1-CH (4) - seguro de salud------------
a110r4<-read_sav("2010-64-260_1-CH_RECH4.SAV")

colnames(a110r4)
ma1014<-data.frame(a110r4$HHID)
#copy$IDXH4_1CH4<-a110r4$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma1014$ECASEID<-paste(a110r4$HHID,"",a110r4$IDXH4)
ma1014$EDNSINSEG<-a110r4$SH11Z # no tiene seguro de salud
ma1014$ENDCOMSEG<-a110r4$SH11D # compañia de seguro
ma1014$ENDPRISEG<-a110r4$SH11E # seguro privado

ma1014 <- ma1014[, -c(1)]
colnames(ma1014)

endesf2010<-left_join(endesf2010,ma1014)
colnames(endesf2010)

#if (identical(copy[['CID1CH1']],copy[['END_CASE_ID4']]) == TRUE) 
#{copy$END_CASE_ID4<- NULL}

## setwd("~/CLIMA UPCH/R codes")
#write.csv2(copy,"ma10-1ch1_ch4.csv",row.names = FALSE)
## setwd("~/CLIMA UPCH/R codes/ALLSAV")

# 2010 cardiovascular disease household level --------------------------------------------------    
#2010 1-CH (9) - cardiovascular [a nivel de hogar] - bd hogar ¿? - not included 
#only available for 2010, 2011 a 2013, - keep as a separated dataframe.
a110r9<-read_sav("2010-1-CH-RECH9.sav")

ma1019<-data.frame(a110r9$HHID)
ma1019$ENDHHID<-a110r9$HHID
#ma1019$ENDCARD<-a110r9$SH28
ma1019$ENDDIAB<-a110r9$SH29 #diabetes

ma1019<- ma1019[-c(1)]
basehouse2010<-left_join(ma10,ma1019)

#endesf20102010<-left_join(a010,ma1019) - not merged, would increase values,is a question at a hh level
#ma10_1ch_9<- data.frame(END_HHID_CH9,ENDCARD)

#2010 2CV -----------------------------------------------------------
#2010 1-CH general - people level

#2010 2-CV - household level - with cardiac disease from 1CH - ?
a210<-read_sav("2010-65-260_2-CV_RECH23.SAV") 
colnames(a210)

ma10223<-data.frame(a210$HHID) #HHID del hogar

ma10223$ENDHHID<-a210$HHID
ma10223$ENDWATER<-a210$HV201 #fuente del agua potable
ma10223$ENDTOILET<-a210$HV205 #hogar toilet facility
ma10223$ENDELEC<-a210$HV206 #hogar electricidad
ma10223$ENDHHJX<-a210$HV219 #sexo del jefe del hogar
ma10223$ENDWLOC<-a210$HV235 #ubicacion fuente de agua
ma10223$ENDWDAY<-a210$SH42 #agua potable todo el dia
ma10223$ENDWWEEK <-a210$SH43 
ma10223$ENDWSAVE <-a210$SH48
ma10223$ENDINTER <-a210$SH61Q #acceso a internet

ma10223 <- ma10223[-c(1)]

endesf2010<-left_join(endesf2010,ma10223)
basehouse2010<-left_join(basehouse2010,ma10223)
#ma10_2<-data.frame(END_HHID_2CH, ENDWATER, ENDTOILET, ENDELEC, ENDHHJX, ENDWLOC, ENDWDAY, ENDWWEEK, ENDWSAVE, ENDINTER)

## setwd("~/CLIMA UPCH/R codes")
#write.csv2(ma10_2,"ma10_2.csv",row.names = FALSE)
## setwd("~/CLIMA UPCH/R codes/ALLSAV")

#could add data of

#2010 3-DEBMF REC0111 - no añadido 
#a310111<-read_sav("2010-66-260_3-DBMEF_REC0111.SAV")
#HID_3_REC0111<-a310111$CASEID
#ENDNKID<-a310111$V137 #numero de niños

#ma10_3<-data.frame(HID_3_REC0111,ENDNKID)

## setwd("~/CLIMA UPCH/R codes")
#write.csv2(ma10_3,"ma10_3.csv",row.names = FALSE)
## setwd("~/CLIMA UPCH/R codes/ALLSAV")

#2010 3-DEBMF REC0111 #no useful - mother tongue learned only by woman   
#  a31091<-read_sav("2010-66-260_3-DBMEF_REC91.SAV")
#  
#  colnames(a31091)

#  HHID_3DE<-a31091$CASEID
#  s119<-a31091$S119 #lengua materna aprendida en la niñez (mujer)

#  ma10_3<-data.frame()
#  # setwd("~/CLIMA UPCH/R codes")
#  write.xlsx(ma10_2,file,sheetName="Sheet1",col.names=FALSE,row.names=FALSE, showNA=TRUE,password=NULL)
#  write.csv2(ma10_2,"ma10_3.csv",row.names = FALSE)
#  # setwd("~/CLIMA UPCH/R codes/ALLSAV")

#2010 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a410<-read_sav("2010-67-260_4-HNAC_RE223132.SAV")

ma10410<-data.frame(a410$CASEID)
ma10410$ECASEID<-a410$CASEID
ma10410$ENDPREG<-a410$V213 #actualmente embarazada
#ma10410$ENDCHT<-a410$V201 #total de niños que alguna vez nacieron
#ma10410$ENDCHDa<-a410$V206 #hijos que han muerto
#ma10410$ENDCHDb<-a410$V207 #hijas que han muerto
#ma10410$ENDCHLIVE<-a410$V218 #numero de niños con vida
ma10410$ENDCHLEMB<-a410$V219 #número de niños con vida + embarazo 

ma10410$ENDnin<-a410$V219 #0-no tiene, 1 - tiene hijos
ma10410$ENDnin[which(ma10410$ENDnin > 1)] = 1


#ma10410$ENDCHLEMBT<-a410$V220 #más de 6 niños + embarazp

ma10410<-ma10410[-c(1)]

#ma10410$ENDCHD <- with(ma10410, ifelse(is.na(ENDCHT) & is.na(ENDCHDa) & is.na(ENDCHDb), NA, 
#rowSums(ma10410[,c("ENDCHDa","ENDCHDb")], na.rm=TRUE)))
#ma10410$ENDCHILD <- with(ma10410, ifelse(is.na(ENDCHD) & is.na(ENDCHT), NA, 
#rowSums(ma10410[,c("ENDCHD","ENDCHDb")], na.rm=TRUE)))

#ma10410 <- transform(ma10410,ENDCHILD=ENDCHT-ENDCHD) #total de niños que alguna vez nacieron y siguen vivos
#ma10410$ENDborn<-ma10410$ENDCHILD 
#ma10410$ENDborn[which(ma10410$ENDborn > 1)] = 1 #0-niños que alguna vez nacieron


endesf2010<-left_join(endesf2010,ma10410)
colnames(endesf2010)
## setwd("~/CLIMA UPCH/R codes")
#write.csv2(ma10_4,"ma10_4.csv",row.names = FALSE)
## setwd("~/CLIMA UPCH/R codes/ALLSAV")

#2010 9-MMVF ------------------------
a910<-read_sav("2010-73-260_9-MMVF_REC84DV.SAV")
ma10_9<-data.frame(a910$CASEID)
colnames(ma10_9)

ma10_9$ECASEID <- a910$CASEID
ma10_9$ENDCORT <- a910$MMC5
ma10_9$ENDVIO1 <- a910$D101A
ma10_9$ENDVIO2 <- a910$D101B
ma10_9$ENDVIO3 <- a910$D101C
ma10_9$ENDVIO4 <- a910$D101D
ma10_9$ENDVIO5 <- a910$D101E
ma10_9$ENDVIO6 <- a910$D101F
ma10_9$ENDVIO7 <- a910$D103A
ma10_9$ENDVIO8 <- a910$D103B
ma10_9$ENDVIO9 <- a910$D103C
ma10_9$ENDVIO10 <- a910$D103D
ma10_9$ENDVIO11 <- a910$D105A
ma10_9$ENDVIO12 <- a910$D105B
ma10_9$ENDVIO13 <- a910$D105C
ma10_9$ENDVIO14 <- a910$D105D
ma10_9$ENDVIO15 <- a910$D105E
ma10_9$ENDVIO16 <- a910$D105F
ma10_9$ENDVIO17 <- a910$D105G
ma10_9$ENDVIO18 <- a910$D105H
ma10_9$ENDVIO19 <- a910$D105I
ma10_9$ENDVIO20 <- a910$D105J
ma10_9$ENDVIO21 <- a910$D110A
ma10_9$ENDVIO22 <- a910$D110B
ma10_9$ENDVIO23 <- a910$D110C
ma10_9$ENDVIO24 <- a910$D110D
ma10_9$ENDVIO25 <- a910$D110E
ma10_9$ENDVIOB1 <- a910$D115C
ma10_9$ENDVIOB2 <- a910$D115E
ma10_9$ENDVIOB3 <- a910$D115G
ma10_9$ENDVIOB4 <- a910$D115I
ma10_9$ENDVIOB5 <- a910$D115J
ma10_9$ENDVIOB6 <- a910$D115K
ma10_9$ENDVIOB7 <- a910$D115L
ma10_9$ENDVIOB8 <- a910$D115N
ma10_9$ENDVIOB9 <- a910$D115P
ma10_9$ENDVIOB10 <- a910$D115R
ma10_9$ENDVIOB11 <- a910$D115T
#ma10_9$ENDVIOB12 <- a910$D115Y #eliminar ENVIOB12 por no ser adecuada para el calculo
ma10_9$ENDVIOB13 <- a910$D115XC
ma10_9$ENDVIOE1 <- a910$D118A
ma10_9$ENDVIOE2 <- a910$D118C
ma10_9$ENDVIOE3 <- a910$D118E
ma10_9$ENDVIOE4 <- a910$D118G
ma10_9$ENDVIOE5 <- a910$D118I
ma10_9$ENDVIOE6 <- a910$D118J
ma10_9$ENDVIOE7 <- a910$D118K
ma10_9$ENDVIOE8 <- a910$D118L
ma10_9$ENDVIOE9 <- a910$D118N
ma10_9$ENDVIOE10 <- a910$D118P
ma10_9$ENDVIOE11 <- a910$D118T
ma10_9$ENDVIOE12 <- a910$D118XC
ma10_9$ENDVIOINT1 <- a910$D122A
ma10_9$ENDVIOINT2 <- a910$D122B
ma10_9$ENDVIOINT3 <- a910$D122C

ma10_9<- ma10_9[-c(1)] 

colnames(ma10_9)

ma10_9$ENDVIO1[which(ma10_9$ENDVIO1 == 8)] = NA
ma10_9$ENDVIO2[which(ma10_9$ENDVIO2 == 8)] = NA
ma10_9$ENDVIO3[which(ma10_9$ENDVIO3 == 8)] = NA
ma10_9$ENDVIO4[which(ma10_9$ENDVIO4 == 8)] = NA
ma10_9$ENDVIO5[which(ma10_9$ENDVIO5 == 8)] = NA
ma10_9$ENDVIO6[which(ma10_9$ENDVIO6 == 8)] = NA

ma10_9$ENDVIO7[which(ma10_9$ENDVIO7 == 3)] = 0
ma10_9$ENDVIO7[which(ma10_9$ENDVIO7 > 1)] = 1

ma10_9$ENDVIO8[which(ma10_9$ENDVIO8 == 3)] = 0
ma10_9$ENDVIO8[which(ma10_9$ENDVIO8 > 1)] = 1

ma10_9$ENDVIO10[which(ma10_9$ENDVIO10 == 3)] = 0
ma10_9$ENDVIO10[which(ma10_9$ENDVIO10 > 1)] = 1

ma10_9$ENDVIO11[which(ma10_9$ENDVIO11 == 3)] = 0
ma10_9$ENDVIO11[which(ma10_9$ENDVIO11 > 1)] = 1

ma10_9$ENDVIO12[which(ma10_9$ENDVIO12 == 3)] = 0
ma10_9$ENDVIO12[which(ma10_9$ENDVIO12 > 1)] = 1

ma10_9$ENDVIO13[which(ma10_9$ENDVIO13 == 3)] = 0
ma10_9$ENDVIO13[which(ma10_9$ENDVIO13 > 1)] = 1

ma10_9$ENDVIO14[which(ma10_9$ENDVIO14 == 3)] = 0
ma10_9$ENDVIO14[which(ma10_9$ENDVIO14 > 1)] = 1

ma10_9$ENDVIO15[which(ma10_9$ENDVIO15 == 3)] = 0
ma10_9$ENDVIO15[which(ma10_9$ENDVIO15 > 1)] = 1

ma10_9$ENDVIO16[which(ma10_9$ENDVIO16 == 3)] = 0
ma10_9$ENDVIO16[which(ma10_9$ENDVIO16 > 1)] = 1

ma10_9$ENDVIO17[which(ma10_9$ENDVIO17 == 3)] = 0
ma10_9$ENDVIO17[which(ma10_9$ENDVIO17 > 1)] = 1

ma10_9$ENDVIO18[which(ma10_9$ENDVIO18 == 3)] = 0
ma10_9$ENDVIO18[which(ma10_9$ENDVIO18 > 1)] = 1

ma10_9$ENDVIO19[which(ma10_9$ENDVIO19 == 3)] = 0
ma10_9$ENDVIO19[which(ma10_9$ENDVIO19 > 1)] = 1

ma10_9$ENDVIOB1[which(ma10_9$ENDVIOB1 == 8)] = NA
ma10_9$ENDVIOB2[which(ma10_9$ENDVIOB2 == 8)] = NA
ma10_9$ENDVIOB3[which(ma10_9$ENDVIOB3 == 8)] = NA
ma10_9$ENDVIOB4[which(ma10_9$ENDVIOB4 == 8)] = NA
ma10_9$ENDVIOB5[which(ma10_9$ENDVIOB5 == 8)] = NA
#ma10_9$ENDVIOB6[which(ma10_9$ENDVIOB6 == 8)] = NA
#ma10_9$ENDVIOB7[which(ma10_9$ENDVIOB7 == 8)] = NA
ma10_9$ENDVIOB8[which(ma10_9$ENDVIOB8 == 8)] = NA
ma10_9$ENDVIOB9[which(ma10_9$ENDVIOB9 == 8)] = NA
#ma10_9$ENDVIOB10[which(ma10_9$ENDVIOB10 == 8)] = NA
ma10_9$ENDVIOB11[which(ma10_9$ENDVIOB11 == 8)] = NA
ma10_9$ENDVIOB13[which(ma10_9$ENDVIOB13 == 8)] = NA

ma10_9$ENDVIOINT1[which(ma10_9$ENDVIOINT1 == 2)] = 1
ma10_9$ENDVIOINT2[which(ma10_9$ENDVIOINT2 == 2)] = 1
ma10_9$ENDVIOINT3[which(ma10_9$ENDVIOINT3 == 2)] = 1

ma10_9$ENDVIO <- with(ma10_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma10_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma10_9$ENDVIO))
table(ma10_9$ENDVIO)

ma10_9$ENDVIO[which(ma10_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino
## setwd("~/CLIMA UPCH/R codes")
#write.csv2(ma10_9,"ma10_9.csv",row.names = FALSE)
## setwd("~/CLIMA UPCH/R codes/ALLSAV")

#reduced df
ma109<-ma10_9
ma109<-ma109[-c(2:54)]

#n_occur1 <- data.frame(table(ma109$ECASEID))
#n_o1<- data.frame(n_occur1[n_occur1$Freq > 0,])

endesf2010<-left_join(endesf2010,ma109)

## setwd("~/CLIMA UPCH/R codes")
#write.csv2(ma109,"ma109.csv",row.names = FALSE)
## setwd("~/CLIMA UPCH/R codes/ALLSAV")

#2010 10-PETA - desnutricion -------------------
a1010<-read_sav("2010-74-260_10-PETA_RECH6.SAV")

ma1010<-data.frame(a1010$HHID)
ma1010$ECASEID<-paste(a1010$HHID,"",a1010$HC0)
ma1010$ENDTAED<-a1010$HC5
ma1010 <- transform(ma1010, ENDTAEDC = ENDTAED / 100)

ma1010$ENDPEED<-a1010$HC8
ma1010 <- transform(ma1010, ENDPEEDC = ENDPEED / 100)

ma1010$ENDPETA<-a1010$HC11
ma1010 <- transform(ma1010, ENDPETAC = ENDPETA / 100)

ma1010<-ma1010[-c(1,3,5,7)]
ma1010$ENDstun<-ma1010$ENDTAEDC
ma1010$ENDstun[which(ma1010$ENDstun > -2)] = 0
ma1010$ENDstun[which(ma1010$ENDstun <= -2)] = 1

ma1010$ENDwas<-ma1010$ENDPETAC
ma1010$ENDwas[which(ma1010$ENDwas > -2)] = 0
ma1010$ENDwas[which(ma1010$ENDwas <= -2)] = 1

endesf2010<-left_join(endesf2010,ma1010)

## setwd("~/CLIMA UPCH/R codes")
#write.csv2(endesf2010,"endes.csv",row.names = FALSE)
## setwd("~/CLIMA UPCH/R codes/ALLSAV")

#ma10_10<-data.frame(END_CASEID10,ENDAGEKID,ENDPESO,ENDTALLA,ENDTEPER,ENDTEDESV,ENDTEMED,ENDPEPER,ENDPEDESV,ENDPEMED,ENDPTPER,ENDPTDESV,ENDPTMED,ENDTALLAMH)
#2010 - labels-------------------------
endesf2010$ECASEID =labelled(endesf2010$ECASEID,
                             label="Peopulation ID")

endesf2010$ENDMSTA1 = labelled(endesf2010$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2010$ENDMSTA2 = labelled(endesf2010$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2010$ENDMSTA3 = labelled(endesf2010$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2010$ENDEDAD1 = labelled(endesf2010$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2010$ENDEDAD2 = labelled(endesf2010$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2010$ENDnin = labelled(endesf2010$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2010$ENDVIO = labelled(endesf2010$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")

endesf2010$ENDstun = labelled(endesf2010$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2010$ENDwas = labelled(endesf2010$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2010$ENDmenor5<- endesf2010$ENDEDAD
endesf2010$ENDmenor5[which(endesf2010$ENDmenor5 < 5)] = 1
endesf2010$ENDmenor5[which(endesf2010$ENDmenor5 > 1)] = 0

endesf2010$ENDmenor5 = labelled(endesf2010$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")

endesf2010$ENDmayor65<- endesf2010$ENDEDAD
endesf2010$ENDmayor65[which(endesf2010$ENDmayor65 <= 65)] = 0
endesf2010$ENDmayor65[which(endesf2010$ENDmayor65 > 1)] = 1

endesf2010$ENDmayor65 = labelled(endesf2010$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult older than 65 years old")


str(endesf2010$ENDMSTA2)
# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2010,"endesf2010.dta")
# write_sav(endesf2010,"endesf2010v2.sav")
# write_dta(basehouse2010,"endes_2010_diab.dta")
# write_sav(basehouse2010,"endes_house.sav")
#write.csv2(ma10_10,"ma10_10.csv",row.names = FALSE)
# setwd("~/CLIMA UPCH/R codes/ALLSAV") 

#2010 end

#****2011 beggin -------------------------------------------------
a011<-read_sav("2011-64-290_1-CH_RECH0.sav")
colnames(a011)
ma11<-data.frame(a011$HHID) #copy of original data to master #RECH0

ma11$ENDHHID<-a011$HHID
ma11$ENDHOMEN<-a011$HV002
ma11$ENDLINEN<-a011$HV003
ma11$ENDHOMEQ<-a011$HV009
ma11$ENDWOMENEL<-a011$HV010
ma11$ENDCHILD5<-a011$HV014
ma11$ENDDOMAIN<-a011$HV023
ma11$ENDREGION<-a011$HV024
ma11$ENDURBRU<-a011$HV025
ma11$ENDRES<-a011$HV026

ma11 <- ma11[, -c(1)]

#2011 1-CH base general - population level -------------
#2011 1-CH (1)
a11<-read_sav("2011-64-290_1-CH_RECH1.sav")
ma111<-data.frame(a11$HHID)

ma111$ENDHHID<-a11$HHID
ma111$ENDIDX<-a11$HVIDX
ma111$ENDRPJEFE<-a11$HV101
ma111$ENDSEXO<-a11$HV104
ma111$ENDEDAD<-a11$HV105
ma111$ENDEDU<-a11$HV109
ma111$ENDMSTA<-a11$HV115 #estatus marital
ma111$ENDEMAR<-a11$HV116

ma111$ECASEID<-paste(ma111$ENDHHID,"",ma111$ENDIDX)

ma111 <- ma111[, -c(1)]

ma111<-ma111[colnames(ma111[c(1,9,2:8)])]  

ma111$ENDMSTA1<-a11$HV115 #marital status #nunca casado-0
ma111$ENDMSTA1[which(ma111$ENDMSTA1 > 1)] = 1

ma111$ENDMSTA2<-a11$HV115 #marital status #nunca casado + viudo =0
ma111$ENDMSTA2[which(ma111$ENDMSTA2 == 2)] = 1
ma111$ENDMSTA2[which(ma111$ENDMSTA2 == 3)] = 0
ma111$ENDMSTA2[which(ma111$ENDMSTA2 == 4)] = 1
ma111$ENDMSTA2[which(ma111$ENDMSTA2 == 5)] = 1

ma111$ENDMSTA3<-a11$HV115 #marital status #nunca casado + viudo + separado =0
ma111$ENDMSTA3[which(ma111$ENDMSTA3 == 2)] = 1
ma111$ENDMSTA3[which(ma111$ENDMSTA3 == 3)] = 0
ma111$ENDMSTA3[which(ma111$ENDMSTA3 == 4)] = 1
ma111$ENDMSTA3[which(ma111$ENDMSTA3 == 5)] = 0

ma111$ENDEDAD1<-a11$HV105 #teenagers who are mother 15-19
ma111$ENDEDAD1[which(ma111$ENDEDAD1 >19)] = 0
ma111$ENDEDAD1[which(ma111$ENDEDAD1 <15)] = 0
ma111$ENDEDAD1[which(ma111$ENDEDAD1 >0)] = 1

ma111$ENDEDAD2<-a11$HV105 #teen who are mother 12-19
ma111$ENDEDAD2[which(ma111$ENDEDAD2 >19)] = 0
ma111$ENDEDAD2[which(ma111$ENDEDAD2 <12)] = 0
ma111$ENDEDAD2[which(ma111$ENDEDAD2 >0)] = 1


endesf2011<-data.frame()
endesf2011<-left_join(ma111,ma11)
colnames(endesf2011)

endesf2011<-endesf2011[colnames(endesf2011[c(2,1,3:23)])]

#2011 1-CH (4) - seguro de salud------------
a114<-read_sav("2011-64-290_1-CH_RECH4.sav")

colnames(a114)
ma114<-data.frame(a114$HHID)
#copy$IDXH4_1CH4<-a114$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma114$ECASEID<-paste(a114$HHID,"",a114$IDXH4)
ma114$EDNSINSEG<-a114$SH11Z # no tiene seguro de salud
ma114$ENDCOMSEG<-a114$SH11D # compañia de seguro
ma114$ENDPRISEG<-a114$SH11E # seguro privado

ma114 <- ma114[, -c(1)]
colnames(ma114)

endesf2011<-left_join(endesf2011,ma114)
colnames(endesf2011)

#2011 cardiovascular disease population level ? --------------------------------------------------    
#2011 1-CH (9) - cardiovascular NOT INCLUDED, ONLY AVAILABLE IN 2010, 2011 a 2013
a119<-read_sav("2011-64-290_1-CH_RECH9.sav")

ma119<-data.frame(a119$HHID)
ma119$ECASEID<-paste(a119$HHID,"",a119$SH101)
ma119$ENDHHID_ES<-a119$HHID
ma119$ECASEID_ES<-paste(a119$HHID,"",a119$SH101)
#ma119$ENDCARD<-a119$SH120 #cardiovascular
ma119$ENDDIAB<-a119$SH106 #diabetes

ma119<- ma119[-c(1)]

endesf2011<-left_join(endesf2011,ma119)  

#2011 2CV -----------------------------------------------------------

#2011 2-CV - household level - with cardiac disease from 1CH - ?
a211<-read_sav("2011-65-290_2-CV_RECH23.sav") 

colnames(a211)

ma11223<-data.frame(a211$HHID) #HHID del hogar

ma11223$ENDHHID<-a211$HHID
ma11223$ENDWATER<-a211$HV201 #fuente del agua potable
ma11223$ENDTOILET<-a211$HV205 #hogar toilet facility
ma11223$ENDELEC<-a211$HV206 #hogar electricidad
ma11223$ENDHHJX<-a211$HV219 #sexo del jefe del hogar
ma11223$ENDWLOC<-a211$HV235 #ubicacion fuente de agua
ma11223$ENDWDAY<-a211$SH42 #agua potable todo el dia
ma11223$ENDWWEEK <-a211$SH43 
ma11223$ENDWSAVE <-a211$SH48
ma11223$ENDINTER <-a211$SH61Q #acceso a internet

ma11223 <- ma11223[-c(1)]

endesf2011<-left_join(endesf2011,ma11223)
basehouse2011<-left_join(ma11,ma11223)
#2011 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a411<-read_sav("2011-67-290_4-HNAC_RE223132.sav")


ma10411<-data.frame(a411$CASEID)
ma10411$ECASEID<-a411$CASEID
ma10411$ENDPREG<-a411$V213 #actualmente embarazada
ma10411$ENDCHLEMB<-a411$V219 #número de niños con vida + embarazo 

ma10411$ENDnin<-a411$V219 #0-no tiene, 1 - tiene hijos
ma10411$ENDnin[which(ma10411$ENDnin > 1)] = 1

ma10411<-ma10411[-c(1)]
ma10411$ECASEID_copy<-a411$CASEID #copy caseid

ma10411<-
  ma10411 %>% extract(ECASEID_copy, c("A", "B"), "([0-9]+) ([0-9]+.*)")

ma10411$ECASEID_copy<-a411$CASEID
ma10411$B<-as.numeric(ma10411$B)

ma10411$ECASEID<-paste("","","","","",ma10411$A,"",ma10411$B)
ma10411$A<-NULL
ma10411$B<-NULL
endesf2011<-left_join(endesf2011,ma10411)

#2011 9-MMVF ------------------------
a911<-read_sav("2011-73-290_9-MMVF_REC84DV.sav")
ma11_9<-data.frame(a911$CASEID)
colnames(ma11_9)

ma11_9$ECASEID <- a911$CASEID
ma11_9$ENDCORT <- a911$MMC5
ma11_9$ENDVIO1 <- a911$D101A
ma11_9$ENDVIO2 <- a911$D101B
ma11_9$ENDVIO3 <- a911$D101C
ma11_9$ENDVIO4 <- a911$D101D
ma11_9$ENDVIO5 <- a911$D101E
ma11_9$ENDVIO6 <- a911$D101F
ma11_9$ENDVIO7 <- a911$D103A
ma11_9$ENDVIO8 <- a911$D103B
ma11_9$ENDVIO9 <- a911$D103C
ma11_9$ENDVIO10 <- a911$D103D
ma11_9$ENDVIO11 <- a911$D105A
ma11_9$ENDVIO12 <- a911$D105B
ma11_9$ENDVIO13 <- a911$D105C
ma11_9$ENDVIO14 <- a911$D105D
ma11_9$ENDVIO15 <- a911$D105E
ma11_9$ENDVIO16 <- a911$D105F
ma11_9$ENDVIO17 <- a911$D105G
ma11_9$ENDVIO18 <- a911$D105H
ma11_9$ENDVIO19 <- a911$D105I
ma11_9$ENDVIO20 <- a911$D105J
ma11_9$ENDVIO21 <- a911$D110A
ma11_9$ENDVIO22 <- a911$D110B
ma11_9$ENDVIO23 <- a911$D110C
ma11_9$ENDVIO24 <- a911$D110D
ma11_9$ENDVIO25 <- a911$D110E
ma11_9$ENDVIOB1 <- a911$D115C
ma11_9$ENDVIOB2 <- a911$D115E
ma11_9$ENDVIOB3 <- a911$D115G
ma11_9$ENDVIOB4 <- a911$D115I
ma11_9$ENDVIOB5 <- a911$D115J
ma11_9$ENDVIOB6 <- a911$D115K
ma11_9$ENDVIOB7 <- a911$D115L
ma11_9$ENDVIOB8 <- a911$D115N
ma11_9$ENDVIOB9 <- a911$D115P
ma11_9$ENDVIOB10 <- a911$D115R
ma11_9$ENDVIOB11 <- a911$D115T
#ma11_9$ENDVIOB12 <- a911$D115Y #no es adecuada para el calculo
ma11_9$ENDVIOB13 <- a911$D115XC
ma11_9$ENDVIOE1 <- a911$D118A
ma11_9$ENDVIOE2 <- a911$D118C
ma11_9$ENDVIOE3 <- a911$D118E
ma11_9$ENDVIOE4 <- a911$D118G
ma11_9$ENDVIOE5 <- a911$D118I
ma11_9$ENDVIOE6 <- a911$D118J
ma11_9$ENDVIOE7 <- a911$D118K
ma11_9$ENDVIOE8 <- a911$D118L
ma11_9$ENDVIOE9 <- a911$D118N
ma11_9$ENDVIOE10 <- a911$D118P
ma11_9$ENDVIOE11 <- a911$D118T
ma11_9$ENDVIOE12 <- a911$D118XC
ma11_9$ENDVIOINT1 <- a911$D122A
ma11_9$ENDVIOINT2 <- a911$D122B
ma11_9$ENDVIOINT3 <- a911$D122C

ma11_9<- ma11_9[-c(1)] #eliminar 

colnames(ma11_9)

ma11_9$ENDVIO1[which(ma11_9$ENDVIO1 == 8)] = NA
ma11_9$ENDVIO2[which(ma11_9$ENDVIO2 == 8)] = NA
ma11_9$ENDVIO3[which(ma11_9$ENDVIO3 == 8)] = NA
ma11_9$ENDVIO4[which(ma11_9$ENDVIO4 == 8)] = NA
ma11_9$ENDVIO5[which(ma11_9$ENDVIO5 == 8)] = NA
ma11_9$ENDVIO6[which(ma11_9$ENDVIO6 == 8)] = NA

ma11_9$ENDVIO7[which(ma11_9$ENDVIO7 == 3)] = 0
ma11_9$ENDVIO7[which(ma11_9$ENDVIO7 > 1)] = 1

ma11_9$ENDVIO8[which(ma11_9$ENDVIO8 == 3)] = 0
ma11_9$ENDVIO8[which(ma11_9$ENDVIO8 > 1)] = 1

ma11_9$ENDVIO10[which(ma11_9$ENDVIO10 == 3)] = 0
ma11_9$ENDVIO10[which(ma11_9$ENDVIO10 > 1)] = 1

ma11_9$ENDVIO11[which(ma11_9$ENDVIO11 == 3)] = 0
ma11_9$ENDVIO11[which(ma11_9$ENDVIO11 > 1)] = 1

ma11_9$ENDVIO12[which(ma11_9$ENDVIO12 == 3)] = 0
ma11_9$ENDVIO12[which(ma11_9$ENDVIO12 > 1)] = 1

ma11_9$ENDVIO13[which(ma11_9$ENDVIO13 == 3)] = 0
ma11_9$ENDVIO13[which(ma11_9$ENDVIO13 > 1)] = 1

ma11_9$ENDVIO14[which(ma11_9$ENDVIO14 == 3)] = 0
ma11_9$ENDVIO14[which(ma11_9$ENDVIO14 > 1)] = 1

ma11_9$ENDVIO15[which(ma11_9$ENDVIO15 == 3)] = 0
ma11_9$ENDVIO15[which(ma11_9$ENDVIO15 > 1)] = 1

ma11_9$ENDVIO16[which(ma11_9$ENDVIO16 == 3)] = 0
ma11_9$ENDVIO16[which(ma11_9$ENDVIO16 > 1)] = 1

ma11_9$ENDVIO17[which(ma11_9$ENDVIO17 == 3)] = 0
ma11_9$ENDVIO17[which(ma11_9$ENDVIO17 > 1)] = 1

ma11_9$ENDVIO18[which(ma11_9$ENDVIO18 == 3)] = 0
ma11_9$ENDVIO18[which(ma11_9$ENDVIO18 > 1)] = 1

ma11_9$ENDVIO19[which(ma11_9$ENDVIO19 == 3)] = 0
ma11_9$ENDVIO19[which(ma11_9$ENDVIO19 > 1)] = 1

ma11_9$ENDVIOB1[which(ma11_9$ENDVIOB1 == 8)] = NA
ma11_9$ENDVIOB2[which(ma11_9$ENDVIOB2 == 8)] = NA
ma11_9$ENDVIOB3[which(ma11_9$ENDVIOB3 == 8)] = NA
ma11_9$ENDVIOB4[which(ma11_9$ENDVIOB4 == 8)] = NA
ma11_9$ENDVIOB5[which(ma11_9$ENDVIOB5 == 8)] = NA
#ma11_9$ENDVIOB6[which(ma11_9$ENDVIOB6 == 8)] = NA
#ma11_9$ENDVIOB7[which(ma11_9$ENDVIOB7 == 8)] = NA
ma11_9$ENDVIOB8[which(ma11_9$ENDVIOB8 == 8)] = NA
ma11_9$ENDVIOB9[which(ma11_9$ENDVIOB9 == 8)] = NA
#ma11_9$ENDVIOB10[which(ma11_9$ENDVIOB10 == 8)] = NA
ma11_9$ENDVIOB11[which(ma11_9$ENDVIOB11 == 8)] = NA
ma11_9$ENDVIOB13[which(ma11_9$ENDVIOB13 == 8)] = NA

ma11_9$ENDVIOINT1[which(ma11_9$ENDVIOINT1 == 2)] = 1
ma11_9$ENDVIOINT2[which(ma11_9$ENDVIOINT2 == 2)] = 1
ma11_9$ENDVIOINT3[which(ma11_9$ENDVIOINT3 == 2)] = 1

ma11_9$ENDVIO <- with(ma11_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma11_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma11_9$ENDVIO))
table(ma11_9$ENDVIO)

ma11_9$ENDVIO[which(ma11_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino

#reduced df
ma1119<-ma11_9
ma1119<-ma1119[-c(2:54)]
colnames(ma1119)
table(ma1119$ENDVIO)

ma1119$ECASEID_copy<-a911$CASEID #copy caseid

ma1119<-
  ma1119 %>% extract(ECASEID_copy, c("A", "B"), "([0-9]+) ([0-9]+.*)")

ma1119$ECASEID_copy<-a911$CASEID
ma1119$B<-as.numeric(ma1119$B)

ma1119$ECASEID<-paste("","","","","",ma1119$A,"",ma1119$B)
ma1119$A<-NULL
ma1119$B<-NULL


endesf2011<-left_join(endesf2011,ma1119)
colnames(endesf2011)

#2011 10-PETA - desnutricion -------------------
a1011<-read_sav("2011-RECH6.sav")

ma1011<-data.frame(a1011$HHID)
ma1011$ECASEID<-paste(a1011$HHID,"",a1011$HC0)
ma1011$ENDTAED<-a1011$HC5
ma1011 <- transform(ma1011, ENDTAEDC = ENDTAED / 100)

ma1011$ENDPEED<-a1011$HC8
ma1011 <- transform(ma1011, ENDPEEDC = ENDPEED / 100)

ma1011$ENDPETA<-a1011$HC11
ma1011 <- transform(ma1011, ENDPETAC = ENDPETA / 100)

ma1011<-ma1011[-c(1,3,5,7)]
ma1011$ENDstun<-ma1011$ENDTAEDC
ma1011$ENDstun[which(ma1011$ENDstun > -2)] = 0
ma1011$ENDstun[which(ma1011$ENDstun <= -2)] = 1

ma1011$ENDwas<-ma1011$ENDPETAC
ma1011$ENDwas[which(ma1011$ENDwas > -2)] = 0
ma1011$ENDwas[which(ma1011$ENDwas <= -2)] = 1
endesf2011<-left_join(endesf2011,ma1011)
colnames(endesf2011)

#2011 labels ---------------------
endesf2011$ECASEID =labelled(endesf2011$ECASEID,
                             label="Peopulation ID")

endesf2011$ENDMSTA1 = labelled(endesf2011$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2011$ENDMSTA2 = labelled(endesf2011$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2011$ENDMSTA3 = labelled(endesf2011$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2011$ENDEDAD1 = labelled(endesf2011$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2011$ENDEDAD2 = labelled(endesf2011$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2011$ENDnin = labelled(endesf2011$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2011$ENDVIO = labelled(endesf2011$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")

#endesf2011$ENDTAEDC = labelled(endesf2011$ENDTAEDC, 
#the value labels
#label="Talla para la edad menor de cinco")

#endesf2011$ENDPEEDC = labelled(endesf2011$ENDPEEDC, 
#the value labels
#label="Peso para la edad menor de cinco")

#endesf2011$ENDPETAC = labelled(endesf2011$ENDPETAC, 
#the value labels
#label="Peso para la talla menor de cinco")

endesf2011$ENDstun = labelled(endesf2011$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2011$ENDwas = labelled(endesf2011$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2011$ENDmenor5<- endesf2011$ENDEDAD
endesf2011$ENDmenor5[which(endesf2011$ENDmenor5 < 5)] = 1
endesf2011$ENDmenor5[which(endesf2011$ENDmenor5 > 1)] = 0

endesf2011$ENDmenor5 = labelled(endesf2011$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")
endesf2011$ENDmayor65<- endesf2011$ENDEDAD
endesf2011$ENDmayor65[which(endesf2011$ENDmayor65 <=65)] = 0 #0- 66, 68
endesf2011$ENDmayor65[which(endesf2011$ENDmayor65 > 65)] = 1 #1- 63,62,etc


endesf2011$ENDmayor65 = labelled(endesf2011$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult > 65")

#---
# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2011,"endesf2011.dta")
# write_sav(endesf2011,"endesf2011v3.sav")
# write_dta(basehouse2011,"endes_2011_diab.dta")
# write_sav(basehouse2011,"endes_house_2011.sav")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")

#2011 end

#****2012 beggin --------------------
a012<-read_sav("2012-64-323_1-CH_RECH0.sav")
ma12<-data.frame(a012$HHID) #copy of original data to master #RECH0

ma12$ENDHHID<-a012$HHID
ma12$ENDHOMEN<-a012$HV002
ma12$ENDLINEN<-a012$HV003
ma12$ENDHOMEQ<-a012$HV009
ma12$ENDWOMENEL<-a012$HV010
ma12$ENDCHILD5<-a012$HV014
ma12$ENDDOMAIN<-a012$HV023
ma12$ENDREGION<-a012$HV024
ma12$ENDURBRU<-a012$HV025
ma12$ENDRES<-a012$HV026

ma12 <- ma12[, -c(1)]

#2012 1-CH base general - population level -------------
#2012 1-CH (1)
a12<-read_sav("2012-64-323_1-CH_RECH1.sav")
ma112<-data.frame(a12$HHID)

ma112$ENDHHID<-a12$HHID
ma112$ENDIDX<-a12$HVIDX
ma112$ENDRPJEFE<-a12$HV101
ma112$ENDSEXO<-a12$HV104
ma112$ENDEDAD<-a12$HV105
ma112$ENDEDU<-a12$HV109
ma112$ENDMSTA<-a12$HV115
ma112$ENDEMAR<-a12$HV116

ma112$ECASEID<-paste(ma112$ENDHHID,"",ma112$ENDIDX)

ma112 <- ma112[, -c(1)]

ma112<-ma112[colnames(ma112[c(1,9,2:8)])]

ma112$ENDMSTA1<-a12$HV115 #marital status #nunca casado-0
ma112$ENDMSTA1[which(ma112$ENDMSTA1 > 1)] = 1

ma112$ENDMSTA2<-a12$HV115 #marital status #nunca casado + viudo =0
ma112$ENDMSTA2[which(ma112$ENDMSTA2 == 2)] = 1
ma112$ENDMSTA2[which(ma112$ENDMSTA2 == 3)] = 0
ma112$ENDMSTA2[which(ma112$ENDMSTA2 == 4)] = 1
ma112$ENDMSTA2[which(ma112$ENDMSTA2 == 5)] = 1

ma112$ENDMSTA3<-a12$HV115 #marital status #nunca casado + viudo + separado =0
ma112$ENDMSTA3[which(ma112$ENDMSTA3 == 2)] = 1
ma112$ENDMSTA3[which(ma112$ENDMSTA3 == 3)] = 0
ma112$ENDMSTA3[which(ma112$ENDMSTA3 == 4)] = 1
ma112$ENDMSTA3[which(ma112$ENDMSTA3 == 5)] = 0

ma112$ENDEDAD1<-a12$HV105 #teenagers who are mother 15-19
ma112$ENDEDAD1[which(ma112$ENDEDAD1 >19)] = 0
ma112$ENDEDAD1[which(ma112$ENDEDAD1 <15)] = 0
ma112$ENDEDAD1[which(ma112$ENDEDAD1 >0)] = 1

ma112$ENDEDAD2<-a12$HV105 #teen who are mother 12-19
ma112$ENDEDAD2[which(ma112$ENDEDAD2 >19)] = 0
ma112$ENDEDAD2[which(ma112$ENDEDAD2 <12)] = 0
ma112$ENDEDAD2[which(ma112$ENDEDAD2 >0)] = 1

endesf2012<-left_join(ma112,ma12)
colnames(endesf2012)

endesf2012<-endesf2012[colnames(endesf2012[c(2,1,3:23)])]

#2012 1-CH (4) - seguro de salud------------
a1214<-read_sav("2012-64-323_1-CH_RECH4.sav")

colnames(a1214)
ma1214<-data.frame(a1214$HHID)
#copy$IDXH4_1CH4<-a1214$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma1214$ECASEID<-paste(a1214$HHID,"",a1214$IDXH4)
ma1214$EDNSINSEG<-a1214$SH11Z # no tiene seguro de salud
ma1214$ENDCOMSEG<-a1214$SH11D # compañia de seguro
ma1214$ENDPRISEG<-a1214$SH11E # seguro privado

ma1214 <- ma1214[, -c(1)]
colnames(ma1214)

endesf2012<-left_join(endesf2012,ma1214)
colnames(endesf2012)

# 2012 cardiovascular disease household level --------------------------------------------------    
#2012 1-CH (9) - cardiovascular [a nivel de hogar] - bd hogar ¿?
a1219<-read_sav("2012-64-323_1-CH_RECH9.SAV")


ma1219<-data.frame(a1219$HHID)
ma1219$ECASEID<-paste(a1219$HHID,"",a1219$SH101)
ma1219$ENDHHID_ES<-a1219$HHID
ma1219$ENSH120<-a1219$SH101 #linea del hogar
#ma1219$ENDCARD<-a1219$SH120 #cardiovascular
ma1219$ENDDIAB<-a1219$SH106 #diabetes

ma1219<- ma1219[-c(1)]

endesf2012<-left_join(endesf2012,ma1219)


#2012 2CV -----------------------------------------------------------
#2012 1-CH general - people level

#2012 2-CV - household level - with cardiac disease from 1CH - ?
a212<-read_sav("2012-65-323_2-CV_RECH23.sav") 

colnames(a212)

ma1223<-data.frame(a212$HHID) #HHID del hogar

ma1223$ENDHHID<-a212$HHID
ma1223$ENDWATER<-a212$HV201 #fuente del agua potable
ma1223$ENDTOILET<-a212$HV205 #hogar toilet facility
ma1223$ENDELEC<-a212$HV206 #hogar electricidad
ma1223$ENDHHJX<-a212$HV219 #sexo del jefe del hogar
ma1223$ENDWLOC<-a212$HV235 #ubicacion fuente de agua
ma1223$ENDWDAY<-a212$SH42 #agua potable todo el dia
ma1223$ENDWWEEK <-a212$SH43 
ma1223$ENDWSAVE <-a212$SH48
ma1223$ENDINTER <-a212$SH61Q #acceso a internet

ma1223 <- ma1223[-c(1)]

endesf2012<-left_join(endesf2012,ma1223)
basehouse2012<-left_join(ma12,ma1223)
#2012 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a412<-read_sav("2012-67-323_4-HNAC_RE212232.sav")


ma124<-data.frame(a412$CASEID)
ma124$ECASEID<-a412$CASEID
ma124$ENDPREG<-a412$V213 #actualmente embarazada
ma124$ENDCHLEMB<-a412$V219 #número de niños con vida + embarazo 

ma124$ENDnin<-a412$V219 #0-no tiene, 1 - tiene hijos
ma124$ENDnin[which(ma124$ENDnin > 1)] = 1

ma124<-ma124[-c(1)]

ma124$ECASEID_copy<-a412$CASEID #copy caseid

ma124<-
  ma124 %>% extract(ECASEID_copy, c("A", "B"), "([0-9]+) ([0-9]+.*)")

ma124$ECASEID_copyP<-a412$CASEID
ma124$B<-as.numeric(ma124$B)

ma124$ECASEID<-paste("","","","","","",ma124$A,"",ma124$B)
ma124$A<-NULL
ma124$B<-NULL

endesf2012<-left_join(endesf2012,ma124)

#2012 9-MMVF ------------------------
a912<-read_sav("2012-73-323_9-MMVF_REC84DV.sav")
ma12_9<-data.frame(a912$CASEID)
colnames(ma12_9)



ma12_9$ECASEID <- a912$CASEID
#ma12_9$ENDCORT <- a912$MMC5
ma12_9$ENDVIO1 <- a912$D101A
ma12_9$ENDVIO2 <- a912$D101B
ma12_9$ENDVIO3 <- a912$D101C
ma12_9$ENDVIO4 <- a912$D101D
ma12_9$ENDVIO5 <- a912$D101E
ma12_9$ENDVIO6 <- a912$D101F
ma12_9$ENDVIO7 <- a912$D103A
ma12_9$ENDVIO8 <- a912$D103B
ma12_9$ENDVIO9 <- a912$D103C
ma12_9$ENDVIO10 <- a912$D103D
ma12_9$ENDVIO11 <- a912$D105A
ma12_9$ENDVIO12 <- a912$D105B
ma12_9$ENDVIO13 <- a912$D105C
ma12_9$ENDVIO14 <- a912$D105D
ma12_9$ENDVIO15 <- a912$D105E
ma12_9$ENDVIO16 <- a912$D105F
ma12_9$ENDVIO17 <- a912$D105G
ma12_9$ENDVIO18 <- a912$D105H
ma12_9$ENDVIO19 <- a912$D105I
ma12_9$ENDVIO20 <- a912$D105J
ma12_9$ENDVIO21 <- a912$D110A
ma12_9$ENDVIO22 <- a912$D110B
ma12_9$ENDVIO23 <- a912$D110C
ma12_9$ENDVIO24 <- a912$D110D
ma12_9$ENDVIO25 <- a912$D110E
ma12_9$ENDVIOB1 <- a912$D115C
ma12_9$ENDVIOB2 <- a912$D115E
ma12_9$ENDVIOB3 <- a912$D115G
ma12_9$ENDVIOB4 <- a912$D115I
ma12_9$ENDVIOB5 <- a912$D115J
ma12_9$ENDVIOB6 <- a912$D115K
ma12_9$ENDVIOB7 <- a912$D115L
ma12_9$ENDVIOB8 <- a912$D115N
ma12_9$ENDVIOB9 <- a912$D115P
ma12_9$ENDVIOB10 <- a912$D115R
ma12_9$ENDVIOB11 <- a912$D115T
#ma12_9$ENDVIOB12 <- a912$D115Y
ma12_9$ENDVIOB13 <- a912$D115XC
ma12_9$ENDVIOE1 <- a912$D118A
ma12_9$ENDVIOE2 <- a912$D118C
ma12_9$ENDVIOE3 <- a912$D118E
ma12_9$ENDVIOE4 <- a912$D118G
ma12_9$ENDVIOE5 <- a912$D118I
ma12_9$ENDVIOE6 <- a912$D118J
ma12_9$ENDVIOE7 <- a912$D118K
ma12_9$ENDVIOE8 <- a912$D118L
ma12_9$ENDVIOE9 <- a912$D118N
ma12_9$ENDVIOE10 <- a912$D118P
ma12_9$ENDVIOE11 <- a912$D118T
ma12_9$ENDVIOE12 <- a912$D118XC
ma12_9$ENDVIOINT1 <- a912$D122A
ma12_9$ENDVIOINT2 <- a912$D122B
ma12_9$ENDVIOINT3 <- a912$D122C


ma12_9<- ma12_9[-c(1)] #eliminar ENVIOB12 por no ser adecuada para el calculo

colnames(ma12_9)

ma12_9$ENDVIO1[which(ma12_9$ENDVIO1 == 8)] = NA
ma12_9$ENDVIO2[which(ma12_9$ENDVIO2 == 8)] = NA
ma12_9$ENDVIO3[which(ma12_9$ENDVIO3 == 8)] = NA
ma12_9$ENDVIO4[which(ma12_9$ENDVIO4 == 8)] = NA
ma12_9$ENDVIO5[which(ma12_9$ENDVIO5 == 8)] = NA
ma12_9$ENDVIO6[which(ma12_9$ENDVIO6 == 8)] = NA

ma12_9$ENDVIO7[which(ma12_9$ENDVIO7 == 3)] = 0
ma12_9$ENDVIO7[which(ma12_9$ENDVIO7 > 1)] = 1

ma12_9$ENDVIO8[which(ma12_9$ENDVIO8 == 3)] = 0
ma12_9$ENDVIO8[which(ma12_9$ENDVIO8 > 1)] = 1

ma12_9$ENDVIO10[which(ma12_9$ENDVIO10 == 3)] = 0
ma12_9$ENDVIO10[which(ma12_9$ENDVIO10 > 1)] = 1

ma12_9$ENDVIO11[which(ma12_9$ENDVIO11 == 3)] = 0
ma12_9$ENDVIO11[which(ma12_9$ENDVIO11 > 1)] = 1

ma12_9$ENDVIO12[which(ma12_9$ENDVIO12 == 3)] = 0
ma12_9$ENDVIO12[which(ma12_9$ENDVIO12 > 1)] = 1

ma12_9$ENDVIO13[which(ma12_9$ENDVIO13 == 3)] = 0
ma12_9$ENDVIO13[which(ma12_9$ENDVIO13 > 1)] = 1

ma12_9$ENDVIO14[which(ma12_9$ENDVIO14 == 3)] = 0
ma12_9$ENDVIO14[which(ma12_9$ENDVIO14 > 1)] = 1

ma12_9$ENDVIO15[which(ma12_9$ENDVIO15 == 3)] = 0
ma12_9$ENDVIO15[which(ma12_9$ENDVIO15 > 1)] = 1

ma12_9$ENDVIO16[which(ma12_9$ENDVIO16 == 3)] = 0
ma12_9$ENDVIO16[which(ma12_9$ENDVIO16 > 1)] = 1

ma12_9$ENDVIO17[which(ma12_9$ENDVIO17 == 3)] = 0
ma12_9$ENDVIO17[which(ma12_9$ENDVIO17 > 1)] = 1

ma12_9$ENDVIO18[which(ma12_9$ENDVIO18 == 3)] = 0
ma12_9$ENDVIO18[which(ma12_9$ENDVIO18 > 1)] = 1

ma12_9$ENDVIO19[which(ma12_9$ENDVIO19 == 3)] = 0
ma12_9$ENDVIO19[which(ma12_9$ENDVIO19 > 1)] = 1

ma12_9$ENDVIOB1[which(ma12_9$ENDVIOB1 == 8)] = NA
ma12_9$ENDVIOB2[which(ma12_9$ENDVIOB2 == 8)] = NA
ma12_9$ENDVIOB3[which(ma12_9$ENDVIOB3 == 8)] = NA
ma12_9$ENDVIOB4[which(ma12_9$ENDVIOB4 == 8)] = NA
ma12_9$ENDVIOB5[which(ma12_9$ENDVIOB5 == 8)] = NA
ma12_9$ENDVIOB6[which(ma12_9$ENDVIOB6 == 8)] = NA
ma12_9$ENDVIOB7[which(ma12_9$ENDVIOB7 == 8)] = NA
ma12_9$ENDVIOB8[which(ma12_9$ENDVIOB8 == 8)] = NA
ma12_9$ENDVIOB9[which(ma12_9$ENDVIOB9 == 8)] = NA
ma12_9$ENDVIOB10[which(ma12_9$ENDVIOB10 == 8)] = NA
ma12_9$ENDVIOB11[which(ma12_9$ENDVIOB11 == 8)] = NA
ma12_9$ENDVIOB13[which(ma12_9$ENDVIOB13 == 8)] = NA
ma12_9$ENDVIOINT1[which(ma12_9$ENDVIOINT1 == 2)] = 1
ma12_9$ENDVIOINT2[which(ma12_9$ENDVIOINT2 == 2)] = 1
ma12_9$ENDVIOINT3[which(ma12_9$ENDVIOINT3 == 2)] = 1

ma12_9$ENDVIO <- with(ma12_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma12_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma12_9$ENDVIO))
table(ma12_9$ENDVIO)

ma12_9$ENDVIO[which(ma12_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino

#reduced df
ma129<-ma12_9
ma129<-ma129[-c(2:53)]

ma129$ECASEID_copy<-a912$CASEID #copy caseid

ma129<-
  ma129 %>% extract(ECASEID_copy, c("A", "B"), "([0-9]+) ([0-9]+.*)")

ma129$ECASEID_copy<-a912$CASEID
ma129$B<-as.numeric(ma129$B)

ma129$ECASEID<-paste("","","","","","",ma129$A,"",ma129$B)
ma129$A<-NULL
ma129$B<-NULL

endesf2012<-left_join(endesf2012,ma129)

#2012 10-PETA - desnutricion -------------------
a1210<-read_sav("2012 - 10-RECH6.sav")

ma1210<-data.frame(a1210$HHID)
ma1210$ECASEID<-paste(a1210$HHID,"",a1210$HC0)
ma1210$ENDTAED<-a1210$HC5
ma1210 <- transform(ma1210, ENDTAEDC = ENDTAED / 100)

ma1210$ENDPEED<-a1210$HC8
ma1210 <- transform(ma1210, ENDPEEDC = ENDPEED / 100)

ma1210$ENDPETA<-a1210$HC11
ma1210 <- transform(ma1210, ENDPETAC = ENDPETA / 100)

ma1210<-ma1210[-c(1,3,5,7)]
ma1210$ENDstun<-ma1210$ENDTAEDC
ma1210$ENDstun[which(ma1210$ENDstun > -2)] = 0
ma1210$ENDstun[which(ma1210$ENDstun <= -2)] = 1

ma1210$ENDwas<-ma1210$ENDPETAC
ma1210$ENDwas[which(ma1210$ENDwas > -2)] = 0
ma1210$ENDwas[which(ma1210$ENDwas <= -2)] = 1
endesf2012<-left_join(endesf2012,ma1210)
colnames(endesf2012)

#2012 labels --------------------------
endesf2012$ECASEID =labelled(endesf2012$ECASEID,
                             label="Peopulation ID")

endesf2012$ENDMSTA1 = labelled(endesf2012$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2012$ENDMSTA2 = labelled(endesf2012$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2012$ENDMSTA3 = labelled(endesf2012$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2012$ENDEDAD1 = labelled(endesf2012$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2012$ENDEDAD2 = labelled(endesf2012$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2012$ENDnin = labelled(endesf2012$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2012$ENDVIO = labelled(endesf2012$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")

endesf2012$ENDstun = labelled(endesf2012$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2012$ENDwas = labelled(endesf2012$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2012$ENDmenor5<- endesf2012$ENDEDAD
endesf2012$ENDmenor5[which(endesf2012$ENDmenor5 < 5)] = 1
endesf2012$ENDmenor5[which(endesf2012$ENDmenor5 > 1)] = 0

endesf2012$ENDmenor5 = labelled(endesf2012$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")
endesf2012$ENDmayor65<- endesf2012$ENDEDAD
endesf2012$ENDmayor65[which(endesf2012$ENDmayor65 <= 65)] = 0
endesf2012$ENDmayor65[which(endesf2012$ENDmayor65 > 65)] = 1

endesf2012$ENDmayor65 = labelled(endesf2012$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult > 65")
#---
# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2012,"endesf2012.dta")
# write_sav(endesf2012,"endesf2012v2.sav")
# write_dta(basehouse2012,"endes_2012_diab.dta")
# write_sav(basehouse2012,"endes_house_2012.sav")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")

#****2013 beggin----------------------------------------------
a013<-read_sav("2013-64-407_1-CH_RECH0.SAV")
ma13<-data.frame(a013$HHID) #copy of original data to master #RECH0

ma13$ENDHHID<-a013$HHID
ma13$ENDHOMEN<-a013$HV002
ma13$ENDLINEN<-a013$HV003
ma13$ENDHOMEQ<-a013$HV009
ma13$ENDWOMENEL<-a013$HV010
ma13$ENDCHILD5<-a013$HV014
ma13$ENDDOMAIN<-a013$HV023
ma13$ENDREGION<-a013$HV024
ma13$ENDURBRU<-a013$HV025
ma13$ENDRES<-a013$HV026

ma13 <- ma13[, -c(1)]

#2013 1-CH base general - population level -------------
#2013 1-CH (1)
a13<-read_sav("2013-64-407_1-CH_RECH1.SAV")
ma113<-data.frame(a13$HHID)

ma113$ENDHHID<-a13$HHID
ma113$ENDIDX<-a13$HVIDX
ma113$ENDRPJEFE<-a13$HV101
ma113$ENDSEXO<-a13$HV104
ma113$ENDEDAD<-a13$HV105
ma113$ENDEDU<-a13$HV109
ma113$ENDMSTA<-a13$HV115
ma113$ENDEMAR<-a13$HV116

ma113$ECASEID<-paste(ma113$ENDHHID,"",ma113$ENDIDX)

ma113 <- ma113[, -c(1)]

ma113<-ma113[colnames(ma113[c(1,9,2:8)])]

ma113$ENDMSTA1<-a13$HV115 #marital status #nunca casado-0
ma113$ENDMSTA1[which(ma113$ENDMSTA1 > 1)] = 1

ma113$ENDMSTA2<-a13$HV115 #marital status #nunca casado + viudo =0
ma113$ENDMSTA2[which(ma113$ENDMSTA2 == 2)] = 1
ma113$ENDMSTA2[which(ma113$ENDMSTA2 == 3)] = 0
ma113$ENDMSTA2[which(ma113$ENDMSTA2 == 4)] = 1
ma113$ENDMSTA2[which(ma113$ENDMSTA2 == 5)] = 1

ma113$ENDMSTA3<-a13$HV115 #marital status #nunca casado + viudo + separado =0
ma113$ENDMSTA3[which(ma113$ENDMSTA3 == 2)] = 1
ma113$ENDMSTA3[which(ma113$ENDMSTA3 == 3)] = 0
ma113$ENDMSTA3[which(ma113$ENDMSTA3 == 4)] = 1
ma113$ENDMSTA3[which(ma113$ENDMSTA3 == 5)] = 0

ma113$ENDEDAD1<-a13$HV105 #teenagers who are mother 15-19
ma113$ENDEDAD1[which(ma113$ENDEDAD1 >19)] = 0
ma113$ENDEDAD1[which(ma113$ENDEDAD1 <15)] = 0
ma113$ENDEDAD1[which(ma113$ENDEDAD1 >0)] = 1

ma113$ENDEDAD2<-a13$HV105 #teen who are mother 12-19
ma113$ENDEDAD2[which(ma113$ENDEDAD2 >19)] = 0
ma113$ENDEDAD2[which(ma113$ENDEDAD2 <12)] = 0
ma113$ENDEDAD2[which(ma113$ENDEDAD2 >0)] = 1

endesf2013<-left_join(ma113,ma13)
colnames(endesf2013)

endesf2013<-endesf2013[colnames(endesf2013[c(2,1,3:23)])]

#2013 1-CH (4) - seguro de salud------------
a1314<-read_sav("2013-64-407_1-CH_RECH4.SAV")

colnames(a1314)
ma1314<-data.frame(a1314$HHID)
#copy$IDXH4_1CH4<-a1314$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma1314$ECASEID<-paste(a1314$HHID,"",a1314$IDXH4)
ma1314$EDNSINSEG<-a1314$SH11Z # no tiene seguro de salud
ma1314$ENDCOMSEG<-a1314$SH11D # compañia de seguro
ma1314$ENDPRISEG<-a1314$SH11E # seguro privado

ma1314 <- ma1314[, -c(1)]
colnames(ma1314)

endesf2013<-left_join(endesf2013,ma1314)
colnames(endesf2013)

# 2013 cardiovascular disease household level --------------------------------------------------    
#2013 1-CH (9) - cardiovascular [a nivel de hogar] - bd hogar ¿?
a1319<-read_sav("2013-64-407_1-CH_RECH9.SAV")

ma1319<-data.frame(a1319$HHID)
ma1319$ECASEID<-paste(a1319$HHID,"",a1319$SH101)
ma1319$ENDHHID_ES<-a1319$HHID
ma1319$ENSH120<-a1319$SH101
#ma1319$ENDCARD<-a1319$SH120 #cardiovascular
ma1319$ENDDIAB<-a1319$SH106 #diabetes

ma1319<- ma1319[-c(1)]

endesf2013<-left_join(endesf2013,ma1319)

#2013 2CV -----------------------------------------------------------
#2013 1-CH general - people level

#2013 2-CV - household level - with cardiac disease from 1CH - ?
a312<-read_sav("2013-65-407_2-CV_RECH23.SAV") 

colnames(a312)

ma1323<-data.frame(a312$HHID) #HHID del hogar

ma1323$ENDHHID<-a312$HHID
ma1323$ENDWATER<-a312$HV201 #fuente del agua potable
ma1323$ENDTOILET<-a312$HV205 #hogar toilet facility
ma1323$ENDELEC<-a312$HV206 #hogar electricidad
ma1323$ENDHHJX<-a312$HV219 #sexo del jefe del hogar
ma1323$ENDWLOC<-a312$HV235 #ubicacion fuente de agua
ma1323$ENDWDAY<-a312$SH42 #agua potable todo el dia
ma1323$ENDWWEEK <-a312$SH43 
ma1323$ENDWSAVE <-a312$SH48
ma1323$ENDINTER <-a312$SH61Q #acceso a internet

ma1323 <- ma1323[-c(1)]

endesf2013<-left_join(endesf2013,ma1323)

basehouse2013<-left_join(ma13,ma1323)

#2013 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a413<-read_sav("2013-67-407_4-HNAC_RE223132.SAV")


ma134<-data.frame(a413$CASEID)
ma134$ECASEID<-a413$CASEID
ma134$ENDPREG<-a413$V213 #actualmente embarazada
ma134$ENDCHLEMB<-a413$V219 #número de niños con vida + embarazo 

ma134$ENDnin<-a413$V219 #0-no tiene, 1 - tiene hijos
ma134$ENDnin[which(ma134$ENDnin > 1)] = 1

ma134<-ma134[-c(1)]

endesf2013<-left_join(endesf2013,ma134)

#2013 9-MMVF ------------------------
a913<-read_sav("2013-73-407_9-MMVF_REC84DV.SAV")
ma13_9<-data.frame(a913$CASEID)
colnames(ma13_9)



ma13_9$ECASEID <- a913$CASEID
ma13_9$ENDCORT <- a913$MMC5
ma13_9$ENDVIO1 <- a913$D101A
ma13_9$ENDVIO2 <- a913$D101B
ma13_9$ENDVIO3 <- a913$D101C
ma13_9$ENDVIO4 <- a913$D101D
ma13_9$ENDVIO5 <- a913$D101E
ma13_9$ENDVIO6 <- a913$D101F
ma13_9$ENDVIO7 <- a913$D103A
ma13_9$ENDVIO8 <- a913$D103B
ma13_9$ENDVIO9 <- a913$D103C
ma13_9$ENDVIO10 <- a913$D103D
ma13_9$ENDVIO11 <- a913$D105A
ma13_9$ENDVIO12 <- a913$D105B
ma13_9$ENDVIO13 <- a913$D105C
ma13_9$ENDVIO14 <- a913$D105D
ma13_9$ENDVIO15 <- a913$D105E
ma13_9$ENDVIO16 <- a913$D105F
ma13_9$ENDVIO17 <- a913$D105G
ma13_9$ENDVIO18 <- a913$D105H
ma13_9$ENDVIO19 <- a913$D105I
ma13_9$ENDVIO20 <- a913$D105J
ma13_9$ENDVIO21 <- a913$D110A
ma13_9$ENDVIO22 <- a913$D110B
ma13_9$ENDVIO23 <- a913$D110C
ma13_9$ENDVIO24 <- a913$D110D
ma13_9$ENDVIO25 <- a913$D110E
ma13_9$ENDVIOB1 <- a913$D115C
ma13_9$ENDVIOB2 <- a913$D115E
ma13_9$ENDVIOB3 <- a913$D115G
ma13_9$ENDVIOB4 <- a913$D115I
ma13_9$ENDVIOB5 <- a913$D115J
ma13_9$ENDVIOB6 <- a913$D115K
ma13_9$ENDVIOB7 <- a913$D115L
ma13_9$ENDVIOB8 <- a913$D115N
ma13_9$ENDVIOB9 <- a913$D115P
ma13_9$ENDVIOB10 <- a913$D115R
ma13_9$ENDVIOB11 <- a913$D115T
#ma13_9$ENDVIOB12 <- a913$D115Y
ma13_9$ENDVIOB13 <- a913$D115XC
ma13_9$ENDVIOE1 <- a913$D118A
ma13_9$ENDVIOE2 <- a913$D118C
ma13_9$ENDVIOE3 <- a913$D118E
ma13_9$ENDVIOE4 <- a913$D118G
ma13_9$ENDVIOE5 <- a913$D118I
ma13_9$ENDVIOE6 <- a913$D118J
ma13_9$ENDVIOE7 <- a913$D118K
ma13_9$ENDVIOE8 <- a913$D118L
ma13_9$ENDVIOE9 <- a913$D118N
ma13_9$ENDVIOE10 <- a913$D118P
ma13_9$ENDVIOE11 <- a913$D118T
ma13_9$ENDVIOE12 <- a913$D118XC
ma13_9$ENDVIOINT1 <- a913$D122A
ma13_9$ENDVIOINT2 <- a913$D122B
ma13_9$ENDVIOINT3 <- a913$D122C


ma13_9<- ma13_9[-c(1)] #eliminar ENVIOB12 por no ser adecuada para el calculo

colnames(ma13_9)

ma13_9$ENDVIO1[which(ma13_9$ENDVIO1 == 8)] = NA
ma13_9$ENDVIO2[which(ma13_9$ENDVIO2 == 8)] = NA
ma13_9$ENDVIO3[which(ma13_9$ENDVIO3 == 8)] = NA
ma13_9$ENDVIO4[which(ma13_9$ENDVIO4 == 8)] = NA
ma13_9$ENDVIO5[which(ma13_9$ENDVIO5 == 8)] = NA
ma13_9$ENDVIO6[which(ma13_9$ENDVIO6 == 8)] = NA

ma13_9$ENDVIO7[which(ma13_9$ENDVIO7 == 3)] = 0
ma13_9$ENDVIO7[which(ma13_9$ENDVIO7 > 1)] = 1

ma13_9$ENDVIO8[which(ma13_9$ENDVIO8 == 3)] = 0
ma13_9$ENDVIO8[which(ma13_9$ENDVIO8 > 1)] = 1

ma13_9$ENDVIO10[which(ma13_9$ENDVIO10 == 3)] = 0
ma13_9$ENDVIO10[which(ma13_9$ENDVIO10 > 1)] = 1

ma13_9$ENDVIO11[which(ma13_9$ENDVIO11 == 3)] = 0
ma13_9$ENDVIO11[which(ma13_9$ENDVIO11 > 1)] = 1

ma13_9$ENDVIO12[which(ma13_9$ENDVIO12 == 3)] = 0
ma13_9$ENDVIO12[which(ma13_9$ENDVIO12 > 1)] = 1

ma13_9$ENDVIO13[which(ma13_9$ENDVIO13 == 3)] = 0
ma13_9$ENDVIO13[which(ma13_9$ENDVIO13 > 1)] = 1

ma13_9$ENDVIO14[which(ma13_9$ENDVIO14 == 3)] = 0
ma13_9$ENDVIO14[which(ma13_9$ENDVIO14 > 1)] = 1

ma13_9$ENDVIO15[which(ma13_9$ENDVIO15 == 3)] = 0
ma13_9$ENDVIO15[which(ma13_9$ENDVIO15 > 1)] = 1

ma13_9$ENDVIO16[which(ma13_9$ENDVIO16 == 3)] = 0
ma13_9$ENDVIO16[which(ma13_9$ENDVIO16 > 1)] = 1

ma13_9$ENDVIO17[which(ma13_9$ENDVIO17 == 3)] = 0
ma13_9$ENDVIO17[which(ma13_9$ENDVIO17 > 1)] = 1

ma13_9$ENDVIO18[which(ma13_9$ENDVIO18 == 3)] = 0
ma13_9$ENDVIO18[which(ma13_9$ENDVIO18 > 1)] = 1

ma13_9$ENDVIO19[which(ma13_9$ENDVIO19 == 3)] = 0
ma13_9$ENDVIO19[which(ma13_9$ENDVIO19 > 1)] = 1

ma13_9$ENDVIOB1[which(ma13_9$ENDVIOB1 == 8)] = NA
ma13_9$ENDVIOB2[which(ma13_9$ENDVIOB2 == 8)] = NA
ma13_9$ENDVIOB3[which(ma13_9$ENDVIOB3 == 8)] = NA
ma13_9$ENDVIOB4[which(ma13_9$ENDVIOB4 == 8)] = NA
ma13_9$ENDVIOB5[which(ma13_9$ENDVIOB5 == 8)] = NA
ma13_9$ENDVIOB6[which(ma13_9$ENDVIOB6 == 8)] = NA
ma13_9$ENDVIOB7[which(ma13_9$ENDVIOB7 == 8)] = NA
ma13_9$ENDVIOB8[which(ma13_9$ENDVIOB8 == 8)] = NA
ma13_9$ENDVIOB9[which(ma13_9$ENDVIOB9 == 8)] = NA
ma13_9$ENDVIOB10[which(ma13_9$ENDVIOB10 == 8)] = NA
ma13_9$ENDVIOB11[which(ma13_9$ENDVIOB11 == 8)] = NA
ma13_9$ENDVIOB13[which(ma13_9$ENDVIOB13 == 8)] = NA
ma13_9$ENDVIOINT1[which(ma13_9$ENDVIOINT1 == 2)] = 1
ma13_9$ENDVIOINT2[which(ma13_9$ENDVIOINT2 == 2)] = 1
ma13_9$ENDVIOINT3[which(ma13_9$ENDVIOINT3 == 2)] = 1

ma13_9$ENDVIO <- with(ma13_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma13_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma13_9$ENDVIO))
table(ma13_9$ENDVIO)

ma13_9$ENDVIO[which(ma13_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino

#reduced df
ma139<-ma13_9
ma139<-ma139[-c(2:54)]

endesf2013<-left_join(endesf2013,ma139)

#2013 10-PETA - desnutricion -------------------
a1310<-read_sav("2013-74-407_10-PETA_RECH6.SAV")

ma1310<-data.frame(a1310$HHID)
ma1310$ECASEID<-paste(a1310$HHID,"",a1310$HC0)
ma1310$ENDTAED<-a1310$HC5
ma1310 <- transform(ma1310, ENDTAEDC = ENDTAED / 100)

ma1310$ENDPEED<-a1310$HC8
ma1310 <- transform(ma1310, ENDPEEDC = ENDPEED / 100)

ma1310$ENDPETA<-a1310$HC11
ma1310 <- transform(ma1310, ENDPETAC = ENDPETA / 100)

ma1310<-ma1310[-c(1,3,5,7)]
ma1310$ENDstun<-ma1310$ENDTAEDC
ma1310$ENDstun[which(ma1310$ENDstun > -2)] = 0
ma1310$ENDstun[which(ma1310$ENDstun <= -2)] = 1

ma1310$ENDwas<-ma1310$ENDPETAC
ma1310$ENDwas[which(ma1310$ENDwas > -2)] = 0
ma1310$ENDwas[which(ma1310$ENDwas <= -2)] = 1
endesf2013<-left_join(endesf2013,ma1310)
colnames(endesf2013)

#2013 labels--------------------------
endesf2013$ECASEID =labelled(endesf2013$ECASEID,
                             label="Peopulation ID")

endesf2013$ENDMSTA1 = labelled(endesf2013$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2013$ENDMSTA2 = labelled(endesf2013$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2013$ENDMSTA3 = labelled(endesf2013$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2013$ENDEDAD1 = labelled(endesf2013$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2013$ENDEDAD2 = labelled(endesf2013$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2013$ENDnin = labelled(endesf2013$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2013$ENDVIO = labelled(endesf2013$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")


endesf2013$ENDstun = labelled(endesf2013$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2013$ENDwas = labelled(endesf2013$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2013$ENDmenor5<- endesf2013$ENDEDAD
endesf2013$ENDmenor5[which(endesf2013$ENDmenor5 < 5)] = 1
endesf2013$ENDmenor5[which(endesf2013$ENDmenor5 > 1)] = 0

endesf2013$ENDmenor5 = labelled(endesf2013$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")
endesf2013$ENDmayor65<- endesf2013$ENDEDAD
endesf2013$ENDmayor65[which(endesf2013$ENDmayor65 <= 65)] = 0
endesf2013$ENDmayor65[which(endesf2013$ENDmayor65 > 65)] = 1

endesf2013$ENDmayor65 = labelled(endesf2013$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult > 65")

#---

# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2013,"endesf2013.dta")
# write_sav(endesf2013,"endesf2013v2.sav")
# write_dta(basehouse2013,"endes_2013_diab.dta")
# write_sav(basehouse2013,"endes_house_2013.sav")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")
#****2014 beggin----------------------------------------------
a014<-read_sav("2014-64-441_1-CH_RECH0.SAV")
ma14<-data.frame(a014$HHID) #copy of original data to master #RECH0

ma14$ENDHHID<-a014$HHID
ma14$ENDHOMEN<-a014$HV002
ma14$ENDLINEN<-a014$HV003
ma14$ENDHOMEQ<-a014$HV009
ma14$ENDWOMENEL<-a014$HV010
ma14$ENDCHILD5<-a014$HV014
ma14$ENDDOMAIN<-a014$HV023
ma14$ENDREGION<-a014$HV024
ma14$ENDURBRU<-a014$HV025
ma14$ENDRES<-a014$HV026

ma14 <- ma14[, -c(1)]

#2014 1-CH base general - population level -------------
#2014 1-CH (1)
a14<-read_sav("2014-64-441_1-CH_RECH1.SAV")
ma114<-data.frame(a14$HHID)

ma114$ENDHHID<-a14$HHID
ma114$ENDIDX<-a14$HVIDX
ma114$ENDRPJEFE<-a14$HV101
ma114$ENDSEXO<-a14$HV104
ma114$ENDEDAD<-a14$HV105
ma114$ENDEDU<-a14$HV109
ma114$ENDMSTA<-a14$HV115
ma114$ENDEMAR<-a14$HV116

ma114$ECASEID<-paste(ma114$ENDHHID,"",ma114$ENDIDX)

ma114 <- ma114[, -c(1)]

ma114<-ma114[colnames(ma114[c(1,9,2:8)])]

ma114$ENDMSTA1<-a14$HV115 #marital status #nunca casado-0
ma114$ENDMSTA1[which(ma114$ENDMSTA1 > 1)] = 1

ma114$ENDMSTA2<-a14$HV115 #marital status #nunca casado + viudo =0
ma114$ENDMSTA2[which(ma114$ENDMSTA2 == 2)] = 1
ma114$ENDMSTA2[which(ma114$ENDMSTA2 == 3)] = 0
ma114$ENDMSTA2[which(ma114$ENDMSTA2 == 4)] = 1
ma114$ENDMSTA2[which(ma114$ENDMSTA2 == 5)] = 1

ma114$ENDMSTA3<-a14$HV115 #marital status #nunca casado + viudo + separado =0
ma114$ENDMSTA3[which(ma114$ENDMSTA3 == 2)] = 1
ma114$ENDMSTA3[which(ma114$ENDMSTA3 == 3)] = 0
ma114$ENDMSTA3[which(ma114$ENDMSTA3 == 4)] = 1
ma114$ENDMSTA3[which(ma114$ENDMSTA3 == 5)] = 0

ma114$ENDEDAD1<-a14$HV105 #teenagers who are mother 15-19
ma114$ENDEDAD1[which(ma114$ENDEDAD1 >19)] = 0
ma114$ENDEDAD1[which(ma114$ENDEDAD1 <15)] = 0
ma114$ENDEDAD1[which(ma114$ENDEDAD1 >0)] = 1

ma114$ENDEDAD2<-a14$HV105 #teen who are mother 12-19
ma114$ENDEDAD2[which(ma114$ENDEDAD2 >19)] = 0
ma114$ENDEDAD2[which(ma114$ENDEDAD2 <12)] = 0
ma114$ENDEDAD2[which(ma114$ENDEDAD2 >0)] = 1

endesf2014<-left_join(ma114,ma14)
colnames(endesf2014)

endesf2014<-endesf2014[colnames(endesf2014[c(2,1,3:23)])]

#2014 1-CH (4) - seguro de salud------------
a1414<-read_sav("2014-64-441_1-CH_RECH4.SAV")

colnames(a1414)
ma1414<-data.frame(a1414$HHID)
#copy$IDXH4_1CH4<-a1414$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma1414$ECASEID<-paste(a1414$HHID,"",a1414$IDXH4)
ma1414$EDNSINSEG<-a1414$SH11Z # no tiene seguro de salud
ma1414$ENDCOMSEG<-a1414$SH11D # compañia de seguro
ma1414$ENDPRISEG<-a1414$SH11E # seguro privado

ma1414 <- ma1414[, -c(1)]
colnames(ma1414)

endesf2014<-left_join(endesf2014,ma1414)
colnames(endesf2014)

# 2014 cardiovascular disease household level --------------------------------------------------    
#2014 1-CH (9) - diabetes, not heart
a1419<-read_sav("2014-413-441_12-ES_CSALUD01.sav")

#modificado 11-06-2021 
ma1419<-data.frame(a1419$QHCLUSTER)
#ma1419$ENDHHID_ES<-paste(a1419$QHCLUSTER,a1419$QHNUMBER,a1419$QHHOME)
#ma1419$ECASEID<-paste(a1419$QHCLUSTER,a1419$QHNUMBER,a1419$QHHOME,"",a1419$QSNUMERO)
ma1419$ENDHHIDx<-paste(a1419$QHCLUSTER,sprintf("%03d",a1419$QHNUMBER),sprintf("%02d",a1419$QHHOME))
# eliminar espacios del ID
searchString <- ' '
replacementString <- ''
ma1419$ENDHHIDx = gsub(searchString,replacementString,ma1419$ENDHHIDx)

ma1419$ECASEID<-paste("","","","","","",ma1419$ENDHHIDx,"",a1419$QSNUMERO)
ma1419$ECASEID_ES<-ma1419$ECASEID

#cierre modificacion 11-06-2021

ma1419$ENSH120<-a1419$QSNUMERO #numero listado del hogar
ma1419$ES_SEXO<-a1419$QSSEXO
#ma1419$ENDCARD<-a1419$SH120 #cardiovascular
ma1419$ENDDIAB<-a1419$QS109 #diabetes

ma1419<- ma1419[-c(1)]

endesf2014<-left_join(endesf2014,ma1419)

#2014 2CV -----------------------------------------------------------
#2014 1-CH general - people level

#2014 2-CV - household level - with cardiac disease from 1CH - ?
a412<-read_sav("2014-65-441_2-CV_RECH23.SAV") 

colnames(a412)

ma1423<-data.frame(a412$HHID) #HHID del hogar

ma1423$ENDHHID<-a412$HHID
ma1423$ENDWATER<-a412$HV201 #fuente del agua potable
ma1423$ENDTOILET<-a412$HV205 #hogar toilet facility
ma1423$ENDELEC<-a412$HV206 #hogar electricidad
ma1423$ENDHHJX<-a412$HV219 #sexo del jefe del hogar
ma1423$ENDWLOC<-a412$HV235 #ubicacion fuente de agua
ma1423$ENDWDAY<-a412$SH42 #agua potable todo el dia
ma1423$ENDWWEEK <-a412$SH43 
ma1423$ENDWSAVE <-a412$SH48
ma1423$ENDINTER <-a412$SH61Q #acceso a internet

ma1423 <- ma1423[-c(1)]

endesf2014<-left_join(endesf2014,ma1423)
basehouse2014<-left_join(ma14,ma1423)

#2014 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a414<-read_sav("2014-67-441_4-HNAC_RE223132.SAV")


ma144<-data.frame(a414$CASEID)
ma144$ECASEID<-a414$CASEID
ma144$ENDPREG<-a414$V213 #actualmente embarazada
ma144$ENDCHLEMB<-a414$V219 #número de niños con vida + embarazo 

ma144$ENDnin<-a414$V219 #0-no tiene, 1 - tiene hijos
ma144$ENDnin[which(ma144$ENDnin > 1)] = 1
ma144<-ma144[-c(1)]

endesf2014<-left_join(endesf2014,ma144)

#2014 9-MMVF ------------------------
a914<-read_sav("2014-73-441_9-MMVF_REC84DV.SAV")
ma14_9<-data.frame(a914$CASEID)
colnames(ma14_9)



ma14_9$ECASEID <- a914$CASEID
ma14_9$ECASEID_MMVF <- a914$CASEID
ma14_9$ENDCORT <- a914$MMC5
ma14_9$ENDVIO1 <- a914$D101A
ma14_9$ENDVIO2 <- a914$D101B
ma14_9$ENDVIO3 <- a914$D101C
ma14_9$ENDVIO4 <- a914$D101D
ma14_9$ENDVIO5 <- a914$D101E
ma14_9$ENDVIO6 <- a914$D101F
ma14_9$ENDVIO7 <- a914$D103A
ma14_9$ENDVIO8 <- a914$D103B
ma14_9$ENDVIO9 <- a914$D103C
ma14_9$ENDVIO10 <- a914$D103D
ma14_9$ENDVIO11 <- a914$D105A
ma14_9$ENDVIO12 <- a914$D105B
ma14_9$ENDVIO13 <- a914$D105C
ma14_9$ENDVIO14 <- a914$D105D
ma14_9$ENDVIO15 <- a914$D105E
ma14_9$ENDVIO16 <- a914$D105F
ma14_9$ENDVIO17 <- a914$D105G
ma14_9$ENDVIO18 <- a914$D105H
ma14_9$ENDVIO19 <- a914$D105I
ma14_9$ENDVIO20 <- a914$D105J
ma14_9$ENDVIO21 <- a914$D110A
ma14_9$ENDVIO22 <- a914$D110B
ma14_9$ENDVIO23 <- a914$D110C
ma14_9$ENDVIO24 <- a914$D110D
ma14_9$ENDVIO25 <- a914$D110E
ma14_9$ENDVIOB1 <- a914$D115C
ma14_9$ENDVIOB2 <- a914$D115E
ma14_9$ENDVIOB3 <- a914$D115G
ma14_9$ENDVIOB4 <- a914$D115I
ma14_9$ENDVIOB5 <- a914$D115J
ma14_9$ENDVIOB6 <- a914$D115K
ma14_9$ENDVIOB7 <- a914$D115L
ma14_9$ENDVIOB8 <- a914$D115N
ma14_9$ENDVIOB9 <- a914$D115P
ma14_9$ENDVIOB10 <- a914$D115R
ma14_9$ENDVIOB11 <- a914$D115T
#ma14_9$ENDVIOB12 <- a914$D115Y
ma14_9$ENDVIOB13 <- a914$D115XC
ma14_9$ENDVIOE1 <- a914$D118A
ma14_9$ENDVIOE2 <- a914$D118C
ma14_9$ENDVIOE3 <- a914$D118E
ma14_9$ENDVIOE4 <- a914$D118G
ma14_9$ENDVIOE5 <- a914$D118I
ma14_9$ENDVIOE6 <- a914$D118J
ma14_9$ENDVIOE7 <- a914$D118K
ma14_9$ENDVIOE8 <- a914$D118L
ma14_9$ENDVIOE9 <- a914$D118N
ma14_9$ENDVIOE10 <- a914$D118P
ma14_9$ENDVIOE11 <- a914$D118T
ma14_9$ENDVIOE12 <- a914$D118XC
ma14_9$ENDVIOINT1 <- a914$D122A
ma14_9$ENDVIOINT2 <- a914$D122B
ma14_9$ENDVIOINT3 <- a914$D122C


ma14_9<- ma14_9[-c(1)] #eliminar ENVIOB12 por no ser adecuada para el calculo

colnames(ma14_9)

ma14_9$ENDVIO1[which(ma14_9$ENDVIO1 == 8)] = NA
ma14_9$ENDVIO2[which(ma14_9$ENDVIO2 == 8)] = NA
ma14_9$ENDVIO3[which(ma14_9$ENDVIO3 == 8)] = NA
ma14_9$ENDVIO4[which(ma14_9$ENDVIO4 == 8)] = NA
ma14_9$ENDVIO5[which(ma14_9$ENDVIO5 == 8)] = NA
ma14_9$ENDVIO6[which(ma14_9$ENDVIO6 == 8)] = NA

ma14_9$ENDVIO7[which(ma14_9$ENDVIO7 == 3)] = 0
ma14_9$ENDVIO7[which(ma14_9$ENDVIO7 > 1)] = 1

ma14_9$ENDVIO8[which(ma14_9$ENDVIO8 == 3)] = 0
ma14_9$ENDVIO8[which(ma14_9$ENDVIO8 > 1)] = 1

ma14_9$ENDVIO10[which(ma14_9$ENDVIO10 == 3)] = 0
ma14_9$ENDVIO10[which(ma14_9$ENDVIO10 > 1)] = 1

ma14_9$ENDVIO11[which(ma14_9$ENDVIO11 == 3)] = 0
ma14_9$ENDVIO11[which(ma14_9$ENDVIO11 > 1)] = 1

ma14_9$ENDVIO12[which(ma14_9$ENDVIO12 == 3)] = 0
ma14_9$ENDVIO12[which(ma14_9$ENDVIO12 > 1)] = 1

ma14_9$ENDVIO13[which(ma14_9$ENDVIO13 == 3)] = 0
ma14_9$ENDVIO13[which(ma14_9$ENDVIO13 > 1)] = 1

ma14_9$ENDVIO14[which(ma14_9$ENDVIO14 == 3)] = 0
ma14_9$ENDVIO14[which(ma14_9$ENDVIO14 > 1)] = 1

ma14_9$ENDVIO15[which(ma14_9$ENDVIO15 == 3)] = 0
ma14_9$ENDVIO15[which(ma14_9$ENDVIO15 > 1)] = 1

ma14_9$ENDVIO16[which(ma14_9$ENDVIO16 == 3)] = 0
ma14_9$ENDVIO16[which(ma14_9$ENDVIO16 > 1)] = 1

ma14_9$ENDVIO17[which(ma14_9$ENDVIO17 == 3)] = 0
ma14_9$ENDVIO17[which(ma14_9$ENDVIO17 > 1)] = 1

ma14_9$ENDVIO18[which(ma14_9$ENDVIO18 == 3)] = 0
ma14_9$ENDVIO18[which(ma14_9$ENDVIO18 > 1)] = 1

ma14_9$ENDVIO19[which(ma14_9$ENDVIO19 == 3)] = 0
ma14_9$ENDVIO19[which(ma14_9$ENDVIO19 > 1)] = 1

ma14_9$ENDVIOB1[which(ma14_9$ENDVIOB1 == 8)] = NA
ma14_9$ENDVIOB2[which(ma14_9$ENDVIOB2 == 8)] = NA
ma14_9$ENDVIOB3[which(ma14_9$ENDVIOB3 == 8)] = NA
ma14_9$ENDVIOB4[which(ma14_9$ENDVIOB4 == 8)] = NA
ma14_9$ENDVIOB5[which(ma14_9$ENDVIOB5 == 8)] = NA
ma14_9$ENDVIOB6[which(ma14_9$ENDVIOB6 == 8)] = NA
ma14_9$ENDVIOB7[which(ma14_9$ENDVIOB7 == 8)] = NA
ma14_9$ENDVIOB8[which(ma14_9$ENDVIOB8 == 8)] = NA
ma14_9$ENDVIOB9[which(ma14_9$ENDVIOB9 == 8)] = NA
ma14_9$ENDVIOB10[which(ma14_9$ENDVIOB10 == 8)] = NA
ma14_9$ENDVIOB11[which(ma14_9$ENDVIOB11 == 8)] = NA
ma14_9$ENDVIOB13[which(ma14_9$ENDVIOB13 == 8)] = NA
ma14_9$ENDVIOINT1[which(ma14_9$ENDVIOINT1 == 2)] = 1
ma14_9$ENDVIOINT2[which(ma14_9$ENDVIOINT2 == 2)] = 1
ma14_9$ENDVIOINT3[which(ma14_9$ENDVIOINT3 == 2)] = 1

ma14_9$ENDVIO <- with(ma14_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma14_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma14_9$ENDVIO))
table(ma14_9$ENDVIO)

ma14_9$ENDVIO[which(ma14_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino

#reduced df
ma149<-ma14_9
ma149<-ma149[-c(2:54)]

endesf2014<-left_join(endesf2014,ma149)

#2014 10-PETA - desnutricion -------------------
a1410<-read_sav("2014-74-441_10-PETA_RECH6.SAV")

ma1410<-data.frame(a1410$HHID)
ma1410$ECASEID<-paste(a1410$HHID,"",a1410$HC0)
ma1410$ENDTAED<-a1410$HC5
ma1410 <- transform(ma1410, ENDTAEDC = ENDTAED / 100)

ma1410$ENDPEED<-a1410$HC8
ma1410 <- transform(ma1410, ENDPEEDC = ENDPEED / 100)

ma1410$ENDPETA<-a1410$HC11
ma1410 <- transform(ma1410, ENDPETAC = ENDPETA / 100)

ma1410<-ma1410[-c(1,3,5,7)]
ma1410$ENDstun<-ma1410$ENDTAEDC
ma1410$ENDstun[which(ma1410$ENDstun > -2)] = 0
ma1410$ENDstun[which(ma1410$ENDstun <= -2)] = 1

ma1410$ENDwas<-ma1410$ENDPETAC
ma1410$ENDwas[which(ma1410$ENDwas > -2)] = 0
ma1410$ENDwas[which(ma1410$ENDwas <= -2)] = 1
endesf2014<-left_join(endesf2014,ma1410)
colnames(endesf2014)

#2014 labels----------
endesf2014$ECASEID =labelled(endesf2014$ECASEID,
                             label="Peopulation ID")

endesf2014$ENDMSTA1 = labelled(endesf2014$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2014$ENDMSTA2 = labelled(endesf2014$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2014$ENDMSTA3 = labelled(endesf2014$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2014$ENDEDAD1 = labelled(endesf2014$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2014$ENDEDAD2 = labelled(endesf2014$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2014$ENDnin = labelled(endesf2014$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2014$ENDVIO = labelled(endesf2014$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")

endesf2014$ENDstun = labelled(endesf2014$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2014$ENDwas = labelled(endesf2014$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2014$ENDmenor5<- endesf2014$ENDEDAD
endesf2014$ENDmenor5[which(endesf2014$ENDmenor5 < 5)] = 1
endesf2014$ENDmenor5[which(endesf2014$ENDmenor5 > 1)] = 0

endesf2014$ENDmenor5 = labelled(endesf2014$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")
endesf2014$ENDmayor65<- endesf2014$ENDEDAD
endesf2014$ENDmayor65[which(endesf2014$ENDmayor65 <= 65)] = 0
endesf2014$ENDmayor65[which(endesf2014$ENDmayor65 > 65)] = 1

endesf2014$ENDmayor65 = labelled(endesf2014$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult > 65")

#---
# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2014,"endesf2014.dta")
# write_sav(endesf2014,"endesf2014v2.sav")
# write_dta(basehouse2014,"endes_2014_diab.dta")
# write_sav(basehouse2014,"endes_house_2014.sav")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")
#****2015 beggin----------------------------------------------
a015<-read_sav("2015-64-504_1-CH_RECH0.SAV")
ma15<-data.frame(a015$HHID) #copy of original data to master #RECH0

ma15$ENDHHID<-a015$HHID
ma15$ENDHOMEN<-a015$HV002
ma15$ENDLINEN<-a015$HV003
ma15$ENDHOMEQ<-a015$HV009
ma15$ENDWOMENEL<-a015$HV010
ma15$ENDCHILD5<-a015$HV014
ma15$ENDDOMAIN<-a015$HV023
ma15$ENDREGION<-a015$HV024
ma15$ENDURBRU<-a015$HV025
ma15$ENDRES<-a015$HV026

ma15 <- ma15[, -c(1)]

#2015 1-CH base general - population level -------------
#2015 1-CH (1)
a15<-read_sav("2015-64-504_1-CH_RECH1.SAV")
ma115<-data.frame(a15$HHID)

ma115$ENDHHID<-a15$HHID
ma115$ENDIDX<-a15$HVIDX
ma115$ENDRPJEFE<-a15$HV101
ma115$ENDSEXO<-a15$HV104
ma115$ENDEDAD<-a15$HV105
ma115$ENDEDU<-a15$HV109
ma115$ENDMSTA<-a15$HV115
ma115$ENDEMAR<-a15$HV116

ma115$ECASEID<-paste(ma115$ENDHHID,"",ma115$ENDIDX)

ma115 <- ma115[, -c(1)]

ma115<-ma115[colnames(ma115[c(1,9,2:8)])]

ma115$ENDMSTA1<-a15$HV115 #marital status #nunca casado-0
ma115$ENDMSTA1[which(ma115$ENDMSTA1 > 1)] = 1

ma115$ENDMSTA2<-a15$HV115 #marital status #nunca casado + viudo =0
ma115$ENDMSTA2[which(ma115$ENDMSTA2 == 2)] = 1
ma115$ENDMSTA2[which(ma115$ENDMSTA2 == 3)] = 0
ma115$ENDMSTA2[which(ma115$ENDMSTA2 == 4)] = 1
ma115$ENDMSTA2[which(ma115$ENDMSTA2 == 5)] = 1

ma115$ENDMSTA3<-a15$HV115 #marital status #nunca casado + viudo + separado =0
ma115$ENDMSTA3[which(ma115$ENDMSTA3 == 2)] = 1
ma115$ENDMSTA3[which(ma115$ENDMSTA3 == 3)] = 0
ma115$ENDMSTA3[which(ma115$ENDMSTA3 == 4)] = 1
ma115$ENDMSTA3[which(ma115$ENDMSTA3 == 5)] = 0

ma115$ENDEDAD1<-a15$HV105 #teenagers who are mother 15-19
ma115$ENDEDAD1[which(ma115$ENDEDAD1 >19)] = 0
ma115$ENDEDAD1[which(ma115$ENDEDAD1 <15)] = 0
ma115$ENDEDAD1[which(ma115$ENDEDAD1 >0)] = 1

ma115$ENDEDAD2<-a15$HV105 #teen who are mother 12-19
ma115$ENDEDAD2[which(ma115$ENDEDAD2 >19)] = 0
ma115$ENDEDAD2[which(ma115$ENDEDAD2 <12)] = 0
ma115$ENDEDAD2[which(ma115$ENDEDAD2 >0)] = 1

endesf2015<-left_join(ma115,ma15)
colnames(endesf2015)

endesf2015<-endesf2015[colnames(endesf2015[c(2,1,3:23)])]

#2015 1-CH (4) - seguro de salud------------
a1514<-read_sav("2015-64-504_1-CH_RECH4.SAV")

colnames(a1514)
ma1514<-data.frame(a1514$HHID)
#copy$IDXH4_1CH4<-a1514$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma1514$ECASEID<-paste(a1514$HHID,"",a1514$IDXH4)
ma1514$EDNSINSEG<-a1514$SH11Z # no tiene seguro de salud
ma1514$ENDCOMSEG<-a1514$SH11D # compañia de seguro
ma1514$ENDPRISEG<-a1514$SH11E # seguro privado

ma1514 <- ma1514[, -c(1)]
colnames(ma1514)

endesf2015<-left_join(endesf2015,ma1514)
colnames(endesf2015)

#2015 cardiovascular disease household level --------------------------------------------------    
#2015 1-CH (9) - cardiovascular [a nivel de hogar] - bd hogar ¿?
a1519<-read_sav("2015-413-504_12-ES_CSALUD01.sav")

#modificado 11-06-2021 
ma1519<-data.frame(a1519$QHCLUSTER)
#ma1519$ENDHHID_ES<-paste(a1519$QHCLUSTER,a1519$QHNUMBER,a1519$QHHOME)
#ma1519$ECASEID<-paste(a1519$QHCLUSTER,a1519$QHNUMBER,a1519$QHHOME,"",a1519$QSNUMERO)
ma1519$ENDHHIDx<-paste(a1519$QHCLUSTER,sprintf("%03d",a1519$QHNUMBER),sprintf("%02d",a1519$QHHOME))
# eliminar espacios del ID
searchString <- ' '
replacementString <- ''
ma1519$ENDHHIDx = gsub(searchString,replacementString,ma1519$ENDHHIDx)

ma1519$ECASEID<-paste("","","","","","",ma1519$ENDHHIDx,"",a1519$QSNUMERO)
ma1519$ECASEID_ES<-ma1519$ECASEID

#cierre modificacion 11-06-2021

ma1519$ENSH120<-a1519$QSNUMERO #numero listado del hogar
ma1519$ES_SEXO<-a1519$QSSEXO
#ma1519$ENDCARD<-a1519$SH120 #cardiovascular
ma1519$ENDDIAB<-a1519$QS109 #diabetes
ma1519<- ma1519[-c(1)]

endesf2015<-left_join(endesf2015,ma1519)

#2015 2CV -----------------------------------------------------------
#2015 1-CH general - people level

#2015 2-CV - household level - with cardiac disease from 1CH - ?
a512<-read_sav("2015-65-504_2-CV_RECH23.SAV") 

colnames(a512)

ma1523<-data.frame(a512$HHID) #HHID del hogar

ma1523$ENDHHID<-a512$HHID
ma1523$ENDWATER<-a512$HV201 #fuente del agua potable
ma1523$ENDTOILET<-a512$HV205 #hogar toilet facility
ma1523$ENDELEC<-a512$HV206 #hogar electricidad
ma1523$ENDHHJX<-a512$HV219 #sexo del jefe del hogar
ma1523$ENDWLOC<-a512$HV235 #ubicacion fuente de agua
ma1523$ENDWDAY<-a512$SH42 #agua potable todo el dia
ma1523$ENDWWEEK <-a512$SH43 
ma1523$ENDWSAVE <-a512$SH48
ma1523$ENDINTER <-a512$SH61Q #acceso a internet

ma1523 <- ma1523[-c(1)]

endesf2015<-left_join(endesf2015,ma1523)
basehouse2015<-left_join(ma15,ma1523)
#2015 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a514<-read_sav("2015-67-504_4-HNAC_RE223132.SAV")


ma154<-data.frame(a514$CASEID)
ma154$ECASEID<-a514$CASEID
ma154$ENDPREG<-a514$V213 #actualmente embarazada
ma154$ENDCHLEMB<-a514$V219 #número de niños con vida + embarazo 

ma154$ENDnin<-a514$V219 #0-no tiene, 1 - tiene hijos
ma154$ENDnin[which(ma154$ENDnin > 1)] = 1
ma154<-ma154[-c(1)]

endesf2015<-left_join(endesf2015,ma154)

#2015 9-MMVF ------------------------
a915<-read_sav("2015-73-504_9-MMVF_REC84DV.SAV")
ma15_9<-data.frame(a915$CASEID)
colnames(ma15_9)

ma15_9$ECASEID <- a915$CASEID
ma15_9$ECASEID_MMVF <- a915$CASEID
ma15_9$ENDCORT <- a915$MMC5
ma15_9$ENDVIO1 <- a915$D101A
ma15_9$ENDVIO2 <- a915$D101B
ma15_9$ENDVIO3 <- a915$D101C
ma15_9$ENDVIO4 <- a915$D101D
ma15_9$ENDVIO5 <- a915$D101E
ma15_9$ENDVIO6 <- a915$D101F
ma15_9$ENDVIO7 <- a915$D103A
ma15_9$ENDVIO8 <- a915$D103B
ma15_9$ENDVIO9 <- a915$D103C
ma15_9$ENDVIO10 <- a915$D103D
ma15_9$ENDVIO11 <- a915$D105A
ma15_9$ENDVIO12 <- a915$D105B
ma15_9$ENDVIO13 <- a915$D105C
ma15_9$ENDVIO14 <- a915$D105D
ma15_9$ENDVIO15 <- a915$D105E
ma15_9$ENDVIO16 <- a915$D105F
ma15_9$ENDVIO17 <- a915$D105G
ma15_9$ENDVIO18 <- a915$D105H
ma15_9$ENDVIO19 <- a915$D105I
ma15_9$ENDVIO20 <- a915$D105J
ma15_9$ENDVIO21 <- a915$D110A
ma15_9$ENDVIO22 <- a915$D110B
ma15_9$ENDVIO23 <- a915$D110C
ma15_9$ENDVIO24 <- a915$D110D
ma15_9$ENDVIO25 <- a915$D110E
ma15_9$ENDVIOB1 <- a915$D115C
ma15_9$ENDVIOB2 <- a915$D115E
ma15_9$ENDVIOB3 <- a915$D115G
ma15_9$ENDVIOB4 <- a915$D115I
ma15_9$ENDVIOB5 <- a915$D115J
ma15_9$ENDVIOB6 <- a915$D115K
ma15_9$ENDVIOB7 <- a915$D115L
ma15_9$ENDVIOB8 <- a915$D115N
ma15_9$ENDVIOB9 <- a915$D115P
ma15_9$ENDVIOB10 <- a915$D115R
ma15_9$ENDVIOB11 <- a915$D115T
#ma15_9$ENDVIOB12 <- a915$D115Y
ma15_9$ENDVIOB13 <- a915$D115XC
ma15_9$ENDVIOE1 <- a915$D118A
ma15_9$ENDVIOE2 <- a915$D118C
ma15_9$ENDVIOE3 <- a915$D118E
ma15_9$ENDVIOE4 <- a915$D118G
ma15_9$ENDVIOE5 <- a915$D118I
ma15_9$ENDVIOE6 <- a915$D118J
ma15_9$ENDVIOE7 <- a915$D118K
ma15_9$ENDVIOE8 <- a915$D118L
ma15_9$ENDVIOE9 <- a915$D118N
ma15_9$ENDVIOE10 <- a915$D118P
ma15_9$ENDVIOE11 <- a915$D118T
ma15_9$ENDVIOE12 <- a915$D118XC
ma15_9$ENDVIOINT1 <- a915$D122A
ma15_9$ENDVIOINT2 <- a915$D122B
ma15_9$ENDVIOINT3 <- a915$D122C


ma15_9<- ma15_9[-c(1)] #eliminar ENVIOB12 por no ser adecuada para el calculo

colnames(ma15_9)

ma15_9$ENDVIO1[which(ma15_9$ENDVIO1 == 8)] = NA
ma15_9$ENDVIO2[which(ma15_9$ENDVIO2 == 8)] = NA
ma15_9$ENDVIO3[which(ma15_9$ENDVIO3 == 8)] = NA
ma15_9$ENDVIO4[which(ma15_9$ENDVIO4 == 8)] = NA
ma15_9$ENDVIO5[which(ma15_9$ENDVIO5 == 8)] = NA
ma15_9$ENDVIO6[which(ma15_9$ENDVIO6 == 8)] = NA

ma15_9$ENDVIO7[which(ma15_9$ENDVIO7 == 3)] = 0
ma15_9$ENDVIO7[which(ma15_9$ENDVIO7 > 1)] = 1

ma15_9$ENDVIO8[which(ma15_9$ENDVIO8 == 3)] = 0
ma15_9$ENDVIO8[which(ma15_9$ENDVIO8 > 1)] = 1

ma15_9$ENDVIO10[which(ma15_9$ENDVIO10 == 3)] = 0
ma15_9$ENDVIO10[which(ma15_9$ENDVIO10 > 1)] = 1

ma15_9$ENDVIO11[which(ma15_9$ENDVIO11 == 3)] = 0
ma15_9$ENDVIO11[which(ma15_9$ENDVIO11 > 1)] = 1

ma15_9$ENDVIO12[which(ma15_9$ENDVIO12 == 3)] = 0
ma15_9$ENDVIO12[which(ma15_9$ENDVIO12 > 1)] = 1

ma15_9$ENDVIO13[which(ma15_9$ENDVIO13 == 3)] = 0
ma15_9$ENDVIO13[which(ma15_9$ENDVIO13 > 1)] = 1

ma15_9$ENDVIO14[which(ma15_9$ENDVIO14 == 3)] = 0
ma15_9$ENDVIO14[which(ma15_9$ENDVIO14 > 1)] = 1

ma15_9$ENDVIO15[which(ma15_9$ENDVIO15 == 3)] = 0
ma15_9$ENDVIO15[which(ma15_9$ENDVIO15 > 1)] = 1

ma15_9$ENDVIO16[which(ma15_9$ENDVIO16 == 3)] = 0
ma15_9$ENDVIO16[which(ma15_9$ENDVIO16 > 1)] = 1

ma15_9$ENDVIO17[which(ma15_9$ENDVIO17 == 3)] = 0
ma15_9$ENDVIO17[which(ma15_9$ENDVIO17 > 1)] = 1

ma15_9$ENDVIO18[which(ma15_9$ENDVIO18 == 3)] = 0
ma15_9$ENDVIO18[which(ma15_9$ENDVIO18 > 1)] = 1

ma15_9$ENDVIO19[which(ma15_9$ENDVIO19 == 3)] = 0
ma15_9$ENDVIO19[which(ma15_9$ENDVIO19 > 1)] = 1

ma15_9$ENDVIOB1[which(ma15_9$ENDVIOB1 == 8)] = NA
ma15_9$ENDVIOB2[which(ma15_9$ENDVIOB2 == 8)] = NA
ma15_9$ENDVIOB3[which(ma15_9$ENDVIOB3 == 8)] = NA
ma15_9$ENDVIOB4[which(ma15_9$ENDVIOB4 == 8)] = NA
ma15_9$ENDVIOB5[which(ma15_9$ENDVIOB5 == 8)] = NA
ma15_9$ENDVIOB6[which(ma15_9$ENDVIOB6 == 8)] = NA
ma15_9$ENDVIOB7[which(ma15_9$ENDVIOB7 == 8)] = NA
ma15_9$ENDVIOB8[which(ma15_9$ENDVIOB8 == 8)] = NA
ma15_9$ENDVIOB9[which(ma15_9$ENDVIOB9 == 8)] = NA
ma15_9$ENDVIOB10[which(ma15_9$ENDVIOB10 == 8)] = NA
ma15_9$ENDVIOB11[which(ma15_9$ENDVIOB11 == 8)] = NA
ma15_9$ENDVIOB13[which(ma15_9$ENDVIOB13 == 8)] = NA
ma15_9$ENDVIOINT1[which(ma15_9$ENDVIOINT1 == 2)] = 1
ma15_9$ENDVIOINT2[which(ma15_9$ENDVIOINT2 == 2)] = 1
ma15_9$ENDVIOINT3[which(ma15_9$ENDVIOINT3 == 2)] = 1

ma15_9$ENDVIO <- with(ma15_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma15_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma15_9$ENDVIO))
table(ma15_9$ENDVIO)

ma15_9$ENDVIO[which(ma15_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino

#reduced df
ma159<-ma15_9
ma159<-ma159[-c(2:55)]

endesf2015<-left_join(endesf2015,ma159)

#2015 10-PETA - desnutricion -------------------
a1510<-read_sav("2015-74-504_10-PETA_RECH6.SAV")

ma1510<-data.frame(a1510$HHID)
ma1510$ECASEID<-paste(a1510$HHID,"",a1510$HC0)
ma1510$ENDTAED<-a1510$HC5
ma1510 <- transform(ma1510, ENDTAEDC = ENDTAED / 100)

ma1510$ENDPEED<-a1510$HC8
ma1510 <- transform(ma1510, ENDPEEDC = ENDPEED / 100)

ma1510$ENDPETA<-a1510$HC11
ma1510 <- transform(ma1510, ENDPETAC = ENDPETA / 100)

ma1510<-ma1510[-c(1,3,5,7)]
ma1510$ENDstun<-ma1510$ENDTAEDC
ma1510$ENDstun[which(ma1510$ENDstun > -2)] = 0
ma1510$ENDstun[which(ma1510$ENDstun <= -2)] = 1

ma1510$ENDwas<-ma1510$ENDPETAC
ma1510$ENDwas[which(ma1510$ENDwas > -2)] = 0
ma1510$ENDwas[which(ma1510$ENDwas <= -2)] = 1
endesf2015<-left_join(endesf2015,ma1510)
colnames(endesf2015)

#2015 labels ---------------
endesf2015$ECASEID =labelled(endesf2015$ECASEID,
                             label="Peopulation ID")

endesf2015$ENDMSTA1 = labelled(endesf2015$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2015$ENDMSTA2 = labelled(endesf2015$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2015$ENDMSTA3 = labelled(endesf2015$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2015$ENDEDAD1 = labelled(endesf2015$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2015$ENDEDAD2 = labelled(endesf2015$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2015$ENDnin = labelled(endesf2015$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2015$ENDVIO = labelled(endesf2015$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")

endesf2015$ENDstun = labelled(endesf2015$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2015$ENDwas = labelled(endesf2015$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2015$ENDmenor5<- endesf2015$ENDEDAD
endesf2015$ENDmenor5[which(endesf2015$ENDmenor5 < 5)] = 1
endesf2015$ENDmenor5[which(endesf2015$ENDmenor5 > 1)] = 0

endesf2015$ENDmenor5 = labelled(endesf2015$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")
endesf2015$ENDmayor65<- endesf2015$ENDEDAD
endesf2015$ENDmayor65[which(endesf2015$ENDmayor65 <= 65)] = 0
endesf2015$ENDmayor65[which(endesf2015$ENDmayor65 > 65)] = 1

endesf2015$ENDmayor65 = labelled(endesf2015$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult > 65")

#---
# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2015,"endesf2015.dta")
# write_sav(endesf2015,"endesf2015v2.sav")
# write_dta(basehouse2015,"endes_2015_diab.dta")
# write_sav(basehouse2015,"endes_house_2015.sav")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")
#****2016 beggin----------------------------------------------
a016<-read_sav("2016-64-548_1-CH_RECH0.SAV")
ma16<-data.frame(a016$HHID) #copy of original data to master #RECH0

ma16$ENDHHID<-a016$HHID
ma16$ENDHOMEN<-a016$HV002
ma16$ENDLINEN<-a016$HV003
ma16$ENDHOMEQ<-a016$HV009
ma16$ENDWOMENEL<-a016$HV010
ma16$ENDCHILD5<-a016$HV014
ma16$ENDDOMAIN<-a016$HV023
ma16$ENDREGION<-a016$HV024
ma16$ENDURBRU<-a016$HV025
ma16$ENDRES<-a016$HV026

ma16 <- ma16[, -c(1)]

#2016 1-CH base general - population level -------------
#2016 1-CH (1)
a16<-read_sav("2016-64-548_1-CH_RECH1.SAV")
ma116<-data.frame(a16$HHID)

ma116$ENDHHID<-a16$HHID
ma116$ENDIDX<-a16$HVIDX
ma116$ENDRPJEFE<-a16$HV101
ma116$ENDSEXO<-a16$HV104
ma116$ENDEDAD<-a16$HV105
ma116$ENDEDU<-a16$HV109
ma116$ENDMSTA<-a16$HV115
ma116$ENDEMAR<-a16$HV116

ma116$ECASEID<-paste(ma116$ENDHHID,"",ma116$ENDIDX)

ma116 <- ma116[, -c(1)]

ma116<-ma116[colnames(ma116[c(1,9,2:8)])]

ma116$ENDMSTA1<-a16$HV115 #marital status #nunca casado-0
ma116$ENDMSTA1[which(ma116$ENDMSTA1 > 1)] = 1

ma116$ENDMSTA2<-a16$HV115 #marital status #nunca casado + viudo =0
ma116$ENDMSTA2[which(ma116$ENDMSTA2 == 2)] = 1
ma116$ENDMSTA2[which(ma116$ENDMSTA2 == 3)] = 0
ma116$ENDMSTA2[which(ma116$ENDMSTA2 == 4)] = 1
ma116$ENDMSTA2[which(ma116$ENDMSTA2 == 5)] = 1

ma116$ENDMSTA3<-a16$HV115 #marital status #nunca casado + viudo + separado =0
ma116$ENDMSTA3[which(ma116$ENDMSTA3 == 2)] = 1
ma116$ENDMSTA3[which(ma116$ENDMSTA3 == 3)] = 0
ma116$ENDMSTA3[which(ma116$ENDMSTA3 == 4)] = 1
ma116$ENDMSTA3[which(ma116$ENDMSTA3 == 5)] = 0

ma116$ENDEDAD1<-a16$HV105 #teenagers who are mother 15-19
ma116$ENDEDAD1[which(ma116$ENDEDAD1 >19)] = 0
ma116$ENDEDAD1[which(ma116$ENDEDAD1 <15)] = 0
ma116$ENDEDAD1[which(ma116$ENDEDAD1 >0)] = 1

ma116$ENDEDAD2<-a16$HV105 #teen who are mother 12-19
ma116$ENDEDAD2[which(ma116$ENDEDAD2 >19)] = 0
ma116$ENDEDAD2[which(ma116$ENDEDAD2 <12)] = 0
ma116$ENDEDAD2[which(ma116$ENDEDAD2 >0)] = 1

endesf2016<-left_join(ma116,ma16)
colnames(endesf2016)

endesf2016<-endesf2016[colnames(endesf2016[c(2,1,3:23)])]

#2016 1-CH (4) - seguro de salud------------
a1614<-read_sav("2016-64-548_1-CH_RECH4.SAV")

colnames(a1614)
ma1614<-data.frame(a1614$HHID)
#copy$IDXH4_1CH4<-a1614$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma1614$ECASEID<-paste(a1614$HHID,"",a1614$IDXH4)
ma1614$EDNSINSEG<-a1614$SH11Z # no tiene seguro de salud
ma1614$ENDCOMSEG<-a1614$SH11D # compañia de seguro
ma1614$ENDPRISEG<-a1614$SH11E # seguro privado

ma1614 <- ma1614[, -c(1)]
colnames(ma1614)

endesf2016<-left_join(endesf2016,ma1614)
colnames(endesf2016)

# 2016 cardiovascular disease household level --------------------------------------------------    
#2016 1-CH (9) - cardiovascular [a nivel de hogar] - bd hogar ¿?
a1619<-read_sav("2016-414-548_12-ES_CSALUD01.sav")

#modificado 11-06-2021 
ma1619<-data.frame(a1619$QHCLUSTER)
#ma1619$ENDHHID_ES<-paste(a1619$QHCLUSTER,a1619$QHNUMBER,a1619$QHHOME)
#ma1619$ECASEID<-paste(a1619$QHCLUSTER,a1619$QHNUMBER,a1619$QHHOME,"",a1619$QSNUMERO)
ma1619$ENDHHIDx<-paste(a1619$QHCLUSTER,sprintf("%03d",a1619$QHNUMBER),sprintf("%02d",a1619$QHHOME))
# eliminar espacios del ID
searchString <- ' '
replacementString <- ''
ma1619$ENDHHIDx = gsub(searchString,replacementString,ma1619$ENDHHIDx)

ma1619$ECASEID<-paste("","","","","","",ma1619$ENDHHIDx,"",a1619$QSNUMERO)
ma1619$ECASEID_ES<-ma1619$ECASEID

#cierre modificacion 11-06-2021

ma1619$ENSH120<-a1619$QSNUMERO #numero listado del hogar
ma1619$ES_SEXO<-a1619$QSSEXO
#ma1619$ENDCARD<-a1619$SH120 #cardiovascular
ma1619$ENDDIAB<-a1619$QS109 #diabetes

ma1619<- ma1619[-c(1)]

endesf2016<-left_join(endesf2016,ma1619)

#2016 2CV -----------------------------------------------------------
#2016 1-CH general - people level

#2016 2-CV - household level - with cardiac disease from 1CH - ?
a612<-read_sav("2016-65-548_2-CV_RECH23.SAV") 

colnames(a612)

ma1623<-data.frame(a612$HHID) #HHID del hogar

ma1623$ENDHHID<-a612$HHID
ma1623$ENDWATER<-a612$HV201 #fuente del agua potable
ma1623$ENDTOILET<-a612$HV205 #hogar toilet facility
ma1623$ENDELEC<-a612$HV206 #hogar electricidad
ma1623$ENDHHJX<-a612$HV219 #sexo del jefe del hogar
ma1623$ENDWLOC<-a612$HV235 #ubicacion fuente de agua
ma1623$ENDWDAY<-a612$SH42 #agua potable todo el dia
ma1623$ENDWWEEK <-a612$SH43 
ma1623$ENDWSAVE <-a612$SH48
ma1623$ENDINTER <-a612$SH61Q #acceso a internet

ma1623 <- ma1623[-c(1)]

endesf2016<-left_join(endesf2016,ma1623)
basehouse2016<-left_join(ma16,ma1623)
#2016 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a614<-read_sav("2016-67-548_4-HNAC_RE223132.SAV")


ma164<-data.frame(a614$CASEID)
ma164$ECASEID<-a614$CASEID
ma164$ENDPREG<-a614$V213 #actualmente embarazada
ma164$ENDCHLEMB<-a614$V219 #número de niños con vida + embarazo 

ma164$ENDnin<-a614$V219 #0-no tiene, 1 - tiene hijos
ma164$ENDnin[which(ma164$ENDnin > 1)] = 1
ma164<-ma164[-c(1)]

endesf2016<-left_join(endesf2016,ma164)

#2016 9-MMVF ------------------------
a916<-read_sav("2016-73-548_9-MMVF_REC84DV.SAV")
ma16_9<-data.frame(a916$CASEID)
colnames(ma16_9)

ma16_9$ECASEID <- a916$CASEID
ma16_9$ECASEID_MMVF <- a916$CASEID
ma16_9$ENDCORT <- a916$MMC5
ma16_9$ENDVIO1 <- a916$D101A
ma16_9$ENDVIO2 <- a916$D101B
ma16_9$ENDVIO3 <- a916$D101C
ma16_9$ENDVIO4 <- a916$D101D
ma16_9$ENDVIO5 <- a916$D101E
ma16_9$ENDVIO6 <- a916$D101F
ma16_9$ENDVIO7 <- a916$D103A
ma16_9$ENDVIO8 <- a916$D103B
ma16_9$ENDVIO9 <- a916$D103C
ma16_9$ENDVIO10 <- a916$D103D
ma16_9$ENDVIO11 <- a916$D105A
ma16_9$ENDVIO12 <- a916$D105B
ma16_9$ENDVIO13 <- a916$D105C
ma16_9$ENDVIO14 <- a916$D105D
ma16_9$ENDVIO15 <- a916$D105E
ma16_9$ENDVIO16 <- a916$D105F
ma16_9$ENDVIO17 <- a916$D105G
ma16_9$ENDVIO18 <- a916$D105H
ma16_9$ENDVIO19 <- a916$D105I
ma16_9$ENDVIO20 <- a916$D105J
ma16_9$ENDVIO21 <- a916$D110A
ma16_9$ENDVIO22 <- a916$D110B
ma16_9$ENDVIO23 <- a916$D110C
ma16_9$ENDVIO24 <- a916$D110D
ma16_9$ENDVIO25 <- a916$D110E
ma16_9$ENDVIOB1 <- a916$D115C
ma16_9$ENDVIOB2 <- a916$D115E
ma16_9$ENDVIOB3 <- a916$D115G
ma16_9$ENDVIOB4 <- a916$D115I
ma16_9$ENDVIOB5 <- a916$D115J
ma16_9$ENDVIOB6 <- a916$D115K
ma16_9$ENDVIOB7 <- a916$D115L
ma16_9$ENDVIOB8 <- a916$D115N
ma16_9$ENDVIOB9 <- a916$D115P
ma16_9$ENDVIOB10 <- a916$D115R
ma16_9$ENDVIOB11 <- a916$D115T
#ma16_9$ENDVIOB12 <- a916$D115Y
ma16_9$ENDVIOB13 <- a916$D115XC
ma16_9$ENDVIOE1 <- a916$D118A
ma16_9$ENDVIOE2 <- a916$D118C
ma16_9$ENDVIOE3 <- a916$D118E
ma16_9$ENDVIOE4 <- a916$D118G
ma16_9$ENDVIOE5 <- a916$D118I
ma16_9$ENDVIOE6 <- a916$D118J
ma16_9$ENDVIOE7 <- a916$D118K
ma16_9$ENDVIOE8 <- a916$D118L
ma16_9$ENDVIOE9 <- a916$D118N
ma16_9$ENDVIOE10 <- a916$D118P
ma16_9$ENDVIOE11 <- a916$D118T
ma16_9$ENDVIOE12 <- a916$D118XC
ma16_9$ENDVIOINT1 <- a916$D122A
ma16_9$ENDVIOINT2 <- a916$D122B
ma16_9$ENDVIOINT3 <- a916$D122C


ma16_9<- ma16_9[-c(1)] #eliminar ENVIOB12 por no ser adecuada para el calculo

colnames(ma16_9)

ma16_9$ENDVIO1[which(ma16_9$ENDVIO1 == 8)] = NA
ma16_9$ENDVIO2[which(ma16_9$ENDVIO2 == 8)] = NA
ma16_9$ENDVIO3[which(ma16_9$ENDVIO3 == 8)] = NA
ma16_9$ENDVIO4[which(ma16_9$ENDVIO4 == 8)] = NA
ma16_9$ENDVIO5[which(ma16_9$ENDVIO5 == 8)] = NA
ma16_9$ENDVIO6[which(ma16_9$ENDVIO6 == 8)] = NA

ma16_9$ENDVIO7[which(ma16_9$ENDVIO7 == 3)] = 0
ma16_9$ENDVIO7[which(ma16_9$ENDVIO7 > 1)] = 1

ma16_9$ENDVIO8[which(ma16_9$ENDVIO8 == 3)] = 0
ma16_9$ENDVIO8[which(ma16_9$ENDVIO8 > 1)] = 1

ma16_9$ENDVIO10[which(ma16_9$ENDVIO10 == 3)] = 0
ma16_9$ENDVIO10[which(ma16_9$ENDVIO10 > 1)] = 1

ma16_9$ENDVIO11[which(ma16_9$ENDVIO11 == 3)] = 0
ma16_9$ENDVIO11[which(ma16_9$ENDVIO11 > 1)] = 1

ma16_9$ENDVIO12[which(ma16_9$ENDVIO12 == 3)] = 0
ma16_9$ENDVIO12[which(ma16_9$ENDVIO12 > 1)] = 1

ma16_9$ENDVIO13[which(ma16_9$ENDVIO13 == 3)] = 0
ma16_9$ENDVIO13[which(ma16_9$ENDVIO13 > 1)] = 1

ma16_9$ENDVIO14[which(ma16_9$ENDVIO14 == 3)] = 0
ma16_9$ENDVIO14[which(ma16_9$ENDVIO14 > 1)] = 1

ma16_9$ENDVIO15[which(ma16_9$ENDVIO15 == 3)] = 0
ma16_9$ENDVIO15[which(ma16_9$ENDVIO15 > 1)] = 1

ma16_9$ENDVIO16[which(ma16_9$ENDVIO16 == 3)] = 0
ma16_9$ENDVIO16[which(ma16_9$ENDVIO16 > 1)] = 1

ma16_9$ENDVIO17[which(ma16_9$ENDVIO17 == 3)] = 0
ma16_9$ENDVIO17[which(ma16_9$ENDVIO17 > 1)] = 1

ma16_9$ENDVIO18[which(ma16_9$ENDVIO18 == 3)] = 0
ma16_9$ENDVIO18[which(ma16_9$ENDVIO18 > 1)] = 1

ma16_9$ENDVIO19[which(ma16_9$ENDVIO19 == 3)] = 0
ma16_9$ENDVIO19[which(ma16_9$ENDVIO19 > 1)] = 1

ma16_9$ENDVIOB1[which(ma16_9$ENDVIOB1 == 8)] = NA
ma16_9$ENDVIOB2[which(ma16_9$ENDVIOB2 == 8)] = NA
ma16_9$ENDVIOB3[which(ma16_9$ENDVIOB3 == 8)] = NA
ma16_9$ENDVIOB4[which(ma16_9$ENDVIOB4 == 8)] = NA
ma16_9$ENDVIOB5[which(ma16_9$ENDVIOB5 == 8)] = NA
ma16_9$ENDVIOB6[which(ma16_9$ENDVIOB6 == 8)] = NA
ma16_9$ENDVIOB7[which(ma16_9$ENDVIOB7 == 8)] = NA
ma16_9$ENDVIOB8[which(ma16_9$ENDVIOB8 == 8)] = NA
ma16_9$ENDVIOB9[which(ma16_9$ENDVIOB9 == 8)] = NA
ma16_9$ENDVIOB10[which(ma16_9$ENDVIOB10 == 8)] = NA
ma16_9$ENDVIOB11[which(ma16_9$ENDVIOB11 == 8)] = NA
ma16_9$ENDVIOB13[which(ma16_9$ENDVIOB13 == 8)] = NA
ma16_9$ENDVIOINT1[which(ma16_9$ENDVIOINT1 == 2)] = 1
ma16_9$ENDVIOINT2[which(ma16_9$ENDVIOINT2 == 2)] = 1
ma16_9$ENDVIOINT3[which(ma16_9$ENDVIOINT3 == 2)] = 1

ma16_9$ENDVIO <- with(ma16_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma16_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma16_9$ENDVIO))
table(ma16_9$ENDVIO)

ma16_9$ENDVIO[which(ma16_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino

#reduced df
ma169<-ma16_9
ma169<-ma169[-c(2:55)]

endesf2016<-left_join(endesf2016,ma169)

#2016 10-PETA - desnutricion -------------------
a1610<-read_sav("2016-74-548_10-PETA_RECH6.sav")

ma1610<-data.frame(a1610$HHID)
ma1610$ECASEID<-paste(a1610$HHID,"",a1610$HC0)
ma1610$ENDTAED<-a1610$HC5
ma1610 <- transform(ma1610, ENDTAEDC = ENDTAED / 100)

ma1610$ENDPEED<-a1610$HC8
ma1610 <- transform(ma1610, ENDPEEDC = ENDPEED / 100)

ma1610$ENDPETA<-a1610$HC11
ma1610 <- transform(ma1610, ENDPETAC = ENDPETA / 100)

ma1610<-ma1610[-c(1,3,5,7)]
ma1610$ENDstun<-ma1610$ENDTAEDC
ma1610$ENDstun[which(ma1610$ENDstun > -2)] = 0
ma1610$ENDstun[which(ma1610$ENDstun <= -2)] = 1

ma1610$ENDwas<-ma1610$ENDPETAC
ma1610$ENDwas[which(ma1610$ENDwas > -2)] = 0
ma1610$ENDwas[which(ma1610$ENDwas <= -2)] = 1
endesf2016<-left_join(endesf2016,ma1610)
colnames(endesf2016)

#2016 labels -----------------------
endesf2016$ECASEID =labelled(endesf2016$ECASEID,
                             label="Peopulation ID")

endesf2016$ENDMSTA1 = labelled(endesf2016$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2016$ENDMSTA2 = labelled(endesf2016$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2016$ENDMSTA3 = labelled(endesf2016$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2016$ENDEDAD1 = labelled(endesf2016$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2016$ENDEDAD2 = labelled(endesf2016$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2016$ENDnin = labelled(endesf2016$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2016$ENDVIO = labelled(endesf2016$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")


endesf2016$ENDstun = labelled(endesf2016$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2016$ENDwas = labelled(endesf2016$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2016$ENDmenor5<- endesf2016$ENDEDAD
endesf2016$ENDmenor5[which(endesf2016$ENDmenor5 < 5)] = 1
endesf2016$ENDmenor5[which(endesf2016$ENDmenor5 > 1)] = 0

endesf2016$ENDmenor5 = labelled(endesf2016$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")
endesf2016$ENDmayor65<- endesf2016$ENDEDAD
endesf2016$ENDmayor65[which(endesf2016$ENDmayor65 <= 65)] = 0
endesf2016$ENDmayor65[which(endesf2016$ENDmayor65 > 65)] = 1

endesf2016$ENDmayor65 = labelled(endesf2016$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult > 65")

#---

# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2016,"endesf2016.dta")
# write_sav(endesf2016,"endesf2016v2.sav")
# write_dta(basehouse2016,"endes_2016_diab.dta")
# write_sav(basehouse2016,"endes_house_2016.sav")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")
#****2017 beggin----------------------------------------------
a017<-read_sav("2017-64-605_1-CH_RECH0.SAV")
ma17<-data.frame(a017$HHID) #copy of original data to master #RECH0

ma17$ENDHHID<-a017$HHID
ma17$ENDHOMEN<-a017$HV002
ma17$ENDLINEN<-a017$HV003
ma17$ENDHOMEQ<-a017$HV009
ma17$ENDWOMENEL<-a017$HV010
ma17$ENDCHILD5<-a017$HV014
ma17$ENDDOMAIN<-a017$HV023
ma17$ENDREGION<-a017$HV024
ma17$ENDURBRU<-a017$HV025
ma17$ENDRES<-a017$HV026

ma17 <- ma17[, -c(1)]

#2017 1-CH base general - population level -------------
#2017 1-CH (1)
a17<-read_sav("2017-64-605_1-CH_RECH1.SAV")
ma117<-data.frame(a17$HHID)

ma117$ENDHHID<-a17$HHID
ma117$ENDIDX<-a17$HVIDX
ma117$ENDRPJEFE<-a17$HV101
ma117$ENDSEXO<-a17$HV104
ma117$ENDEDAD<-a17$HV105
ma117$ENDEDU<-a17$HV109
ma117$ENDMSTA<-a17$HV115
ma117$ENDEMAR<-a17$HV116

ma117$ECASEID<-paste(ma117$ENDHHID,"",ma117$ENDIDX)

ma117 <- ma117[, -c(1)]

ma117<-ma117[colnames(ma117[c(1,9,2:8)])]

ma117$ENDMSTA1<-a17$HV115 #marital status #nunca casado-0
ma117$ENDMSTA1[which(ma117$ENDMSTA1 > 1)] = 1

ma117$ENDMSTA2<-a17$HV115 #marital status #nunca casado + viudo =0
ma117$ENDMSTA2[which(ma117$ENDMSTA2 == 2)] = 1
ma117$ENDMSTA2[which(ma117$ENDMSTA2 == 3)] = 0
ma117$ENDMSTA2[which(ma117$ENDMSTA2 == 4)] = 1
ma117$ENDMSTA2[which(ma117$ENDMSTA2 == 5)] = 1

ma117$ENDMSTA3<-a17$HV115 #marital status #nunca casado + viudo + separado =0
ma117$ENDMSTA3[which(ma117$ENDMSTA3 == 2)] = 1
ma117$ENDMSTA3[which(ma117$ENDMSTA3 == 3)] = 0
ma117$ENDMSTA3[which(ma117$ENDMSTA3 == 4)] = 1
ma117$ENDMSTA3[which(ma117$ENDMSTA3 == 5)] = 0

ma117$ENDEDAD1<-a17$HV105 #teenagers who are mother 15-19
ma117$ENDEDAD1[which(ma117$ENDEDAD1 >19)] = 0
ma117$ENDEDAD1[which(ma117$ENDEDAD1 <15)] = 0
ma117$ENDEDAD1[which(ma117$ENDEDAD1 >0)] = 1

ma117$ENDEDAD2<-a17$HV105 #teen who are mother 12-19
ma117$ENDEDAD2[which(ma117$ENDEDAD2 >19)] = 0
ma117$ENDEDAD2[which(ma117$ENDEDAD2 <12)] = 0
ma117$ENDEDAD2[which(ma117$ENDEDAD2 >0)] = 1

endesf2017<-left_join(ma117,ma17)
colnames(endesf2017)

endesf2017<-endesf2017[colnames(endesf2017[c(2,1,3:23)])]

#2017 1-CH (4) - seguro de salud------------
a1714<-read_sav("2017-64-605_1-CH_RECH4.SAV")

colnames(a1714)
ma1714<-data.frame(a1714$HHID)
#copy$IDXH4_1CH4<-a1714$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma1714$ECASEID<-paste(a1714$HHID,"",a1714$IDXH4)
ma1714$EDNSINSEG<-a1714$SH11Z # no tiene seguro de salud
ma1714$ENDCOMSEG<-a1714$SH11D # compañia de seguro
ma1714$ENDPRISEG<-a1714$SH11E # seguro privado

ma1714 <- ma1714[, -c(1)]
colnames(ma1714)

endesf2017<-left_join(endesf2017,ma1714)
colnames(endesf2017)

# 2017 cardiovascular disease household level --------------------------------------------------    
#2017 1-CH (9) - cardiovascular [a nivel de population] - bd hogar ¿?
a1719<-read_sav("2017-413-605_12-ES_CSALUD01.sav")

#modificado 11-06-2021 
ma1719<-data.frame(a1719$QHCLUSTER)
#ma1719$ENDHHID_ES<-paste(a1719$QHCLUSTER,a1719$QHNUMBER,a1719$QHHOME)
#ma1719$ECASEID<-paste(a1719$QHCLUSTER,a1719$QHNUMBER,a1719$QHHOME,"",a1719$QSNUMERO)
ma1719$ENDHHIDx<-paste(a1719$QHCLUSTER,sprintf("%03d",a1719$QHNUMBER),sprintf("%02d",a1719$QHHOME))
# eliminar espacios del ID
searchString <- ' '
replacementString <- ''
ma1719$ENDHHIDx = gsub(searchString,replacementString,ma1719$ENDHHIDx)

ma1719$ECASEID<-paste("","","","","","",ma1719$ENDHHIDx,"",a1719$QSNUMERO)
ma1719$ECASEID_ES<-ma1719$ECASEID

#cierre modificacion 11-06-2021

ma1719$ENSH120<-a1719$QSNUMERO #numero listado del hogar
ma1719$ES_SEXO<-a1719$QSSEXO
#ma1719$ENDCARD<-a1719$SH120 #cardiovascular
ma1719$ENDDIAB<-a1719$QS109 #diabetes

ma1719<- ma1719[-c(1)]

endesf2017<-left_join(endesf2017,ma1719)

#2017 2CV -----------------------------------------------------------
#2017 1-CH general - people level

#2017 2-CV - household level - with cardiac disease from 1CH - ?
a712<-read_sav("2017-65-605_2-CV_RECH23.SAV") 

colnames(a712)

ma1723<-data.frame(a712$HHID) #HHID del hogar

ma1723$ENDHHID<-a712$HHID
ma1723$ENDWATER<-a712$HV201 #fuente del agua potable
ma1723$ENDTOILET<-a712$HV205 #hogar toilet facility
ma1723$ENDELEC<-a712$HV206 #hogar electricidad
ma1723$ENDHHJX<-a712$HV219 #sexo del jefe del hogar
ma1723$ENDWLOC<-a712$HV235 #ubicacion fuente de agua
ma1723$ENDWDAY<-a712$SH42 #agua potable todo el dia
ma1723$ENDWWEEK <-a712$SH43 
ma1723$ENDWSAVE <-a712$SH48
ma1723$ENDINTER <-a712$SH61Q #acceso a internet

ma1723 <- ma1723[-c(1)]

endesf2017<-left_join(endesf2017,ma1723)
basehouse2017<-left_join(ma17,ma1723)
#2017 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a714<-read_sav("2017-67-605_4-HNAC_RE223132.SAV")


ma174<-data.frame(a714$CASEID)
ma174$ECASEID<-a714$CASEID
ma174$ENDPREG<-a714$V213 #actualmente embarazada
ma174$ENDCHLEMB<-a714$V219 #número de niños con vida + embarazo 

ma174$ENDnin<-a714$V219 #0-no tiene, 1 - tiene hijos
ma174$ENDnin[which(ma174$ENDnin > 1)] = 1

ma174<-ma174[-c(1)]

endesf2017<-left_join(endesf2017,ma174)

#2017 9-MMVF ------------------------
a917<-read_sav("2017-73-605_9-MMVF_REC84DV.SAV")
ma17_9<-data.frame(a917$CASEID)
colnames(ma17_9)

ma17_9$ECASEID <- a917$CASEID
ma17_9$ECASEID_MMFV <- a917$CASEID
ma17_9$ENDCORT <- a917$MMC5
ma17_9$ENDVIO1 <- a917$D101A
ma17_9$ENDVIO2 <- a917$D101B
ma17_9$ENDVIO3 <- a917$D101C
ma17_9$ENDVIO4 <- a917$D101D
ma17_9$ENDVIO5 <- a917$D101E
ma17_9$ENDVIO6 <- a917$D101F
ma17_9$ENDVIO7 <- a917$D103A
ma17_9$ENDVIO8 <- a917$D103B
ma17_9$ENDVIO9 <- a917$D103C
ma17_9$ENDVIO10 <- a917$D103D
ma17_9$ENDVIO11 <- a917$D105A
ma17_9$ENDVIO12 <- a917$D105B
ma17_9$ENDVIO13 <- a917$D105C
ma17_9$ENDVIO14 <- a917$D105D
ma17_9$ENDVIO15 <- a917$D105E
ma17_9$ENDVIO16 <- a917$D105F
ma17_9$ENDVIO17 <- a917$D105G
ma17_9$ENDVIO18 <- a917$D105H
ma17_9$ENDVIO19 <- a917$D105I
ma17_9$ENDVIO20 <- a917$D105J
ma17_9$ENDVIO21 <- a917$D110A
ma17_9$ENDVIO22 <- a917$D110B
ma17_9$ENDVIO23 <- a917$D110C
ma17_9$ENDVIO24 <- a917$D110D
ma17_9$ENDVIO25 <- a917$D110E
ma17_9$ENDVIOB1 <- a917$D115C
ma17_9$ENDVIOB2 <- a917$D115E
ma17_9$ENDVIOB3 <- a917$D115G
ma17_9$ENDVIOB4 <- a917$D115I
ma17_9$ENDVIOB5 <- a917$D115J
ma17_9$ENDVIOB6 <- a917$D115K
ma17_9$ENDVIOB7 <- a917$D115L
ma17_9$ENDVIOB8 <- a917$D115N
ma17_9$ENDVIOB9 <- a917$D115P
ma17_9$ENDVIOB10 <- a917$D115R
ma17_9$ENDVIOB11 <- a917$D115T
#ma17_9$ENDVIOB12 <- a917$D115Y
ma17_9$ENDVIOB13 <- a917$D115XC
ma17_9$ENDVIOE1 <- a917$D118A
ma17_9$ENDVIOE2 <- a917$D118C
ma17_9$ENDVIOE3 <- a917$D118E
ma17_9$ENDVIOE4 <- a917$D118G
ma17_9$ENDVIOE5 <- a917$D118I
ma17_9$ENDVIOE6 <- a917$D118J
ma17_9$ENDVIOE7 <- a917$D118K
ma17_9$ENDVIOE8 <- a917$D118L
ma17_9$ENDVIOE9 <- a917$D118N
ma17_9$ENDVIOE10 <- a917$D118P
ma17_9$ENDVIOE11 <- a917$D118T
ma17_9$ENDVIOE12 <- a917$D118XC
ma17_9$ENDVIOINT1 <- a917$D122A
ma17_9$ENDVIOINT2 <- a917$D122B
ma17_9$ENDVIOINT3 <- a917$D122C


ma17_9<- ma17_9[-c(1)] #eliminar ENVIOB12 por no ser adecuada para el calculo

colnames(ma17_9)

ma17_9$ENDVIO1[which(ma17_9$ENDVIO1 == 8)] = NA
ma17_9$ENDVIO2[which(ma17_9$ENDVIO2 == 8)] = NA
ma17_9$ENDVIO3[which(ma17_9$ENDVIO3 == 8)] = NA
ma17_9$ENDVIO4[which(ma17_9$ENDVIO4 == 8)] = NA
ma17_9$ENDVIO5[which(ma17_9$ENDVIO5 == 8)] = NA
ma17_9$ENDVIO6[which(ma17_9$ENDVIO6 == 8)] = NA

ma17_9$ENDVIO7[which(ma17_9$ENDVIO7 == 3)] = 0
ma17_9$ENDVIO7[which(ma17_9$ENDVIO7 > 1)] = 1

ma17_9$ENDVIO8[which(ma17_9$ENDVIO8 == 3)] = 0
ma17_9$ENDVIO8[which(ma17_9$ENDVIO8 > 1)] = 1

ma17_9$ENDVIO10[which(ma17_9$ENDVIO10 == 3)] = 0
ma17_9$ENDVIO10[which(ma17_9$ENDVIO10 > 1)] = 1

ma17_9$ENDVIO11[which(ma17_9$ENDVIO11 == 3)] = 0
ma17_9$ENDVIO11[which(ma17_9$ENDVIO11 > 1)] = 1

ma17_9$ENDVIO12[which(ma17_9$ENDVIO12 == 3)] = 0
ma17_9$ENDVIO12[which(ma17_9$ENDVIO12 > 1)] = 1

ma17_9$ENDVIO13[which(ma17_9$ENDVIO13 == 3)] = 0
ma17_9$ENDVIO13[which(ma17_9$ENDVIO13 > 1)] = 1

ma17_9$ENDVIO14[which(ma17_9$ENDVIO14 == 3)] = 0
ma17_9$ENDVIO14[which(ma17_9$ENDVIO14 > 1)] = 1

ma17_9$ENDVIO15[which(ma17_9$ENDVIO15 == 3)] = 0
ma17_9$ENDVIO15[which(ma17_9$ENDVIO15 > 1)] = 1

ma17_9$ENDVIO16[which(ma17_9$ENDVIO16 == 3)] = 0
ma17_9$ENDVIO16[which(ma17_9$ENDVIO16 > 1)] = 1

ma17_9$ENDVIO17[which(ma17_9$ENDVIO17 == 3)] = 0
ma17_9$ENDVIO17[which(ma17_9$ENDVIO17 > 1)] = 1

ma17_9$ENDVIO18[which(ma17_9$ENDVIO18 == 3)] = 0
ma17_9$ENDVIO18[which(ma17_9$ENDVIO18 > 1)] = 1

ma17_9$ENDVIO19[which(ma17_9$ENDVIO19 == 3)] = 0
ma17_9$ENDVIO19[which(ma17_9$ENDVIO19 > 1)] = 1

ma17_9$ENDVIOB1[which(ma17_9$ENDVIOB1 == 8)] = NA
ma17_9$ENDVIOB2[which(ma17_9$ENDVIOB2 == 8)] = NA
ma17_9$ENDVIOB3[which(ma17_9$ENDVIOB3 == 8)] = NA
ma17_9$ENDVIOB4[which(ma17_9$ENDVIOB4 == 8)] = NA
ma17_9$ENDVIOB5[which(ma17_9$ENDVIOB5 == 8)] = NA
ma17_9$ENDVIOB6[which(ma17_9$ENDVIOB6 == 8)] = NA
ma17_9$ENDVIOB7[which(ma17_9$ENDVIOB7 == 8)] = NA
ma17_9$ENDVIOB8[which(ma17_9$ENDVIOB8 == 8)] = NA
ma17_9$ENDVIOB9[which(ma17_9$ENDVIOB9 == 8)] = NA
ma17_9$ENDVIOB10[which(ma17_9$ENDVIOB10 == 8)] = NA
ma17_9$ENDVIOB11[which(ma17_9$ENDVIOB11 == 8)] = NA
ma17_9$ENDVIOB13[which(ma17_9$ENDVIOB13 == 8)] = NA
ma17_9$ENDVIOINT1[which(ma17_9$ENDVIOINT1 == 2)] = 1
ma17_9$ENDVIOINT2[which(ma17_9$ENDVIOINT2 == 2)] = 1
ma17_9$ENDVIOINT3[which(ma17_9$ENDVIOINT3 == 2)] = 1

ma17_9$ENDVIO <- with(ma17_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma17_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma17_9$ENDVIO))
table(ma17_9$ENDVIO)

ma17_9$ENDVIO[which(ma17_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino

#reduced df
ma179<-ma17_9
ma179<-ma179[-c(2:55)]

endesf2017<-left_join(endesf2017,ma179)

#2017 10-PETA - desnutricion -------------------
a1710<-read_sav("2017-74-605_10-PETA_RECH6.SAV")

ma1710<-data.frame(a1710$HHID)
ma1710$ECASEID<-paste(a1710$HHID,"",a1710$HC0)
ma1710$ENDTAED<-a1710$HC5
ma1710 <- transform(ma1710, ENDTAEDC = ENDTAED / 100)

ma1710$ENDPEED<-a1710$HC8
ma1710 <- transform(ma1710, ENDPEEDC = ENDPEED / 100)

ma1710$ENDPETA<-a1710$HC11
ma1710 <- transform(ma1710, ENDPETAC = ENDPETA / 100)

ma1710<-ma1710[-c(1,3,5,7)]
ma1710$ENDstun<-ma1710$ENDTAEDC
ma1710$ENDstun[which(ma1710$ENDstun > -2)] = 0
ma1710$ENDstun[which(ma1710$ENDstun <= -2)] = 1

ma1710$ENDwas<-ma1710$ENDPETAC
ma1710$ENDwas[which(ma1710$ENDwas > -2)] = 0
ma1710$ENDwas[which(ma1710$ENDwas <= -2)] = 1
endesf2017<-left_join(endesf2017,ma1710)
colnames(endesf2017)

#2017 labels -----------------
endesf2017$ECASEID =labelled(endesf2017$ECASEID,
                             label="Peopulation ID")

endesf2017$ENDMSTA1 = labelled(endesf2017$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2017$ENDMSTA2 = labelled(endesf2017$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2017$ENDMSTA3 = labelled(endesf2017$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2017$ENDEDAD1 = labelled(endesf2017$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2017$ENDEDAD2 = labelled(endesf2017$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2017$ENDnin = labelled(endesf2017$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2017$ENDVIO = labelled(endesf2017$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")

endesf2017$ENDstun = labelled(endesf2017$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2017$ENDwas = labelled(endesf2017$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2017$ENDmenor5<- endesf2017$ENDEDAD
endesf2017$ENDmenor5[which(endesf2017$ENDmenor5 < 5)] = 1
endesf2017$ENDmenor5[which(endesf2017$ENDmenor5 > 1)] = 0

endesf2017$ENDmenor5 = labelled(endesf2017$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")
endesf2017$ENDmayor65<- endesf2017$ENDEDAD
endesf2017$ENDmayor65[which(endesf2017$ENDmayor65 <= 65)] = 0
endesf2017$ENDmayor65[which(endesf2017$ENDmayor65 > 65)] = 1

endesf2017$ENDmayor65 = labelled(endesf2017$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult > 65")

#---

# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2017,"endesf2017.dta")
# write_sav(endesf2017,"endesf2017v2.sav")
# write_dta(basehouse2017,"endes_2017_diab.dta")
# write_sav(basehouse2017,"endes_house_2017.sav")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")
#****2018 beggin----------------------------------------------
a018<-read_sav("2018-64-638_1-CH_RECH0.SAV")
ma18<-data.frame(a018$HHID) #copy of original data to master #RECH0

ma18$ENDHHID<-a018$HHID
ma18$ENDHOMEN<-a018$HV002
ma18$ENDLINEN<-a018$HV003
ma18$ENDHOMEQ<-a018$HV009
ma18$ENDWOMENEL<-a018$HV010
ma18$ENDCHILD5<-a018$HV014
ma18$ENDDOMAIN<-a018$HV023
ma18$ENDREGION<-a018$HV024
ma18$ENDURBRU<-a018$HV025
ma18$ENDRES<-a018$HV026

ma18 <- ma18[, -c(1)]

#2018 1-CH base general - population level -------------
#2018 1-CH (1)
a18<-read_sav("2018-64-638_1-CH_RECH1.SAV")
ma118<-data.frame(a18$HHID)

ma118$ENDHHID<-a18$HHID
ma118$ENDIDX<-a18$HVIDX
ma118$ENDRPJEFE<-a18$HV101
ma118$ENDSEXO<-a18$HV104
ma118$ENDEDAD<-a18$HV105
ma118$ENDEDU<-a18$HV109
ma118$ENDMSTA<-a18$HV115
ma118$ENDEMAR<-a18$HV116

ma118$ECASEID<-paste(ma118$ENDHHID,"",ma118$ENDIDX)

ma118 <- ma118[, -c(1)]

ma118<-ma118[colnames(ma118[c(1,9,2:8)])]

ma118$ENDMSTA1<-a18$HV115 #marital status #nunca casado-0
ma118$ENDMSTA1[which(ma118$ENDMSTA1 > 1)] = 1

ma118$ENDMSTA2<-a18$HV115 #marital status #nunca casado + viudo =0
ma118$ENDMSTA2[which(ma118$ENDMSTA2 == 2)] = 1
ma118$ENDMSTA2[which(ma118$ENDMSTA2 == 3)] = 0
ma118$ENDMSTA2[which(ma118$ENDMSTA2 == 4)] = 1
ma118$ENDMSTA2[which(ma118$ENDMSTA2 == 5)] = 1

ma118$ENDMSTA3<-a18$HV115 #marital status #nunca casado + viudo + separado =0
ma118$ENDMSTA3[which(ma118$ENDMSTA3 == 2)] = 1
ma118$ENDMSTA3[which(ma118$ENDMSTA3 == 3)] = 0
ma118$ENDMSTA3[which(ma118$ENDMSTA3 == 4)] = 1
ma118$ENDMSTA3[which(ma118$ENDMSTA3 == 5)] = 0

ma118$ENDEDAD1<-a18$HV105 #teenagers who are mother 15-19
ma118$ENDEDAD1[which(ma118$ENDEDAD1 >19)] = 0
ma118$ENDEDAD1[which(ma118$ENDEDAD1 <15)] = 0
ma118$ENDEDAD1[which(ma118$ENDEDAD1 >0)] = 1

ma118$ENDEDAD2<-a18$HV105 #teen who are mother 12-19
ma118$ENDEDAD2[which(ma118$ENDEDAD2 >19)] = 0
ma118$ENDEDAD2[which(ma118$ENDEDAD2 <12)] = 0
ma118$ENDEDAD2[which(ma118$ENDEDAD2 >0)] = 1

endesf2018<-left_join(ma118,ma18)
colnames(endesf2018)

endesf2018<-endesf2018[colnames(endesf2018[c(2,1,3:23)])]

#2018 1-CH (4) - seguro de salud------------
a1814<-read_sav("2018-64-638_1-CH_RECH4.SAV")

colnames(a1814)
ma1814<-data.frame(a1814$HHID)
#copy$IDXH4_1CH4<-a1814$IDXH4  #unnecesary
#case id 1 ch rech 4 

ma1814$ECASEID<-paste(a1814$HHID,"",a1814$IDXH4)
ma1814$EDNSINSEG<-a1814$SH11Z # no tiene seguro de salud
ma1814$ENDCOMSEG<-a1814$SH11D # compañia de seguro
ma1814$ENDPRISEG<-a1814$SH11E # seguro privado

ma1814 <- ma1814[, -c(1)]
colnames(ma1814)

endesf2018<-left_join(endesf2018,ma1814)
colnames(endesf2018)

# 2018 cardiovascular disease household level --------------------------------------------------    
#2018 1-CH (9) - cardiovascular [a nivel de hogar] - bd hogar ¿?
a1819<-read_sav("2018-414-638_12-ES_CSALUD01.SAV")

#modificado 11-06-2021 
ma1819<-data.frame(a1819$QHCLUSTER)
#ma1819$ENDHHID_ES<-paste(a1819$QHCLUSTER,a1819$QHNUMBER,a1819$QHHOME)
#ma1819$ECASEID<-paste(a1819$QHCLUSTER,a1819$QHNUMBER,a1819$QHHOME,"",a1819$QSNUMERO)
ma1819$ENDHHIDx<-paste(a1819$QHCLUSTER,sprintf("%03d",a1819$QHNUMBER),sprintf("%02d",a1819$QHHOME))
# eliminar espacios del ID
searchString <- ' '
replacementString <- ''
ma1819$ENDHHIDx = gsub(searchString,replacementString,ma1819$ENDHHIDx)

ma1819$ECASEID<-paste("","","","","","",ma1819$ENDHHIDx,"",a1819$QSNUMERO)
ma1819$ECASEID_ES<-ma1819$ECASEID

#cierre modificacion 11-06-2021

ma1819$ENSH120<-a1819$QSNUMERO #numero listado del hogar
ma1819$ES_SEXO<-a1819$QSSEXO
#ma1819$ENDCARD<-a1819$SH120 #cardiovascular
ma1819$ENDDIAB<-a1819$QS109 #diabetes

ma1819<- ma1819[-c(1)]

endesf2018<-left_join(endesf2018,ma1819)

#2018 2CV -----------------------------------------------------------
#2018 1-CH general - people level

#2018 2-CV - household level - with cardiac disease from 1CH - ?
a812<-read_sav("2018-65-638_2-CV_RECH23.SAV") 

colnames(a812)

ma1823<-data.frame(a812$HHID) #HHID del hogar

ma1823$ENDHHID<-a812$HHID
ma1823$ENDWATER<-a812$HV201 #fuente del agua potable
ma1823$ENDTOILET<-a812$HV205 #hogar toilet facility
ma1823$ENDELEC<-a812$HV206 #hogar electricidad
ma1823$ENDHHJX<-a812$HV219 #sexo del jefe del hogar
ma1823$ENDWLOC<-a812$HV235 #ubicacion fuente de agua
ma1823$ENDWDAY<-a812$SH42 #agua potable todo el dia
ma1823$ENDWWEEK <-a812$SH43 
ma1823$ENDWSAVE <-a812$SH48
ma1823$ENDINTER <-a812$SH61Q #acceso a internet

ma1823 <- ma1823[-c(1)]

endesf2018<-left_join(endesf2018,ma1823)
basehouse2018<-left_join(ma18,ma1823)
#2018 EMBARAZO 4-HNAC comparar datos ? embarazada ------------
a814<-read_sav("2018-67-638_4-HNAC_RE223132.SAV")


ma184<-data.frame(a814$CASEID)
ma184$ECASEID<-a814$CASEID
ma184$ENDPREG<-a814$V213 #actualmente embarazada
ma184$ENDCHLEMB<-a814$V219 #número de niños con vida + embarazo 

ma184$ENDnin<-a814$V219 #0-no tiene, 1 - tiene hijos
ma184$ENDnin[which(ma184$ENDnin > 1)] = 1
ma184<-ma184[-c(1)]

endesf2018<-left_join(endesf2018,ma184)

#2018 9-MMVF ------------------------
a918<-read_sav("2018-73-638_9-MMVF_REC84DV.SAV")
ma18_9<-data.frame(a918$CASEID)
colnames(ma18_9)

ma18_9$ECASEID <- a918$CASEID
ma18_9$ECASEID_MMVF <- a918$CASEID
ma18_9$ENDCORT <- a918$MMC5
ma18_9$ENDVIO1 <- a918$D101A
ma18_9$ENDVIO2 <- a918$D101B
ma18_9$ENDVIO3 <- a918$D101C
ma18_9$ENDVIO4 <- a918$D101D
ma18_9$ENDVIO5 <- a918$D101E
ma18_9$ENDVIO6 <- a918$D101F
ma18_9$ENDVIO7 <- a918$D103A
ma18_9$ENDVIO8 <- a918$D103B
ma18_9$ENDVIO9 <- a918$D103C
ma18_9$ENDVIO10 <- a918$D103D
ma18_9$ENDVIO11 <- a918$D105A
ma18_9$ENDVIO12 <- a918$D105B
ma18_9$ENDVIO13 <- a918$D105C
ma18_9$ENDVIO14 <- a918$D105D
ma18_9$ENDVIO15 <- a918$D105E
ma18_9$ENDVIO16 <- a918$D105F
ma18_9$ENDVIO17 <- a918$D105G
ma18_9$ENDVIO18 <- a918$D105H
ma18_9$ENDVIO19 <- a918$D105I
ma18_9$ENDVIO20 <- a918$D105J
ma18_9$ENDVIO21 <- a918$D110A
ma18_9$ENDVIO22 <- a918$D110B
ma18_9$ENDVIO23 <- a918$D110C
ma18_9$ENDVIO24 <- a918$D110D
ma18_9$ENDVIO25 <- a918$D110E
ma18_9$ENDVIOB1 <- a918$D115C
ma18_9$ENDVIOB2 <- a918$D115E
ma18_9$ENDVIOB3 <- a918$D115G
ma18_9$ENDVIOB4 <- a918$D115I
ma18_9$ENDVIOB5 <- a918$D115J
ma18_9$ENDVIOB6 <- a918$D115K
ma18_9$ENDVIOB7 <- a918$D115L
ma18_9$ENDVIOB8 <- a918$D115N
ma18_9$ENDVIOB9 <- a918$D115P
ma18_9$ENDVIOB10 <- a918$D115R
ma18_9$ENDVIOB11 <- a918$D115T
#ma18_9$ENDVIOB12 <- a918$D115Y
ma18_9$ENDVIOB13 <- a918$D115XC
ma18_9$ENDVIOE1 <- a918$D118A
ma18_9$ENDVIOE2 <- a918$D118C
ma18_9$ENDVIOE3 <- a918$D118E
ma18_9$ENDVIOE4 <- a918$D118G
ma18_9$ENDVIOE5 <- a918$D118I
ma18_9$ENDVIOE6 <- a918$D118J
ma18_9$ENDVIOE7 <- a918$D118K
ma18_9$ENDVIOE8 <- a918$D118L
ma18_9$ENDVIOE9 <- a918$D118N
ma18_9$ENDVIOE10 <- a918$D118P
ma18_9$ENDVIOE11 <- a918$D118T
ma18_9$ENDVIOE12 <- a918$D118XC
ma18_9$ENDVIOINT1 <- a918$D122A
ma18_9$ENDVIOINT2 <- a918$D122B
ma18_9$ENDVIOINT3 <- a918$D122C


ma18_9<- ma18_9[-c(1)] #eliminar ENVIOB12 por no ser adecuada para el calculo

colnames(ma18_9)

ma18_9$ENDVIO1[which(ma18_9$ENDVIO1 == 8)] = NA
ma18_9$ENDVIO2[which(ma18_9$ENDVIO2 == 8)] = NA
ma18_9$ENDVIO3[which(ma18_9$ENDVIO3 == 8)] = NA
ma18_9$ENDVIO4[which(ma18_9$ENDVIO4 == 8)] = NA
ma18_9$ENDVIO5[which(ma18_9$ENDVIO5 == 8)] = NA
ma18_9$ENDVIO6[which(ma18_9$ENDVIO6 == 8)] = NA

ma18_9$ENDVIO7[which(ma18_9$ENDVIO7 == 3)] = 0
ma18_9$ENDVIO7[which(ma18_9$ENDVIO7 > 1)] = 1

ma18_9$ENDVIO8[which(ma18_9$ENDVIO8 == 3)] = 0
ma18_9$ENDVIO8[which(ma18_9$ENDVIO8 > 1)] = 1

ma18_9$ENDVIO10[which(ma18_9$ENDVIO10 == 3)] = 0
ma18_9$ENDVIO10[which(ma18_9$ENDVIO10 > 1)] = 1

ma18_9$ENDVIO11[which(ma18_9$ENDVIO11 == 3)] = 0
ma18_9$ENDVIO11[which(ma18_9$ENDVIO11 > 1)] = 1

ma18_9$ENDVIO12[which(ma18_9$ENDVIO12 == 3)] = 0
ma18_9$ENDVIO12[which(ma18_9$ENDVIO12 > 1)] = 1

ma18_9$ENDVIO13[which(ma18_9$ENDVIO13 == 3)] = 0
ma18_9$ENDVIO13[which(ma18_9$ENDVIO13 > 1)] = 1

ma18_9$ENDVIO14[which(ma18_9$ENDVIO14 == 3)] = 0
ma18_9$ENDVIO14[which(ma18_9$ENDVIO14 > 1)] = 1

ma18_9$ENDVIO15[which(ma18_9$ENDVIO15 == 3)] = 0
ma18_9$ENDVIO15[which(ma18_9$ENDVIO15 > 1)] = 1

ma18_9$ENDVIO16[which(ma18_9$ENDVIO16 == 3)] = 0
ma18_9$ENDVIO16[which(ma18_9$ENDVIO16 > 1)] = 1

ma18_9$ENDVIO17[which(ma18_9$ENDVIO17 == 3)] = 0
ma18_9$ENDVIO17[which(ma18_9$ENDVIO17 > 1)] = 1

ma18_9$ENDVIO18[which(ma18_9$ENDVIO18 == 3)] = 0
ma18_9$ENDVIO18[which(ma18_9$ENDVIO18 > 1)] = 1

ma18_9$ENDVIO19[which(ma18_9$ENDVIO19 == 3)] = 0
ma18_9$ENDVIO19[which(ma18_9$ENDVIO19 > 1)] = 1

ma18_9$ENDVIOB1[which(ma18_9$ENDVIOB1 == 8)] = NA
ma18_9$ENDVIOB2[which(ma18_9$ENDVIOB2 == 8)] = NA
ma18_9$ENDVIOB3[which(ma18_9$ENDVIOB3 == 8)] = NA
ma18_9$ENDVIOB4[which(ma18_9$ENDVIOB4 == 8)] = NA
ma18_9$ENDVIOB5[which(ma18_9$ENDVIOB5 == 8)] = NA
ma18_9$ENDVIOB6[which(ma18_9$ENDVIOB6 == 8)] = NA
ma18_9$ENDVIOB7[which(ma18_9$ENDVIOB7 == 8)] = NA
ma18_9$ENDVIOB8[which(ma18_9$ENDVIOB8 == 8)] = NA
ma18_9$ENDVIOB9[which(ma18_9$ENDVIOB9 == 8)] = NA
ma18_9$ENDVIOB10[which(ma18_9$ENDVIOB10 == 8)] = NA
ma18_9$ENDVIOB11[which(ma18_9$ENDVIOB11 == 8)] = NA
ma18_9$ENDVIOB13[which(ma18_9$ENDVIOB13 == 8)] = NA
ma18_9$ENDVIOINT1[which(ma18_9$ENDVIOINT1 == 2)] = 1
ma18_9$ENDVIOINT2[which(ma18_9$ENDVIOINT2 == 2)] = 1
ma18_9$ENDVIOINT3[which(ma18_9$ENDVIOINT3 == 2)] = 1

ma18_9$ENDVIO <- with(ma18_9, ifelse(is.na(ENDVIO1) & is.na(ENDVIO2) & is.na(ENDVIO3) & is.na(ENDVIO4) & is.na(ENDVIO5) & is.na(ENDVIO6) & is.na(ENDVIO7) & is.na(ENDVIO8) & is.na(ENDVIO9) & is.na(ENDVIO10) & is.na(ENDVIO11) & is.na(ENDVIO12) & is.na(ENDVIO13) & is.na(ENDVIO14) & is.na(ENDVIO15) & is.na(ENDVIO16) & is.na(ENDVIO17) & is.na(ENDVIO18) & is.na(ENDVIO19) & is.na(ENDVIO20) & is.na(ENDVIO21) & is.na(ENDVIO22) & is.na(ENDVIO23) & is.na(ENDVIO24) & is.na(ENDVIO25) & is.na(ENDVIOB1) & is.na(ENDVIOB2) & is.na(ENDVIOB3) & is.na(ENDVIOB4) & is.na(ENDVIOB5) & is.na(ENDVIOB6) & is.na(ENDVIOB7) & is.na(ENDVIOB8) & is.na(ENDVIOB9) & is.na(ENDVIOB10) & is.na(ENDVIOB11) & is.na(ENDVIOB13) & is.na(ENDVIOE1) & is.na(ENDVIOE2) & is.na(ENDVIOE3) & is.na(ENDVIOE4) & is.na(ENDVIOE5) & is.na(ENDVIOE6) & is.na(ENDVIOE7) & is.na(ENDVIOE8) & is.na(ENDVIOE9) & is.na(ENDVIOE10) & is.na(ENDVIOE11) & is.na(ENDVIOE12) & is.na(ENDVIOINT1) & is.na(ENDVIOINT2) & is.na(ENDVIOINT3), NA, 
                                     rowSums(ma18_9[,c("ENDVIO1", "ENDVIO2", "ENDVIO3", "ENDVIO4", "ENDVIO5", "ENDVIO6", "ENDVIO7", "ENDVIO8", "ENDVIO9", "ENDVIO10", "ENDVIO11", "ENDVIO12", "ENDVIO13", "ENDVIO14", "ENDVIO15", "ENDVIO16", "ENDVIO17", "ENDVIO18", "ENDVIO19", "ENDVIO20", "ENDVIO21", "ENDVIO22", "ENDVIO23", "ENDVIO24", "ENDVIO25", "ENDVIOB1", "ENDVIOB2", "ENDVIOB3", "ENDVIOB4", "ENDVIOB5", "ENDVIOB6", "ENDVIOB7", "ENDVIOB8", "ENDVIOB9", "ENDVIOB10", "ENDVIOB11", "ENDVIOB13", "ENDVIOE1", "ENDVIOE2", "ENDVIOE3", "ENDVIOE4", "ENDVIOE5", "ENDVIOE6", "ENDVIOE7", "ENDVIOE8", "ENDVIOE9", "ENDVIOE10", "ENDVIOE11", "ENDVIOE12", "ENDVIOINT1", "ENDVIOINT2", "ENDVIOINT3")], na.rm=TRUE)))
sum(is.na(ma18_9$ENDVIO))
table(ma18_9$ENDVIO)

ma18_9$ENDVIO[which(ma18_9$ENDVIO > 1)] = 1 #ha sufrido algun tipo de violencia por parte del sexo masculino

#reduced df
ma189<-ma18_9
ma189<-ma189[-c(2:54)]

endesf2018<-left_join(endesf2018,ma189)

#2018 10-PETA - desnutricion -------------------
a1810<-read_sav("2018-74-638_10-PETA_RECH6.SAV")

ma1810<-data.frame(a1810$HHID)
ma1810$ECASEID<-paste(a1810$HHID,"",a1810$HC0)
ma1810$ENDTAED<-a1810$HC5
ma1810 <- transform(ma1810, ENDTAEDC = ENDTAED / 100)

ma1810$ENDPEED<-a1810$HC8
ma1810 <- transform(ma1810, ENDPEEDC = ENDPEED / 100)

ma1810$ENDPETA<-a1810$HC11
ma1810 <- transform(ma1810, ENDPETAC = ENDPETA / 100)

ma1810<-ma1810[-c(1,3,5,7)]
ma1810$ENDstun<-ma1810$ENDTAEDC
ma1810$ENDstun[which(ma1810$ENDstun > -2)] = 0
ma1810$ENDstun[which(ma1810$ENDstun <= -2)] = 1

ma1810$ENDwas<-ma1810$ENDPETAC
ma1810$ENDwas[which(ma1810$ENDwas > -2)] = 0
ma1810$ENDwas[which(ma1810$ENDwas <= -2)] = 1
endesf2018<-left_join(endesf2018,ma1810)
colnames(endesf2018)

#2018 labels---------------------------

endesf2018$ECASEID =labelled(endesf2018$ECASEID,
                             label="Peopulation ID")

endesf2018$ENDMSTA1 = labelled(endesf2018$ENDMSTA1, 
                               #the value labels
                               c(Other= 1, Never_married = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married")

endesf2018$ENDMSTA2 = labelled(endesf2018$ENDMSTA2, 
                               #the value labels
                               c(Other = 1, Never_married_widower = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married_widower")


endesf2018$ENDMSTA3 = labelled(endesf2018$ENDMSTA3, 
                               #the value labels
                               c(Other = 1, NM_W_Sep = 0), 
                               # we can also assign a Variable Label in SPSS style
                               label="Never married, widower and separated")

endesf2018$ENDEDAD1 = labelled(endesf2018$ENDEDAD1, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_15_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 15-19")

endesf2018$ENDEDAD2 = labelled(endesf2018$ENDEDAD2, 
                               #the value labels
                               c(Not_teenagers = 0, Tennagers_12_19 = 1), 
                               # we can also assign a Variable Label in SPSS style
                               label="Tennagers from 12-19")

endesf2018$ENDnin = labelled(endesf2018$ENDnin, 
                             #the value labels
                             c(No_child = 0, Child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever had child?")

endesf2018$ENDVIO = labelled(endesf2018$ENDVIO, 
                             #the value labels
                             c(No_gender_violence = 0, Gender_Violence = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Ever experimented gender violence (physical, mental or sexual)?")

endesf2018$ENDstun = labelled(endesf2018$ENDstun, 
                              #the value labels
                              c(Normal_child = 0, Stunting_child = 1), 
                              # we can also assign a Variable Label in SPSS style
                              label="Stunting children")

endesf2018$ENDwas = labelled(endesf2018$ENDwas, 
                             #the value labels
                             c(Normal_child = 0, Wasting_child = 1), 
                             # we can also assign a Variable Label in SPSS style
                             label="Wasting children")

endesf2018$ENDmenor5<- endesf2018$ENDEDAD
endesf2018$ENDmenor5[which(endesf2018$ENDmenor5 < 5)] = 1
endesf2018$ENDmenor5[which(endesf2018$ENDmenor5 > 1)] = 0

endesf2018$ENDmenor5 = labelled(endesf2018$ENDmenor5, 
                                #the value labels
                                c(Person_older_than_5 = 0, Child_younger_from_five = 1), 
                                # we can also assign a Variable Label in SPSS style
                                label="Child below 5 years old")
endesf2018$ENDmayor65<- endesf2018$ENDEDAD
endesf2018$ENDmayor65[which(endesf2018$ENDmayor65 <= 65)] = 0
endesf2018$ENDmayor65[which(endesf2018$ENDmayor65 > 65)] = 1

endesf2018$ENDmayor65 = labelled(endesf2018$ENDmayor65, 
                                 #the value labels
                                 c(Person_yg_than_65 = 0, Adult_older_than_65 = 1), 
                                 # we can also assign a Variable Label in SPSS style
                                 label="Adult > 65")

#---

# setwd("~/CLIMA UPCH/R codes")
# write_dta(endesf2018,"endesf2018.dta")
# write_sav(endesf2018,"endesf2018v2.sav")
# write_dta(basehouse2018,"endes_2018_diab.dta")
# write_sav(basehouse2018,"endes_house_2018.sav")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")

#****data merge on excel--------------------------
# setwd("~/CLIMA UPCH/R codes")

#install.packages("openxlsx")
require(openxlsx)
list_of_datasets <- list("2010" = endesf2010, "2011" = endesf2012, "2012" = endesf2012, "2013" = endesf2013, "2014" = endesf2014, "2015" = endesf2015, "2016" = endesf2016, "2017" = endesf2017, "2018" = endesf2018)

write.xlsx(list_of_datasets, file = "endes_2010_2018.xlsx")
write.xlsx(ma1019, file = "endes_2010_diab_comp.xlsx")

# setwd("~/CLIMA UPCH/R codes/ALLSAV")
