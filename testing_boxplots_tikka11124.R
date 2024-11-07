library(scales);library("plotrix");library(Maaslin2);  library(ggtext);library(lavaan);library(psych);library("xlsx");library(lsr);library(quantreg);library("readxl")
library(semPlot);library(mediation);require(lme4);require(reshape2);library(mlma);library(binilib);library(plyr);library("viridis");library(RColorBrewer) 
library(magrittr); library(ggplot2);library( "censReg" );library(ggsankey);library(dplyr);library(tidyverse);library(dmetar);library(meta);library(ggforestplot); 
## Warning, do not add: 'library(forestplot)'! It is not working with ggforestplot... (9.9.24)
library(mdatools);library(circlize);library(igraph);library('bigsnpr');library(rcompanion);library(scRNAseq);
library(tibble);library(stringr);library(MOFA2);library('qpgraph') 
library("grid"); library("ggplotify");library(ggpubr);library(rstatix);library(datarium);library(RColorBrewer); library(ggh4x); library(effsize)
library(chorddiag);library(corrplot);library(scater);library(mdatools);library(car);library(FSA);library(pathviewr);library(glmnet)
library("lmtest");library(PerformanceAnalytics);library(psych);library("readxl");library(ggforce);library(ComplexHeatmap);library(ragg) 
library('Hmisc');library(correlation);library(ggcorrplot);library(pheatmap);library(mgcv);library('ppcor');library(superb)
library(rmdformats);library(prettydoc);library(hrbrthemes);library(tint);library(tufte)
library(extrafont);library(scatterplot3d); library(rgl);library(ggsankey);library(ggsankeyfier)
# https://require.predictiveecology.org/


#First set your data folder:
setwd("C:/Users/patati/Desktop/TurkuOW/RWork")

#And then load the primary steroid data
NAFLD=read_excel("NAFLD_SteroidStudy.xlsx",sheet = "LFAT_steroidsDATA") # This is partly auxiliary
oknames=colnames(NAFLD); NAFLD=data.frame(NAFLD)

#The names of the steroid groups need to be imported early on:
groups=read.csv("groups_17823.csv", header = TRUE, sep=";")
groups=groups[,c('Group','Abbreviation')]
groups=groups[groups[,'Abbreviation']!='F',]
groups=groups[order(groups[,'Group']),]
groups[,'Abbreviation'][groups[,'Abbreviation']=='17aOH-P4']='17a-OHP4'

ForSteroidNames=read_excel("C:/Users/patati/Desktop/TurkuOW/RWork/NAFLD_SteroidStudy_for groups.xlsx",sheet = "Steroid name abbreviations") # This is partly auxiliary
groups2=data.frame(ForSteroidNames)[,1:4]; g1=read.csv("C:/Users/patati/Desktop/TurkuOW/RWork/groups_17823.csv", header = TRUE, sep=";")
groups2=cbind(g1[,'Group'], groups2[,c('Abbreviation','Abbreviation_old','Name')])
groups2=groups2[groups2[,'Abbreviation']!='FF',]; colnames(groups2)[1]="Group";
groups2=groups2[order(groups2[,'Group']),]
groups2[,'Abbreviation'][groups2[,'Abbreviation']=='17aOH-P4']='17a-OHP4'

#P4 was found from elsewhere to have the following characteristics:
NAFLD[,'P4'] = as.numeric(NAFLD[,'P4'])
NAFLD[,'P4'][is.na(NAFLD[,'P4'])] = 22557.3330346846#median(NAFLD[,'P4'], na.rm=TRUE) 
NAFLD[,5:7][NAFLD[,5:7]==0.01]=0; colnames(NAFLD)=oknames
MASLD=read_excel("Combined.Matrix.For.Pauli.2023.10.17.Excel.Formatv2.xlsx") # This is the main file
oknames=colnames(MASLD); MASLD=data.frame(MASLD); colnames(MASLD)=oknames   # All kinds of tricks are needed for getting the right data format
rownames(MASLD)=MASLD[,1]
MASLD[,'P4'] = as.numeric(MASLD[,'P4']) #The same comment as above
MASLD[,'P4'][is.na(MASLD[,'P4'])] = 22557.3330346846 
eva=c('Grade(0-3)', 'Stage(0-4)','Necroinflammation')
MASLD[,eva][MASLD[,eva]==0.01]=0; 
td=c('11-KDHT','AN','DHT','17a-OHP5','E2','P5','DOC')
val=c(103,252,51,200,26.5,253,10); vale=c(100,250,50,200,25,250,10)
for (i in 1:7) {MASLD[,td][i][MASLD[,td][i]==val[i]]=vale[i]} 

# These (E) are ok as per lab:
ME=read.csv('E_tikka231023.csv',header=TRUE, sep=";")
ME2=rownames(MASLD[MASLD[,'E']==106000,]) 
to=ME[which(ME[,1] %in% ME2),'patient.number']
te=ME[which(ME[,1] %in% ME2),'E']
MASLD[as.character(to),'E']=te
# These (11-KA4) will perhaps change in the lab (sometime after 24.10.23):
M11=read.csv('11KA4_tikka231023.csv',header=TRUE, sep=";")
# M11[,1][c(1:5,9)];MASLD[as.character(M11[,1][c(1:5,9)]),'11-KA4'] #These were denoted with 'big interference'
MASLD[as.character(M11[,1][c(1:5,9)]),'11-KA4'] = NA #Alternatively: median(MASLD[!rownames(MASLD) %in% as.character(M11[,1][c(1:5,9)]),'11-KA4'])
a=MASLD[order(MASLD[,'BMI']),'BMI']
b=NAFLD[order(NAFLD[,'BMI']),'BMI']
them=unique(b[! b %in% a])
NAFLD=NAFLD[order(NAFLD[,'BMI']),] 
NAFLD=NAFLD[NAFLD[,'BMI']!=them,]
MASLD=MASLD[order(MASLD[,'BMI']),]
#https://appsilon.com/imputation-in-r/ #https://www.datasciencemadesimple.com/get-minimum-value-of-a-column-in-r-2/?expand_article=1
# New data import withouth changing the conames: https://readxl.tidyverse.org/articles/column-names.html
Bali=data.frame(read_excel("Liver_bile_acids_PFAS.xlsx",sheet = "Liver_BA",.name_repair = "minimal")); row.names(Bali)=Bali[,1]
Pfase=data.frame(read_excel("Liver_bile_acids_PFAS.xlsx",sheet = "PFAS_serum",.name_repair = "minimal")); rownames(Pfase)=as.vector(unlist(Pfase[,1]))
Base=data.frame(read_excel("Liver_bile_acids_PFAS.xlsx",sheet = "Serum_BA",.name_repair = "minimal"));rownames(Base)=as.vector(unlist(Base[,1]))
C4=data.frame(read_excel("Liver_bile_acids_PFAS.xlsx",sheet = "C4",.name_repair = "minimal")); rownames(C4)=as.vector(unlist(C4[,1]))
Clini=data.frame(read_excel("Matching clinical data_all.xlsx",sheet = "Sheet1",.name_repair = "minimal")); rownames(Clini)=as.vector(unlist(Clini[,1]));
#https://www.analyticsvidhya.com/blog/2021/06/hypothesis-testing-parametric-and-non-parametric-tests-in-statistics/
MASLD[1:2,2:27] #or head(MASLD);

# The below ordering needs to be changed...
Bali=Bali[as.character(MASLD$PatientNumber),];Bali[1:3,2:10] #https://stackoverflow.com/questions/54264980/r-how-to-set-row-names-attribute-as-numeric-from-character I did otherway around
Base=Base[as.character(MASLD$PatientNumber),];#Base[1:3,2:10]
Clini=Clini[as.character(MASLD$PatientNumber),];#Clini[1:3,2:10] # Many of these are irrelevant here... so not opening them, they would exhaust this file
C4=C4[as.character(MASLD$PatientNumber),];#C4[1:3,]
Pfase=Pfase[as.character(MASLD$PatientNumber),];Pfase[1:3,2:10]

# Menopause markers:
menopause=read_excel("Putative_metabolic_markers_menopause.xlsx",sheet='menopause markers',.name_repair = "minimal"); #rownames(Clini)=as.vector(unlist(Clini[,1]));
menopause=menopause[8:dim(menopause)[1],]; menopause=menopause[,-15]; menopause[2,2:14]=menopause[1,2:14]; menopause=data.frame(menopause); menopause[2,13:14]=c('v1','v2'); #dim(menopause)
colnames(menopause)=c('row_names',menopause[2,2:dim(menopause)[2]]); menopause=menopause[3:dim(menopause)[1],];rownames(menopause)=as.vector(unlist(menopause[,1]));
menopause=menopause[as.character(MASLD$PatientNumber),]
colnames(Pfase)[colnames(Pfase)=='PFHxA.1']='PFHxA_Branched'
Pfase=Pfase[,colnames(Pfase)!='Benzylparaben.1']
Pfase[Pfase[,'Benzylparaben']>10,'Benzylparaben']=NA 

Jeihou=data.frame(read_excel("Copy of BA_liverfat_RawData.xls",.name_repair = "minimal")); row.names(Jeihou)=Jeihou[,1];Jeihou=Jeihou[as.character(MASLD$PatientNumber),]
u=Jeihou[Jeihou[,'GHDGA']=='<LLOQ',1]; a=u[!is.na(u)]; b=rownames(Bali[Bali[,'GHDGA']==1,]);
uu=Jeihou[Jeihou[,'GHDGA']=='No Result',1]; aa=uu[!is.na(uu)]; 
Bali[as.character(a),'GHDGA']=min(Bali[,'GHDGA'],na.rm=TRUE)/2
heps=Bali[Bali[,'GHDGA']==1,1] 
Bali[as.character(heps),'GHDGA']=NA
#https://www.datasciencemadesimple.com/get-minimum-value-of-a-column-in-r-2/?expand_article=1
mat=Bali[,c('TbMCA','ToMCA','TDCA','TDHCA','TLCA')]
mat[!mat>1]=10000
mat[mat==2]=10000 #Colmins did not work so I used (i.e. colmins ei toiminut ja käytin):
hip=do.call(pmin, lapply(1:nrow(mat), function(i)mat[i,])) #https://stackoverflow.com/questions/13676878/fastest-way-to-get-min-from-every-column-in-a-matrix
hou=c('TbMCA','ToMCA','TDCA','TDHCA','TLCA')
for (i in 1:5) {Bali[Bali[,hou[i]]==1,hou[i]]=hip[i]}
for (i in 1:5) {Bali[Bali[,hou[i]]==2,hou[i]]=hip[i]}

# An imputation for the missing values:
C4[is.na(C4[,2]),2]=median(C4[!is.na(C4[,2]),2]) #assuming that these were not below quantitation and replacing with median
#https://www.geeksforgeeks.org/performing-logarithmic-computations-in-r-programming-log-log10-log1p-and-log2-functions/
#https://stackoverflow.com/questions/50476717/i-want-to-align-match-two-unequal-columns

#Matching two unequal columns.. match the names of one original column (dat2) to ones that are missing (dat1 with to other) #Not sure if this should be this difficult...
tv=cbind(MASLD[,1],NAFLD[,2:7],Clini[,'HOMA.IR'],MASLD[,colnames(NAFLD[,8:27])],Bali[,2:dim(Bali)[2]], C4[,2:dim(C4)[2]],Base[,2:dim(Base)[2]],Pfase[,(2:(dim(Pfase)[2]))], MASLD[,'PFAS']);
colnames(tv)[colnames(tv)=='C4[, 2:dim(C4)[2]]']='C4';colnames(tv)[colnames(tv)=='Clini[, \"HOMA.IR\"]']='HOMA-IR'
colnames(tv)[colnames(tv)=='MASLD[, \"PFAS\"]']='PFAS';
colnames(tv)[colnames(tv)=="MASLD[, 1]" ]='PatientNumber';#colnames(tv)#
rownames(tv)=unlist(Bali[,1]); 
hep=colnames(tv)[!colnames(tv) %in% c( "Benzylparaben" ,"Methylparaben")] 

# Not sure when it is the best time to take not needed variables away, perhaps at the very end?
tv=tv[,hep]
# Here I add the lipids. In the future, I need to divide all the groups in their own components e.g. dataframe called 'lipids' so that adding them will be more straightforward:
tv=cbind(tv,MASLD[,(dim(MASLD)[2]-13):dim(MASLD)[2]]) 
# hupo=match(   colnames(tv)[colnames(tv) %in% groups2[,3]], groups2[,3] ) # do ni; https://www.geeksforgeeks.org/how-to-find-index-of-element-in-vector-in-r/
# tvauxe=tv
# colnames(tv)[colnames(tv) %in% groups2[,3]]=groups2[hupo,2]


# The basic preprocessing is just the below lines:
tve=tv[,2:dim(tv)[2]]; tve[tve == 0] <- NA; #Almost all variables are here
tv_half <- tve %>% mutate(replace(., is.na(.), min(., na.rm = T)/2)) #https://mdatools.com/docs/preprocessing--autoscaling.html
tv_half_log2 <- log2(tv_half);
tv_auto <- prep.autoscale(as.matrix(tv_half_log2), center = TRUE, scale = TRUE);  #https://svkucheryavski.gitbooks.io/mdatools/content/preprocessing/text.html
tv_all=cbind(tv[,1],tv_auto); 

# Changing the column names needs to have separate variables for each type of variable (contaminant, steroid, etc.)
x1=colnames(tv_all[,c(1:8)]); v2=dim(NAFLD)[2]+1
x2=colnames(tv_all[,9:v2]);v3=(dim(Bali)[2]+v2);x3=colnames(tv_all[,(v2+1):(v3)]);v4=(dim(Base)[2])+v3
x4=colnames(tv_all[,(v3+1):(v4-1)]);x5=colnames(tv_all[,(v4):(dim(tv_all)[2])]); 
x3 <- paste(x3, "_L", sep="") #https://stackoverflow.com/questions/6984796/how-to-paste-a-string-on-each-element-of-a-vector-of-strings-using-apply-in-r
x4=gsub("(-[0-9]*)*.1", "", x4) #https://stackoverflow.com/questions/18997297/remove-ending-of-string-with-gsub
x4 <- paste(x4, "_S", sep="")# https://rdrr.io/bioc/qpgraph/man/qpNrr.html
x5a=x5[1:9]
x6=x5[10:length(x5)] #Dividing to lipids
x5=x5a  #Making sure that PFAS are separate
nm = c(x1,x2,x3,x4,x5,x6); nm=c('PatientNumber','Gender','AGE','BMI','Steatosis Grade','Fibrosis Stage','Necroinflammation','HOMA-IR',nm[9:length(nm)])
colnames(tv_all)=nm; #tv_all[1:5,1:30]; #NAFLD[1:2,1:28];
colnames(tv_all)[colnames(tv_all)=='MASLD[, \"PFAS\"]']='PFAS';
# This (deletion) is good to do after all the previous:
x5=x5[x5!='PFAS'];x5=x5[x5!='Perfluorodecyl.ethanoic.acid']; x6=x6[x6!='Total_TG'] # x1;x2;x3;x4;x5;
tv_all=tv_all[,!colnames(tv_all) %in% c('Total_TG','PFAS',"Perfluorodecyl.ethanoic.acid")]

# In case you would need just the logged values:
tv_half_log22=cbind(tv[,1],tv_half_log2);
x1=colnames(tv_half_log22[,c(1:8)]); v2=dim(NAFLD)[2]+1
x2=colnames(tv_half_log22[,9:v2]);v3=(dim(Bali)[2]+v2);
x3=colnames(tv_half_log22[,(v2+1):(v3)]);v4=(dim(Base)[2])+v3
x3=x3[c(length(x3),1:(length(x3)-1))]
x4=colnames(tv_half_log22[,(v3+1):(v4-1)]);
x5=colnames(tv_half_log22[,(v4):(dim(tv_half_log22)[2])]);
x3 <- paste(x3, "_L", sep="") 
#https://stackoverflow.com/questions/6984796/how-to-paste-a-string-on-each-element-of-a-vector-of-strings-using-apply-in-r
x4=gsub("(-[0-9]*)*.1", "", x4) #https://stackoverflow.com/questions/18997297/remove-ending-of-string-with-gsub
x4 <- paste(x4, "_S", sep="")# https://rdrr.io/bioc/qpgraph/man/qpNrr.html
x5a=x5[1:9]
x6=x5[10:length(x5)] #dividing to lipids
x5=x5a  #making sure that PFAS are separate
nm = c(x1,x2,x3,x4,x5,x6); nm=c('PatientNumber','Gender','AGE','BMI','Steatosis Grade','Fibrosis Stage','Necroinflammation','HOMA-IR',nm[9:length(nm)])
colnames(tv_half_log22)=nm; #tv_half_log22[1:5,1:30]; #NAFLD[1:2,1:28];
colnames(tv_half_log22)[colnames(tv_half_log22)=='MASLD[, \"PFAS\"]']='PFAS';
# This (deletion) is good to do after all the previous:
x5=x5[x5!='PFAS'];x5=x5[x5!='Perfluorodecyl.ethanoic.acid']; x6=x6[x6!='Total_TG'] # x1;x2;x3;x4;x5;
tv_half_log22=tv_half_log22[,!colnames(tv_half_log22) %in% c('Total_TG','PFAS',"Perfluorodecyl.ethanoic.acid")]

# This needs to be done early on:
colnames(tv)[colnames(tv)=='17aOH-P4']='17a-OHP4'
colnames(tv_half_log22)[colnames(tv_half_log22)=='17aOH-P4']='17a-OHP4'
colnames(tv_all)[colnames(tv_all)=='17aOH-P4']='17a-OHP4'

tv_all=tv_all[,!colnames(tv_all) %in% c('Total_TG','PFAS','Perfluorodecyl.ethanoic.acid')]
tv_all=tv_all[,!colnames(tv_all) %in% x4]

# In case you would need nonscaled covariates and scaled/logged all other variables:
tv_covscl=tv_all
tv_covNS=cbind(tv[,1:8],tv_all[,9:dim(tv_all)[2]])
tv_LOG_covscl=tv_half_log22
tv_LOG_covNS=cbind(tv[,1:8],tv_half_log22[,9:dim(tv_half_log22)[2]])
colnames(tv_covNS)[1:8]=colnames(tv_all)[1:8]
colnames(tv_LOG_covNS)[1:8]=colnames(tv_all)[1:8]
# This is needed occasionally:
tv_c=tv_covscl 
# https://stackoverflow.com/questions/6984796/how-to-paste-a-string-on-each-element-of-a-vector-of-strings-using-apply-in-r
# https://stackoverflow.com/questions/18997297/remove-ending-of-string-with-gsub # https://rdrr.io/bioc/qpgraph/man/qpNrr.html

hupo=match(   colnames(tv_c)[colnames(tv_c) %in% groups2[,3]], groups2[,3] ) 
# do ni; https://www.geeksforgeeks.org/how-to-find-index-of-element-in-vector-in-r/
tvauxe=tv_c
colnames(tv_c)[colnames(tv_c) %in% groups2[,3]]=groups2[hupo,2]

# This needs to be done also soon, to gather all the treatment etc. variable names separately...: 
Treatment=colnames(tv_all)[52:58];
Mediator=colnames(tv_all)[9:28];
Outcome=colnames(tv_all)[c(29:51,59:71)]; ##https://sparkbyexamples.com/r-programming/r-remove-from-vector-with-examples/

Outcome=Outcome[!Outcome %in% c('Total_TG','PFAS','Perfluorodecyl.ethanoic.acid')]
Outcome=Outcome[! Outcome %in% x4] #https://sparkbyexamples.com/r-programming/r-remove-from-vector-with-examples/
Mediator[Mediator=="17aOH-P4"]="17a-OHP4"
Treatment=Treatment[!Treatment %in% c('Perfluorodecyl.ethanoic.acid')]

tv_half_log22[,'11-KA4'][tv_half_log22[,'11-KA4']==min(tv_half_log22[,'11-KA4'])]=median(tv_half_log22[,'11-KA4'])
other='241024e';ie=tv_half_log22## 'Steatosis Grade','Fibrosis Stage','Necroinflammation','HOMA-IR'
hupo=match(   colnames(ie)[colnames(ie) %in% groups2[,3]], groups2[,3] ) 
colnames(ie)[colnames(ie) %in% groups2[,3]]=groups2[hupo,2]

windowsFonts(A = windowsFont("Calibri (Body)"))

# The significance levels are: '****<0.001', '***<0.01', '**<0.05', '*<0.1'
Outcome='Steatosis Grade';Out='Steatosis'; oute='Steatosis Grade';num=0;Group='All';
# boxplots(ie,Group,Outcome,Out,oute,other);Group='Female';boxplots(ie,Group,Outcome,Out,oute,other);Group='Male';boxplots(ie,Group,Outcome,Out,oute,other)

library(devtools) # Make sure that the devtools library is loaded
install_github("paulitikka/Boxplots")
library(Boxplots)
# https://stackoverflow.com/questions/15170399/change-r-default-library-path-using-libpaths-in-rprofile-site-fails-to-work


boxplots(ie,Group,Outcome,Out,oute,other)




