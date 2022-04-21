### R-script for reading and combining Field sheets for Danish flora and Vegetation
# The script has been developed for the plant monitoring programme in relation to course in Danish flora and vegetation
# script by Bjarke Madsen, bjarke.madsen@bio.au.dk and Urs Treier, urs.treier@bio.au.dk
# Last Edit --> 27-05-2021 


library(dplyr)
library(reshape2)
library(data.table)
library(readxl)
library(stringr)
library(openxlsx)

rm(list=ls())
##Set working directory

setwd("C:/R_Workspace/DFV2021")

## Set path containing Field sheets
Path21 = "./Feltskemaer/"

##Read Ellenberg-document

ArtsEllenberg = read.csv2(file = "Datasheets/Floraliste.csv")


setwd(Path21)
## creates a list of all .xlsx files in wd folder
file.list = list.files(pattern = '*.xlsx')

df.list = sapply(file.list,function(x) openxlsx::read.xlsx(x, sheet = 2), simplify = F)
## binds all dataframes together by row
df <- rbindlist(df.list, idcol = "id", fill = T)

df$`Lys.-.Ellenbeg.L` = str_replace_all(df$`Lys.-.Ellenbeg.L`, "-", "")
df$`Fugtighed.-.Ellenberg.F` = str_replace_all(df$`Fugtighed.-.Ellenberg.F`, "-", "")
df$`Allkalinitet.(reaktion/"pH").-.Ellenberg.R` = str_replace_all(df$`Allkalinitet.(reaktion/"pH").-.Ellenberg.R`, "-", "")
df$`Næringstof.-.Ellenberg.N` = str_replace_all(df$`Næringstof.-.Ellenberg.N`, "-", "")
df$`Salt.-.Ellenberg.S` = str_replace_all(df$`Salt.-.Ellenberg.S`, "-", "")
df$`Temperature.-.Ellenberg.T` = str_replace_all(df$`Temperature.-.Ellenberg.T`, "-", "")
df$`Kontinentalitet.-.Ellenberg.K` = str_replace_all(df$`Kontinentalitet.-.Ellenberg.K`, "-", "")

## Put in 0's instead of NA's 
#df[is.na(df)] <- 0

## subset dataframe
df2 = df[which(df$`Fund.(1=tilstede)`==1)]
unique(df2$DANSK.NAVN)

## Save Species file
#write.csv2(df2, file = "C:/R_workspace/DFV2021/Output/210527_DFV21_Species.csv")


DF.Scores <- df
## Because the Ellenberg values are now listed as characters we change those columns to numeric 
DF.Scores[,c(2,12:19,21:23)] <- lapply(DF.Scores[,c(2,12:19,21:23)], function(x) as.numeric(as.character(x)))
EllScores = DF.Scores


EllenScoreMeans = EllScores %>% filter(EllScores$`Fund.(1=tilstede)` != 0) %>% group_by(id) %>% summarise_at(vars(c(11:18,20:22)),mean, na.rm =TRUE)

#Count species
Speciescount = EllScores
Speciescount$SPcount = ifelse(rowSums(EllScores[,2]) != 0, 1,0)
SpeciesSum = Speciescount %>% filter(Speciescount$SPcount != 0) %>% group_by(id) %>% summarise_at(vars(1,SPcount),sum, na.rm =TRUE)

EllenbergScores = merge(EllenScoreMeans, SpeciesSum[,-2], by = "id", all.x=T)

#write.csv2(EllenbergScores, file = "C:/R_workspace/DFV2021/Output/210531_DFV21_EllenbergScores.csv")

###########################################################################################################
####### Environmental data sheet ############################################### ##########################
## Make a new dataframe
df.env <- data.frame(matrix(ncol = 32, nrow = 0))

## Specify the column names for the dataframe
df.names <- c("siteName","siteNo","date","plotNo","group","list/plot","NatType","lat","long","alt","aspect","slope",
  "pH","hum","temp", "clay","tex","lit", "vegh",
  "capyN","capyE","capyS","capyW",
  "tre.c","shr.c","dsh.c","for.c","gra.c","bry.c","lic.c","bgr.c","sto.c")

## Assign the column names to dataframe
colnames(df.env) <- df.names

l.C <- c("D","B","D","B","B","D","F","F","F","B","D","F","B","D","F","B","B","D","F","D","F","D","F","D","B","B","D","F","B","F","A","B","B","F")
l.C2 <- c("4","2","4","2","2","4","6","6","6","2","4","6","2","4","6","2","2","4","6","4","6","4","6","4","2","2","4","6","2","6","1","2","2","6")

l.R <- c(  5,  5,  6,  6,  7,  7,  7,  5,  6,  9,  9,  9, 10, 10, 10, 11, 12, 11, 12, 12, 11, 16, 14, 14, 14, 15, 15, 15, 16,  3, 18,  3,  2, 16)
C <- list(C=l.C, R=l.R)

setNames = c("siteName","NatType","date","group","long","pH","tex","lit","vegh","gra.c","bry.c","sto.c","notes","NA1","NA2",
             "siteNo","plotNo","lat","soil","clay","capyN","capyS","for.c",
             "lic.c","bgr.c","NA3","NA4","circle/list","aspect","slope","alt","temp","hum","capyW","capyE",
             "dsh.c","shr.c","tre.c", "NA5")

df.list.env = sapply(file.list,function(x) openxlsx::read.xlsx(x, sheet = "Miljø", rows = l.R, cols = l.C2, colNames = F, startRow = 2), simplify = F)

df.list.env2 = sapply(df.list.env,function(x) as.data.frame(unlist(x))[14:52,])

df.list = as.data.frame(df.list.env2)
rownames(df.list) <- setNames
df.environment = as.data.frame(t(df.list))
df.environment$id = rownames(df.environment)

#########################

df.all = merge(df2, df.environment, by = "id", all.x=T)

## Counts and summaries

Counts = df.all %>% group_by(LATINSK.NAVN) %>% summarise(seen_by_groups = n_distinct(group),
                                                                seen_in_plots = n_distinct(id))



df.speciesCounts = merge(df2, Counts, by = "LATINSK.NAVN" )


ObservedSpecies = distinct(df.speciesCounts, LATINSK.NAVN, .keep_all = TRUE)

ObservedSpecies2 = ObservedSpecies[,c(1,4:11,30,31)]

#write.csv2(ObservedSpecies2, file = "C:/R_workspace/DFV2021/Output/210531_DFV21_ObservedSpecies.csv")











# function that converts coordinates into decimal format
#_______________________________________________________________________________*____________________________ #
#
ConvertCoordinates<-function(c){
  x <- gsub("[,]",".",c)
  x <- as.character(sub("[NSEWnsew]","",x))
  if(!is.na(as.numeric(x))) x <- as.numeric(x) 
  if(is.numeric(x)){
    if(grepl("S",c)) {x <- as.numeric(paste0("-",x))}
    if(grepl("s",c)) {x <- as.numeric(paste0("-",x))}
    if(grepl("W",c)) {x <- as.numeric(paste0("-",x))}
    if(grepl("w",c)) {x <- as.numeric(paste0("-",x))}
    return(x)}
  else{
    x <- as.character(sub("[NSEWnsew]","",x))
    x <- sapply(strsplit(x, "[°']"), as.numeric)
    if(is.null(nrow(x))){
      warning(" Check for inconsistencies/errors in your coordinate data input! Data have not been converted.")
      return(c)}
    else{ 
      if(!is.null(nrow(x) >2) && length(nrow(x) >2) == 1 && !is.na(nrow(x) >2) && nrow(x) >2){
        x <- x[1, ] + x[2, ]/60 + x[3, ]/3600}
      if(!is.null(nrow(x)==2) && length(nrow(x)==2) == 1 && !is.na(nrow(x)==2) && nrow(x)==2){
        x <- x[1, ] + x[2, ]/60}
      if(grepl("S",c)) {x <- as.numeric(paste0("-",x))}
      if(grepl("s",c)) {x <- as.numeric(paste0("-",x))}
      if(grepl("W",c)) {x <- as.numeric(paste0("-",x))}
      if(grepl("w",c)) {x <- as.numeric(paste0("-",x))}
      return(x)}}}

#
# Start of quality control, variable computing and final editing of the dataframe                             #
#_______________________________________________________________________________*____________________________ #
#                                                                                                             #
# performing corrections and editing in the dataframe: step by step procedure for each variable
#____

#
#01
df.all$site<- as.numeric(as.character(df.all$siteNo))                           # 01: as.numeric
unique(df.all$site)
#02
df$date <- as.Date(as.character(df.all$date),"%d/%m/%y")                            # 02: as.date
unique(df.bu$date)
unique(df$date)
#03
df$plot<- as.numeric(as.character(df$plot))                                     # 03: as.numeric
unique(df.bu$plot)
unique(df$plot)
#04
df$plot<- as.numeric(as.character(df$group))                                    # 04: as.numeric
unique(df.bu$group)
unique(df$group)
#05
df$lat<- ConvertCoordinates(df.all$lat)                                             # 05: as.numeric
unique(df.bu$lat)
unique(df$lat)                      



















## List the variable cell names exactly as in the excel sheet
var.listDFV21 <- list(" Stednavn", " Stednummer", " Dato (xx.xx.xxxx)", " Plotnummer", " Gruppenummer ","5-m cirkel/liste" ,
                      " Naturtype"," GPS Y [N/S] (xx.xxxx)"," GPS X [E/W] (xx.xxxx)"," Højde (m a.s.l.)"," Retning (grader) "," Hældning (grader)",
                      " pH"," Fugtighed"," Temperatur"," %Ler"," Tekstur"," Førnelag (cm)"," Vegetationshøjde",
                      " Krone-åbninger N"," Krone-åbninger Ø"," Krone-åbninger S"," Krone-åbninger V",
                      " %Træer (Basalt areal)"," %Buske (<3m)"," %Dværg-buske"," %Urter & bregner"," %Graminoider",
                      " %Mosser"," %Laver"," %Bar jord"," %Sten (>20cm)",
                      )

var.listDFV21 <- list("Stednavn", "Stednummer", "Dato (xx.xx.xxxx)", "Plotnummer", "Gruppenummer ","5-m cirkel/liste" ,
                      "Naturtype","GPS Y [N/S] (xx.xxxx)","GPS X [E/W] (xx.xxxx)","Højde (m a.s.l.)","Retning (grader) ","Hældning (grader)",
                      "pH","Fugtighed","Temperatur","%Ler","Tekstur","Førnelag (cm)","Vegetationshøjde",
                      "Krone-åbninger N","Krone-åbninger Ø","Krone-åbninger S","Krone-åbninger V",
                      "%Træer (Basalt areal)","%Buske (<3m)","%Dværg-buske","%Urter & bregner","%Graminoider",
                      "%Mosser","%Laver","%Bar jord","%Sten (>20cm)")

var.list = var.listDFV21
## list the files to be read
file.list = list.files(pattern = '*.xlsx')

## Assign an object with the amount of excel files
no_of_excel_files <- length(file.list)

my

df.list2 = sapply(file.list,function(x) openxlsx::read.xlsx(x, sheet = 1), simplify = F)
df.list2[is.na(df.list2)] <- 0






## Loop to read in the excel files and looks for every variable and puts the values in the data frame

  for (i in 1:length(var.list)){
    
    #i=3
    IND <- which(df.list2[,] == var.list[i],arr.ind = TRUE)
    
    
    if(nrow(IND)==2){IND <- IND[1,]}
    
    var_name <-  df.list2[IND[1],IND[2]]
    
    if(IND[2]==1){
      value <- df.list2[IND[1],IND[2]+1]}else if(IND[2]==6){
        value <- df.list2[IND[1],IND[2]+3] 
      }
    
    
    df.env[s,i] <- value
  }
  
var.list


for (s in 1:no_of_excel_files){
  my_data <- read_excel(file.list[s], sheet = 1) #filesnames is list of your files
  
  
  my_table <- my_data[(which(my_data[,1] == "Species and cover")+1):nrow(my_data),] #finding the table
  colnames(my_table)<-my_table[1,]
  
  
  my_table <- my_table[-1,]#remove the first row
  my_vec<-list()
  
  #test <- as.vector(my_table[ii,2:11])
  
  for(ii in 1:11)
  {
    #ii<-1
    my_vec <-c(my_vec,(as.list(my_table[ii,2:11])))
  }
  
  my_mat<-data.frame(matrix(unlist(my_vec), ncol=110, byrow=T))
  my_mat = lapply(my_mat, function(x) as.character(x))
  my_mat <- as.vector(unlist(my_mat))       
  
  df.env[s,18:127]<-my_mat
  
}

#write.csv2(df.env, file = "C:/R_workspace/Mols/PlantSampling/201027_ExpRewild17_Env.csv")
