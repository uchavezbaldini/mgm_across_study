# -----------------------------------------------------------------------
# ------------------- Data Preparation ----------------------------------
# -----------------------------------------------------------------------

## ---------------------------------------------------------
## Load packages
## ---------------------------------------------------------
library(readr)
library(dplyr)
library(summarytools)

## ---------------------------------------------------------
## Prepare data
## ---------------------------------------------------------
#Load data
database <- read_delim("BBC database 4-5-2022_adjusted.csv", ";", escape_double = FALSE, 
                       col_types = cols(Datum_Onderzoek = col_date(format = "%m/%d/%Y")), trim_ws = TRUE)
names(database)

mydata <- database %>% filter(Session==1)

mydata <- mydata %>% filter(Categorie_Diagnose_as1 !=19) #5 dropped (n=1155)

mydata[mydata ==999] <- NA

mydata$anxiety <- (mydata$has_vr1 + mydata$has_vr2 + mydata$has_vr3 + mydata$has_vr5 + mydata$has_vr7)
mydata$depression <- (mydata$ids_vr1 + mydata$ids_vr2 + mydata$ids_vr3 + mydata$ids_vr4 + mydata$ids_vr5
                      + mydata$ids_vr11 + mydata$ids_vr12 + mydata$ids_vr13 + mydata$ids_vr14+ mydata$ids_vr15
                      + mydata$ids_vr16+ mydata$ids_vr18+ mydata$ids_vr19+ mydata$ids_vr20+ mydata$ids_vr21+ mydata$ids_vr24)

all <- mydata[c("subjectID","dimensie_vr3","dimensie_vr7","dimensie_vr8","dimensie_vr9","dimensie_vr16",
           "dimensie_vr17","ze_vr2", "ze_vr14","ze_vr20","anxiety","depression",
           "pq16_severity")]

save(all, file="all.Rdata")

#Check missing data
library(naniar)
mcar_test(all[c(2:13)])

all <- na.omit(all)

#Prepare data

#Reverse score
all[,c("dimensie_vr16")] <- 8-all[,c("dimensie_vr16")]
all[,c("ze_vr2")] <- 6-all[,c("ze_vr2")]
all[,c("ze_vr14")] <- 6-all[,c("ze_vr14")]
all[,c("ze_vr20")] <- all[,c("ze_vr20")]*-1

#Item reduction (WTO)
library(EGAnet)
library(wTO)

WTO <- all[c(2:13)]
redund <- node.redundant(WTO, method = "wTO", type = "adapt")

if(interactive()){combine <- node.redundant.combine(redund, type = "sum")}

reduced <- combine[["data"]]

save(redund, file="redund.Rdata")
save(combine, file="combine.Rdata")

#Aggregate variables 
##Autonomy: dimensie_vr3 + dimensie_vr8 + dimensie_vr9
##Disorder: dimensie_vr7 + dimensie_vr16
##Social: ze_vr2 + ze_vr14
## Identity: dimensie_vr17 + ze_vr20

all$autonomy <- (all$dimensie_vr3 + all$dimensie_vr8 + all$dimensie_vr9)/3
all$disorder <- (all$dimensie_vr7 + all$dimensie_vr16)/2
all$social <- (all$ze_vr2 + all$ze_vr14)/2
all$identity <- (all$dimensie_vr17 + all$ze_vr20)/2

all <- all[c(1, 14:17, 11:13)]

mydata$medicatie <- 0
mydata <- mydata %>% mutate(medicatie= case_when(grepl("antidepressiva", Categorie_Medicatie1) | grepl("antidepressiva", Categorie_Medicatie2) | grepl("antidepressiva", Categorie_Medicatie3) ~1,
                                                 grepl("antipsychotic", Categorie_Medicatie1) | grepl("antipsychotic", Categorie_Medicatie2) | grepl("antipsychotic", Categorie_Medicatie3) ~2,
                                                 grepl("benzodiazepine", Categorie_Medicatie1) | grepl("benzodiazepine", Categorie_Medicatie2) | grepl("benzodiazepine", Categorie_Medicatie3) ~3,
                                                 grepl("psychostimulans", Categorie_Medicatie1) | grepl("psychostimulans", Categorie_Medicatie2)|grepl("psychostimulans", Categorie_Medicatie3) ~4,
                                                 grepl("stemmingsstabilisatoren", Categorie_Medicatie1) | grepl("stemmingsstabilisatoren", Categorie_Medicatie2) | grepl("stemmingsstabilisatoren", Categorie_Medicatie3) ~5,
                                                 is.na(Categorie_Medicatie1)~0))

covariates <- mydata[c("subjectID","Leeftijd", "Geslacht1m2v", "Categorie_Diagnose_as1", "medicatie")]

covariates <- covariates %>% mutate(Categorie_Diagnose_as1= recode(Categorie_Diagnose_as1,
                                                                   `1` = 1,
                                                                   `2` = 2,
                                                                   `3` = 3,
                                                                   `4` = 1,
                                                                   `5` = 7,
                                                                   `6` = 7,
                                                                   `7` = 7,
                                                                   `8` = 4,
                                                                   `9` = 7,
                                                                   `10`= 7,
                                                                   `11`= 1,
                                                                   `12`= 7,
                                                                   `13`= 4,
                                                                   `14`= 7,
                                                                   `15`= 7,
                                                                   `16`= 5,
                                                                   `17`= 6,
                                                                   `18`= 7,
                                                                   `20`= 4))

covariates$medicatie[is.na(covariates$medicatie)] <- 0
covariates <- covariates %>% mutate(medicatie= recode(medicatie,
                                                      `1` = 1,
                                                      `2` = 1,
                                                      `3` = 1,
                                                      `4` = 1,
                                                      `5` = 1,
                                                      `0` = 0))

finalAllid <- merge(all, covariates, by = "subjectID")

save(finalAllid, file="finalAllid.Rdata")

#Drop subjectID
finalAll <- finalAllid[ -c(1)]

#Check distributions
hist(finalAll$Soc)
hist(finalAll$Id)
hist(finalAll$Aut)
hist(finalAll$Dis)
hist(finalAll$Anx) 
hist(finalAll$Dep)
hist(finalAll$Psy) 

boxplot.stats(finalAll$Soc)$out
boxplot.stats(finalAll$Id)$out
boxplot.stats(finalAll$Aut)$out
boxplot.stats(finalAll$Dis)$out
boxplot.stats(finalAll$Anx)$out 
boxplot.stats(finalAll$Dep)$out 
boxplot.stats(finalAll$Psy)$out #