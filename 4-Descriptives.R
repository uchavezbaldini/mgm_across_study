# -------------------------------------------------------------------------
# --------------- Sample Characteristics ----------------------------------
# -------------------------------------------------------------------------
#Load packages
library(tidyverse)
library(stringr)
library(summarytools)

descriptives <- mydata[c(1,4,13,15,25,43,45,48,51,54,57,117,1045)]
descriptives <- merge(finalAllid, descriptives, by = "subjectID")

#Recode variables: education (based off Verhage (1996)) and diagnosis
descriptives <- descriptives %>% mutate(demogr_vr7= recode(demogr_vr7,
                                                                   `7` = "Low",
                                                                   `8` = "Low",
                                                                   `9` = "Low",
                                                                   `3` = "Middle",
                                                                   `6` = "Middle",
                                                                   `1` = "High",
                                                                   `2` = "High",
                                                                   `4` = "High",
                                                                   `5` = "High",
                                                                   `10` = "Unknown"))

descriptives$medicatie_descr <- descriptives$Categorie_Medicatie1
descriptives <- descriptives %>% mutate(medicatie_descr= case_when(grepl("antidepressiva", Categorie_Medicatie1) | grepl("antidepressiva", Categorie_Medicatie2) | grepl("antidepressiva", Categorie_Medicatie3) ~1,
                                                                         grepl("antipsychotic", Categorie_Medicatie1) | grepl("antipsychotic", Categorie_Medicatie2) | grepl("antipsychotic", Categorie_Medicatie3) ~2,
                                                                         grepl("benzodiazepine", Categorie_Medicatie1) | grepl("benzodiazepine", Categorie_Medicatie2) | grepl("benzodiazepine", Categorie_Medicatie3) ~3,
                                                                         grepl("psychostimulans", Categorie_Medicatie1) | grepl("psychostimulans", Categorie_Medicatie2)|grepl("psychostimulans", Categorie_Medicatie3) ~4,
                                                                         grepl("stemmingsstabilisatoren", Categorie_Medicatie1) | grepl("stemmingsstabilisatoren", Categorie_Medicatie2) | grepl("stemmingsstabilisatoren", Categorie_Medicatie3) ~5,
                                                                         is.na(Categorie_Medicatie1)~0))

descriptives <- descriptives %>% 
  mutate(comorbid = case_when(Categorie_Diagnose_as12 != 0 ~ 1,
                              is.na(Categorie_Diagnose_as12)~ 0))

descriptives <- descriptives[c(1,13,9,10,14,15,11,16,26,17:22,13,25,23,2:8,24)] #reorder
library(xlsx)
write.xlsx(descriptives, file = "Existential factors data 11-5-2022.xlsx")

#age 
descr(descriptives$Leeftijd, stats = c("mean", "sd", "min", "max"), transpose = TRUE, round.digits=1)

descriptives %>% 
  group_by(psychosocial) %>% 
  summarize(mean = mean(Leeftijd),
            sd = sd(Leeftijd))
#gender
freq(descriptives$Geslacht1m2v, round.digits=1)

ctable(descriptives$psychosocial, descriptives$Geslacht1m2v)

#education
freq(descriptives$demogr_vr7, round.digits=1)
ctable(descriptives$psychosocial, descriptives$demogr_vr7)

#diagnosis
freq(descriptives$Categorie_Diagnose_as1)
ctable(descriptives$psychosocial, descriptives$Categorie_Diagnose_as1)

freq(descriptives$Categorie_Diagnose_as12)
ctable(descriptives$psychosocial, descriptives$Categorie_Diagnose_as12)

descriptives <- descriptives %>% 
  unite("diagnosis", DSM5:Diagnose_as1, na.rm = TRUE, remove = FALSE)

descriptives$diagnosis <- str_to_lower(descriptives$diagnosis)
descriptives$diagnosis <- str_replace_all(descriptives$diagnosis, "[[:punct:]]", " ")

#SZ= paranoide, Schizoaffective, NAO/ongedifferentieerde, waanstoornis, Schizofreniforme, UHR, Kortdurende
descriptives %>% filter(Categorie_Diagnose_as1==1)%>%
  sum(str_count(descriptives$diagnosis, "parano"))

sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "gedesorganis"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "katato"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "parano"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "schizoaff|schizo aff"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "NAO|ongediff|uhr"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "waan"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "schizofreniform"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "kortdurend"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==1,]$diagnosis, "schizofrenie"))

#Dep
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==2,]$diagnosis, "een"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==2,]$diagnosis, "reci|pers|chro"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==2,]$diagnosis, "dysthym"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==2,]$diagnosis, "cyclothyme"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==2,]$diagnosis, "NAO|ongespecificeerd"))

#OCD
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==4,]$diagnosis, "obs|ocd|obe|dwang"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==4,]$diagnosis, "trich"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==4,]$diagnosis, "body|BDD|morfo|lichaamsbeleving"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==4,]$diagnosis, "skin|exco"))
sum(str_count(descriptives[descriptives$Categorie_Diagnose_as1==4,]$diagnosis, "verzam"))

#medication
freq(descriptives$medicatie_descr)
ctable(descriptives$psychosocial, descriptives$medicatie_descr)

#symptom descriptives
symptoms_descr <- descriptives[c(11,2:8)]

labels_descr <- c("Diagnosis","Aut","Awr","Soc","Id","Anx","Dep","Psy")

colnames(symptoms_descr) <- labels_descr

(symps <- stby(data      = symptoms_descr, 
               INDICES   = symptoms_descr$Diagnosis, 
               FUN       = descr, 
               stats     = c("mean", "sd", "min", "max"), 
               transpose = TRUE, order = "p"))

print(symps,
      file = "descr_symptoms.html", 
      report.title = "Symptom descriptives")

descr(symptoms_descr$Aut, stats = c("mean", "sd", "min", "max"), transpose = TRUE, round.digits=2)
descr(symptoms_descr$Awr, stats = c("mean", "sd", "min", "max"), transpose = TRUE, round.digits=2)
descr(symptoms_descr$Id, stats = c("mean", "sd", "min", "max"), transpose = TRUE, round.digits=2)
descr(symptoms_descr$Soc, stats = c("mean", "sd", "min", "max"), transpose = TRUE, round.digits=2)
descr(symptoms_descr$Anx, stats = c("mean", "sd", "min", "max"), transpose = TRUE, round.digits=2)
descr(symptoms_descr$Dep, stats = c("mean", "sd", "min", "max"), transpose = TRUE, round.digits=2)
descr(symptoms_descr$Psy, stats = c("mean", "sd", "min", "max"), transpose = TRUE, round.digits=2)

###################
#Compare Diagnoses#
###################
library(stats)
library(FSA)

descriptives$Categorie_Diagnose_as1 <- ordered(descriptives$Categorie_Diagnose_as1,
                                               levels = c(1,2,3,4,5,6,7))
#age
summary(aov(Leeftijd~Categorie_Diagnose_as1, data= descriptives))

#gender
chisq.test(descriptives$Geslacht1m2v, descriptives$Categorie_Diagnose_as1)

#education
edu <- table(descriptives$demogr_vr7, descriptives$Categorie_Diagnose_as1)
prop.table(edu,2)

chisq.test(edu)

#comorbidity
table(descriptives$comorbid, descriptives$Categorie_Diagnose_as1)
chisq.test(descriptives$comorbid, descriptives$Categorie_Diagnose_as1)

#medication
m <- table(descriptives$medicatie_descr, descriptives$Categorie_Diagnose_as1)
prop.table(m,2)

chisq.test(m)

#symptoms
summary(aov(autonomy~Categorie_Diagnose_as1, data= descriptives))
TukeyHSD(aov(autonomy~Categorie_Diagnose_as1, data= descriptives))
summary(aov(disorder~Categorie_Diagnose_as1, data= descriptives))
summary(aov(identity~Categorie_Diagnose_as1, data= descriptives))
summary(aov(social~Categorie_Diagnose_as1, data= descriptives))
summary(aov(anxiety~Categorie_Diagnose_as1, data= descriptives))
summary(aov(depression~Categorie_Diagnose_as1, data= descriptives))
summary(aov(pq16_severity~Categorie_Diagnose_as1, data= descriptives))


kruskal.test(autonomy~Categorie_Diagnose_as1, data= descriptives)
kruskal.test(disorder~Categorie_Diagnose_as1, data= descriptives)
kruskal.test(identity~Categorie_Diagnose_as1, data= descriptives)
kruskal.test(social~Categorie_Diagnose_as1, data= descriptives)
kruskal.test(anxiety~Categorie_Diagnose_as1, data= descriptives)
kruskal.test(depression~Categorie_Diagnose_as1, data= descriptives)
kruskal.test(pq16_severity~Categorie_Diagnose_as1, data= descriptives)
dunnTest(pq16_severity~Categorie_Diagnose_as1, data= descriptives,
         method="bh")


#COVID-19
descriptives <- descriptives %>%
  mutate(covid = if_else(Datum_Onderzoek > "2020-03-01", '1', '0'))

