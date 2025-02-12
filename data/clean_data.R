library(tidyverse)
library(readxl)
library(writexl)
library(meta)
library(metafor)
library(netmeta)
library(metafor)


#Manually cleaned dataset from EPPI-reviewer (future iterations will allowed automated cleaning procedures)
data<-read_xlsx("data/LSR3_H_2024-01-22.xlsx")

#Tsukada et al 2023: cleaning of the QTc continuous data reported for multiple assessments within a day and the AUC was calculated; then divided by 24 hours)
data_cont<-read_xlsx("data/LSR3_H_qtc_2023-11-24.xlsx") #Manually cleaned data from EPPI reviewer, only QTc continues data to be used here

data_qtc<-data_cont %>% filter(grepl("QTc", Title)) %>% #Calculate TE and seTE for the difference between ulotaront placebo for each assessment of the QTc interval in Tsukada et al 203
  mutate(timepoint=as.double(gsub("hours","",Timepoint)), 
         outcome="QTc interval msec", comparison="TAAR1 agonist vs. placebo",
         sm="MD", TE=`Mean difference`, seTE=SE...21) %>% 
  select(outcome, comparison, timepoint, sm, TE, seTE) %>% mutate(lb=TE-1.96*seTE, ub=TE+1.96*seTE)

data_qtc<-data_qtc[order(data_qtc$timepoint),] #Order the QTc assessments by the time of assessmennt

#Function for calculating AUC
AUC <- function (conc, time) 
{
  auc <- 0
  for (i in 2:(length(time))) {
    auc <- auc + (time[i] - time[i - 1]) * (conc[i] + conc[i - 1])/2
  }
  auc
}

#Calculate the mean AUC of QTc interval, and the divide by 24 hours to have the assessment in msec 
qtc_auc_mean<-AUC(data_qtc$TE, data_qtc$timepoint)/24
#Calculate the lower boundary of the AUC of QTc interval, and the divide by 24 hours to have the assessment in msec 
qtc_aub_lb<-AUC(data_qtc$lb, data_qtc$timepoint)/24
#Calculate the upper boundary of the AUC of QTc interval, and the divide by 24 hours o have the assessment in msec  
qtc_aub_ub<-AUC(data_qtc$ub, data_qtc$timepoint)/24

qtc_aub_se<-(qtc_aub_ub-qtc_aub_lb)/3.92

#Add the above-calculated MD differennce for QTc interval between ulotaront and placebo in Tsukada et al 2023
data<-data %>% mutate(qtc_md_point=ifelse(study_name=="Tsukada (2023)", qtc_auc_mean, NA),
                      qtc_md_se=ifelse(study_name=="Tsukada (2023)",qtc_aub_se, NA)) %>% 
  mutate(study_name_drug=ifelse(study_name=="NCT04512066 (2020)", "NCT04512066 (2020) - ralmitaront",
                                paste0(study_name," - ulotaront")), #new study name variable to include also the name of the TAAR1 agonist used
         crossover_periods=ifelse(study_name=="Szabo (2023)", 3, #Variable to be used to divide the sample when calculating summary data
                                 ifelse(study_name=="Tsukada (2023)" | study_name=="Hopkins (2021)", 2, 1))) #Tsukada 3 arms but only 2 used here

#save the dataset
write_xlsx(data, "data/data_2024-01-22_LSR3_H.xlsx")

#data_unique_studies<-data %>% select(study_name, study_design, population, state, sample_n, age_mean) %>% unique() #only for descriptives

#Cleaning of the included studies (RCT or not, completed or ongoing)
studies<-(read_xlsx('data/included_studies_2024-01-22_LSR3_H.xlsx', na = "NA")) 
studies<-studies %>% mutate(rct=ifelse(grepl("RCT", Design), 1,0),
                            conditions=ifelse(grepl("schizophrenia", Population),"Schizophrenia",
                                              ifelse(grepl("Parkinson", Population),"Parkinson's disease psychosis",
                                                     ifelse(grepl("narcolepsy/cataplexy", Population) | 
                                                              grepl("MDD", Population) | 
                                                              grepl("GAD", Population), "Other mental health conditions", "Healthy volunteers"))),
                            state=ifelse(grepl("acute", Population),"acute",
                                         ifelse(grepl("negative", Population),"negative symptoms",
                                                ifelse(grepl("stable", Population),"stable", NA)))) %>%
  mutate(sample_n=as.integer(`Sample size`)) %>% filter(rct==1 | Name=="*NCT04038957")

studies<-studies[order(studies$Status, studies$Sponsor, studies$rct, studies$Population, studies$state),] #Ordering for a better presentation

write_xlsx(studies, 'data/studies_2024-01-22_LSR3_H.xlsx')


#Dataset with effect sizes for the different doses in order to plot them separately

master<-data

master_sch<-master %>% filter(population=="Schizophrenia spectrum" & timepoint=="3-13 weeks" &
                                study_design=="parallel-RCT" & drug_name!="risperidone") %>%
  select(study_name, drug_name, dose, overall_mean, overall_sd, overall_n)

master_sch_placebo<-master_sch %>% filter(drug_name=="placebo") %>%
  rename(placebo_mean=overall_mean, placebo_sd=overall_sd, placebo_n=overall_n) %>%
  select(study_name, placebo_mean, placebo_sd, placebo_n)

master_sch_drug<-master_sch %>% filter(drug_name!="placebo") %>%
  left_join(master_sch_placebo)

master_sch_arms<-master_sch_drug %>% select(study_name) %>% group_by(study_name) %>% summarise(dose_arms=n())

master_sch_drug<-master_sch_drug %>% left_join(master_sch_arms) %>% 
  mutate(placebo_n=as.integer(placebo_n/dose_arms))

master_sch_drug<-escalc(measure="SMD", data=master_sch_drug, m1i=overall_mean, m2i=placebo_mean,
                        sd1i=overall_sd, sd2i=placebo_sd, n1i=overall_n, n2i=placebo_n)
master_sch_drug<-master_sch_drug %>% mutate(dose=ifelse(dose=="50-75", 62.5, as.integer(dose))) %>%
  mutate(drug_name=ifelse(drug_name=="ulotaront", "1. ulotaront", "2. ralmitaront"),
         schedule=ifelse(study_name=="Koblan (2020)", "flexible", "fixed"))

write_xlsx(master_sch_drug, "data/data_doses_2024-01-22_LSR3_H.xlsx")
