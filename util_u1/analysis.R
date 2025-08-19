library(tidyverse)
library(readxl)
library(writexl)
library(meta)
library(netmeta)
library(robvis)


rm(list=ls())

master<-read_xlsx("data_u1/data_2025-08-15_LSR3_H.xlsx") #Main dataset

rob<-read_xlsx("data_u1/rob_secondary_2025.08.15.xlsx") #RoB assessments for all outcomes

outcome_names<-read_xlsx("data_u1/outcome_names.xlsx") #Refined outcome names usesd later for refining the SoE table
outcome_names<-outcome_names %>% mutate(id_outcome_population=paste0(outcome, population)) 


continuous_outcomes_smd<-c("overall", "positive", "negative", "functioning", "cognition", "depression")
continuous_outcomes_md<-c("weight","prolactin") #qtc interval not used here as data only from a single study that were caclulated using AUC
dichotomous_outcomes_common<-c("dropout_any", 'dropout_ae', "adverse_event", "response", "relapse",
                               "anticholinergic_symptom", "anxiety","agitation", "headache", "hypotension", "dizziness",
                               "nausea_vomitting", "sedation", "hyperprolactinemia", "qtc_prolongation",
                               "insomnia", "akathisia", "eps_symptoms", "weight_increased", "psychosis")
dichotomous_outcomes_rare<-c("death", "serious")

effic_outcomes<-c("overall", "positive", "negative", "functioning", "cognition", "depression", "response", "relapse")
primary_outcome<-"overall"

meta_outcome<-data.frame(outcome=NA,  comparison=NA, timepoint=NA, k=NA, n=NA,population=NA, duration=NA,
                         sm=NA,  low_bias=NA, moderate_bias=NA, high_bias=NA, k_schiz=NA, n_schiz=NA,
                         TE.placebo.random=NA, seTE.placebo.random=NA, 
                         TE.placebo.fixed=NA, seTE.placebo.fixed=NA,
                         cer_point_0=NA,
                         tau2=NA,  i2=NA,
                         TE.random=NA, seTE.random=NA, TE.fixed=NA, seTE.fixed=NA)

o<-"nausea_vomitting"
time<-"1 day-2 weeks"
comparison="taar1_vs_antipsychotic"

for(o in c(continuous_outcomes_smd, continuous_outcomes_md, dichotomous_outcomes_common, dichotomous_outcomes_rare)){
  for(time in unique(master$timepoint)){
      
      for(comparison in c("taar1_vs_placebo", "taar1_vs_antipsychotic", "taar1_vs_placebo_addon")){
        
        master_i<-master
        
        if(o %in% continuous_outcomes_md){
          master_i$mean<-master[[paste0(o,"_mean")]]
          master_i$sd<-master[[paste0(o,"_sd")]]
          master_i$n<-master[[paste0(o,"_n")]]
          prediction_true=TRUE
          random_true=TRUE
          fixed_true=TRUE #present both fixed and random effects due to small number of studies
          approach="Inverse"
          sm_used="MD"
          outcome_type="continuous"
          minust_trasformation=1
        } else if (o %in%  continuous_outcomes_smd){
          
          master_i$mean<-master[[paste0(o,"_mean")]]*(-1) #minus transformation as all of these are symptoms coded wiht negative for improvement
          master_i$sd<-master[[paste0(o,"_sd")]]
          master_i$n<-master[[paste0(o,"_n")]]
          sm_used="SMD"
          prediction_true=TRUE
          random_true=TRUE
          fixed_true=TRUE #present both fixed and random effects due to small number of studies
          approach="Inverse"
          outcome_type="continuous"
          
        } else if (o %in% dichotomous_outcomes_common){
          master_i$mean<-master[[paste0(o,"_e")]] #used as n with event 
          master_i$sd<-NA
          master_i$n<-master$randomized_n #Use the randomized n in the dichotomous outcomes, asssuming missing patients did not have the outcome (post-hoc from protcol regrding safety outcomes but this we usullay do)
          random_true=TRUE
          fixed_true=TRUE #present both fixed and random effects due to small number of studies
          prediction_true=TRUE
          approach="Inverse"
          sm_used="OR"
          outcome_type="dichotomous"
        } else if(o %in% dichotomous_outcomes_rare){
          master_i$mean<-master[[paste0(o,"_e")]] #used as n with event 
          master_i$sd<-NA
          master_i$n<-master$randomized_n
          random_true=FALSE #Do not present random-effects in outcomes deemed as rare
          prediction_true=FALSE
          fixed_true=TRUE
          approach="MH"
          sm_used="OR"
          outcome_type="dichotomous"
        }
        
        
        if(o %in% effic_outcomes){
          left_label="TAAR1 worse"
          right_label="TAAR1 better"
          prediction_subgroup_true=TRUE
          subgroup_hetstat_true= TRUE
          overall_hetstat_true = FALSE
          test_subgroup_true = FALSE
          overall_true = FALSE
          subgroup_true=TRUE  #present subgroups in all but without testing
        } else {
          left_label="TAAR1 better"
          right_label="TAAR1 worse"
          prediction_subgroup_true=FALSE
          subgroup_hetstat_true= FALSE
          overall_hetstat_true = TRUE
          test_subgroup_true= FALSE 
          overall_true = TRUE
          subgroup_true=TRUE #present subgroups in all but without testing
        }
        
        
        
        if(comparison=="taar1_vs_placebo"){
          master_i<-master_i %>% 
            filter(timepoint==time) %>%  #keep the timepoint
            filter(!is.na(mean)) %>% #keep only arms with data
            select(study_name, study_name_drug, crossover_periods, population, duration_weeks, drug_new, mean, sd, n) %>%  #select only the relevant data for the analysis 
            filter(drug_new=="TAAR1 agonist" | drug_new=="placebo")  #keep the arms relevant for the comparison 
        } else if(comparison=="taar1_vs_antipsychotic"){
          master_i<-master_i %>%  
             filter(timepoint==time) %>%  #keep the timepoint
              filter(!is.na(mean)) %>% #keep only arms with data
             dplyr::select(study_name, study_name_drug, crossover_periods, population, duration_weeks, drug_new, mean, sd, n) %>% 
              filter(drug_new=="TAAR1 agonist" | drug_new=="D2 antipsychotic") 
        } else if(comparison=="taar1_vs_placebo_addon"){
          master_i<-master_i %>% 
            filter(timepoint==time) %>%  #keep the timepoint
            filter(!is.na(mean)) %>% #keep only arms with data
            select(study_name, study_name_drug, crossover_periods, population, duration_weeks, drug_new, mean, sd, n) %>%  #select only the relevant data for the analysis 
            filter(drug_new=="TAAR1 agonist (add-on)" | drug_new=="placebo (add-on)")  %>% 
            mutate(drug_new=ifelse(drug_new=="TAAR1 agonist (add-on)", "TAAR1 agonist", #renormalize to allow the same classificaiton below
                                   ifelse(drug_new=="placebo (add-on)", "placebo", drug_new)))
        }

        if(o %in% c(continuous_outcomes_md, continuous_outcomes_smd)){
          
          master_pooled_i<-master_i %>%  #pooling the different doses or drugs of the same category (not relevant here)
            group_by(study_name, study_name_drug, drug_new, population, duration_weeks)%>%
            summarise(n_an=sum(n),
                      mean_an=weighted.mean(mean,n), 
                      sd_an=sqrt(((sum((n - 1) * sd^2 + n * mean^2)) - sum(n) * mean_an^2)/(sum(n) -  1)))
          
          master_pooled_i_studies<-master_pooled_i %>% 
            select(study_name) %>% group_by(study_name) %>% summarise(count=n()) %>% filter(count!=1) #find studies with only data for one arm 
          
          master_pooled_i<-master_pooled_i %>% filter(study_name %in% master_pooled_i_studies$study_name) 
          
          if(nrow(master_pooled_i)>1){
          pairwise_i<-pairwise(data=master_pooled_i, studlab = study_name_drug, treat=drug_new,
                               mean=mean_an, sd=sd_an, n=n_an, sm=sm_used)
          
          pairwise_i<-as.data.frame(pairwise_i) %>% 
            filter(treat1=="TAAR1 agonist" | treat2=="TAAR1 agonist") %>%
            mutate(treat1_new=ifelse(treat1=="TAAR1 agonist" , treat1, treat2),
                   treat2_new=ifelse(treat1=="TAAR1 agonist" , treat2, treat1),
                   mean1_new=ifelse(treat1=="TAAR1 agonist" , mean1, mean2),
                   mean2_new=ifelse(treat1=="TAAR1 agonist" , mean2, mean1),
                   sd1_new=ifelse(treat1=="TAAR1 agonist" , sd1, sd2),
                   sd2_new=ifelse(treat1=="TAAR1 agonist" , sd2, sd1),
                   n1_new=ifelse(treat1=="TAAR1 agonist" ,n1, n2),
                   n2_new=ifelse(treat1=="TAAR1 agonist" , n2, n1)) %>%
            mutate(comp=paste0(treat1_new," vs. ", treat2_new)) %>%
            select(study_name, study_name_drug, duration_weeks,
                   comp, population, treat1_new, treat2_new, mean1_new, sd1_new, n1_new, mean2_new,  sd2_new, n2_new) %>% 
            mutate(n_total=n1_new + n2_new) %>%
            unique()
          
          
          rob_i<-rob %>% filter(outcome==o & timepoint==time)
          
          pairwise_i<- pairwise_i %>% left_join(rob_i) %>% 
              unique()
          
          meta_comp<-metacont(data=pairwise_i,  #method.random.ci = "HK",  adhoc.hakn.ci="se", #reml default for tau, Do not use HK correction due to <5 studies in most meta-analyses
                             mean.e = mean1_new, sd.e=sd1_new, n.e=n1_new, mean.c = mean2_new, sd.c=sd2_new, n.c=n2_new,
                             sm=sm_used, random=random_true, fixed=fixed_true,    
                             studlab = study_name_drug, prediction = prediction_true, subgroup = population, 
                             prediction.subgroup = prediction_subgroup_true)
          
          new_master_i_name<- paste0("master_i_", comparison,"_", time,"_",o)  
          new_pairwise_i_name <- paste0("pairwise_i_", comparison,"_", time,"_",o)  
          new_meta_comp_name <- paste0("meta_comp_", comparison,"_",time,"_", o)  
          assign( new_master_i_name, master_i)
          assign( new_pairwise_i_name, pairwise_i)
          assign(new_meta_comp_name, meta_comp)
          
          }

        } else if(o %in% c(dichotomous_outcomes_common, dichotomous_outcomes_rare)){
          
          master_pooled_i<-master_i %>%  #pooling the different doses or drugs of the same category (not relevant here)
            group_by(study_name, study_name_drug, crossover_periods, drug_new, population, duration_weeks)%>%
            summarise(n_an=sum(n),
                      e_an=sum(as.numeric(mean))) %>% 
            unique()
          
          master_pooled_i_studies<-master_pooled_i %>% 
            select(study_name_drug) %>% group_by(study_name_drug) %>% summarise(count=n()) %>% filter(count!=1) #find studies with only data for one arm 
          
          master_pooled_i<-master_pooled_i %>% filter(study_name_drug %in% master_pooled_i_studies$study_name_drug) 
        
            if(nrow(master_pooled_i)>1){
         pairwise_i<-pairwise(data=master_pooled_i, studlab = study_name, treat=drug_new,
                                  event=e_an, n=n_an, sm=sm_used)
         
         pairwise_i<-as.data.frame(pairwise_i) %>% 
            filter(treat1=="TAAR1 agonist" | treat2=="TAAR1 agonist") %>%
            mutate(treat1_new=ifelse(treat1=="TAAR1 agonist" , treat1, treat2),
                   treat2_new=ifelse(treat1=="TAAR1 agonist" , treat2, treat1),
                   event1_new=ifelse(treat1=="TAAR1 agonist" , event1, event2),
                   event2_new=ifelse(treat1=="TAAR1 agonist" , event2, event1),
                   n1_new=ifelse(treat1=="TAAR1 agonist" ,n1, n2),
                   n2_new=ifelse(treat1=="TAAR1 agonist" , n2, n1),
                   TE=ifelse(treat1=="TAAR1 agonist" , TE, -TE)) %>%#change the direction of TE to normalize the effects 
            mutate(comp=paste0(treat1_new," vs. ", treat2_new)) %>%
            select(study_name, study_name_drug, comp, duration_weeks,
                   population, crossover_periods, treat1_new, treat2_new, event1_new, n1_new, event2_new, n2_new, TE, seTE) %>%
           mutate(event1_new_corrected=ifelse((event1_new==0) | (event2_new==0) | (n1_new-event1_new==0) | (n2_new-event2_new==0), event1_new+0.5, event1_new), #To correct for crosssover, apply continuity correction by adding 0.5 to all event or non-event in case of 0.
                  event2_new_corrected=ifelse((event1_new==0) | (event2_new==0) | (n1_new-event1_new==0) | (n2_new-event2_new==0), event2_new+0.5, event2_new),
                  subtr_event1_new_corrected=ifelse((event1_new==0) | (event2_new==0) | (n1_new-event1_new==0) | (n2_new-event2_new==0), n1_new- event1_new+0.5, n1_new- event1_new),
                  subtr_event2_new_corrected=ifelse((event1_new==0) | (event2_new==0) | (n1_new-event1_new==0) | (n2_new-event2_new==0), n2_new- event2_new+0.5, n2_new- event2_new)) %>% 
           mutate(n_total=(n1_new+n2_new)/crossover_periods,
                  n_total_corrected=ifelse((event1_new==0) | (event2_new==0) | (n1_new-event1_new==0) | (n2_new-event2_new==0), 2+n_total, n_total)) %>%
           mutate(varTE=ifelse(crossover_periods==1, seTE^2,       #correct for crossover studies using correlation of 0.2 and the formula in Elbourne, n = n total of the whole crossover (not by adding twice the same population)
                               seTE^2-0.4*(n_total_corrected)/(sqrt(event1_new_corrected*subtr_event1_new_corrected*event2_new_corrected* subtr_event2_new_corrected)))) %>% 
           unique()
          
         rob_i<-rob %>% filter(outcome==o & timepoint==time)
         pairwise_i<- pairwise_i %>% left_join(rob_i) %>% 
           unique()
         
         if(length(unique(pairwise_i$crossover_periods))==1){
         meta_comp<-metabin(data=pairwise_i, 
                            event.e = event1_new, n.e=n1_new, event.c = event2_new, n.c=n2_new,
                            sm=sm_used, random=random_true, fixed=fixed_true, #Default method MH.exact for fixed effects, IV-REML for random-effects
                            studlab = study_name_drug, prediction = prediction_true, subgroup=population)
         } else{
           meta_comp<-metagen(data=pairwise_i, #Present forest plots using crossover corrections
                              TE = TE, seTE=sqrt(varTE), 
                              sm=sm_used, random=random_true, fixed=fixed_true,  
                              studlab = study_name_drug, prediction = prediction_true, subgroup=population)
            }
         
         
         meta_comp_plac<-metaprop(data=pairwise_i, event=event2_new, n=n2_new,  method="GLMM", #GLMM was used, and logit transformation the default 
                                  random=random_true, fixed=fixed_true, 
                                  studlab = study_name_drug, prediction = prediction_true, subgroup=population)
         
         meta_comp_plac_0<-metaprop(data=pairwise_i, event=event2_new, n=n2_new,  method="Inverse", #Inverse variance with continuity correction to be used when <0.1% in the previous analysis
                                  random=random_true, fixed=fixed_true, 
                                  studlab = study_name_drug, prediction = prediction_true, subgroup=population)
         
         
         new_master_i_name<- paste0("master_i_", comparison,"_", time,"_",o)  
        new_pairwise_i_name <- paste0("pairwise_i_", comparison,"_", time,"_",o)  
         new_meta_comp_name <- paste0("meta_comp_", comparison,"_",time,"_", o)  
         assign( new_master_i_name, master_i)
         assign(new_pairwise_i_name, pairwise_i)
         assign(new_meta_comp_name, meta_comp)
         
           }
        }
        
    if(nrow(master_pooled_i)>1){        
      if((o %in% effic_outcomes) & length(meta_comp$subgroup.levels)>1){
        
        if(o %in% c(continuous_outcomes_md, continuous_outcomes_smd)){
            
          
            for(p in 1:length(meta_comp$subgroup.levels)){
              
              meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time,
                                         k_schiz=as.integer(nrow(pairwise_i[pairwise_i$population=="Schizophrenia spectrum (acute episode)" |
                                                                              pairwise_i$population=="Schizophrenia spectrum (clinically stable)" |
                                                                              pairwise_i$population=="Schizophrenia spectrum (negative symptoms)" |
                                                                              pairwise_i$population=="Schizophrenia spectrum (clinically stable, MetS)",])),
                                         n_schiz=as.integer(sum(pairwise_i[pairwise_i$population=="Schizophrenia spectrum (acute episode)" |
                                                                             pairwise_i$population=="Schizophrenia spectrum (clinically stable)" |
                                                                             pairwise_i$population=="Schizophrenia spectrum (negative symptoms)" |
                                                                             pairwise_i$population=="Schizophrenia spectrum (clinically stable, MetS)",]$n_total)),
                                         k=as.integer(meta_comp$k.all.w[p]), 
                                         n=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$n_total),
                                         population=meta_comp$subgroup.levels[p],
                                         duration=paste0(min(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$duration_weeks),"-", 
                                                         max(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$duration_weeks)),
                                         low_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="Low", na.rm=TRUE), 
                                         moderate_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="Some concerns", na.rm=TRUE), 
                                         high_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="High", na.rm=TRUE),
                                         sm=sm_used,
                                         TE.placebo.random=NA, seTE.placebo.random=NA, 
                                         TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                                         cer_point_0=NA, #estimate of frequency in the control group in case of <0.1% events, in order to esimtate the ACR
                                         TE.random=as.double(meta_comp$TE.random.w[p]), seTE.random=as.double(meta_comp$seTE.random.w[p]),
                                         tau2=as.double(meta_comp$tau2.w[p]), 
                                         i2=as.double(meta_comp$I2.w[p]),
                                         TE.fixed=as.double(meta_comp$TE.fixed.w[p]), seTE.fixed=as.double(meta_comp$seTE.fixed.w[p]))
              meta_outcome<-rbind(meta_outcome, meta_outcome_i)
            }
        } else {
          
          
          for(p in 1:length(meta_comp$subgroup.levels)){
            
            
            meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time,
                                       k_schiz=as.integer(nrow(pairwise_i[pairwise_i$population=="Schizophrenia spectrum (acute episode)" |
                                                                            pairwise_i$population=="Schizophrenia spectrum (clinically stable)" |
                                                                            pairwise_i$population=="Schizophrenia spectrum (negative symptoms)" |
                                                                            pairwise_i$population=="Schizophrenia spectrum (clinically stable, MetS)",])),
                                       n_schiz=as.integer(sum(pairwise_i[pairwise_i$population=="Schizophrenia spectrum (acute episode)" |
                                                                           pairwise_i$population=="Schizophrenia spectrum (clinically stable)" |
                                                                           pairwise_i$population=="Schizophrenia spectrum (negative symptoms)" |
                                                                           pairwise_i$population=="Schizophrenia spectrum (clinically stable, MetS)",]$n_total)),
                                       k=as.integer(meta_comp$k.all.w[p]), 
                                       n=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$n_total),
                                       population=meta_comp$subgroup.levels[p],
                                       duration=paste0(min(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$duration_weeks),"-", 
                                                       max(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$duration_weeks)),
                                       sm=sm_used,
                                       low_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="Low", na.rm=TRUE), 
                                       moderate_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="Some concerns", na.rm=TRUE), 
                                       high_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="High", na.rm=TRUE),
                                       TE.placebo.random=meta_comp_plac$TE.random.w[p], seTE.placebo.random=meta_comp_plac$seTE.random.w[p], 
                                       TE.placebo.fixed=meta_comp_plac$TE.fixed.w[p], seTE.placebo.fixed=meta_comp_plac$seTE.fixed.w[p], 
                                       TE.random=as.double(meta_comp$TE.random.w[p]), seTE.random=as.double(meta_comp$seTE.random.w[p]),
                                       cer_point_0=exp(meta_comp_plac_0$TE.random)/(1+exp(meta_comp_plac_0$TE.random)), #estimate of frequency in the control group in case of <0.1% events, in order to esimtate the ACR
                                       tau2=as.double(meta_comp$tau2.w[p]), 
                                       i2=as.double(meta_comp$I2.w[p]),
                                       TE.fixed=as.double(meta_comp$TE.fixed.w[p]), seTE.fixed=as.double(meta_comp$seTE.fixed.w[p]))
            meta_outcome<-rbind(meta_outcome, meta_outcome_i)
          }
          
        }} else {
          
        if(o %in% c(continuous_outcomes_md, continuous_outcomes_smd)){
           meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time, 
                                      k_schiz=as.integer(nrow(pairwise_i[pairwise_i$population=="Schizophrenia spectrum (acute episode)" |
                                                                           pairwise_i$population=="Schizophrenia spectrum (clinically stable)" |
                                                                           pairwise_i$population=="Schizophrenia spectrum (negative symptoms)" |
                                                                           pairwise_i$population=="Schizophrenia spectrum (clinically stable, MetS)",])),
                                      n_schiz=as.integer(sum(pairwise_i[pairwise_i$population=="Schizophrenia spectrum (acute episode)" |
                                                                          pairwise_i$population=="Schizophrenia spectrum (clinically stable)" |
                                                                          pairwise_i$population=="Schizophrenia spectrum (negative symptoms)" |
                                                                          pairwise_i$population=="Schizophrenia spectrum (clinically stable, MetS)",]$n_total)),
                                     k=meta_comp$k.all, 
                                     n=sum(pairwise_i$n_total),
                                     population=paste0(meta_comp$subgroup.levels),
                                     sm=sm_used, 
                                     low_bias=sum(pairwise_i$Overall=="Low", na.rm=TRUE), 
                                     moderate_bias=sum(pairwise_i$Overall=="Some concerns", na.rm=TRUE), 
                                     high_bias=sum(pairwise_i$Overall=="High", na.rm=TRUE),
                                     duration=paste0(min(pairwise_i$duration_weeks),"-", max(pairwise_i$duration_weeks)),
                                     TE.placebo.random=NA, seTE.placebo.random=NA, 
                                     TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                                     cer_point_0=NA, #estimate of frequency in the control group in case of 0% events, in order to esimtate the ACR
                                     TE.random=meta_comp$TE.random, seTE.random=meta_comp$seTE.random, 
                                     tau2=meta_comp$tau2, i2=meta_comp$I2,
                                     TE.fixed=meta_comp$TE.fixed, seTE.fixed=meta_comp$seTE.fixed)
           meta_outcome<-rbind(meta_outcome, meta_outcome_i)
          }
           else{
          meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time, 
                                     k_schiz=as.integer(nrow(pairwise_i[pairwise_i$population=="Schizophrenia spectrum (acute episode)" |
                                                                          pairwise_i$population=="Schizophrenia spectrum (clinically stable)" |
                                                                          pairwise_i$population=="Schizophrenia spectrum (negative symptoms)" |
                                                                          pairwise_i$population=="Schizophrenia spectrum (clinically stable, MetS)",])),
                                     n_schiz=as.integer(sum(pairwise_i[pairwise_i$population=="Schizophrenia spectrum (acute episode)" |
                                                                         pairwise_i$population=="Schizophrenia spectrum (clinically stable)" |
                                                                         pairwise_i$population=="Schizophrenia spectrum (negative symptoms)" |
                                                                         pairwise_i$population=="Schizophrenia spectrum (clinically stable, MetS)",]$n_total)),
                                     k=meta_comp$k.all,
                                     n=sum(pairwise_i$n_total), 
                                     low_bias=sum(pairwise_i$Overall=="Low", na.rm=TRUE), 
                                     moderate_bias=sum(pairwise_i$Overall=="Some concerns", na.rm=TRUE), 
                                     high_bias=sum(pairwise_i$Overall=="High", na.rm=TRUE),
                                     population=paste0(meta_comp$subgroup.levels, collapse=", "),
                                     duration=paste0(min(pairwise_i$duration_weeks),"-", max(pairwise_i$duration_weeks)),
                                     sm=sm_used, 
                                     TE.placebo.random=meta_comp_plac$TE.random, seTE.placebo.random=meta_comp_plac$seTE.random, 
                                     TE.placebo.fixed=meta_comp_plac$TE.fixed, seTE.placebo.fixed=meta_comp_plac$TE.fixed, 
                                     cer_point_0=exp(meta_comp_plac_0$TE.random)/(1+exp(meta_comp_plac_0$TE.random)), #estimate of frequency in the control group in case of <0.1% events, in order to esimtate the ACR
                                     TE.random=meta_comp$TE.random, seTE.random=meta_comp$seTE.random,
                                     tau2=meta_comp$tau2, i2=meta_comp$I2,
                                     TE.fixed=meta_comp$TE.fixed, seTE.fixed=meta_comp$seTE.fixed)
          meta_outcome<-rbind(meta_outcome, meta_outcome_i)
        }
        
        }
        }
        }
        
  }
}
   
      
#Conducting analysis for QTc interval msec using the data from Tsukada et al 2023.
master_qtc<-master[master$study_name=="Tsukada (2023)" & master$drug_name=="ulotaront",]
master_qtc<-master_qtc %>% select(study_name, study_name_drug, qtc_md_point, qtc_md_se, qtc_prolongation_n, timepoint, duration_weeks, population) %>%
  mutate(comparison="taar1_vs_placebo")

rob_qtc<-rob %>% filter(outcome=="qtc_interval" & timepoint=="1 day-2 weeks")

master_qtc<-master_qtc %>% left_join(rob_qtc)

`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`<- metagen(data=master_qtc,
               TE=qtc_md_point, seTE=qtc_md_se, sm="MD", studlab = study_name_drug)


meta_outcome_qtc<-data.frame(outcome="qtc_interval", comparison="taar1_vs_placebo",timepoint=unique(master_qtc$timepoint), 
                           k=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$k.all,
                           n=sum(master_qtc$qtc_prolongation_n),
                           k_schiz=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$k.all,
                           n_schiz=sum(master_qtc$qtc_prolongation_n),
                           population=master_qtc$population,
                           low_bias=sum(master_qtc$Overall=="Low", na.rm=TRUE), 
                           moderate_bias=sum(master_qtc$Overall=="Some concerns", na.rm=TRUE), 
                           high_bias=sum(master_qtc$Overall=="High", na.rm=TRUE),
                           duration=paste0(min(master_qtc$duration_weeks),"-", max(master_qtc$duration_weeks)),
                           sm="MD", 
                           TE.placebo.random=NA, seTE.placebo.random=NA, 
                           cer_point_0=NA, #estimate of frequency in the control group in case of <0.1% events with GLMM, in order to esimtate the ACR
                           TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                           TE.random=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$TE.random, seTE.random=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$seTE.random,
                           tau2=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$tau2, 
                            i2=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$I2,
                           TE.fixed=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$TE.fixed, seTE.fixed=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$seTE.fixed)

meta_outcome<-rbind(meta_outcome, meta_outcome_qtc) 

#Prepartion of the meta outcome object
meta_outcome<-meta_outcome %>% filter(!is.na(outcome))

results_matrix<-read_xlsx("data_u1/reporting_bias_table_2025.08.15.xlsx") #Calculate the number of possible n and k for efficacy and other outcomes

results_matrix_summary<-results_matrix %>% 
  mutate(timepoint=case_when(
    Timepoint=="1 day to 2 weeks" ~ "1 day-2 weeks",
    Timepoint=="3-13 weeks" ~ "3-13 weeks",
    Timepoint=="> 13 weeks" ~ ">13 weeks"
  ),
  comparison=case_when(
    Comparison=="TAAR1 agonists vs. placebo" ~"taar1_vs_placebo",
    Comparison=="TAAR1 agonists vs. placebo (addon)" ~"taar1_vs_placebo_addon",
    Comparison=="TAAR1 agonists vs. antipsychotics" ~"taar1_vs_antipsychotic"
  ),
  population=case_when(
Population=="Schizophrenia (negative symptoms)" ~ "Schizophrenia spectrum (negative symptoms)",
Population=="Schizophrenia (acute)"~ "Schizophrenia spectrum (acute episode)",
Population=="Parkinson's disease psychosis" ~ "Parkinson Disease Psychosis",
Population=="Schizophrenia (clinically stable)" | Population =="Schizophrenia (clinically stable, MetS)" ~ "Schizophrenia spectrum (clinically stable)",
Population =="Narcolepsy-cataplexy" | Population =="Volunteers" ~ "Other"
  )) %>% 
 # filter(`Age group`!="Adolescents") %>% #Exclude the age group of adolescents, as unclear if this part was conducted and no other study on this age group 
  rename(sample_size=`Sample size`, study_name=`Study name`, age_group=`Age group`) %>%
  select(comparison, timepoint, population, age_group, study_name, sample_size) 

results_matrix_summary_efficacy<-results_matrix_summary %>%
  filter(age_group!="Adolescents") %>% #Exclude the age group from the efficacy as no data were reported, and it would have been analyzed separately
  select(comparison, timepoint, population, sample_size) %>%
  group_by(comparison, timepoint, population) %>%
  summarise(k_possible=n(),
            n_possible=sum(sample_size)) %>%
  filter(population!="Other") %>%
  mutate(outcome_type="efficacy", 
         population_outcome=paste0(population,"_", outcome_type)) %>%
  select(comparison, timepoint, population_outcome, k_possible, n_possible) %>%
  ungroup()

results_matrix_summary_other<-results_matrix_summary %>%
  select(comparison, timepoint, sample_size) %>%
  group_by(comparison, timepoint) %>%
  summarise(k_possible=n(),
            n_possible=sum(sample_size)) %>%
  mutate(outcome_type="other",
         population="Any",
         population_outcome=paste0(population, "_", outcome_type)) %>%
  select(comparison, timepoint, population_outcome, k_possible, n_possible) %>%
  ungroup()

results_matrix_soe<-rbind(results_matrix_summary_efficacy, results_matrix_summary_other)

  


meta_outcome_soe<-meta_outcome %>% 
  mutate(duration=ifelse(duration=="0.14-0.14", "1 day",
                         ifelse(duration=="0.14-2", "1 day to 2 weeks",
                                ifelse(duration=="4-4", "4 weeks", 
                                       ifelse(duration=="4-6", "4-6 weeks",
                                              ifelse(duration=="6-6", "6 weeks", 
                                                     ifelse(duration=="4-12", "4-12 weeks", 
                                                            ifelse(duration=="12-12", "12 weeks", 
                                                                   ifelse(duration=="52-52", "52 weeks", 
                                                                          ifelse(duration=="6-12", "6-12 weeks", NA))))))))))%>% 
  mutate(outcome_type=ifelse(outcome %in% effic_outcomes, "efficacy", "other"), 
         population_outcome=ifelse(outcome_type=="efficacy", paste0(population,"_", outcome_type), "Any_other")) %>% 
  left_join(results_matrix_soe) %>%
  mutate(k_prop=k/k_possible, 
         n_prop=ifelse(k_prop==1, 1, n/n_possible),
         k_schiz_prop=k_schiz/k, n_schiz_prop=n_schiz/n,
         moderate_to_high_bias_prop=(moderate_bias+high_bias)/k) %>%
  mutate(TE_lb.random=TE.random-1.96*seTE.random, 
         TE_ub.random=TE.random+1.96*seTE.random,
         TE_lb.fixed=TE.fixed-1.96*seTE.fixed, 
         TE_ub.fixed=TE.fixed+1.96*seTE.fixed) %>%
  mutate(cer_point=exp(TE.placebo.random)/(1+exp(TE.placebo.random))) %>%
  mutate(point.random=ifelse(sm=="OR", round(exp(TE.random), 2), round(TE.random, 2)),
         lb.random=ifelse(sm=="OR", round(exp(TE_lb.random), 2), round(TE_lb.random, 2)),
         ub.random=ifelse(sm=="OR", round(exp(TE_ub.random), 2), round(TE_ub.random, 2)),
         point.fixed=ifelse(sm=="OR", round(exp(TE.fixed), 2), round(TE.fixed, 2)),
         lb.fixed=ifelse(sm=="OR", round(exp(TE_lb.fixed), 2), round(TE_lb.fixed, 2)),
         ub.fixed=ifelse(sm=="OR", round(exp(TE_ub.fixed), 2), round(TE_ub.fixed, 2))) %>% 
  mutate(ACR=ifelse(sm=="OR", ifelse(cer_point<0.1, 
                                     round(100*point.random*cer_point_0/(1-cer_point_0+point.random*cer_point_0),1), 
                                     round(100*point.random*cer_point/(1-cer_point+point.random*cer_point),1)),
                                     NA),
         CER=ifelse(!is.na(point.random), round(100*cer_point, 1), NA),
         t2=round(tau2,4))  %>% 
  mutate(association=ifelse(!is.na(point.random), 
                            ifelse(k>1 & !is.na(t2), ifelse(sm=="OR", paste0("N=", k, " n=", n, "; Random-effects: ", ACR,"% vs. ", CER,"%, ",
                                                                             sm,"=",point.random,", 95%CI: ", lb.random, ", ", ub.random,"; Fixed-effecs: ", 
                                                                             sm,"=",point.fixed,", 95%CI: ", lb.fixed, ", ", ub.fixed),
                                   paste0("N=", k, " n=", n, "; Random-effects: ", sm,"=",point.random,", 95%CI: ", lb.random, ", ", ub.random,"; Fixed-effecs: ", 
                                          sm,"=",point.fixed,", 95%CI: ", lb.fixed, ", ", ub.fixed)), 
                                   ifelse(sm=="OR", paste0("N=", k, " n=", n, "; ", ACR,"% vs. ", CER,"%, ", sm,"=",point.random,", 95%CI: ", lb.random, ", ", ub.random),
                                          paste0("N=", k, " n=", n, "; ", sm,"=",point.random,", 95%CI: ", lb.random, ", ", ub.random))),
                                   paste0("N=", k, " n=", n, "; not estimable effect size 0 events in both arms")),
         study_limitations=ifelse(moderate_to_high_bias_prop==0 & k==1, "1 study with an overall low risk of bias.",
                                  ifelse(moderate_bias==1 & k==1, "1 study with overall some concerns in risk of bias.",
                                         ifelse(high_bias==1 & k==1, "1 study with overall high risk of bias.",
                                                ifelse(moderate_to_high_bias_prop==0 & k>1, " All studies had overall low risk of bias.",
                                                paste0(round(100*(1-moderate_to_high_bias_prop),1),"% had overall low risk of bias"))))),
         reporting_bias=ifelse(k_prop==1 & k_possible==1, "1 eligible study with usable data.",
                               ifelse(k_prop==1 & k_possible>1, paste0("All eligible studies had usable data."),
                                      ifelse(k_prop<1, paste0(round(100*k_prop,1), "% of eligible studies and ", round(100*n_prop,1),"% of participants had usable data."), NA))),
         indirectness=ifelse(outcome_type=="efficacy", "No clear indication of indirectness.",
                             ifelse(k_schiz_prop==1, "No clear indication of indirectness.",
                                    paste0(round(100*k_schiz_prop,1), "% studies and ", round(100*n_schiz_prop,1), "% participants with schizophrenia.")))) %>% 
  mutate(population=ifelse(outcome_type=="efficacy", population, NA),
         id_outcome_population=paste0(outcome, population))  %>%
left_join(outcome_names)   %>%
  select(comparison, outcome_type, id_outcome_population, order, outcome_new, population, timepoint, duration, association, study_limitations, reporting_bias, indirectness)%>%
  mutate(outcome_new=ifelse(grepl("overall", substr(id_outcome_population, start=1, stop=12)),"Overall symptoms", 
                            ifelse(grepl("positive", substr(id_outcome_population, start=1, stop=12)), "Positive symptoms",
                                   ifelse(grepl("negative", substr(id_outcome_population, start=1, stop=12)), "Negative symptoms",
                                          ifelse(grepl("depression",substr(id_outcome_population, start=1, stop=12)), "Depressive symptoms", 
                                                 ifelse(grepl("cognition", substr(id_outcome_population, start=1, stop=12)), "Cognitive impairment",
                                                        ifelse(grepl("response", substr(id_outcome_population, start=1, stop=12)), "Response to treatment", 
                                                               ifelse(grepl("relapse", substr(id_outcome_population, start=1, stop=12)), "Relapse", outcome_new)))))))) %>% 
  mutate(order=ifelse(grepl("overall", substr(id_outcome_population, start=1, stop=12)),1, 
                      ifelse(grepl("positive", substr(id_outcome_population, start=1, stop=12)),3,
                            ifelse(grepl("negative", substr(id_outcome_population, start=1, stop=12)), 4,
                                   ifelse(grepl("depression", substr(id_outcome_population, start=1, stop=12)), 5, 
                                   ifelse(grepl("cognition", substr(id_outcome_population, start=1, stop=12)), 6,
                                          ifelse(grepl("response", substr(id_outcome_population, start=1, stop=12)), 2, 
                                                 ifelse(grepl("relapse", substr(id_outcome_population, start=1, stop=12)), 2.5, order)))))))) %>% 
  mutate(comparison_order=case_when(
    comparison=="taar1_vs_placebo"~1,
    comparison=="taar1_vs_antipsychotic"~2,
    comparison=="taar1_vs_placebo_addon"~3),
         population_order=case_when(
           population=="Schizophrenia spectrum (acute episode)" ~ 1,
           population=="Schizophrenia spectrum (negative symptoms)" ~ 2,
           population=="Schizophrenia spectrum (clinically stable)" ~ 3,
           population=="Parkinson Disease Psychosis" ~ 4), 
         time_order=case_when(
           timepoint=="1 day-2 weeks" ~1,
           timepoint=="3-13 weeks" ~2,
           timepoint==">13 weeks" ~3)) %>% 
  mutate(text_comparison=case_when(
    comparison=="taar1_vs_placebo" ~ "TAAR1 agonists vs. placebo",
    comparison=="taar1_vs_antipsychotic" ~ "TAAR1 agonists vs. antipsychotics", 
    comparison=="taar1_vs_placebo_addon" ~ "TAAR1 agonists vs. placebo (addon)")) %>%
  mutate(text_population=case_when(
    is.na(population)~ "any eligible population",
    population=="Schizophrenia spectrum (acute episode)" ~ "adults with an acute episode of schizophrenia",
    population=="Schizophrenia spectrum (clinically stable)" ~ "adults with clinically stable schizophrenia",
    population=="Schizophrenia spectrum (negative symptoms)" ~ "adults with schizophrenia and predominant negative symptoms",
    population=="Parkinson Disease Psychosis" ~"Parkinson Disease Psychosis")) %>%
  mutate(text_description=paste0(text_comparison," in ", text_population)) %>%
  mutate(other_bias=ifelse(comparison=="taar1_vs_placebo" & (id_outcome_population=="overallSchizophrenia spectrum (acute episode)" | id_outcome_population=="responseSchizophrenia spectrum (acute episode)" |
                             id_outcome_population=="positiveSchizophrenia spectrum (acute episode)" | id_outcome_population=="negativeSchizophrenia spectrum (acute episode)"),
                           "Three trials were conducted during COVID-19 (2 in ulotaront, 1 in ralmitaront), which in some cases could be associated with factors related to smaller effect sizes. Specifically, a pooled analysis of the two Phase III ulotaront trials, using only the participants enrolled before COVID-19, showed effect sizes like the Phase II ulotaront trial. This may have underestimated the effects of TAAR1 agonists compared to placebo in favor of the latter.", "No clear indication of other biases"))


meta_outcome_soe<-meta_outcome_soe[order(meta_outcome_soe$comparison_order, meta_outcome_soe$outcome_type,
                                         meta_outcome_soe$population_order, 
                                         meta_outcome_soe$order, meta_outcome_soe$time_order),]

meta_outcome_soe<-meta_outcome_soe %>% select(text_description, outcome_type, outcome_new, duration, association, study_limitations, reporting_bias, indirectness, other_bias)

write_xlsx(meta_outcome_soe, "data_u1/meta_outcome_soe.xlsx")

#Meta-analysis of the primary outcome for schizophrenia per TAAR1 agonist (using the same code as above)
master_drug<- master %>% 
    filter(timepoint=="3-13 weeks") %>%  
  filter(population=="Schizophrenia spectrum (acute episode)") %>%
    filter(!is.na(overall_mean)) %>% 
    select(study_name, study_name_drug, crossover_periods, population, duration_weeks,drug_new, drug_name, dose, overall_mean, overall_sd, overall_n) %>%  
    filter(drug_new=="TAAR1 agonist" | drug_new=="placebo")  

master_pooled_drug<-master_drug %>%  
  group_by(study_name, study_name_drug, drug_name, population, duration_weeks)%>%
  summarise(n_an=sum(overall_n),
            mean_an=weighted.mean(overall_mean,overall_n), 
            sd_an=sqrt(((sum((overall_n - 1) * overall_sd^2 + overall_n * overall_mean^2)) - sum(overall_n) * mean_an^2)/(sum(overall_n) -  1)))

master_pooled_drug_studies<-master_pooled_drug %>% 
  select(study_name) %>% group_by(study_name) %>% summarise(count=n()) %>% filter(count!=1) 

master_pooled_drug<-master_pooled_drug %>% filter(study_name %in% master_pooled_drug_studies$study_name) 

pairwise_drug<-pairwise(data=master_pooled_drug, studlab = study_name_drug, treat=drug_name,
                       mean=mean_an, sd=sd_an, n=n_an, sm="SMD")
  
  pairwise_drug<-as.data.frame(pairwise_drug) %>% 
    mutate(treat1_new=ifelse(treat1!="placebo" , treat1, treat2),
           treat2_new=ifelse(treat1!="placebo" , treat2, treat1),
           mean1_new=ifelse(treat1!="placebo" , -1*mean1, -1*mean2),
           mean2_new=ifelse(treat1!="placebo" , -1*mean2, -1*mean1),
           sd1_new=ifelse(treat1!="placebo" , sd1, sd2),
           sd2_new=ifelse(treat1!="placebo" , sd2, sd1),
           n1_new=ifelse(treat1!="placebo" ,n1, n2),
           n2_new=ifelse(treat1!="placebo" , n2, n1)) %>%
    mutate(comp=paste0(treat1_new," vs. ", treat2_new)) %>%
    select(study_name, study_name_drug, duration_weeks,
           comp, population, treat1_new, treat2_new, mean1_new, sd1_new, n1_new, mean2_new,  sd2_new, n2_new) %>% 
    mutate(n_total=n1_new + n2_new) %>%
    mutate(drug=treat1_new) %>%
    unique()
  
  
  rob_drug<-rob %>% filter(outcome=="overall" & timepoint=="3-13 weeks")
  
  pairwise_drug<- pairwise_drug %>% left_join(rob_drug) %>% 
    unique()
  
  meta_comp_drug<-metacont(data=pairwise_drug,  
                      mean.e = mean1_new, sd.e=sd1_new, n.e=n1_new, mean.c = mean2_new, sd.c=sd2_new, n.c=n2_new,
                      sm="SMD", random=TRUE, fixed=TRUE,    
                      studlab = study_name_drug, prediction = FALSE, subgroup = drug, 
                      prediction.subgroup = FALSE)
  
  
#Meta-analysis of the efficacy outcomes for acute schizophenia and Parkinson's disease psyschosis in order to present them in separate forest plots
  master_separate<- master %>% 
    filter(timepoint=="3-13 weeks") %>%  
    filter(!is.na(overall_mean)) %>% 
    select(study_name, study_name_drug, crossover_periods, population, duration_weeks,drug_new, 
           overall_mean, overall_sd, overall_n,
           positive_mean, positive_sd, positive_n,
           negative_mean, negative_sd, negative_n,
           response_e, randomized_n) %>%  
    filter(drug_new=="TAAR1 agonist" | drug_new=="placebo")  
  
  master_pooled_separate <-master_separate %>%  
    group_by(study_name, study_name_drug, drug_new, population, duration_weeks)%>%
    summarise(n_an=sum(randomized_n), 
              response_an=sum(as.integer(response_e)),
      n_ov_an=sum(overall_n),
              mean_ov_an=weighted.mean(overall_mean,overall_n), 
              sd_ov_an=sqrt(((sum((overall_n - 1) * overall_sd^2 + overall_n * overall_mean^2)) - sum(overall_n) * mean_ov_an^2)/(sum(overall_n) -  1)),
      n_pos_an=sum(positive_n),
      mean_pos_an=weighted.mean(positive_mean,positive_n), 
      sd_pos_an=sqrt(((sum((positive_n - 1) * positive_sd^2 + positive_n * positive_mean^2)) - sum(positive_n) * mean_pos_an^2)/(sum(positive_n) -  1)),
      n_neg_an=sum(negative_n),
      mean_neg_an=weighted.mean(negative_mean,negative_n), 
      sd_neg_an=sqrt(((sum((negative_n - 1) * negative_sd^2 + negative_n * negative_mean^2)) - sum(negative_n) * mean_neg_an^2)/(sum(negative_n) -  1)))
  
  pairwise_separate<-pairwise(data=master_pooled_separate, studlab = study_name_drug, treat=drug_new,
                          mean=mean_ov_an, sd=sd_ov_an, n=n_ov_an, sm="SMD")
  
  pairwise_separate<-as.data.frame(pairwise_separate) %>% 
    mutate(treat1_new=ifelse(treat1!="placebo" , treat1, treat2),
           treat2_new=ifelse(treat1!="placebo" , treat2, treat1),
           overall_mean1_new=ifelse(treat1!="placebo" , -1*mean1, -1*mean2),
           overall_mean2_new=ifelse(treat1!="placebo" , -1*mean2, -1*mean1),
           overall_sd1_new=ifelse(treat1!="placebo" , sd1, sd2),
           overall_sd2_new=ifelse(treat1!="placebo" , sd2, sd1),
           overall_n1_new=ifelse(treat1!="placebo" ,n1, n2),
          overall_n2_new=ifelse(treat1!="placebo" , n2, n1), 
           positive_mean1_new=ifelse(treat1!="placebo" , -1*mean_pos_an1, -1*mean_pos_an2),
          positive_mean2_new=ifelse(treat1!="placebo" , -1*mean_pos_an2, -1*mean_pos_an1),
          positive_sd1_new=ifelse(treat1!="placebo" , sd_pos_an1, sd_pos_an2),
          positive_sd2_new=ifelse(treat1!="placebo" , sd_pos_an2, sd_pos_an1),
          positive_n1_new=ifelse(treat1!="placebo" ,n_pos_an1, n_pos_an2),
          positive_n2_new=ifelse(treat1!="placebo" , n_pos_an2, n_pos_an1), 
          negative_mean1_new=ifelse(treat1!="placebo" , -1*mean_neg_an1, -1*mean_neg_an2),
          negative_mean2_new=ifelse(treat1!="placebo" , -1*mean_neg_an2, -1*mean_neg_an1),
          negative_sd1_new=ifelse(treat1!="placebo" , sd_neg_an1, sd_neg_an2),
          negative_sd2_new=ifelse(treat1!="placebo" , sd_neg_an2, sd_neg_an1),
          negative_n1_new=ifelse(treat1!="placebo" ,n_neg_an1, n_neg_an2),
          negative_n2_new=ifelse(treat1!="placebo" , n_neg_an2, n_neg_an1), 
          response1=ifelse(treat1!="placebo" ,response_an1, response_an2),
          response2=ifelse(treat1!="placebo" ,response_an2, response_an2),
            random1=ifelse(treat1!="placebo" ,n_an1, n_an2),
            random2=ifelse(treat1!="placebo" ,n_an2, n_an1)) %>%
    mutate(comp=paste0(treat1_new," vs. ", treat2_new)) %>%
    select(study_name, study_name_drug, duration_weeks,
           comp, population, treat1_new, treat2_new,
           overall_mean1_new,  overall_sd1_new,  overall_n1_new,  overall_mean2_new,  overall_sd2_new,  overall_n2_new,
           positive_mean1_new, positive_sd1_new, positive_n1_new, positive_mean2_new,  positive_sd2_new, positive_n2_new,
           negative_mean1_new, negative_sd1_new, negative_n1_new, negative_mean2_new,  negative_sd2_new, negative_n2_new,
           response1, random1, response2, random2) %>% 
    unique()
  
  pairwise_separate_overall<-pairwise_separate  
  
  rob_overall<-rob %>% filter(outcome=="overall" & timepoint=="3-13 weeks")  %>% select(study_name, Overall) %>% rename(Overall_overall=Overall)
  rob_positive<-rob %>% filter(outcome=="positive" & timepoint=="3-13 weeks")  %>% select(study_name, Overall)%>% rename(Overall_positive=Overall)
  rob_resopnse<-rob %>% filter(outcome=="response" & timepoint=="3-13 weeks")  %>% select(study_name, Overall)%>% rename(Overall_response=Overall)
  rob_negative<-rob %>% filter(outcome=="negative" & timepoint=="3-13 weeks")  %>% select(study_name, Overall)%>% rename(Overall_negative=Overall)
  
  pairwise_separate<-pairwise_separate %>% left_join(rob_overall) %>% left_join(rob_positive) %>% left_join(rob_resopnse) %>% left_join(rob_negative)
  
  meta_comp_overall_sch<-metacont(data=pairwise_separate[pairwise_separate$population=="Schizophrenia spectrum (acute episode)",],  
                           mean.e = overall_mean1_new, sd.e=overall_sd1_new, n.e=overall_n1_new, mean.c = overall_mean2_new, sd.c=overall_sd2_new, n.c=overall_n2_new,
                           sm="SMD", random=TRUE, fixed=TRUE,    
                           studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                           prediction.subgroup = FALSE)
  meta_comp_overall_pdp<-metacont(data=pairwise_separate[pairwise_separate$population=="Parkinson Disease Psychosis",],  
                                  mean.e = overall_mean1_new, sd.e=overall_sd1_new, n.e=overall_n1_new, mean.c = overall_mean2_new, sd.c=overall_sd2_new, n.c=overall_n2_new,
                                  sm="SMD", random=TRUE, fixed=TRUE,    
                                  studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                  prediction.subgroup = FALSE)
  
  meta_comp_overall_neg<-metacont(data=pairwise_separate[pairwise_separate$population=="Schizophrenia spectrum (negative symptoms)",],  
                                  mean.e = overall_mean1_new, sd.e=overall_sd1_new, n.e=overall_n1_new, mean.c = overall_mean2_new, sd.c=overall_sd2_new, n.c=overall_n2_new,
                                  sm="SMD", random=TRUE, fixed=TRUE,    
                                  studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                  prediction.subgroup = FALSE)
  
  meta_comp_positive_sch<-metacont(data=pairwise_separate[pairwise_separate$population=="Schizophrenia spectrum (acute episode)" & !is.na(pairwise_separate$positive_mean1_new),],  
                                  mean.e = positive_mean1_new, sd.e=positive_sd1_new, n.e=positive_n1_new, mean.c = positive_mean2_new, sd.c=positive_sd2_new, n.c=positive_n2_new,
                                  sm="SMD", random=TRUE, fixed=TRUE,    
                                  studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                  prediction.subgroup = FALSE)
  meta_comp_positive_pdp<-metacont(data=pairwise_separate[pairwise_separate$population=="Parkinson Disease Psychosis",],  
                                  mean.e = positive_mean1_new, sd.e=positive_sd1_new, n.e=positive_n1_new, mean.c = positive_mean2_new, sd.c=positive_sd2_new, n.c=positive_n2_new,
                                  sm="SMD", random=TRUE, fixed=TRUE,    
                                  studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                  prediction.subgroup = FALSE)
  
  meta_comp_positive_neg<-metacont(data=pairwise_separate[pairwise_separate$population=="Schizophrenia spectrum (negative symptoms)" & !is.na(pairwise_separate$positive_mean1_new),],  
                                   mean.e = positive_mean1_new, sd.e=positive_sd1_new, n.e=positive_n1_new, mean.c = positive_mean2_new, sd.c=positive_sd2_new, n.c=positive_n2_new,
                                   sm="SMD", random=TRUE, fixed=TRUE,    
                                   studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                   prediction.subgroup = FALSE)

  meta_comp_response_sch<-metabin(data=pairwise_separate[pairwise_separate$population=="Schizophrenia spectrum (acute episode)",],  
                     event.e = response1, n.e=random1, event.c = response2, n.c=random2,
                     sm="OR", random=TRUE, fixed=TRUE,    
                     studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                     prediction.subgroup = FALSE)
  
  meta_comp_response_pdp<-metabin(data=pairwise_separate[pairwise_separate$population=="Parkinson Disease Psychosis",],  
                                  event.e = response1, n.e=random1, event.c = response2, n.c=random2,
                                  sm="OR", random=TRUE, fixed=TRUE,    
                                  studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                  prediction.subgroup = FALSE)
  
  meta_comp_response_neg<-metabin(data=pairwise_separate[pairwise_separate$population=="Schizophrenia spectrum (negative symptoms)",],  
                                  event.e = response1, n.e=random1, event.c = response2, n.c=random2,
                                  sm="OR", random=TRUE, fixed=TRUE,    
                                  studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                  prediction.subgroup = FALSE)

  meta_comp_negative_sch<-metacont(data=pairwise_separate[pairwise_separate$population=="Schizophrenia spectrum (acute episode)" & !is.na(pairwise_separate$positive_mean1_new),],  
                                   mean.e = negative_mean1_new, sd.e=negative_sd1_new, 
                                   n.e=negative_n1_new, mean.c = negative_mean2_new, sd.c=negative_sd2_new, n.c=negative_n2_new,
                                   sm="SMD", random=TRUE, fixed=TRUE,    
                                   studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                   prediction.subgroup = FALSE)

  meta_comp_negative_neg<-metacont(data=pairwise_separate[pairwise_separate$population=="Schizophrenia spectrum (negative symptoms)" & !is.na(pairwise_separate$positive_mean1_new),],  
                                   mean.e = negative_mean1_new, sd.e=negative_sd1_new, 
                                   n.e=negative_n1_new, mean.c = negative_mean2_new, sd.c=negative_sd2_new, n.c=negative_n2_new,
                                   sm="SMD", random=TRUE, fixed=TRUE,    
                                   studlab = study_name_drug, prediction = FALSE, subgroup = population, 
                                   prediction.subgroup = FALSE)
  
  
#Overall risk of bias of the studies using the highest across outcomes
rob_all<-rob %>% select(study_name, D1, D2, D3, D4, D5, DS, Overall)
rob_all[rob_all=="Low"]<-"0"
rob_all[rob_all=="Some concerns"]<-"1"
rob_all[rob_all=="High"]<-"2"

rob_all<-rob_all %>% group_by(study_name) %>%
  summarise(D1=as.character(max(as.integer(D1))), 
            D2=as.character(max(as.integer(D2))),
            D3=as.character(max(as.integer(D3))),
            D4=as.character(max(as.integer(D4))),
            D5=as.character(max(as.integer(D5))),
            DS=as.character(max(as.integer(DS))),
            Overall=as.character(max(as.integer(Overall))))

rob_all[rob_all==0]<-"Low"
rob_all[rob_all==1]<-"Some concerns"
rob_all[rob_all==2]<-"High"

colnames(rob_all) <- c("Study", "Bias due to randomization", "Bias due to deviations from intended intervention",
                    "Bias due to missing data", "Bias due to outcome measurement", "Bias due to selected reported results",
                    "Bias due to period and carryover effects", "Overall bias")

#Function to create RoB plots
rob_plot<-function(data){ #RoB function using the extended meta objects
  data<-data$data[, c("study_name","D1", "D2", "D3", "D4", "D5", "DS", "Overall")]
  colnames(data) <- c("Study", "Bias due to randomization", "Bias due to deviations from intended intervention",
                      "Bias due to missing data", "Bias due to outcome measurement", "Bias due to selected reported results",
                      "Bias due to period and carryover effects", "Overall bias")
  
    rob_traffic<-rob_traffic_light(data=data, tool = "Generic",  psize = 12)
  return(rob_traffic)
}

rob_summary<-function(data){ #RoB function using the extended meta objects
  data<-data$data[, c("study_name","D1", "D2", "D3", "D4", "D5", "DS", "Overall")]
  colnames(data) <- c("Study", "Bias due to randomization", "Bias due to deviations from intended intervention",
                      "Bias due to missing data", "Bias due to outcome measurement", "Bias due to selected reported results",
                      "Bias due to period and carryover effects", "Overall bias")
  
  rob_summary<-rob_summary(data=data, tool = "Generic",  psize = 12)
  return(rob_summary)
}

