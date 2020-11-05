# ---- Program Header ----
# Project: 
# Program Name: 
# Author: 
# Created: 
# Purpose: 
# Revision History:
# Date        Author        Revision
# 

# ---- Initialize Libraries ----
library(dplyr)
library(lubridate)
library(haven)
library(openxlsx)
library(tidyverse)
library(janitor)

# ---- Load Functions ----
setwd("C:/Users/JordanMyer/Desktop/New OneDrive/Emanate Life Sciences/DM - Inflammatix - Documents/INF-04/11. Clinical Progamming/11.3 Production Reports/11.3.3 Input Files")
source("../11.3.1  R Production Programs/INF Global Functions.R")
cleaner()
source("../11.3.1  R Production Programs/INF Global Functions.R")

# ---- Load Raw Data ----

importSubjectSummary <- read.xlsx(MostRecentFile("EDC/EDC Reports/",".*Medrio_SubjectDataSummaryReport.*xlsx$","ctime"))
importSD <- read_sas("EDC/DS_SD.sas7bdat")
# ---- 

singleCleanPatient <- function(subjID){
  
  singleSubject <- importSubjectSummary %>% filter(Subject == subjID)
  subjectStatus <- singleSubject$Subject.Status[1]
  studyStatusDF <- importSD %>% filter(SubjectID == subjID)
  studyStatus <- studyStatusDF$DSDECOD[1]
  SSFormStatus <- singleSubject %>% 
    filter(Visit!="Adjudication") %>% 
    filter(Form!="Release CRF for Adjudication") %>%
    filter(Visit!="Central Lab Results")%>% 
    count(Form.Status)
  completeForms <- nrow(singleSubject %>% filter(Form.Status== "Complete"))
  missingForms <- nrow(singleSubject %>% filter(Form.Status== "Missing"))
  
  
  formsWithUnresolvedQueries <- nrow(singleSubject %>% filter(Open.Queries!=0))
  
  monitoringRequired <- nrow(singleSubject %>% filter(Form.Monitored=="No"&Form.Status=="Complete"))
  DMReviewRequired <- nrow(singleSubject %>% filter(Form.DM.Approved=="No"&Form.Status=="Complete"))
  
  SSLabRows <- singleSubject %>% filter(Visit == "Central Lab Results")
  CLPending <- ifelse(test = SSLabRows[1,6]=="NotExpected"&SSLabRows[2,6]=="NotExpected",yes = "N/A",no = 
                      ifelse(test = SSLabRows[1,6] == "Complete" &SSLabRows[2,6] == "Complete", yes = "No","Yes"))
  readyToAdj <- ifelse(test = (missingForms==0&formsWithUnresolvedQueries==0&
                                 monitoringRequired==0&DMReviewRequired==0&
                                 CLPending == "No"), yes = "Yes",no = "No")
  SSDF <- data.frame(Subject = subjID, `Subject Status` = subjectStatus, studyStatus = studyStatus,
                    `Completed Forms` = completeForms,`Missing Forms` = missingForms,
                   `Forms With Unresolved Queries` = formsWithUnresolvedQueries,
                   `Monitoring Required` = monitoringRequired, `DM Review Required` = DMReviewRequired,
                   CLResults = CLPending, Ready4Adj = readyToAdj)

  newNames <- c("Subject","Subject Status","Study Status","Completed Forms","Missing Forms"," Forms with Unresolved Queries",
              "Monitoring Required","DM Review Required","Pending Lab Data Entry","Ready For Adjudication")
  finalDF <- set_names(SSDF,newNames)
  return(finalDF)}

allSubjects <- unique(importSubjectSummary %>% filter(Subject!="") %>% pull(Subject))

firstPatient <- singleCleanPatient(allSubjects[1])
cleanPatientTracker <- firstPatient
for(i in 2:length(allSubjects)){
  cleanPatientTracker <- rbind(cleanPatientTracker,singleCleanPatient(allSubjects[i]))
}

write.xlsx(cleanPatientTracker,"Clean Patient Tracker 2020 11 04.xlsx")


