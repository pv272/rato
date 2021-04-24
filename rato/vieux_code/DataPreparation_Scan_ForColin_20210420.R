
library(tidyverse)
library(lubridate)
library(janitor)


# General -----------------------------------------------------------------

#All scan relevant info are made of behavioural data and session info (date at which observation was done, Colony etc). 

#behavioural data are made of Instaneous behaviour (behaviours of group members recorded every 4 minutes) and continuous behaviour that are recorded adlibitum with the time left between 2 instantaneous samples.

#For both Instaneous and continuous behaviours, old data are only available as summary of session whereas new data are available as raw data 

#Data will be prepared at different time scale (Raw, Session summary, Day summary)

#Regardless of the scale at which the data are prepared, the data preparation always begin by the reformating of database extract. In the current code it is labelled Session reformatting - Instant reformatting - Cont reformatting 

#I have remove some data check section from the code to make it more readable for you. Code of datacheck 

#It would be good to think about big table output that have standardized name


# Reformatting ------------------------------------------------------------

#all the reformatting section could be seen at the part between database extract (red in power point) and the orange part. 

#I have here included the reformatting of all database extract object that will be required to prepare the behavioural data at the raw - Session - Day level

#Reformatting includes filtering out some data and is highlighted #CLEAN in the code. Instead of repeating the code, this could be done by function 

#Reformatting also includes renaming columns and adding columns. This is not ideal and I will ask TimVink to make column names as consistet as possible in the database (for example always use AnimalID instead of Subject/AnimalID). Doing so may also decrease the data variable that must be tunnel trhough environement variable using {{}}

#useful functions could be 
#Session remove ObsRef == NA, remove ObsDate == NA
#Data remove ObsRef == NA, remove behaviours == NA, remove Behaviour Count == NA
#Remove duplicated data (require session and data info)


# Reformatting Session ----------------------------------------------------
#spellt as in database, but have removed the qry_ prefix
#remove NA ObsRef 
#remove NA ObsDate
#remove duplicated (but this could lead to seesion without behaviour to be removed instead of the one with behaviour)

ScanSession_Mrdb <- qry_ScanSession_Mrdb %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>%
  #REFORMAT
  mutate(ObsDate = ymd(ObsDate),
         ObsTime = hms(ObsTime)) %>% 
  #CLEAN
  filter(!(is.na(ObsRef))) %>% #1650
  filter(!(is.na(ObsDate))) %>% #1648, two obs without dates
  distinct(Colony,ObsDate,ObsTime,.keep_all = TRUE) %>% #1642 => 6 duplicated all have behaviours recorded, thus they can be removed
  #ADD
  mutate(ObsType = "Scan",
         DataSource = "Moleratdatabase",
         ScanNumber_Estimated = round((TotalDuration/(3600))*15))

ScanSession_MrRaw <- qry_ScanSession_MrRaw %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>%
  #REFORMAT
  mutate(ObsDate = ymd(ObsDate),
         ObsTime = hms(ObsTime)) %>% 
  #CLEAN
  filter(!(is.na(ObsRef))) %>% #2513
  filter(!(is.na(ObsDate))) %>% #2513
  distinct(Colony,ObsDate,ObsTime,.keep_all = TRUE) %>% #2507
  #ADD 
  mutate(ObsType = "Scan",
         DataSource = "MR_RawData",
         ScanNumber_Estimated = round((TotalDuration/(3600))*15))


ScanSession_All <- qry_ScanSession_All %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>%
  #REFORMAT
  mutate(ObsDate = ymd(ObsDate),
         ObsTime = hms(ObsTime)) %>% #4175
  #CLEAN
  filter(!(is.na(ObsRef))) %>% #4163 loss of 12
  filter(!(is.na(ObsDate))) %>% #4161 loss of 2
  distinct(Colony,ObsDate,ObsTime,.keep_all = TRUE) %>% #4149 loss ff 12
  #ADD
  mutate(ObsType = "Scan",
         ScanNumber_Estimated = (TotalDuration/(3600))*20) %>%
  #OBSREF_DAY
  group_by(Colony,ObsDate) %>% 
  mutate(ObsRef_Day = cur_group_id()) %>% 
  ungroup() %>%
  #UNGROUP
  mutate(ObsRef_Session = case_when(DataSource == "Moleratdatabase" ~ ObsRef,
                                    DataSource == "MR_RawData" ~ ObsRef_Day))
#4149
#2507


# Reformatting Behavioural data -------------------------------------------
# Duplicated data are not removed
# ObsRef not corresponding to real observartion (New data, the way the colony of scan was assigned) have not been removed


# Reformatting Instant ----------------------------------------------------
#Remove behaviour that are not associated with any ObsRef 
#removing duplicates

ScanInstantSummaryNoModifier_Mrdb <- qry_ScanInstantSummaryNoModifier_Mrdb %>% 
  #RENAME
  rename(ObsRef = ScanRef, 
         ObsFileID = ScanFileID,
         PartnerCorrection = ParnerCorrection) %>% #must be corrected in DB
  #CLEAN
  filter(!(is.na(ObsRef)),  #245'079
         !(is.na(BehaviourCount))) %>% #245'061  18 behaviours with NA (see data check below, I think it comes from not all behaviours being exorted in summary, but unsure why it returns a NA only for locoWork)
  distinct() %>% #245'049
  #ADD
  mutate(ObsFileID = as.integer(ObsFileID),#To make format similar with MrRaw
         DataSource = "Moleratdatabase",
         BehaviourType = "Instant")
View(ScanInstantSummaryNoModifier_Mrdb)


ScanInstant_MrRaw <- qry_ScanInstant_MrRaw %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>% #1'032'536
  #CLEAN
  filter(!(is.na(ObsRef))) %>%  #987'638
  distinct() %>% #987'638
  #ADD
  mutate(DataSource = "MR_RawData",
         BehaviourType = "Instant")
View(ScanInstant_MrRaw)
#987'638


ScanInstantSummary_MrRaw <- qry_ScanInstantSummary_MrRaw %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>% #177,076
  #CLEAN
  filter(!(is.na(ObsRef)), #169,657 
         !(is.na(BehaviourCount))) %>% #169'657
  distinct() %>% #169'657
  #ADD
  mutate(DataSource = "MR_RawData",
         BehaviourType = "Instant")
View(ScanInstantSummary_MrRaw)


ScanInstantSummaryNoModifier_MrRaw <- qry_ScanInstantSummaryNoModifier_MrRaw %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>% #348'185
  #CLEAN
  filter(!(is.na(ObsRef)), #332,826
         !(is.na(BehaviourCount))) %>% #332,826
  distinct() %>% #330'352 This duplicates need to be assessed, probably group by missing in SQL qry
  #ADD
  mutate(DataSource = "MR_RawData",
         BehaviourType = "Instant")
View(ScanInstantSummaryNoModifier_MrRaw)

# Refomratting Cont -------------------------------------------------------

ScanContSummary_Mrdb <- qry_ScanContSummary_Mrdb %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>%
  #REFORMAT
  mutate(ModifierValue1 = as.character(ModifierValue1),
         ModifierValue2 = as.character(ModifierValue2),
         ObsFileID = as.integer(ObsFileID)) %>% 
  #CLEAN
  filter(!(is.na(ObsRef))) %>% #178'512
  distinct() %>% #178'075 strange to have duplicates
  #ADD
  mutate(DataSource = "Moleratdatabase",
         BehaviourType = "Continuous")


ScanCont_MrRaw <- qry_ScanCont_MrRaw %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>%
  #CLEAN
  filter(!(is.na(ObsRef))) %>% #110'956
  distinct() %>% #110'956
  #ADD
  mutate(DataSource = "MR_RawData",
         BehaviourType = "Continuous")


ScanContSummary_MrRaw <- qry_ScanContSummary_MrRaw %>% 
  #RENAME
  rename(ObsRef = ScanRef,
         ObsFileID = ScanFileID) %>%
  #CLEAN
  filter(!(is.na(ObsRef))) %>% #83'385
  distinct() %>% #83'385
  #ADD
  mutate(DataSource = "MR_RawData",
         BehaviourType = "Continuous")



# Raw data ----------------------------------------------------------------
#generate 1 row per behaviour
#I need to desummarize instant and cont data that are only available as summary
#I have already all the code available but have not included it here to keep the code shorter and more clear


# Summary data ------------------------------------------------------------


# Summary Behaviour List --------------------------------------------------

#For instantaneous behaviours the 0 are given in data summary
#For continuous behaviours, 0 are not given in data summary (count are only based on what has been observed) and must be generated 

#Thus one must generate a list of all possible combination of ObsRef/Subject/Behaviours which we will use to generate 0 in data summary

# ALL data summary, regardless of the scale at which we will want produce them will require the generation of 0 

#Some behaviours may have to be recategorized in comparision to how they are provided by the data base and I have written a few function for this

#All combination between cont behaviour and their modifier
Cont_List <- ScanBehaviour_Cont_List %>% 
  select(Label,Value,AnimalModif) %>% 
  #SHOW 2ND MODIFIER
  left_join(.,ScanBehaviour_Cont_List %>% 
              filter(ModifierNumber == 2) %>% 
              select(Value,OtherModif,CodeLabel) %>% 
              rename(ModifierClass1 = OtherModif,
                     ModifierLabel1 = CodeLabel)) %>% 
  #SHOW 3RD MODIFIER
  left_join(.,ScanBehaviour_Cont_List %>% 
              filter(ModifierNumber == 3) %>% 
              select(Value,OtherModif,CodeLabel) %>% 
              rename(ModifierClass2 = OtherModif,
                     ModifierLabel2 = CodeLabel)) %>% 
  mutate(Directionality = case_when (Label %in% c("Pump","Sex Forplay","Coprophagy","DropFS","Food comp","Nest Build","Pass","Sparr") ~ "No",
                                     TRUE ~ "Yes")) %>% 
  mutate(Dyad = case_when(!(is.na(AnimalModif)) ~ "Yes",
                          TRUE ~ "No"))%>% #
  rename(Behaviour = Label) %>% 
  mutate(DyadType = case_when( Dyad == "No" ~ "NonDyadic",
                               Dyad == "Yes" & Directionality == "No" ~ "Indirected",
                               Dyad == "Yes" & Directionality == "Yes" ~ "Directed")) %>% 
  left_join(.,tibble(Directionality = "Yes",
                     ReceivedValue = c(0,1)))
View(Cont_List)

#Behaviours of interest - that I want generate zero for
#the recatgeorization may vary from project to project but I have tried to anticipate what I will need in most of the cases
#Here I recategorize copulation behaviours and passes behaviours
Cont_List_Recategorized <- Cont_List %>% 
  cont_copulation_fragment() %>% #fragment copulation behaviours
  cont_pass_fragment() %>% #fragment pass behaviours 
  distinct(Behaviour,Dyad,Directionality,ReceivedValue,DyadType)
View(Cont_List_Recategorized)
#42


# Summary session  --------------------------------------------------------
#summarize data within session
#Code is ready but have left it out to keep code a bit shorter


# Summary day -------------------------------------------------------------
#Summarize data by 12 hours session (Old data are left as they were, New data are grouped by day)
#This is the behavioural data I am using for the LHAMT project I shared the GitHub repository with you. The aim will be to compare the behavioural differece between breeders and non-breeders


#Mrdb is at the whole session scale (mostly 12-24 hours) and will be left as such
#MrRaw data is at the ObsFileID scale (mostly 3-4 hours) and will be merged by day 
#Because they are at different time scale, the session 



####################################Merge Mrdb and MrRaw Session details

#Session
#Bring Session details at the level of the day
#I won't use this file to join on behaviours as I have removed 
ScanSession_Day <- ScanSession_All %>% 
  #GENERATE DAY INFO
  group_by(DataSource,ObsRef_Day) %>% 
  mutate(TotalScanNumber_Day = sum(TotalScanNumber),
         ScanNumber_Estimated_Day = sum(ScanNumber_Estimated),
         TotalDuration_Day = sum(TotalDuration),
         ContinuousDuration_Day = sum(ContinuousDuration)) %>% 
  slice_min(ObsTime) %>% 
  ungroup() %>% 
  #UNGROUP
  #DISTINCT
  distinct(DataSource,ObsRef_Day,.keep_all = TRUE) %>% 
  #SELECT
  select(DataSource,ObsRef_Day,ObsRef,Colony,ObsDate,ObsTime,TotalDuration_Day,TotalScanNumber_Day,TotalDuration_Day, ContinuousDuration_Day)

View(ScanSession_Day)
#2333

####################################Merge Mrdb and MrRaw behaviours
#Merge Instant Behaviour
#should only return animal that were recorded
ScanInstantSummaryNoModifier_All <- bind_rows(ScanInstantSummaryNoModifier_Mrdb,ScanInstantSummaryNoModifier_MrRaw) %>%
  #ADD OBSREF_DAY
  inner_join(.,ScanSession_All %>% 
               select(DataSource,ObsRef,ObsFileID,ObsRef_Day,Colony)) %>%
  #RELOCATE
  relocate(DataSource:BehaviourType) %>% 
  relocate(ObsRef_Day,.after = ObsFileID)
View(ScanInstantSummaryNoModifier_All)
#572'068 if use left join 
#570'462 if inner join, in that case better because behavioural data include data from duplicated session


#Merge Continuous Behaviour
ScanContSummary_All <- bind_rows(ScanContSummary_Mrdb,ScanContSummary_MrRaw) %>% 
  #ADD OBSREF_SESSION
  inner_join(.,ScanSession_All %>% 
               select(DataSource,ObsRef,ObsFileID,ObsRef_Day,Colony)) %>%
  #RELOCATE
  relocate(DataSource:BehaviourType) %>%
  relocate(ObsRef_Day,.after = ObsFileID)
View(ScanContSummary_All)
#261'460
#260'462 if inner join 


################################ ObsRef/Subject/Behaviour List
#I created the list by taking the animals that were observed in instantaneous and in continuous either as focal or partner
#If no filter is done, animals not belonging to the group may be returned (because during scan 2 animals belonging to two different colonies could be interractiing in social interactions by mistake)

#This incorrect 0 could be filtered if animals have a total of 0 instantaneous within a scan and/or by querying membership at a later stage and filtering out animals wich membership differ from colony associated with ObsRef_Day
ObsRefSubject_Day <- 
  #ALL FOCAL OBSERVED
  bind_rows(ScanInstantSummaryNoModifier_All,ScanContSummary_All) %>%
  distinct(DataSource,ObsRef_Day,Subject) %>% 
  #ALL PARTNER OBSERVED (COULD CONTAIN ANIMAL FROM DIFFERENT COLONY)
  rbind(.,ScanContSummary_All %>% 
          distinct(DataSource,ObsRef_Day,PartnerID) %>% 
          rename(Subject = PartnerID) %>% 
          filter(Subject != "Unknown")) %>% 
  distinct() %>% 
  #REMOVE UNKNOWN ANIMAL
  filter(Subject %in% tblAnimalID$AnimalID) %>% #to my surprise tblAnimalId contains an unknown animal
  #ADD SCAN COLONY
  inner_join(.,ScanSession_Day %>% 
               select(ObsRef_Day,ObsDate,Colony) %>% 
               rename(ScanColony = Colony)) %>% 
  #ADD MEMBERSHIP OF PARTNER
  membership(.,animalid = Subject, date = ObsDate) %>%
  #REMOVE PARTNER FROM DIFFERENT COLONY
  filter(ScanColony == Colony)
View(ObsRefSubject_Day)
#19'006 after excluding Partners that were not member of colony recorded as independnet variable in ScanSession.
#This will require a datcheck and being removed from Dyadic observation

#Combine ObsRefSubject and behaviours of interest. 
ObsRefSubjectBehav_Day <- ObsRefSubject_Day %>% 
  merge(.,Cont_List_Recategorized)
View(ObsRefSubjectBehav_Day)
# 798'252


################################Instantaneous number
#Compute scan number for each individual
ScanNumber_Subject_Day <- scannumber(Data = ScanInstantSummaryNoModifier_All,DataSource,ObsRef_Day,Subject, Level = Subject)
View(ScanNumber_Subject_Day)
#18'857
#a loss of approximately 103 rows in comparision to all possible combinations

#Max number of scan per day
ScanNumber_Max_Day <- scannumber_max(Data = ScanInstantSummaryNoModifier_All,DataSource,ObsRef_Day,Subject, ObsLevel = ObsRef_Day) %>% 
  arrange(DataSource, desc(MaxScanNumber_ObsRef_Day))
View(ScanNumber_Max_Day)
#2320


################################Summary Instantaneous
#summary is already available 
#zero have already been generated
#only need to sum social interactions

#Summary
#it could be that animals that were part of the colony were not part of the scan
Instant_Summary_Day <- instant_summary(data = ScanInstantSummaryNoModifier_All,DataSource,ObsRef_Day,Subject,BehaviourType,Behaviour) %>% #317'888
  filter(Subject %in% tblAnimalID$AnimalID) ##317'204
View(Instant_Summary_Day)
#317'204


################################Summary continuous

#Recategorize continuous behaviour to match the list of ObsRef_Day/Subject/Behaviour
#fragment pass and copulation behaviours
ScanContSummary_All_Recategorized <- ScanContSummary_All %>% 
  cont_pass_fragment() %>% 
  cont_copulation_fragment()
View(ScanContSummary_All_Recategorized)
#260'371

#Prpeare cont data 
#Generate rows for behaviours involved as partners and received
#The function (at the difference than for Instant) already eliminates the animal that have not been corrected. I guess it may be better to have this outside of the function
Cont_Summary_Day <- cont_prepare(BehaviourData =  ScanContSummary_All_Recategorized, BehaviourList = Cont_List_Recategorized, DyadType = DyadType)
View(Cont_Summary_Day)
names(Cont_Summary_Day)
#484'648

#Generate zero 
#add duration and repeat 
#Compute total number of repeat (pump, call)
Cont_Summary0_Day <- 
  #GENERATE 0
  cont_0(Cont_Summary_Day,ObsRefSubjectBehav = ObsRefSubjectBehav_Day) %>% 
  #ADD DURATION
  cont_duration() %>% 
  #ADD REPEAT 
  cont_number() %>% 
  # SUMMARIZE COUNT DURATION REPEAT
  cont_summarize(data = ., DataSource,BehaviourType,ObsRef_Day,Subject,Behaviour,ReceivedValue) %>%
  #RELOCATE
  relocate(DataSource,ObsRef_Day) %>%
  #ARRANGE
  arrange(ObsRef_Day,Subject,Behaviour,ReceivedValue) 
View(Cont_Summary0_Day)
#796'320


View(ObsRefSubjectBehav_Day %>% 
       distinct(DataSource,ObsRef_Day,Subject,Behaviour,ReceivedValue))
#796'320
#No loss of rows in comparison to all possible combinations => all good


################################Bind Instant and continuous behaviours

#Bind Scan and continuous behaviour 
Scan_Data_Day <- bind_rows(Instant_Summary_Day,Cont_Summary0_Day) %>% 
  relocate(DataSource:BehaviourType) %>% 
  relocate(Behaviour:Count,.after=ReceivedValue)
View(Scan_Data_Day)
#1'116'220


################################Join to scan details
#Further cleaning and data check probably has to be done 
names(ScanSession_Day)

Scan_Day <- ScanSession_Day %>% #2333
  #ADD COMPUTED NUMBER SCAN SESSION
  left_join(.,ScanNumber_Max_Day) %>% #2333
  #ADD INDIVIDUAL BEHAVIOUR 
  left_join(.,Scan_Data_Day) %>% #1'116'220
  #ADD COMPUTED NUMBER SCAN INDIVIDUAL
  left_join(.,ScanNumber_Subject_Day)
View(Scan_Day)

