#This is what aims to replace the data import part of DMR functions. At the moment I have only transferred what I have had to use (thus not the behaviours) 


# load package ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(RMySQL)
library(getPass)


# Connect to DB -----------------------------------------------------------

#this functions allows you to connect to the database from R
db_connect<-function(username,local=0){
  library(getPass)
  library(RMySQL)
  
  if(local == 0){
    hostname <- "kalahariresearch.org"
  }
  else{
    hostname <- "192.168.11.6"
  }
  
  con<-dbConnect(MySQL(), user = username,password = getPass(),  
                 dbname = 'Moleratdatabase', host = hostname)
  return(con)
}


#connect to DB, local = 1 if at project or local = 0 if AMS server  
con<-db_connect(username = 'philippev',local=0)

# killDbConnections <- function () {
#   
#   all_cons <- dbListConnections(MySQL())
#   
#   print(all_cons)
#   
#   for(con in all_cons)
#     +  dbDisconnect(con)
#   
#   print(paste(length(all_cons), " connections killed."))
#   
# }
# 
# killDbConnections()


# Life History ------------------------------------------------------------

# Extract information from database. Would there be a way to import all databases at once?
# would we really want having to always load the database? I think there should just be a function that could be run once from the package to call all db objects

#Extract membership from the database
MembershipBetweenV2 <- con %>%
  dbGetQuery ("SELECT 
    AnimalRef,
    AnimalID,
    DATE(MemberFrom) AS MemberFrom,
    DATE(MemberTo) AS MemberTo,
    MembershipBetweenV2.ColonyRef,
    MemberDays,
    MembershipBetweenV2.Colony,
    tblColonyCodes.ColonyLocation
    -- MembershipBetweenV2.CurrentPop -- Not the same as ColonyLocation, as it returns for all rows of membershipV2 where the animal currently is
FROM
    MoleratViews_Pending.MembershipBetweenV2
LEFT JOIN
    Moleratdatabase.tblColonyCodes ON MoleratViews_Pending.MembershipBetweenV2.ColonyRef = tblColonyCodes.ColonyRef
WHERE MembershipBetweenV2.Colony <> 'Exported_Nigel'
AND MembershipBetweenV2.Colony <> 'Exported_Conny'
-- AND MembershipBetweenV2.CurrentPop = 'L'") %>% #CurrentPop indicates where the animal is and not where it was. Therefore if one wants to return groupSize in the past
  mutate(MemberFrom=ymd(MemberFrom),MemberTo=ymd(MemberTo)) %>% 
  select(AnimalRef,AnimalID,MemberFrom,MemberTo,Colony,ColonyLocation)
View(MembershipBetweenV2)

#AnimalID
tblAnimalID <- con %>%
  dbGetQuery ("SELECT * FROM Moleratdatabase.tblAnimalID")

#Sex
tblSex<-con %>% 
  dbGetQuery("SELECT *
FROM Moleratdatabase.tblSex
") %>% 
  select(AnimalID,Sex)

#Weights
#includes escape weight
tblWeights<-con %>% 
  dbGetQuery("SELECT * FROM user_philippev.Weight_AnimalID") %>% 
  mutate(WeightDate=ymd(WeightDate)) %>% 
  select(AnimalID,WeightDate,Weight,WeightType) %>% 
  filter(!(is.na(Weight)))


#Birth dates
qry_BirthDate <- con %>%
  dbGetQuery ("SELECT * FROM MoleratViews.qry_BirthDate") %>% 
  rename(BirthDate = LabBirthdate) %>% 
  mutate(BirthDate = ymd(BirthDate))

#death dates
qry_DeathDate <- con %>%
  dbGetQuery ("SELECT * FROM MoleratViews.qry_DeathDate") %>% 
  mutate(DeathDate = ymd(DeathDate))

#tblPairing
#gives pairing date and colony
tblPairing <- con %>%
  dbGetQuery ("SELECT * FROM MR_MainData.tblPairing") %>% 
  mutate(PairingDate = case_when(AnimalID == "DRF015" & Colony == "Roms" | AnimalID == "LAM015" &Colony == "Roms" ~ "2019-03-14",
                                 AnimalID == "WEM011" & Colony == "Vunit" ~ "2019-05-04",
                                 TRUE ~ as.character(PairingDate))) %>% 
  mutate(PairingDate=ymd(PairingDate)) 


#ColonyCodes
tblColonyCodes <- con %>%
  dbGetQuery ("SELECT * FROM Moleratdatabase.tblColonyCodes") %>% 
  select(Colony,ColonyOrigin,ColonyLocation)


#Litter code
tblLitterCode <- con %>%
  dbGetQuery ("SELECT LitterRef,
MotherID,
DATE(DateOfBirth) AS BirthDate,
Exact
FROM Moleratdatabase.tblLitterCode") %>% 
  mutate(BirthDate = ymd(BirthDate)) %>% 
  filter(!(LitterRef %in% c(31,139))) %>%  #Field population animals
  filter(!(LitterRef %in% c(383,384,392))) #Litters with no date of birth and zero pups. Posted on Gitlab



##############From userphilippev

#Escapes
Escape <- con %>%
  dbGetQuery ("SELECT * FROM user_philippev.Escape") %>% 
  mutate(Escape_Date = ymd(Escape_Date)) %>% 
  rename(EscapeDate = Escape_Date) %>% 
  arrange(EscapeDate,AnimalID) %>% 
  mutate(EscapeRef = row_number())

#Experiments
Experiments_SubjectID <- con %>%
  dbGetQuery ("SELECT * FROM user_philippev.Experiments_SubjectID") %>% 
  mutate(ExperimentStartDate = ymd(ExperimentStartDate))

#parentage 
#updated until litter 536, should be updated
PV_Parentage <- con %>%
  dbGetQuery ("SELECT * FROM user_philippev.PV_Parentage") %>% 
  mutate(FatherID = case_when(FatherID == "Z1M007 " ~ "Z1M007",
                              FatherID == "G7M028 " ~ "G7M028",
                              LitterRef == 15 ~ "Z3M010",
                              MotherID == "HEF003" ~ "Unknown",
                              LitterRef == 565 ~ "JA1M003",
                              TRUE ~ FatherID))#To correct a wrong entry, will have to be corrected in next update


#First Colony
#For Captive bred it will be equal to birth colony
FirstColony <- con %>%
  dbGetQuery ("SELECT AnimalID,
Colony AS FirstColony
FROM MoleratViews_Pending.FirstColony")



# Scan --------------------------------------------------------------------


#########################Behaviour List
#Not sure how I will tackle this. It will depends on the zero I want create I guess
#will be necessary to generate the zeroes
#How will I bring back the Modifier? Shall I do a manual list based on the grouping I want to do? 

#Instantaneous Behaviour
ScanBehaviour_Instant_List <- con %>% 
  dbGetQuery("SELECT * FROM MR_MainData.tblCodeList
WHERE CodeRef = 'ScanInstantBehav'")
View(ScanBehaviour_Instant_List)

#Continuous Behaviour No Modifier
ScanBehaviour_Cont_List <- con %>% 
  dbGetQuery("SELECT * FROM user_philippev.Scan_BehavModif_Cont")
View(ScanBehaviour_Cont_List)


#########################Scan Session
#Must made a GitLab entries for duplicates Session

#Old import in moleratdatabase
#Date shown with time. Made a Gitlab entry and go around for now
tblScanSessionDetails <- con %>% 
  dbGetQuery("SELECT tblScanSessionDetails.*,
              tblColonyCodes.Colony,
              DATE(StartDate) AS ObsDate
              FROM Moleratdatabase.tblScanSessionDetails
              LEFT JOIN Moleratdatabase.tblColonyCodes ON tblScanSessionDetails.ColonyRef = tblColonyCodes.ColonyRef") %>% 
  rename(ObsRef = ScanRef) %>% 
  mutate(ObsDate = ymd(ObsDate),
         ObsType = "Scan")

#Old import after transformation to make it compatible with new import
#Date shown with time. Made a Gitlab entry and go around for now
qry_ScanSession_Mrdb <-con %>%
  dbGetQuery("SELECT qry_ScanSession_Mrdb.*,
             tblColonyCodes.Colony,
             DATE(StartDate) AS ObsDate,
             TIME(StartTime) AS ObsTime
             FROM MR_RawData.qry_ScanSession_Mrdb
             LEFT JOIN Moleratdatabase.tblColonyCodes ON qry_ScanSession_Mrdb.ColonyRef = tblColonyCodes.ColonyRef") 
View(qry_ScanSession_Mrdb)
#1650


qry_ScanSession_MrRaw <- con %>%
  dbGetQuery("SELECT qry_ScanSession_MrRaw.*,
              tblColonyCodes.Colony,
              DATE(StartDate) AS ObsDate,
              TIME(StartTime) AS ObsTime
              FROM MR_RawData.qry_ScanSession_MrRaw
              LEFT JOIN Moleratdatabase.tblColonyCodes ON qry_ScanSession_MrRaw.ColonyRef = tblColonyCodes.ColonyRef") %>% 
  distinct() #This must be fixed in DB. Post an issue in Gitlab
View(qry_ScanSession_MrRaw)
#2525


#Old and new import 
#The size of the tunnel system is still missing. Had posted something on GitLab
#There is no overlap between ScanRef of old and new data as new data starts at 1700, but there could be overalap at some stage
qry_ScanSession_All <- con %>%
  dbGetQuery("SELECT qry_ScanSession_All.*,
              tblColonyCodes.Colony,
              DATE(StartDate) AS ObsDate,
              TIME(StartTime) AS ObsTime
              FROM MR_RawData.qry_ScanSession_All
             LEFT JOIN Moleratdatabase.tblColonyCodes ON qry_ScanSession_All.ColonyRef = tblColonyCodes.ColonyRef") %>% 
  distinct()# because of error in MrRaw
View(qry_ScanSession_All)
#4175


#########################Instantaneous
#In none of the prep I have removed the animal that are not part of tblAnimalID. I think it is better to have a good datacheck on this on DB and possibly remoe in the data prep file

#Old data summary
#Grouped by PartnerID (only modif available for old data)
#I don't understand why we have added PartnerCorrection and not Subject Correction
qry_ScanInstantSummaryNoModifier_Mrdb <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_ScanInstantSummaryNoModifier_Mrdb")
names(qry_ScanInstantSummaryNoModifier_Mrdb)
#245'079
  
#New import Raw data
qry_ScanInstant_MrRaw <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_ScanInstant_MrRaw")
View(qry_ScanInstant_MrRaw)
#1'016'978

#New import Summary
#Grouped by all modifier
qry_ScanInstantSummary_MrRaw <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_ScanInstantSummary_MrRaw")
View(qry_ScanInstantSummary_MrRaw)
#174'028

#New import summary 
#Grouped by PartnerID
qry_ScanInstantSummaryNoModifier_MrRaw <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_ScanInstantSummaryNoModifier_MrRaw")
View(qry_ScanInstantSummaryNoModifier_MrRaw)
#342'035

#An issue has been posted on GitLab about the possibility to merge data per day of observation for the new data, that would allow to combine with old data while keeping comparable timeline. At the moment _All returns data at different timescale. This is also valid for continuous


#########################Continuous data

#Old data summary
qry_ScanContSummary_Mrdb <-  con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_ScanContSummary_Mrdb") 
View(qry_ScanContSummary_Mrdb)
#178'512

#New data raw
qry_ScanCont_MrRaw <-  con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_ScanCont_MrRaw") 
View(qry_ScanCont_MrRaw)
#114'701

#New data summary
qry_ScanContSummary_MrRaw <-  con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_ScanContSummary_MrRaw")
View(qry_ScanContSummary_MrRaw)
#86'390



# Focal -------------------------------------------------------------------


#########################Behaviour List
#This could be done as a view on DB
FocalBehaviour_List_All <- con %>%
  dbGetQuery("SELECT temp.*,
usys_FocalBehaviourCorrection.BehaviourRaw,
usys_FocalBehaviourCorrection.ReceivedValue
FROM
(SELECT CodeRef,
Label,
Value
FROM MR_MainData.tblCodeList
WHERE CodeRef = 'FocalPointBehav' 
OR CodeRef = 'FocalStateBehav') AS temp
LEFT JOIN MR_RawData.usys_FocalBehaviourCorrection ON temp.Value = usys_FocalBehaviourCorrection.BehaviourValue") %>% 
  mutate(Value = as.integer(Value)) %>% 
  arrange(CodeRef,Value)


#########################Modifier List
#Will be needed to generate the 0
#can inspire myself from the scan


#########################Session
#FocalDate = > ObsDate: Should it be made in the DB directly?
#FocalRef => ObsRef: In Session and data qry
#ObsType => "Focal": Only done for Session. Not sure it is useful at this stage here, as it could simply be added at a later stage, just before it is combined with Focal data

qry_FocalSession_Mrdb <-con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalSession_Mrdb") %>% 
  rename(ObsRef = FocalRef,
         ObsDate = FocalDate) %>% 
  mutate(ObsDate = ymd(ObsDate),
         ObsType = "Focal")

qry_FocalSession_MrRaw <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalSession_MrRaw") %>% 
  rename(ObsRef = FocalFileID, #Inconsistencies, in other session view, it is called FocalRef
         ObsDate = FocalDate) %>% 
  mutate(ObsDate = ymd(ObsDate),
         ObsType = "Focal")

qry_FocalSession_All <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalSession_All") %>% 
  rename(ObsRef = FocalRef,
         ObsDate = FocalDate) %>% 
  mutate(ObsDate = ymd(ObsDate),
         ObsType = "Focal")


#########################State
#Data Source: Only missing for qry_FocalState_MrRaw 
#FocalRef and FocalFileID => ObsRef
#Behaviour Type => "State"

qry_FocalState_MrRaw <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalState_MrRaw") %>% 
  rename(ObsRef = FocalFileID) %>% 
  mutate(DataSource = "MR_RawData", #as it is only missing from that view
         BehaviourType = "State")

qry_FocalStateSummary_Mrdb <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalStateSummary_Mrdb") %>% 
  rename(ObsRef = FocalRef) %>% 
  mutate(BehaviourType = "State")
  
qry_FocalStateSummary_MrRaw <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalStateSummary_MrRaw") %>% 
  rename(ObsRef = FocalFileID) %>% 
  mutate(BehaviourType = "State")

qry_FocalStateSummary_All <- con %>%
               dbGetQuery("SELECT * FROM MR_RawData.qry_FocalStateSummary_All") %>% 
  rename(ObsRef = FocalRef) %>% 
  mutate(BehaviourType = "State")

#Check difference in column 
compare_df_cols(qry_FocalStateSummary_Mrdb,qry_FocalStateSummary_MrRaw,qry_FocalStateSummary_All)

#########################Point
#Data Source: Only missing for qry_FocalPoint_MrRaw
#FocalRef and FocalFileID => ObsRef
#Behaviour Type => "Point"

qry_FocalPoint_MrRaw <- con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalPoint_MrRaw") %>% 
  rename(ObsRef = FocalFileID) %>%
  mutate(DataSource = "MR_RawData",
  BehaviourType = "Point")

qry_FocalPointSummary_Mrdb <-con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalPointSummary_Mrdb") %>% 
  rename(ObsRef = FocalRef) %>%
  mutate(BehaviourType = "Point")

qry_FocalPointSummary_MrRaw<-con %>%
  dbGetQuery("SELECT * FROM MR_RawData.qry_FocalPointSummary_MrRaw") %>% 
  rename(ObsRef = FocalFileID) %>%
  mutate(BehaviourType = "Point")

qry_FocalPointSummary_All <-con %>%
               dbGetQuery("SELECT * FROM MR_RawData.qry_FocalPointSummary_All") %>% 
  rename(ObsRef = FocalRef) %>%
  mutate(BehaviourType = "Point")



# Samples -----------------------------------------------------------------


#Collected Urine samples
#excludes pilot studies and some experimental manipulation studies but I think this should not be done here and more downstream. 
Urine_Collected <- con %>%
  dbGetQuery ("SELECT 
Temp.Colony,
Moleratdatabase.tblAnimalID.AnimalID,
MR_MainData.tblUrineSamples.UrineNumber,
UrineCondition.Label,
Moleratdatabase.tblExpDescript.ExpName,
DATE (MR_MainData.tblUrineSamples.Date) AS UrineDate,
MR_MainData.tblUrineSamples.In,
MR_MainData.tblUrineSamples.Out,
TIME (MR_MainData.tblUrineSamples.Delay) AS UrineDelay,
MR_MainData.tblUrineSamples.UrineVolumeCollected AS VolumeCollected
FROM MR_MainData.tblUrineSamples
LEFT JOIN Moleratdatabase.tblAnimalID ON MR_MainData.tblUrineSamples.AnimalRef = tblAnimalID.RowRef
LEFT JOIN Moleratdatabase.tblExpDescript ON MR_MainData.tblUrineSamples.Experiment = tblExpDescript.ExptRef
LEFT JOIN (SELECT *
FROM Moleratdatabase.tblCodeList
WHERE CodeRef= 'UrineCondition') AS UrineCondition ON MR_MainData.tblUrineSamples.`Condition` = UrineCondition.`Value`
LEFT JOIN MoleratViews_Pending.`MemberShipBetween` AS `Temp` ON `MR_MainData`.`tblUrineSamples`.`Date` BETWEEN `Temp`.`MemberFrom` AND `Temp`.`MemberTo` AND `MR_MainData`.`tblUrineSamples`.`AnimalRef` = `Temp`.`AnimalRef`
ORDER BY UrineNumber") %>% 
  mutate(UrineDate = ymd(UrineDate)) %>% 
  mutate(SampleType = "Urine")


Plasma_Collected <- con %>%
  dbGetQuery ("SELECT 
Moleratdatabase.tblAnimalID.AnimalID,
Temp.Colony, -- return the colony the animal was part of
Moleratdatabase.tblPlasmaSamples.PlasmaNumber AS SampleID,
DATE (Moleratdatabase.tblPlasmaSamples.SampleDate) AS PlasmaDate,
TIME (Moleratdatabase.tblPlasmaSamples.Time) AS PlasmaTime,
Moleratdatabase.tblPlasmaSamples.Delay,
Moleratdatabase.tblPlasmaSamples.Volume AS `Plasma Volume`,
Moleratdatabase.tblExpDescript.ExpName AS PlasmaExp
FROM Moleratdatabase.tblPlasmaSamples 
LEFT JOIN Moleratdatabase.tblAnimalID ON Moleratdatabase.tblPlasmaSamples.AnimalID = tblAnimalID.AnimalID
LEFT JOIN Moleratdatabase.tblExpDescript ON Moleratdatabase.tblPlasmaSamples.Experiment = tblExpDescript.ExptRef
LEFT JOIN Moleratdatabase.tblTeloSample ON Moleratdatabase.tblPlasmaSamples.PlasmaNumber = tblTeloSample.TeloNumber
LEFT JOIN MoleratViews_Pending.`MembershipBetweenV2` AS `Temp` ON `Moleratdatabase`.`tblPlasmaSamples`.`SampleDate` BETWEEN `Temp`.`MemberFrom` AND `Temp`.`MemberTo` AND `Moleratdatabase`.`tblPlasmaSamples`.`AnimalID` = `Temp`.`AnimalID`
WHERE Moleratdatabase.tblPlasmaSamples.PlasmaNumber IS NOT NULL
ORDER BY Moleratdatabase.tblPlasmaSamples.PlasmaNumber") %>% 
  mutate(PlasmaDate = ymd(PlasmaDate)) %>% 
  mutate(SampleType = "Plasma")

#to inform whether samples have already been exported or not
Samples_Exported <- con %>%
  dbGetQuery ("SELECT * FROM user_philippev.Samples_Exported") %>% 
  mutate(Date_Exported = ymd(Date_Exported))


# Hormones ----------------------------------------------------------------
#This will be added as database gets populated


