library(janitor)
library(tidyverse)
library(lubridate)


# Open Questions ----------------------------------------------------------

#Which of the steps in the following codes would we prefer to rely on functions?
#possibly all of them? I have no clue to be fair
#ask Colin

# Pairing -----------------------------------------------------------------

Pairing_Info <- tblPairing %>%
  arrange(PairingDate) %>% 
  distinct(AnimalID,Colony,PairingDate,.keep_all = TRUE) %>% #there are duplicate entries (see below. Have asked Dave to correct)
  #GROUP BY ANIMALID
  group_by(AnimalID) %>% 
  mutate(Pairing_IndividualCount_Total = row_number()) %>% #How many time animals have been paired
  ungroup() %>% 
  #UNGROUP
  #GROUP BY ANIMALID, COLONY 
  group_by(AnimalID, Colony) %>% 
  mutate(Pairing_IndividualCount_Group= row_number()) %>% #How many times animals have been paired in that group
  ungroup() %>%   
  #UNGROUP
  #GROUP BY Colony, DATE
  group_by(Colony) %>% 
  mutate(Pairing_GroupCount = dense_rank(PairingDate)) %>% #How many pairing occured in that group
  ungroup() %>% 
  #UNGROUP
  rename(PairingColony = Colony) %>% 
  mutate(PairingDate = ymd(PairingDate))



# Breeding ----------------------------------------------------------------


# LitterRef/MotherID/ParturitionDate
Litter_Female <- tblLitterCode %>% 
  mutate(MotherID = toupper(MotherID)) %>% 
  rename(AnimalID = MotherID) %>% 
  rename(ParturitionDate = BirthDate) %>% 
  select(-Exact)


# LitterRef/FatherID/ParturitionDate
# Note there can be several father within a LitterRef
# Note that all litters may not be in PV_Parentage (30/09/2020 all litters since litterRef 533 born in originally WC colony are missing)
Litter_Male <- tblLitterCode %>% 
  inner_join(.,PV_Parentage,by=("LitterRef")) %>% #inner join to avoid creating NA for LitterRef not in PV_Parentage
  select(LitterRef,FatherID,BirthDate) %>% 
  rename(AnimalID = FatherID) %>%
  rename(ParturitionDate = BirthDate) %>% 
  select(LitterRef,AnimalID,ParturitionDate) %>% 
  filter(!is.na(LitterRef), !(AnimalID %in% c("Unknown ", "Unknown"))) %>% 
  distinct() #because 1 father often sire several offsping in a litter


#Bind rows of female and male LitterRef/AnimalID/ParturitionDate
Litter_MaleFemale <- bind_rows(Litter_Female,Litter_Male)
View(Litter_MaleFemale)
#1056 rows on the 05/10/202
#1108 rows on the 22/03/2021

#Membership of breeders throughout their lives
Breeder_Membership <- Litter_MaleFemale %>%
  distinct(AnimalID) %>% 
  left_join(.,MembershipBetweenV2) %>% 
  left_join(.,tblSex)


#Membership of breeders on the day they produced a litter
Breeders_ParturitionMembership <- Litter_MaleFemale %>% 
  rename(Date = ParturitionDate) %>% 
  membership(.,Membership = MembershipBetweenV2) %>% 
  left_join(.,tblSex) %>% 
  rename(ParturitionColony = Colony) %>% 
  arrange(ParturitionColony,Date) %>% 
  distinct() %>% 
  filter(!(AnimalID == "BEF003" & Date == "2017-04-21" & ParturitionColony == "Virgin Islands"))#because parturition occurred on the day Virgin Island was formed. #I decide that I will retain the row where the animal gave birth in BET001 to keep same row nmber
View(Breeders_ParturitionMembership)
#1108 rows all good
names(MembershipBetweenV2)


#Data check
#Males that had died at parturition
# View(Breeders_ParturitionMembership %>% 
#        filter(ParturitionColony == "DEAD"))


# #Data check
# #Check mismatch Litter_MaleFemale and Breeders_ParturitionMembership
# View(anti_join(Litter_MaleFemale,Breeders_ParturitionMembership))
# #All good


# Litters conception date
# use 88 as gestation length here
Litters_ConceptionDate <- Litter_MaleFemale %>%
  rename(Date = ParturitionDate) %>%
  conception(.,gestationlength = 88) %>%
  select(LitterRef,AnimalID,ConceptionDate)


# Membership at conception (it still could be that they had emigrated)
Breeders_ConceptionMembership <- Litters_ConceptionDate %>% 
  rename(Date = ConceptionDate) %>% 
  membership(.,Membership = MembershipBetweenV2) %>% #get membership at conception
  rename(ConceptionDate = Date) %>% 
  rename(ConceptionColony = Colony)
View(Breeders_ConceptionMembership)
#1054 rows loss of 1 row
#This is because L14F011, LitterRef 26 conceived in the field before entering the lab

# #Data Check
# View(anti_join(Litters_ConceptionDate,Breeders_ConceptionMembership))
# #This is because L14F011, LitterRef 26 conceived in the field before entering the lab


#query Pairing date in groups that led females to produce a litter
#I assumed that the gestation length here should be of at least 80 days 
PairingConception <- Litter_MaleFemale %>% 
  left_join(.,Pairing_Info %>% 
              select(AnimalID,Pairing_IndividualCount_Total,PairingDate,PairingColony)) %>% 
  mutate(PairingParturition_DayDiff = ParturitionDate - PairingDate) %>% 
  filter(PairingParturition_DayDiff > 80) %>% #for a pairing to lead to parturition I assumed 80 days at least would have to be passed (could actually be more 85)
  # GROUP BY LITTER REF, ANIMALID
  group_by(LitterRef,AnimalID) %>% 
  slice(which.min(PairingParturition_DayDiff)) %>%
  ungroup()
# UNGROUP


#################################Join all information
# Sex
# Litter Ref
# Conception date 
# ConceptionMembership 
# LitterRef
# Parturition date 
# Parturition Membership 
# Conception number total (which is equivalent to toal parturion since I neglected abortion)
# Conception number in that group (which is equvalent to parturition in that group)

Litter_Info <- 
  #BREEDER ID - PARTURITION DATE - LITTER 
  Litter_MaleFemale %>% 
  #ADD SEX
  left_join(.,tblSex) %>% 
  #ADD PAIRING THAT LED TO CONCEPTION
  left_join(.,PairingConception) %>%
  #ADD CONCEPTION DATE
  left_join(.,Litters_ConceptionDate) %>% 
  rename(OffspringLitterRef = LitterRef) %>% #in order not to confuse with focal animalLitterRef)
  #ADD CONCEPTION COLONY
  left_join(.,Breeders_ConceptionMembership) %>%
  #GROUP BY ANIMALID
  #ADD INDIVIDUAL COUNT OF LITTERS PRODUCED
  group_by(AnimalID) %>% 
  mutate(IndividualConceptionCount_Total = row_number()) %>% 
  ungroup() %>% 
  #UNGROUP
  #GROUP BY ANIMALID, CONCEPTION COLONY
  #ADD INDIVIDUAL COUNT OF LITTER PRODUCED IN COLONY
  group_by(AnimalID,ConceptionColony) %>% 
  mutate(IndividualConceptionCount_Colony = row_number()) %>% #Conception number since arrival in colony
  ungroup() %>% 
  #UNGROUP
  #GROUP BY CONCEPTION COLONY
  #ADD COUNT OF LITTER PRODUCED IN COLONY
  group_by(ConceptionColony) %>% 
  mutate(ColonyConceptionCount_Colony = dense_rank(ConceptionDate)) %>% #
  ungroup() %>% 
  #UNGROUP
  left_join(.,Breeders_ParturitionMembership %>% #Parturition Colony
              rename(ParturitionDate = Date) %>% 
              select(-Sex)) %>% #change name to allow join; one row added! To check
  #GROUP BY ANIMALID, PARTURITION COLONY
  group_by(AnimalID,ParturitionColony) %>% 
  mutate(IndividualParturitionCount_Colony = row_number()) %>% #Conception number since arrival in colony
  ungroup() %>% 
  #UNGROUP
  #GROUP BY CONCEPTION COLONY
  #ADD COUNT OF LITTER CONCEIVED IN COLONY
  group_by(ParturitionColony) %>% 
  mutate(ColonyParturitionCount_Colony = dense_rank(ParturitionDate)) %>% #Conception number since arrival in colony
  ungroup() %>% 
  #UNGROUP
  relocate(Sex,.after = AnimalID) %>% 
  relocate(ParturitionDate,.before=ParturitionColony) %>% 
  distinct() %>% 
  arrange(AnimalID)
View(Litter_Info)
#A datacheck must be done on that table
#NA conception colony
#NA Parturition colony; For example I think ROF001 who gave birth in TST002 returns a NA, in males it could indicate that the male was dead at parturition?
View(Litter_Info)



#Filter information about first conception
Litter_Info_First <- Litter_Info %>% 
  filter(IndividualConceptionCount_Total == 1) %>% 
  rename(FirstConceptionDate = ConceptionDate,
         FirstConceptionColony = ConceptionColony,
         FirstParturitionDate = ParturitionDate,
         FirstParturitionColony = ParturitionColony)
names(Litter_Info_First)


#Filter information about last conception
Litter_Info_Last <- Litter_Info %>% 
  #GROUP BY 
  group_by(AnimalID) %>% 
  slice_max(IndividualConceptionCount_Total) %>% 
  ungroup() %>% 
  #UNGROUP
  rename(LastConceptionDate = ConceptionDate,
         LastConceptionColony = ConceptionColony,
         LastParturitionDate = ParturitionDate,
         LastParturitionColony = ParturitionColony,
         LastIndividualConceptionCount_Total = IndividualConceptionCount_Total)



# Animal ID ---------------------------------------------------------------

#Developing Colony
#Currently 40 days after birth 
DevelopingColony <- qry_BirthDate %>% 
  select(-AnimalRef) %>% 
  rename(Date = BirthDate) %>% 
  mutate(Date = Date + 40) %>%  #I assume that after 40 days no animals were moved out of group until they were adult 
  membership(.,Membership =MembershipBetweenV2) %>% 
  rename(DevelopingColony = Colony) %>% 
  select(-Date)
View(DevelopingColony)
#1654 entries
#Some animals have no developing colonies a possibility being that they are still developing and birth date + 40 pushes them in the future for which colony is unknown.


#Birth Colony
BirthColony <- qry_BirthDate %>% 
  select(-AnimalRef) %>% 
  rename(Date = BirthDate) %>% 
  membership(.,Membership = MembershipBetweenV2) %>% 
  filter(Colony != "DEAD") %>% 
  rename(BirthColony = Colony)
#1678 entries, no loss of row
names(BirthColony)

names(qry_BirthDate)


#Generate AnimalInfo
#this is the same than the function idinfo_static()
#I am wondering if I should not add the first conception and parturition data and stuff there (although it is more related to breeding information, they are definitely static)
#generally ask Clin what is best practice
Animal_Info <-tblAnimalID %>% 
  select(AnimalID,WildCaught,Queen,LitterRef) %>% #add WildCaught, Queen and LitterRef info
  left_join(.,tblSex) %>% 
  left_join(.,qry_BirthDate %>% 
              select(-AnimalRef)) %>% #add BirthDate%>% 
  left_join(.,BirthColony) %>% 
  left_join(.,FirstColony) %>% 
  left_join(.,DevelopingColony) %>% 
  left_join(.,Litter_Info_First %>% 
              select(AnimalID,FirstConceptionDate,FirstConceptionColony,FirstParturitionDate, FirstParturitionColony)) %>% 
  left_join(.,Litter_Info_Last %>% 
              select(AnimalID,LastConceptionDate,LastConceptionColony,LastParturitionDate,LastParturitionColony)) %>% 
  left_join(.,qry_DeathDate %>% 
              select(-AnimalRef)) %>% #add death dates
  mutate(DeathAge = DeathDate-BirthDate) %>%  #add age of death
  relocate(Sex,.after=AnimalID) %>% 
  distinct(AnimalID,.keep_all = TRUE)#to remove duplicated row of animal ID due to animal changing group at the date defined to query developing group
#returns 3 more rows than in AnimalID


# Urine/Plasma Samples Info Generation ------------------------------------

#Format urine collected to bind with plasma
#Does not contain level = "Stress"
Urine_AnimalID_Date  <- Urine_Collected %>% 
  select(Colony,AnimalID,SampleType,UrineDate,UrineNumber) %>% 
  rename(CollectionDate = UrineDate, SampleID = UrineNumber)


#Format plasma collected to bind with urine
Plasma_AnimalID_Date <- Plasma_Collected %>% 
  select(Colony,AnimalID,SampleType,PlasmaDate,SampleID) %>% 
  rename(CollectionDate = PlasmaDate)


#Join plasma and urine collected 
PlasmaUrine_Collected <- bind_rows(Urine_AnimalID_Date,Plasma_AnimalID_Date) %>% 
  #GROUP BY ANIMALID, COLLECTION DATE, SAMPLE TYPE
  group_by(Colony,AnimalID,CollectionDate,SampleType,) %>% 
  mutate(SampleRepeat = row_number()) %>% #generate a number of repeat for that day and sample type
  ungroup()
#UNGROUP
#35'308 samples collected on the 05/10/2020, after exclusion of stress samples


#Membership of AnimalID on days plasma and urine samples were collected
PlasmaUrine_Membership <- membership(PlasmaUrine_Collected %>% 
                                       distinct(AnimalID,CollectionDate) %>% 
                                       rename(Date = CollectionDate), MembershipBetweenV2) %>% 
  select(-ColonyLocation) %>% 
  rename(Colony_Queried = Colony,
         CollectionDate = Date)


#Plasma and urine collected on day animals changed groups (thus double Membership)
PlasmaUrine_DoubleMembership <- PlasmaUrine_Membership %>% 
  #GROUP BY 
  group_by(AnimalID,CollectionDate) %>% 
  mutate(ColonyCount = n_distinct(Colony_Queried)) %>% 
  filter(ColonyCount > 1) %>% 
  ungroup() %>% 
  #UNGROUP
  mutate(GroupChangeDay = "Yes") %>% 
  select(-ColonyCount,-Colony_Queried)
#470 rows (235 animalID/date combinations) for which there is a double entry, meaning sample was collected when animal changed group
#This would cascade in errors in group compositions on the day of sampling


#combine 
PlasmaUrine <- PlasmaUrine_Collected %>% 
  left_join(.,PlasmaUrine_Membership) %>% 
  left_join(.,PlasmaUrine_DoubleMembership) %>% 
  replace_na(list(GroupChangeDay = "No")) %>% 
  relocate(Colony_Queried, .after = Colony) %>% 
  relocate(CollectionDate, .after = AnimalID) %>% 
  relocate(GroupChangeDay, .after = CollectionDate) %>% 
  relocate(SampleID,.after = SampleRepeat)



# Group breeding status info generation -----------------------------------


##################Group composition

#Get the dates during which colony existed 
Colony_MinMaxDates <- MembershipBetweenV2 %>% 
  filter(!is.na(MemberTo), Colony != c("AllColonies", "","AllColonies "),Colony !="DEAD") %>% 
  group_by(Colony) %>% 
  summarise(FirstDate = min(MemberFrom), 
            LastDate  = max(MemberTo))


#Min and Max dates for each colony in long format
#This will be useful at later stage
Colony_MinMaxDates_Long <- Colony_MinMaxDates %>% 
  pivot_longer(!Colony,names_to = "Date_Type", values_to = "Date") %>% 
  arrange(Colony,Date,Date_Type) #to put Last Date before Transition Date if a colony has the two breeders removed on exctinction date
View(Colony_MinMaxDates_Long)


# generate group composition for every day a colony existed in the lab
#this relies on PV_Parentage being updated or else some info on father's date of conception will be missing
ColonyComposition_AllDay <- Colony_MinMaxDates %>% 
  #GROUP BY COLONY
  group_by(Colony) %>% 
  tidyr::expand(Date = seq.Date(from = FirstDate, to = LastDate, "day")) %>% #create a sequence of dates from first date to last date of colony
  ungroup() %>% 
  #UNGROUP
  groupcomp(.,DF2 = MembershipBetweenV2) #use groupcomp() to get the members of groups 



###################First Conception within group 
#do the same with parturition if it is the criteria we want to use to determine when an individual became a breeder

FirstIndividualConception_Group <- Litter_Info %>% 
  #GROUP BY ANIMALID, CONCEPTION COLONY
  group_by(AnimalID, ConceptionColony) %>% 
  slice(which.min(ConceptionDate)) %>% 
  ungroup() %>% 
  #UNGROUP
  mutate(AnimalID = case_when(LitterRef == 364 ~ "JA1M001",
                              TRUE ~ AnimalID)) %>% #this is an artifice to assign a correct breeding status to Virgin Islands but the identity of breeding male may no be correct
  mutate(ConceptionDate = case_when(AnimalID %in% c("Z1M015","Z1M016") ~ ymd("2013-12-26"),
                                    TRUE ~ ConceptionDate)) #artifice to asign a correct DateFrom to Both in Zunki 1 (Since we did not have parentage of first litter. The same could occurr for other groups but have not checked yet)


###################Breeding status transition

#we make the assumption that an individual is a breeder from the moment it has conceived. 
# An alternative would be to assume that a queen is present from the moment she gives birth (the other individual must know she is a queen).
#Breeder number is based on how many individuals of a given sex that have already conceived are present in that group. This include femal that may have gotten pregnant as a consequence of temporary male immigration. 
# Thus, a queen or queenless group can get back to breeding as a result of immigration or inbreeding
#Wild-caught group with a queen and or a king will return 0 breeder before the conception of the first litter
#a possible issue with this is that if a new breeder conceived exactly on the date that the previous breeder disappeared the number of breeding female would stay 1 though the id of the breeding female would be different


#get the number of breeding female in a group 
BF_AnimalID_Date <- ColonyComposition_AllDay  %>% 
  left_join(.,FirstIndividualConception_Group %>% 
              select(AnimalID,Sex,ConceptionColony,ConceptionDate) %>% 
              filter(Sex == "F"), by=c("AnimalID" = "AnimalID", "Colony" = "ConceptionColony")) %>% 
  distinct() %>% #to eliminate duplicate due to Animal ID being part of two colonies on conception date
  mutate(BreederFemale_Presence = case_when(is.na(ConceptionDate) ~ as.Date(NA), 
                                            ConceptionDate > Date ~ as.Date(NA), 
                                            ConceptionDate <= Date ~ as.Date(ConceptionDate))) %>% 
  #GROUP BY COLONY, DATE
  group_by(Colony, Date) %>% 
  mutate(n.female.breeders = sum(!(is.na(BreederFemale_Presence)))) %>% 
  ungroup() # takes about 1 minute
#UNGROUP 


#Collapse de data of newdat1 per date for each group
#QueenPresence will return a NA in a group until a female has conceived in that group. Tis means that a female that has bred in a group and enters a new group is not considered a breeder until she conceives again. This seems right for the purpose of the code and must be kept in mind 
#due to a limitation in code of BF_AnimalID_Date, a change of breeding female would go unotice if a new breeding femal conceive on the day the previous one disappear
BF_Group_Date <- BF_AnimalID_Date %>%
  select(Colony, Date, n.female.breeders) %>% 
  distinct() %>% 
  #GROUP BY COLONY
  group_by(Colony) %>% 
  mutate(change.female.breeders = n.female.breeders - lag(n.female.breeders)) %>% 
  mutate(QueenPresence = case_when(n.female.breeders >= "1" ~ "Yes",
                                   change.female.breeders == "-1" & n.female.breeders == "0" ~ "No", 
                                   TRUE ~ as.character(NA))) %>% 
  mutate(QueenPresence  = zoo::na.locf(QueenPresence, na.rm = FALSE)) %>% #To replace all NA following a tranistion by the most recent non-NA prior to it (the transition)
  # UNGROUP
  ungroup() %>% 
  mutate(TransitionType_F = case_when(change.female.breeders == "-1" & n.female.breeders == "0" ~ "QueenLoss",
                                      change.female.breeders == "1" & n.female.breeders > "0" ~ "NewQueen",
                                      TRUE ~ as.character(NA))) %>% #generate a Transition Type
  data.frame() 


#groups have become queenless (the loss of a queen brought the nb of queen to zero)
QueenlessGroups <- BF_Group_Date %>%
  filter(TransitionType_F == "QueenLoss") %>%
  arrange(Colony)
View(QueenlessGroups)
#42 occurences where a group lost its queen
#some of these sequences may be very short


#Identity of animals removed at each transition
QueenLoss_AnimalID <- QueenlessGroups %>%
  mutate(DateTo = Date - 1) %>%
  left_join(.,Breeder_Membership %>%
              filter(Sex == "F"),by=c("DateTo" = "MemberTo", "Colony" = "Colony")) %>%
  select(Colony,Date,TransitionType_F,AnimalID, MemberFrom,DateTo)
View(QueenLoss_AnimalID)


#Repeat the code done in females for males
BM_AnimalID_Date<- ColonyComposition_AllDay  %>% 
  left_join(.,FirstIndividualConception_Group %>% 
              select(AnimalID,Sex,ConceptionColony,ConceptionDate) %>% 
              filter(Sex == "M"), by=c("AnimalID" = "AnimalID", "Colony" = "ConceptionColony")) %>% 
  distinct() %>% #to eliminate duplicate due to Animal ID being part of two colonies on conception date
  mutate(BreederMale_Presence = case_when(is.na(ConceptionDate) ~ as.Date(NA), 
                                          ConceptionDate > Date ~ as.Date(NA), 
                                          ConceptionDate <= Date ~ as.Date(ConceptionDate))) %>% 
  #GROUP BY COLONY, DATE
  group_by(Colony, Date) %>% 
  mutate(n.male.breeders = sum(!(is.na(BreederMale_Presence)))) %>% 
  ungroup() # takes about 1 minute
#UNGROUP 


BM_Group_Date <- BM_AnimalID_Date %>%
  select(Colony, Date, n.male.breeders) %>% 
  distinct() %>% 
  #Group BY COLONY
  group_by(Colony) %>% 
  mutate(change.male.breeders = n.male.breeders - lag(n.male.breeders)) %>% #Not sure I understand this part. how is that different from  (n.female.breeders - lag(n.female.breeders))
  mutate(KingPresence = case_when(n.male.breeders >= "1" ~ "Yes",
                                  change.male.breeders == "-1" & n.male.breeders == "0" ~ "No", 
                                  TRUE ~ as.character(NA))) %>% 
  mutate(KingPresence  = zoo::na.locf(KingPresence, na.rm = FALSE)) %>% #To replace all NA following a tranistion by the most recent non-NA prior to it (the transition)
  ungroup() %>% 
  #UNGROUP
  mutate(TransitionType_M = case_when(change.male.breeders == "-1" & n.male.breeders == "0" ~ "KingLoss",
                                      change.male.breeders >= "1" & n.male.breeders > "0" ~ "NewKing",
                                      TRUE ~ as.character(NA))) %>% #generate a Transition Type
  data.frame() 

View(BM_Group_Date %>% 
       filter(Colony == "Zunki 1"))


##Data check to be done like in females
#groups have become queenless (the loss of a queen brought the nb of queen to zero)
KinglessGroups <- BM_Group_Date  %>%
  filter(TransitionType_M == "KingLoss") %>%
  arrange(Colony)
View(KinglessGroups)
#50 occurences where a group become kingless
#some of these sequences may be very short


View(KinglessGroups %>%
       group_by(Colony) %>%
       filter(n() > 1) %>%
       ungroup())
# 7 groups became kingless more than once, none of these groups are originally WC


#Identity of animals removed at each transition
KingLoss_AnimalID <- KinglessGroups %>%
  mutate(DateTo = Date - 1) %>%
  left_join(.,Breeder_Membership %>%
              filter(Sex == "M"),by=c("DateTo" = "MemberTo", "Colony" = "Colony")) %>%
  select(Colony,Date,TransitionType_M,AnimalID, MemberFrom,DateTo)


# Bind females and males transition info 
# with the group size at the time of the transition
Group_BreederPresence <- BF_Group_Date %>% 
  left_join(BM_Group_Date, by = c("Colony", "Date")) %>% #join male info to female info
  mutate(BreederPresence_Status = case_when(is.na(QueenPresence) | is.na(KingPresence) ~ as.character(NA), #any groups before an individual conceived in that group
                                            QueenPresence == "Yes" & KingPresence == "Yes" ~ "Both", #does here not mean the group is breeding
                                            QueenPresence == "No" & KingPresence == "Yes" ~ "Queenless",
                                            QueenPresence == "Yes" & KingPresence == "No" ~ "Kingless", 
                                            QueenPresence == "No" & KingPresence == "No" ~ "Breederless"))
View(Group_BreederPresence)


# using  all the created datasets we can finally generate a table that breaks it up into the time groups spend in a given state
#Tranistions dates return the date after the transition is complete
Transition_Dates <- Group_BreederPresence  %>%
  replace_na(list(BreederPresence_Status = "PriorInGroupConception")) %>% 
  select(Colony, Date, BreederPresence_Status, TransitionType_F, TransitionType_M) %>%
  #GROUP BY COLONY
  group_by(Colony) %>% 
  mutate(BreederPresence_Status_Last = lag(BreederPresence_Status)) %>% 
  mutate(Transition = case_when(BreederPresence_Status == BreederPresence_Status_Last ~ as.character("N"), 
                                BreederPresence_Status != BreederPresence_Status_Last  ~ as.character("Y"))) %>%
  #UNGROUP
  ungroup() %>% 
  filter(Transition == "Y")#Only retain transition dates


#Add Min, Max and Transition dates in the same tibble
MinTransitionMax_Dates <- bind_rows(Colony_MinMaxDates_Long, Transition_Dates %>% 
                                      mutate(Date_Type = "Transition") %>% 
                                      select(Colony,Date_Type,Date)) %>% 
  arrange(Colony,Date)


#Create date interval with status for all lab colonies
# I would still need to add inforamtion of breeder gain, not only loss
StatusPeriod_AllGroups <- MinTransitionMax_Dates %>% 
  left_join(.,Transition_Dates) %>%  
  distinct(Colony,Date,.keep_all = TRUE) %>% # 
  #%>% 
  #select(Date,Colony,BreederPresence_Status,BreederPresence_Status_Next)) %>% 
  #GROUP BY COLONY
  group_by(Colony) %>% 
  mutate(DateTo = lead(Date)-1) %>% 
  ungroup() %>%
  #UNGROUP 
  #filter(!(is.na(DateTo))) %>% #to eliminate the last row of each group where DateTo will be NA
  #GROUP BY COLONY
  group_by(Colony) %>% 
  mutate(Group_BreederStatus_Interval = lead(BreederPresence_Status_Last)) %>% #assign status to each interval. Only the last interval will be assigned a NA
  ungroup() %>% 
  #UNGROUP
  rename(DateFrom = Date) %>% 
  rename(Group_BreederStatus = Group_BreederStatus_Interval) %>% 
  mutate(Group_BreederStatus = case_when(is.na(lead(BreederPresence_Status_Last)) ~ BreederPresence_Status,
                                         TRUE ~ Group_BreederStatus)) %>% #add the status of the last interval
  mutate(Group_BreederStatus_Duration = DateTo - DateFrom) %>% 
  left_join(.,QueenLoss_AnimalID %>% 
              select(Colony, AnimalID,Date),by = c("Colony" = "Colony", "DateFrom" = "Date")) %>% #add ID of QueenLoss, would also need to add QueenAddition
  left_join(.,KingLoss_AnimalID %>% 
              select(Colony, AnimalID,Date),by = c("Colony" = "Colony", "DateFrom" = "Date")) %>% #add ID of kingLoss, would also need to add KingAddition
  select(-Transition, -BreederPresence_Status_Last, -BreederPresence_Status) %>% 
  rename(BF_ID = AnimalID.x,
         BM_ID = AnimalID.y)

View(StatusPeriod_AllGroups)
