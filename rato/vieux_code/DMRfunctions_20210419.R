
# Question about function -------------------------------------------------
#is it better to have functions that contains join to fetch other information?
#Is it better to have functions that avoid using joins and whether other information are provided in the forms of argument (1 or several DF must be prepared prior running the function)
#At the moment I have both and am a bit confused about which one is best, currently no real consistencies in my code. Having one would be useful as it would allow me to know



# General consideration ---------------------------------------------------
#would be good to come with a nomenclature with function argument, for example to distinguish data from data variable are tunnel through the environment variable {{}}. When do we use capitals when don't we



# Animal Info Static ------------------------------------------------------
#could be broken into separate function
#does the same job as data preparation file

#IDinfo() extract relevant information from an individual
#data is only a list of individual
#basically return same output as Animal_Info our table from static part of the orange rectangle in power point
idinfo_static <- function(data, AnimalID = AnimalID){
  data %>% 
    rename(AnimalID = {{AnimalID}}) %>% 
    left_join(.,tblAnimalID %>% 
                select(AnimalID,WildCaught,Queen,LitterRef)) %>% #add WildCaught, Queen and LitterRef info
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
}



# Animal Info Dynamic -----------------------------------------------------
# Generally a philosophical question is whether I will 
#i)run idinfo_static() first and join the relevant dates
#ii) or whether I want the dynamic function to be self sufficient and only require AnimalID and Date and starting point
#Should ask Colin about it


#Get the colony an animal was part of at any given date 
#data is a dataframe with 2 column. The first column is AnimalId and the second is Date (must be spelt like that)
#Membership is the object extracted from the database. Do we need to specify as an argument? or not since it will be uploaded from db anyway? or do we want retain some flexibility
#!!!! will return two colonies if an animal was part of 2 colonies on the same day


#Function improvements 
#would be nice to add the flexibility to return MemberFrom and memberTo on demand. Actually probably better to have a different function for this? 
membership <- function(Data, animalid = AnimalID, date = Date, Membership = MembershipBetweenV2) { 
  Data %>%
    inner_join(., Membership %>% 
                 rename("{{animalid}}" := AnimalID)) %>% 
    filter({{date}} >= MemberFrom & {{date}} <= MemberTo) %>% 
    select(-c(AnimalRef,ColonyLocation,MemberFrom,MemberTo))
  }


# membership <- function(data, Membership = MembershipBetweenV2, Joinby = AnimalID, Date = Date) { data %>% 
#     rename (AnimalID = {{animalid}})
#   inner_join(., Membership, by = set_names("AnimalID", nm = ensym(Joinby))) %>% 
#     filter({{Date}} >= MemberFrom & {{Date}} <= MemberTo) %>% 
#     select(-c(AnimalRef,ColonyLocation))
# }


#groupcomp(), extract the group composition of the animals which colony has been established 
#data is the dataframe where a date and a colony are provided
#Membership is MembershipBetweenV2 object obtained from the database
#would be nive to have the possibility to return MemberFrom and memberTo if needed as add that as an alternative 
groupcomp<-function(Data, date = Date, colony = Colony, Membership = MembershipBetweenV2){
  Data %>% 
    distinct({{date}},{{colony}}, .keep_all = TRUE) %>% 
    inner_join(.,Membership) %>% 
    filter({{date}} >= MemberFrom & {{date}} <=MemberTo) %>% 
    select(-AnimalRef, -ColonyLocation,-MemberFrom,-MemberTo)
}


#get_closestweight() returns the closest weight of all individuals present in a colony at a given time. It will only returns a row if a date was collected because a time difference cannot be computed without a date of weight
#DF1 is a dataframe with a column Date and a column AnimalId. will return the closest weight of all group members at a given date if use output groupcomp()
#DF2 is the weight extracted from the database

#Improvements that could be made 
#before, after or absolute could be specified as an argument and variable name would be adjusted

closestweight<-function(Data, animalid = AnimalID, date = Date, Weight = tblWeights){
  Data %>% 
    rename(AnimalID = {{animalid}}) %>% #in case of  a different variable name in data
    inner_join(.,Weight) %>%
mutate(DayDiff = abs(round(difftime(WeightDate,{{date}},units="days")))) %>%
    # GROUP BY ANIMALID, DATE
group_by(AnimalID,{{date}}) %>% 
filter(DayDiff == min(DayDiff)) %>%
ungroup() %>%
    #UNGROUP
    #GROUP BY ANIMALID,DATE,DAYDIFF
group_by(AnimalID,{{date}},DayDiff) %>% 
summarise(Weight=mean(Weight)) %>% 
ungroup() %>%
    #UNGROUP
rename(WeightDayDiff=DayDiff) %>% 
arrange(AnimalID,{{date}})
}

#Try add closest weight 
names(tblWeights)
add_closestweight<-function(Data, animalid = AnimalID, date = Date, Weight = tblWeights){
  Data %>% 
    rename(AnimalID = {{animalid}}) %>% #in case of  a different variable name in data
    inner_join(.,Weight) %>%
    mutate(DayDiff = abs(round(difftime(WeightDate,{{date}},units="days")))) %>%
    select(-WeightType, -WeightDate) %>% 
    # GROUP BY ANIMALID, DATE
    group_by(AnimalID,{{date}}) %>% 
    filter(DayDiff == min(DayDiff)) %>%
    ungroup() %>%
    #UNGROUP
    #GROUP BY ANIMALID,DATE,DAYDIFF
    group_by(AnimalID,{{date}},DayDiff) %>% 
    mutate(Weight=mean(Weight)) %>% 
    distinct() %>% 
    ungroup() %>%
    #UNGROUP
    rename(WeightDayDiff=DayDiff) %>% 
    relocate(WeightDayDiff,.before=Weight)
}






closestweight_after<-function(DF1,DF2 = tblWeights, Date = Date){
  inner_join(DF1 ,DF2, by="AnimalID") %>%
    mutate(DayDiff = round(difftime(WeightDate,{{Date}},units="days"))) %>% 
    filter(DayDiff >= 0) %>% 
    group_by(AnimalID,{{Date}}) %>% 
    filter(DayDiff == min(DayDiff)) %>%
    ungroup() %>% 
    group_by(AnimalID,{{Date}},DayDiff) %>% 
    summarise(Weight=mean(Weight)) %>% 
    ungroup() %>% 
    rename(WeightDayDiff=DayDiff) %>% 
    arrange(AnimalID,{{Date}})
}

closestweight_before<-function(DF1,DF2 = tblWeights, Date = Date){
  inner_join(DF1 ,DF2, by="AnimalID") %>%
    mutate(DayDiff = round(difftime(WeightDate,{{Date}},units="days"))) %>% 
    filter(DayDiff <= 0) %>% 
    group_by(AnimalID,{{Date}}) %>% 
    filter(DayDiff == max(DayDiff)) %>%
    ungroup() %>% 
    group_by(AnimalID,{{Date}},DayDiff) %>% 
    summarise(Weight=mean(Weight)) %>% 
    ungroup() %>% 
    rename(WeightDayDiff=DayDiff) %>% 
    arrange(AnimalID,{{Date}})
}


#age()
#currently written if Birth date is already in the DF, thus it is not self sufficient. I have no clue what is best
#Would be nice to have months or day as an argument. Conditional piping and then output name include this variable
#a more general function would simply be DateDiff, which I basically use all the time
age_inmonths <- function(data, BirthDate = BirthDate , Date = Date){
  data %>%
    mutate(Age_Months = ({{Date}} - {{BirthDate}})/30.4375)
}

age_indays <- function(data, BirthDate = BirthDate , Date = Date){
  data %>%
    mutate(Age_Days = ({{Date}} - {{BirthDate}}))
}


#add age if I have already BirthDate available
add_age <- function(data, birthdate = BirthDate , date = Date, unit = NULL ){
  if (unit == "months"){
    data %>%
    mutate(Age_Months = ({{date}} - {{birthdate}})/30.4375)
} else if (unit == "days") {
    data %>%
    mutate(Age_Days = ({{date}} - {{birthdate}}))
}
}

#Would it be better to join DF1 and DF2 = qryBirthDate? The difference is that iI need add a left_join in the code when wanting to add a column age in a DF, whereas current option allows to pipe the function directly onto the DF
# age_inmonths <- function(DF1, Date = Date, DF2 = qry_BirthDate %>% select(-AnimalRef)){  
#   DF1 %>% 
#     left_join(.,DF2) %>% 
#     mutate(Age_Months = ({{Date}} - BirthDate)/30.4375)
           }


#breedingstatus()
#currently written if ran idinfo_static first
#Animal is considered a breeder once it has succesfully conceived
#However, the function also includes whether the female was originally captured as a breeder. I believe this statement could be removed from the function a
#DF1 can be any data set that has a AnimalID, Date of interest and first conception date, and whether wildcaught queen
#For males, accuracy of information relies on pv_parentage being updated
#Add a paired status 
add_breedingstatus <- function(data, date = Date, firstconceptiondate = FirstConceptionDate){
  data %>% 
    mutate(BreedingStatus = case_when(Queen == 1 | {{date}} - {{firstconceptiondate}} >= 0 ~ "Breeder",
                                      TRUE ~ "Helper"))
}

#if one only wants to consider a female to breed if she has successfully conceived in the lab
add_breedingstatus <- function(data, date = Date, firstconceptiondate = FirstConceptionDate){
    data %>% 
      mutate(BreedingStatus = case_when({{date}} - {{firstconceptiondate}} >= 0 ~ "Breeder",
                                        TRUE ~ "Helper"))
  }


#Would it be better to join DF1 and DF2 = qryBirthDate? The difference is that iI need add a left_join in the code when wanting to add a column age in a DF, whereas option above allows to pipe the function directly onto the DF
#function needs to be written


# Gestation ---------------------------------------------------------------
#A few informations are essential to get the gestation status
#There may be uncertainty of parturition within the last 3 months of life and last 3 months prior to today if animal is still alive
#First Conception date (as we want assign a gestation status only for animals that have conceived once)
#How many days til next parturition 


#gestation() returns the conception date
#DF1 is a list of animalID and parturition date
conception<-function(data, date = Date, gestationlength = 100){
  data %>%
    mutate(ConceptionDate = {{date}} - gestationlength)
}


#NextParturition info
#will be useful to assign gestation status
#But should it be moved into gestation function instead of being separate?
#Also which column from Litter_Info shall I select?
#should probably focus on parturition info and we should have similar function for pairing and conception?
next_parturitioninfo <- function(data, date = Date, animalID = AnimalID){
  data %>% 
    rename(AnimalID = {{animalID}}) %>%
    #INNER JOIN LITTER_INFO
    inner_join(.,Litter_Info %>% 
                select(AnimalID, PairingDate,PairingColony, ConceptionDate,ConceptionColony,ParturitionDate, ParturitionColony,IndividualConceptionCount_Total, OffspringLitterRef)) %>% 
    #RETAIN DATE OF INTEREST 
    filter({{date}} < ParturitionDate) %>% 
    #DAY DIFF 
    mutate(ParturitionDayDiff = {{date}} - ParturitionDate) %>%
    #GROUP BY 
    group_by(AnimalID, {{date}}) %>% 
    slice_max(ParturitionDayDiff) %>% 
    ungroup() %>% 
    #UNGROUP
  rename_with(.cols = c(PairingDate:ParturitionDayDiff),
              ~ paste0("NextParturition_",.))
}


latest_parturitioninfo <- function(data, date = Date, animalID = AnimalID){
  data %>% 
    rename(AnimalID = {{animalID}}) %>%
    #INNER JOIN LITTER_INFO
    inner_join(.,Litter_Info %>% 
                 select(AnimalID, IndividualConceptionCount_Total, OffspringLitterRef, ParturitionDate, ParturitionColony)) %>% 
    #RETAIN DATE OF INTEREST 
    filter({{date}} >= ParturitionDate) %>% 
    #DAY DIFF 
    mutate(ParturitionDayDiff = {{date}} - ParturitionDate) %>%
    #GROUP BY 
    group_by(AnimalID, {{date}}) %>% 
    slice_min(ParturitionDayDiff) %>% 
    ungroup() %>% 
    #UNGROUP
    rename_with(.cols = c(IndividualConceptionCount_Total, OffspringLitterRef, ParturitionDate,ParturitionColony,ParturitionDayDiff),
                ~ paste0("Latest_",.))
}






#{{date}} >= FirstParturitionDate - {{gestationlength}}, #remove dates before first conception as the breeding status of animal is questionable. This is something I am un


#gestationstatus()
#First make the function consider only dates for which gestation status can easily be assigned (before the last parturition)


#Open questions
#Q1) do I want to define a gestation status for all animals in a DF? or do I want the DF defined as argument to only contain BF already? Usually, one only wants the gestation status of animals defined as breeder, but the starting DF is a matter of choice (do we want run function over entire df? left join over subset?)
#Q2) Do I want the function to be able to run on a DF %>% gestatisonstatus() or DF %>% left_join(.,gestationstatus())
#IMPORTANT: The function does not take into account abortion data


#To include at later stage
#gestation length as an additional function argument
#After last conception the gestation status is 
#iia) breeding group, 
## iib) unknown in a kingless group that has lost its king less than 3 months ago, c) not pregnant if the king has disappeared more than 3 months ago. This category could actually be given a special category. For b and c one needs the breeding status of the group, but this could be included


#Alternative 1: idinfostatic has been run first and contains relevant dates
#DF needs a date of relevance, First conception date and 
# gestationstatus <- function(DF, Date = Date, FirstConceptionDate = FirstConceptionDate){
#   DF %>% 
#     mutate(EndDate = case_when(!(is.na(DeathDate)) ~ DeathDate,
#                                TRUE ~ today())) %>% #assign an "End Date for each female
#     left_join(.,Litter_Info %>% 
#                 filter(Sex == "F") %>% 
#                 select(AnimalID, IndividualConceptionCount_Total, OffspringLitterRef, ParturitionDate)) %>% #shall I add conception or parturition group or whatever?
#     mutate(ParturitionCollection_DayDiff = as.integer({{Date}} - ParturitionDate),#delay between date of interest and parturition
#     ) %>% 
#     filter({{Date}} >= FirstParturitionDate -105, 
#            {{Date}} < ParturitionDate |
#              {{Date}} > LastParturitionDate & ParturitionDate == LastParturitionDate) %>%
#     #GROUP BY ANIMALID, DATE
#     group_by(AnimalID, {{Date}}) %>% 
#     slice_max(ParturitionCollection_DayDiff) %>% #I think slice_min(abs(ParturitionCollection_DayDiff)) would work
#     ungroup() %>% 
#     #UNGROUP
#     rename(NextLitterRef = OffspringLitterRef, 
#            NextParturitionDate = ParturitionDate,
#            NextParturitionCollection_DayDiff = ParturitionCollection_DayDiff
#     ) %>% 
#     mutate(GestationStatus = case_when(NextParturitionCollection_DayDiff < -105  ~ "NotPregnant", 
#                                        NextParturitionCollection_DayDiff >= -105 & NextParturitionCollection_DayDiff <= -90 ~ "Conception",
#                                        NextParturitionCollection_DayDiff >= -90 & NextParturitionCollection_DayDiff < -60 ~ "FirstTrimester",
#                                        NextParturitionCollection_DayDiff >= -60 & NextParturitionCollection_DayDiff < -30 ~ "SecondTrimester",
#                                        NextParturitionCollection_DayDiff >= -30 & NextParturitionCollection_DayDiff <=0 ~ "ThirdTrimester",
#                                        NextParturitionCollection_DayDiff > 0 ~ "PostLastParturition"))  %>% 
#     rename(NextLitter = IndividualConceptionCount_Total)
# }

#Alternative 2
#specify how we want split gestation status
#need run static() first
#need run next_parturitioninfo()
#need death date, FirstParturitionDate to get Next_ParturitionDayDiff
#should probably return gestation status only
#Would have to check the case_when order as it must go for most specific to most general
gestationstatus <- function(data, date = Date, deathdate = DeathDate, lastparturitiondate = LastParturitionDate, firstparturitiondate = FirstParturitionDate, next_parturitiondaydiff = Next_ParturitionDayDiff, gestationsplit = NULL, gestationlength = 90){
  
  Temp <- data %>% 
    #ADD ENDDATE TENURE
    mutate(BreedingTenure_EndDate = case_when(!(is.na({{deathdate}})) ~ DeathDate, 
                                              TRUE ~ today())) %>% 
    #ADD STARTDATE UNDETERMINED PERIOD 
    mutate(Gestation_Undetermined_StartDate = case_when(BreedingTenure_EndDate - {{gestationlength}} >= {{lastparturitiondate}} ~ BreedingTenure_EndDate - {{gestationlength}},
                                                        BreedingTenure_EndDate - {{gestationlength}} < {{lastparturitiondate}} ~ {{lastparturitiondate}}))
  
    #SEMESTER 
    if (gestationsplit == "semester"){
      Temp %>% 
        mutate(GestationStatus_Semester = case_when(
          #PRIOR FIRST CONCEPTION
          {{date}} < ({{firstparturitiondate}} - {{gestationlength}}) ~ "PreFirstConception",
          #NOT PREGNANT
          {{next_parturitiondaydiff}} < -{{gestationlength}}  ~ "NotPregnant", 
          #SEMESTER 1
                                           {{next_parturitiondaydiff}} >= - {{gestationlength}} & {{next_parturitiondaydiff}} < -({{gestationlength}}/2) ~ "FirstSemester",
          #SEMESTER 2
                                           {{next_parturitiondaydiff}} >= -({{gestationlength}}/2) & {{next_parturitiondaydiff}} <=0 ~ "SecondSemester",
          #NOT PREGNANT 
                                           is.na({{next_parturitiondaydiff}}) & {{date}} < Gestation_Undetermined_StartDate ~"NotPregnant",
          #UNDETERMINED
                                           is.na({{next_parturitiondaydiff}}) & {{date}} >= Gestation_Undetermined_StartDate ~"Undetermined"))

      #TRIMESTER
      } else if (gestationsplit == "trimester") {
        
  Temp %>% 
    mutate(GestationStatus_Trimester = case_when(
      #PRIOR FIRST CONCEPTION
      {{date}} < ({{firstparturitiondate}} - {{gestationlength}}) ~ "PreFirstConception",
      #NOT PREGNANT
      {{next_parturitiondaydiff}} < -{{gestationlength}}  ~ "NotPregnant", 
      #FIRST TRIMESTER
                                     {{next_parturitiondaydiff}} >= - {{gestationlength}} & {{next_parturitiondaydiff}} < -({{gestationlength}}*(2/3)) ~ "FirstTrimester",
      #SECOND TRIMESTER
                                     {{next_parturitiondaydiff}} >= -({{gestationlength}}*(2/3)) & {{next_parturitiondaydiff}} < -({{gestationlength}}*(1/3)) ~ "SecondTrimester",
      #THIRD TRIMESTER
                                     {{next_parturitiondaydiff}} >= -({{gestationlength}}*(1/3)) & {{next_parturitiondaydiff}} <=0 ~ "ThirdTrimester",
      #NOT PREGNANT 
                                      is.na({{next_parturitiondaydiff}}) & {{date}} < Gestation_Undetermined_StartDate ~"NotPregnant",
      #UNDETERMINED
                                     is.na({{next_parturitiondaydiff}}) & {{date}} >= Gestation_Undetermined_StartDate ~"Undetermined"))
      }
}


# Lactation ---------------------------------------------------------------
#Generate a function that tells whether a female is lactating 
#the most correct way to do this would be 
#Pups of a predetermined age are present 
#The female is knows to have given birth to these pups (As there are 1-2 cases where a previosu breeding female may be present in the group but not the one that gave birth)



# Pairing -----------------------------------------------------------------


#pairinginfo()
#DF1 is a list of AnimalID and Pairing date
#DF2 is the Pairing_Info object generated above
pairinginfo<-function(DF1, DF2 = Pairing_Info){
  DF1 %>% 
    left_join(.,DF2)
}


# functions ideas ---------------------------------------------------------


# metafunction ------------------------------------------------------------

daydiff <- function (data, )
  
next <- function(data1,data2,date1,date2)
  
latest <- 
  
first <- 
  
last <- 


# Group Info --------------------------------------------------------------


#############################################Group size. 

#is it better to have a general function like that
count <- function(data,..., NewVarName = NULL){ 
  data %>% 
    group_by(...) %>% 
    summarize("{{NewVarName}}" := n()) %>% 
    ungroup()
}

#or better like this
groupsize <- function(data, Date = Date, Colony = Colony){ 
  data %>% 
    group_by({{Date}}, {{Colony}}) %>% 
    summarize(GroupSize = n()) %>% 
    ungroup()
}

#or better if the start would only be list of colony and date? 

#I have no clue which function of the two would be better
#Count is obviously more genral


#####################################################Pup 
#Could we generally write a function that counts based on value of argument (Sex, Age, BreedingStatus)

pupnumber <- function(data,...,age = Age_Days, agelimit = 60){
  data %>% 
    group_by(...) %>% 
    summarize("Pup_{{agelimit}}" := sum({{age}}< agelimit, na.rm= TRUE)) %>% 
    ungroup()
}




pupage <- function(data,..., age = Age_Days, agelimit = 60){
  data %>% 
    filter({{age}} <= agelimit) %>% 
    #GROUP BY
    group_by(...) %>% 
    slice_min({{age}}, with_ties = FALSE) %>% 
    ungroup() %>% 
    rename("PupAge_{{agelimit}}" := {{age}}) %>% 
    select(-AnimalID)
  #UNGROUP
}




pupcategory <- 

#####################################################Pup Age


#####################################################Pup category

#DF requires AnimalID, Sex, Age (in days)
#Assumes all wild caught animal are over 18 which is probably wrong (would need check based on weight, or assign a fact age to animal when they entered the lab. I could ask Jack about it as I think he worked stg out for his field work) 

#Would be awesome to have one function that counts whatever we want based on argument. But not sure it is possible

groupinfo <- function(DF, Date = Date, Age = Age){ 
  DF %>% 
  #GROUP BY COLONY, DATE
  group_by(Colony,{{Date}}) %>% 
  mutate(GroupSize=n()) %>% # group size
  mutate(Pup14=sum({{Age}}<15, na.rm= TRUE)) %>% #Pup number 
  mutate(Pup30=sum({{Age}}<31, na.rm= TRUE)) %>% #Pup number 
  mutate(Pup14_Presence=ifelse(Pup14==0,"No","Yes")) %>%  #Pup presence
  mutate(Pup30_Presence=ifelse(Pup30==0,"No","Yes")) %>% 
  mutate(Male12 = sum(Sex == "M" & {{Age}} > 340 |Sex == "M" & is.na ({{Age}}))) %>% 
  mutate(Female12 = sum(Sex == "F" & {{Age}} > 340 |Sex == "F" & is.na ({{Age}}))) %>% 
  mutate(Female18 = sum(Sex == "F" & {{Age}} > 520 |Sex == "F" & is.na ({{Age}}))) %>% 
  ungroup() %>% 
  #UNGROUP 
  distinct(Colony,{{Date}},.keep_all = TRUE) %>% 
  select(Colony, {{Date}}, GroupSize:Female18)
#UNGROUP
}





# Older function
# #GroupID_info() gets additional information on group composition 
# #DF1 is the output from IDInfo()
# #I could probably use a if condition as shown just after  
# 
# idgroup_info<-function(DF1){DF1 %>%
#   #GROUP BY COLONY AND DATE FOR FOLLOWING MUTATE CALLS: GET INDIVIDUAL AND GROUP CHARACTERISTIC 
#   group_by(Colony,Date) %>% 
#   mutate(GroupSize=n()) %>% # group size
#   mutate(WeightRank=min_rank(desc(Weight)), 
#          AgeRank=min_rank(desc(Age))) %>% # Weight rank, check what it does with NA
#   mutate(CompNB_5=sapply(Weight, function(x) sum(abs(x-Weight)<5)-1),
#          CompNB_10=sapply(Weight, function(x) sum(abs(x-Weight)<10)-1),
#          CompNB_15=sapply(Weight, function(x) sum(abs(x-Weight)<15)-1), 
#          CompNB_20=sapply(Weight, function(x) sum(abs(x-Weight)<20)-1)) %>% 
#   mutate(PupNB=sum(Age<1, na.rm= TRUE)) %>% 
#   mutate(PupPresence=ifelse(PupNB==0,"No","Yes")) %>% 
#   mutate(MinAge=min(Age,na.rm= TRUE)) %>% 
#   ungroup() %>% 
#   # UNGROUP
#   # GROUP BY DATE, COLONY AND SEX FOR FOLLOWING MUTATE CALLS
#   group_by(Date,Colony,Sex) %>% 
#   mutate(QueueSize=n()) %>% #queue size, that is number of males and females
#   mutate(WeightRank_Queue=min_rank(desc(Weight))) %>% #Weight rank
#   mutate(CompNB_5_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<5)-1),
#          CompNB_10_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<10)-1),
#          CompNB_15_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<15)-1), 
#          CompNB_20_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<20)-1)) %>% 
#   ungroup() 
#   # UNGROUP
# }




# Instant Count -----------------------------------------------------------

#Total scan number for each subject
scannumber <- function(Data,...,Level = AnimalID){
  Data %>% 
    #GROUP BY 
    group_by(...) %>% 
    summarize("ScanNumber_{{Level}}" := sum(BehaviourCount)) %>% 
    ungroup()
    #UNGROUP
}

scannumber_active <- function(Data,...,Level = AnimalID){
  Data %>% 
    filter(!(Behaviour %in% c("Rest","Huddeling"))) %>% 
    #GROUP BY 
    group_by(...) %>% 
    summarize("ActiveScanNumber_{{Level}}" := sum(BehaviourCount)) %>% 
    ungroup()
  #UNGROUP
}

#Max scan number within session 
scannumber_max<- function(Data,..., ObsLevel){
  Data %>% 
    #GROUP BY 
    group_by(...) %>% 
    summarize(ScanNumber = sum(BehaviourCount)) %>% 
    ungroup() %>% 
    #UNGROUP
    select(-Subject) %>% 
    #GROUP BY
    group_by({{ObsLevel}}) %>% 
    slice_max(ScanNumber, with_ties = FALSE) %>% 
    ungroup() %>% 
    #UNGROUP
    rename("MaxScanNumber_{{ObsLevel}}" := ScanNumber)
}


#Could the two function above be put in the same function?
#I should try this function to see whether it works
#is it of any use?
# scannumber <- function(.data,...){
#   .data %>% 
#     #SUBJECT
#     group_by(...) %>% 
#     summarize(ScanNumber_Subject = sum(BehaviourCount)) %>% 
#     ungroup() %>% 
#     #UNGROUP
#     #SESSION
#     group_by(ObsRef,ObsFileID) %>% #Put as {{}}? 
#     mutate(ScanNumber_Session = max(ScanNumber_Subject)) %>% 
#     ungroup()
#   #UNGROUP
# }



# Instant behav categorize ------------------------------------------------
#It would be good to have a function that allows the recategorization of behaviour based on how many behavioural categories we may want


# Instant behav summary ---------------------------------------------------
#only generate a summary for animals that were observed in the scan
instant_summary <- function(data,...,BehaviourCount = BehaviourCount){
  data %>% 
    #GROUP BY 
    group_by(...) %>% 
    summarize(Count = sum({{BehaviourCount}})) %>% 
    ungroup() %>% 
    relocate(Count,.after=Behaviour)
}


# Cont behav categorize ---------------------------------------------------

#Detail Pass
#could add an argument for a conditional fragmentation?
#Could add an argument in the function to reverse the process
#Could add an argument in the fucbtion that would allow to revert the process (Change = defragemnt. Basically a pipe or another is executed based on function args.I tried breifly biut seems more complex than what I thought. Ask Colin

cont_pass_fragment <- function(data){
  data %>% 
    mutate(Behaviour = case_when(Behaviour == "Pass" & ModifierLabel1 == "Nose-nose" & ModifierLabel2 == "Over" ~ "PassOver",
                                 Behaviour == "Pass" & ModifierLabel1 == "Nose-nose" & ModifierLabel2 == "Sidewards" ~ "PassSidewards", 
                                 Behaviour == "Pass" & ModifierLabel1 == "Nose-nose" & ModifierLabel2 == "Under" ~ "PassUnder",
                                 Behaviour == "Pass" & ModifierLabel1 == "Nose-tail" & ModifierLabel2 == "Over" ~ "PassOver_NT",
                                 Behaviour == "Pass" & ModifierLabel1 == "Nose-tail" & ModifierLabel2 == "Sidewards" ~ "PassSidewards_NT",
                                 Behaviour == "Pass" & ModifierLabel1 == "Nose-tail" & ModifierLabel2 == "Under" ~ "PassUnder_NT",
                                 Behaviour == "Pass" & ModifierLabel1 == "Tail-nose" & ModifierLabel2 == "Over" ~ "PassOver_TN",
                                 Behaviour == "Pass" & ModifierLabel1 == "Tail-nose" & ModifierLabel2 == "Sidewards" ~ "PassSidewards_TN",
                                 Behaviour == "Pass" & ModifierLabel1 == "Tail-nose" & ModifierLabel2 == "Under" ~ "PassUnder_TN",
                                 Behaviour == "Pass" & ModifierLabel1 == "Tail-Tail" & ModifierLabel2 == "Over" ~ "PassOver_TT",
                                 Behaviour == "Pass" & ModifierLabel1 == "Tail-Tail" & ModifierLabel2 == "Sidewards" ~ "PassSidewards_TT",
                                 Behaviour == "Pass" & ModifierLabel1 == "Tail-Tail" & ModifierLabel2 == "Under" ~ "PassUnder_TT",
                                 Behaviour == "Pass" & ModifierLabel1 == "Unspecified" & ModifierLabel2 == "Over" ~ "PassOver_US",
                                 Behaviour == "Pass" & ModifierLabel1 == "Unspecified" & ModifierLabel2 == "Sidewards" ~ "PassSidewards_US",
                                 Behaviour == "Pass" & ModifierLabel1 == "Unspecified" & ModifierLabel2 == "Under" ~ "PassUnder_US",
                                 TRUE ~ Behaviour))
  }

#reverse pass for animals recorded as PartnerID
#Would also be nice to be able to revert if pass has been fragmented differently
cont_pass_invert <- function(.data){
  .data %>% mutate(Behaviour = case_when(Behaviour == "PassOver" ~ "PassUnder",
                                         Behaviour == "PassUnder" ~ "PassOver",
                                         Behaviour == "PassOver_NT" ~ "PassUnder_TN",
                                         Behaviour == "PassSidewards_NT" ~ "PassSidewards_TN",
                                         Behaviour == "PassUnder_NT" ~ "PassOver_TN",
                                         Behaviour == "PassOver_TN" ~ "PassUnder_NT" ,
                                         Behaviour == "PassSidewards_TN" ~ "PassSidewards_NT",
                                         Behaviour == "PassUnder_TN" ~ "PassOver_NT",
                                         Behaviour == "PassOver_TT" ~ "PassUnder_TT",
                                         Behaviour == "PassUnder_TT" ~ "PassOver_TT",
                                         Behaviour == "PassOver_US" ~ "PassUnder_US",
                                         Behaviour == "PassUnder_US" ~ "PassOver_US",
                                         TRUE ~ Behaviour))
    
}


#args could be added in case one wants to recategorize based on other modifier
cont_copulation_fragment <- function(.data){
  .data %>% 
    mutate(Behaviour = case_when(Behaviour == "Copulation" & ModifierLabel1 == "Front" ~ "Copulation_Front",
                                 TRUE ~ Behaviour))
}



# Cont behav summary ------------------------------------------------------

#BehaviourData: Observed data, will typically be a summary from the DB
#BehaviourList: List of behaviours for which we want generate a summary. 
#Behaviour Characteristic: Column of "BehaviourList" that defines whether the behaviour is NonDyadic, Indirected, Directed
#.data and BehaviourList must have matching behaviour name
#bind rows of NoDyad, Indirected dyad as focal, Indirected dyad as Partner, Directed dyad as focal, Directed dyad as partner
#assigns a received value for directed dyad
#only retain animals that are in tblAnimalID => all incorrected 
#
cont_prepare <- function(BehaviourData, BehaviourList, DyadType = DyadType){
  #NO DYADS
  BehaviourData  %>% 
  filter(Behaviour %in% (BehaviourList %>% 
                           filter({{DyadType}} == "NonDyadic") %>% 
                           select(Behaviour) %>% 
                           pull())) %>% 
  select(-PartnerID) %>% 
  
  #DYADS INDIRECTED FOCAL
  bind_rows(., BehaviourData %>% 
              filter(Behaviour %in% (BehaviourList %>% 
                                       filter({{DyadType}} == "Indirected") %>% 
                                       select(Behaviour) %>% 
                                       pull())) %>% 
              select(-PartnerID) %>% 
              mutate(SubjectType = "Focal")) %>% 
  
  #DYADS INDIRECTED PARTNER 
  bind_rows(.,BehaviourData  %>% 
              filter(Behaviour %in% (BehaviourList %>% 
                                       filter({{DyadType}} == "Indirected") %>% 
                                       select(Behaviour) %>% 
                                       pull())) %>% 
              select(-Subject) %>% 
              rename(Subject = PartnerID) %>% 
              mutate(SubjectType = "Partner") %>% 
              #need reverse pass
              cont_pass_invert()) %>% 
  
  #DYADS DIRECTED FOCAL
  bind_rows(.,BehaviourData  %>%
              filter(Behaviour %in% (BehaviourList %>% 
                                       filter({{DyadType}} == "Directed") %>% 
                                       select(Behaviour) %>% 
                                       pull())) %>% 
              select(-PartnerID) %>% 
              mutate(SubjectType = "Focal",
                     ReceivedValue = 1)) %>% 
  
  #DYADS DIRECTED PARTNER
  bind_rows(.,BehaviourData %>% 
              filter(Behaviour %in% (BehaviourList %>% 
                                       filter({{DyadType}} == "Directed") %>% 
                                       select(Behaviour) %>% 
                                       pull())) %>% 
              select(-PartnerID) %>% 
              mutate(SubjectType = "Partner",
                     ReceivedValue = 0)) %>%
  filter(Subject %in% tblAnimalID$AnimalID)#only retain animals that are in DB
}


#Assign duration and total duration
#when Behaviour Count = 0, duration is assigned 0
cont_duration <- function(data,BehaviourCount = BehaviourCount){
  data %>% 
    mutate(Duration = case_when(Behaviour %in% c("Sparr","Groom","Sex Forplay") & BehaviourCount > 0 ~ as.numeric(ModifierValue1),
                                Behaviour %in% c("Sparr","Groom","Sex Forplay") & BehaviourCount == 0 ~ 0)) %>% 
    mutate(TotalDuration = {{BehaviourCount}} * Duration)
}

#Assign repeats within bouts
#when Behaviour Count = 0, repeat is assigned 0
cont_number <- function(data,BehaviourCount = BehaviourCount){
  data %>% 
    mutate(Number = case_when(Behaviour %in% c("Pump","Submiss") & {{BehaviourCount}} > 0 ~ as.numeric(ModifierValue1),
                                Behaviour %in% c("Pump","Submiss") & {{BehaviourCount}} == 0 ~ 0)) %>% 
    mutate(TotalNumber = {{BehaviourCount}} * Number)
}


#generate 0
#.data = dataframe for which we want generate 0
#ObsRefSubjectBehav = list of all combinations of subjects,dates and behaviour
#a potential issue here is that animals that may have wrongly been
cont_0 <- function(data,ObsRefSubjectBehav){
  ObsRefSubjectBehav %>% 
  left_join(.,data) %>% 
  replace_na(list(BehaviourCount = 0,
                  BehaviourType = "Continuous"))
}
  

#generate summary of cont behaviours 
#not sure if better to summarize with a lot of group by or mutate and do the selection of columns after the function
#if move ... at the end it does something very weird with sum of Behaviour Count and multiply the number of ObsRef 
cont_summarize <- function(data,...,BehaviourCount = BehaviourCount, TotalDuration = TotalDuration, TotalNumber = TotalNumber){
  data %>% 
    #GROUP BY 
    group_by(...) %>% 
    summarize (Count = sum({{BehaviourCount}}),#would it be better to have separate function for Count, Duration and number?
            Duration = sum({{TotalDuration}}),
            Number = sum({{TotalNumber}})) %>% 
  ungroup()
}
    #distinct() #%>% 
    #SELECT (UNSURE WHETHER GOOD TO HAVE THAT IN FUNCTION AS COLUMN NAME MAY CHANGE)
    # select(-SubjectCorrection,
    #        -BehaviourValue,
    #        -BehaviourCount,
    #        -PartnerCorrection,
    #        -ModifierRef1,
    #        -ModifierClass1,
    #        -ModifierValue1,
    #        -ModifierLabel1,
    #        -ModifierRef2,
    #        -ModifierClass2,
    #        -ModifierValue2,
    #        -ModifierLabel2,
    #        -CorrectionNeeded1,
    #        -CorrectionNeeded2,
    #        -TotalDuration,#if cont_duration not run, will cause isse
    #        -TotalNumber,#if cont_number not run will cause issue 
    #        -SubjectType) 
}



# Focal state -------------------------------------------------------------


#the function take a list of FocalRef as input
#It requires the object Focal_Behav_Name, Focal_SessionDetails and Focal_Behav to be in the environment. This probably could be improved
#it returns the proportion and the count of every state behaviours for each focal observation provided in the list
#At the difference than scan observation, I have so far not made any categories of behaviours 



focal_behaviour_state_count<-function(DF){
  
# generate combination of FocalRef and StateBehav -------------------------
# sex foreplay should have a direction but for a long time could never be received 
Focal_Behav_State_Combination<-merge(DF,Focal_Behav_Name %>%
  filter(BehaviourType=="State") %>%
    select(Behaviour) %>% 
    rbind("Huddeling_R") %>% 
    rbind("Beg/Suckle_R") %>%
    rbind("Groom_R")) %>% 
  select(ObsRef,Behaviour)


# Generate count and total duration of state Behav ------------------------

#only retain state behaviours
Focal_Behav_State<-inner_join(DF,
   Focal_Behav %>% mutate(Behaviour = ifelse(Behaviour %in% c("Groom","Huddeling","Beg/Suckle") & Received == "Received", paste0(Behaviour,"_R"),Behaviour))) %>% #this modify the existing Behaviour and add a suffix to the received behaviour. This is conformed the combination we have created just above) %>%
    filter(!is.na(Behav_Duration)) %>% # to retain only state behaviour. an alternative is filter(BehaviourType=="State")
    mutate(ObsType="Focal_State")

   
#Make a count of state behaviours except Huddle that must be dealt separately (receive huddling would probably be teh case, but cannot be bothered rn)
Focal_Behav_State_NoHuddling_Count <- Focal_Behav_State %>% 
  filter(Behaviour != "Huddeling") %>% 
  #GROUP BY FOCALREF AND BEHAVIOUR 
  group_by(ObsRef,Behaviour) %>% 
  summarize(Behav_Duration=sum(Behav_Duration),Behav_Count=sum(BehaviourCount)) %>%  
  ungroup()
  #UNGROUP


#Make a count of huddling
Focal_Behav_State_Huddling_Count <- Focal_Behav_State %>% 
  filter(Behaviour == "Huddeling") %>% 
  distinct(FocalBehavRef,.keep_all = TRUE) %>% #to eliminate duplicate of huddling
  #GROUP BY FOCALREF AND BEHAVIOUR 
  group_by(ObsRef,Behaviour) %>% 
  summarize(Behav_Duration=sum(Behav_Duration),Behav_Count=sum(BehaviourCount)) %>%  
  ungroup()
#UNGROUP
  
#Append count of huddling and other behaviours 
Focal_Behav_State_Count<-bind_rows(Focal_Behav_State_NoHuddling_Count,Focal_Behav_State_Huddling_Count)


# join all combination ScanRef/AnimalID/BehaviourCont with count --------
Focal_Behav_State_Count_WithZero<-left_join(Focal_Behav_State_Combination, Focal_Behav_State_Count) %>% 
replace_na(list(Behav_Duration = 0, Behav_Count = 0)) 


# join to focal session details -------------------------------------------
# This is only necesaary to get DurationSeen and obtain frequency and duration.
#I leave animalID in Focal_SessionDetails to facilitate datacheck when subsequently joining to Focal_SessionInfo in my data prep files yet strictly speaking do not need it
#At the moment the proportion and frequency are calculated based on TotalSeen, whereas it would be better to be calculated on TotalSee_Active (after the exclusion of rest)

Focal_Behav_State_Count_SessionDetails<-inner_join(Focal_Behav_State_Count_WithZero,Focal_SessionDetails %>% select(ObsRef,AnimalID,TotalSeen)) %>% 
mutate(Frequency=Behav_Count/(TotalSeen/3600)) %>% #frequency per hour of observation
mutate(Proportion = (Behav_Duration/TotalSeen)*100)  #proportion of time spent doing an activity

return(Focal_Behav_State_Count_SessionDetails)

}



# Focal Point -------------------------------------------------------------

# focal_behaviour_point_count () compute the count of point behaviours
#There are originally 17 point behaviours
#14 of these behaviour can either be received or given
#Pump, DropFS, Pass, have no direction thus one should only use the Given behaviour and exclude their received and total count
#I have added the subtypes of copulation (back/front) and pass (Over/Under/Sidewards; NoseNose/NoseTail/TailNose/TailTail )
#do I want to differentiate between different subtype of behaviours? or eliminate 

focal_behaviour_point_count<-function(DF){

# Generate new point Behav --------------------------------------------
Focal_Behav_Point<-inner_join(DF,Focal_Behav) %>% 
  filter(BehaviourType == "Point") %>%
  #filter(Received !="Received")
  mutate(Behaviour2=
      ifelse(Behaviour=="Copulation" & Copulation_Direction == "Front","CopulationF",
    ifelse(Behaviour=="Pass",paste0("Pass_",Pass_Direction,"_",Pass_Location),Behaviour))) #this create behaviours that contain the details of copulation and scan


# Generate count of given Behav -------------------------------------

Focal_Behav_Point_Count_Given <- Focal_Behav_Point %>% 
  filter(is.na (Received) | Received == "Initiated") %>% #this only retained the behaviour given or with no direction
  #GROUP BY FocalRef, Behaviour
  group_by(ObsRef,Behaviour2) %>% 
  summarise(G_Count=sum(BehaviourCount)) %>% 
  ungroup() %>% 
#UNGROUP
  rename(Behaviour = Behaviour2)


#Get the total of pass so I don't need compute it later
Focal_Behav_Pass<-Focal_Behav_Point %>% 
  filter(Behaviour == "Pass") %>% #this only retained the behaviour given or with no direction
  #GROUP BY FocalRef, Behaviour
  group_by(ObsRef,Behaviour) %>% 
  summarise(G_Count=sum(BehaviourCount)) %>% 
  ungroup() 
#UNGROUP


# Generate count of received Behav ----------------------------------
Focal_Behav_Point_Count_Received <- Focal_Behav_Point %>% 
  filter(Received == "Received") %>% #this only retained the behaviour given or with no direction
  #GROUP BY FocalRef, Behaviour
  group_by(ObsRef,Behaviour2) %>% 
  summarise(R_Count=sum(BehaviourCount)) %>% 
  ungroup() %>% 
#UNGROUP
  rename(Behaviour = Behaviour2)
 

# Join count of given and received Behav ----------------------------
#Pass give the total number of pass

Focal_Behav_Point_Count<-full_join(Focal_Behav_Point_Count_Given,Focal_Behav_Point_Count_Received) %>%
  full_join(.,Focal_Behav_Pass) %>% 
  replace_na(list(G_Count = 0, R_Count = 0)) %>% #replace the NA with a 0
mutate(T_Count=G_Count+R_Count)


# Generate combination FocalRef and Behav -----------------------------

#Create a list of behaviour that matches the one in Focal_Behav_Point_Count
Focal_Behav_Name_Point_All<- Focal_Behav_Name %>% filter (BehaviourType == "Point") %>% 
  select (-BehaviourType) %>% 
  filter(Behaviour != "Pass") %>%
  rbind("Pass") %>% #will be for total number of pass
  rbind("Pass_TailToNose_Under") %>% 
  rbind("Pass_NoseToNose_Sidewards") %>%
  rbind("Pass_NoseToNose_Over") %>%
  rbind("Pass_NoseToNose_Under") %>% 
  rbind("Pass_NoseToTail_Over") %>% 
  rbind("Pass_NoseToTail_Under") %>% 
  rbind("Pass_TailToTail_Under") %>% 
  rbind("Pass_TailToNose_Over") %>% 
  rbind("Pass_NoseToTail_Sidewards") %>% 
  rbind("Pass_TailToNose_Sidewards") %>% 
  rbind("Pass_TailToTail_Over") %>% 
  rbind("Pass_TailToTail_Sidewards") %>% 
  rbind("CopulationF") 

#create all combination of FocalRef and behaviours
Focal_Behav_Combination<-merge(DF,Focal_Behav_Name_Point_All)


# Generate zeros of given and received Behav  -------------------------

Focal_Behav_Point_Count_WithZero<-left_join(Focal_Behav_Combination,Focal_Behav_Point_Count) %>% 
replace_na(list(G_Count = 0, R_Count = 0,T_Count=0))


# Generate long format of all point Behav count ----------------------

Focal_Behav_Point_Give<-Focal_Behav_Point_Count_WithZero %>%
  select(-R_Count,-T_Count) %>% 
  mutate(Received="G") %>% 
  rename(Count=G_Count)
  
  
Focal_Behav_Point_Receive<-Focal_Behav_Point_Count_WithZero%>%
  select(-G_Count,-T_Count) %>% 
  mutate(Received="R") %>% 
  rename(Count=R_Count)


Focal_Behav_Point_Total<-Focal_Behav_Point_Count_WithZero %>%
  select(-G_Count,-R_Count)%>% 
  mutate(Received="T") %>% 
  rename(Count=T_Count) 

Focal_Behav_Cont_Long<-bind_rows(Focal_Behav_Point_Give,Focal_Behav_Point_Receive,Focal_Behav_Point_Total) %>% 
  select(ObsRef,Behaviour,Received,Count)


# Add frequency of behaviour ----------------------------------------------

# This is only necesaary to get DurationSeen and obtain frequency and duration.
#I leave animalID in Focal_SessionDetails to facilitate datacheck when subsequently joining to Focal_SessionInfo in my data prep files yet strictly speaking do not need it

Focal_Behav_Cont_Long_Count_SessionDetails<-inner_join(Focal_Behav_Cont_Long,Focal_SessionDetails %>% select(ObsRef,AnimalID,TotalSeen)) %>% 
mutate(Frequency=Count/(TotalSeen/3600)) #frequency per hour of observation


return(Focal_Behav_Cont_Long_Count_SessionDetails)

}


