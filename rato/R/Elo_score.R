#' get_membership
#'
#' return the membership table from the database
#'
#'@name get_membership
#'@aliases get_membership
#'@param con a connection to the database
#'@return a tibble
#'@import DBI
#'@import dplyr
#'@import lubridate
#''@export
#'@examples
#'  con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#'  dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#'  Membership <- get_membership(con)

get_membership <- function(con){
  Membership <- con %>%
    DBI::dbGetQuery ("SELECT
    AnimalRef,
    AnimalID,
    DATE(MemberFrom) AS MemberFrom,
    DATE(MemberTo) AS MemberTo,
    MemberShipBetween.ColonyRef,
    MemberDays,
     MemberShipBetween.Colony AS QueriedColony,
    tblColonyCodes.ColonyOrigin
FROM
    MoleratViews_Pending.MemberShipBetween
LEFT JOIN
    Moleratdatabase.tblColonyCodes ON MoleratViews_Pending.MemberShipBetween.ColonyRef = tblColonyCodes.ColonyRef
WHERE MemberShipBetween.ColonyRef <> 120
AND MemberShipBetween.Colony <> 'Exported_Nigel'") %>%
    dplyr::mutate(MemberFrom=lubridate::ymd(MemberFrom),MemberTo=lubridate::ymd(MemberTo)) %>%
    dplyr::select(AnimalRef,AnimalID,MemberFrom,MemberTo,QueriedColony,ColonyOrigin)
}

##################################################################################################
#' get_scan_call
#'
#' return scan call for all colonies
#'
#'@name get_scan_calls
#'@aliases get_scan_calls
#'@param con a connection to the database
#'@return a tibble
#'@import DBI
#'@import dplyr
#'@import lubridate
#'@import tibble
#''@export
#'@examples
#' con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#' dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#' scan_call <- get_scan_call(con)
#'
get_scan_call <- function(con) {
ScanCall <- con %>%
  DBI::dbGetQuery("SELECT *
               FROM user_philippev.SubCall_Scan
               ") %>%
  dplyr::rename(Date=ObsDate) %>%
  dplyr::mutate(Date=lubridate::ymd(Date)) %>%
  lapply(., function(x) rep(x,.$BehaviourCount)) %>% #to repeat the rows for interactions that have happened several times
  tibble::as_tibble(.)
}


##################################################################################################
#' get_focal_calls
#'
#' return focal call for all colonies
#'
#'@name get_focal_calls
#'@aliases get_focal_calls
#'@param con a connection to the database
#'@return a tibble
#'@import DBI
#'@import dplyr
#'@import lubridate
#'@import tibble
#''@export
#'@examples
#' con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#' dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#' focal_call <- get_focal_call(con)
#'
get_focal_call <- function(con) {
FocalCall <- con %>%
  DBI::dbGetQuery("SELECT * FROM user_philippev.SubCall_Focal") %>%
  dplyr::rename(Date=ObsDate,NBCall=NbCall) %>%
  dplyr::mutate(Date=lubridate::ymd(Date)) %>%
  lapply(., function(x) rep(x,.$BehaviourCount)) %>%
  tibble::as_tibble(.)
FocalCall_tidy <- FocalCall %>%
  filter (Received == 1) %>%
  rename(Loser=AnimalID, Winner=Partner) %>%
  bind_rows(FocalCall %>%
              filter (Received == 0) %>%
              rename(Winner=AnimalID, Loser=Partner)) %>%
  select(-Received)
}

##################################################################################################
#' get_all_call
#'
#' return a tibble with combined focal and scan calls
#'
#'@name get_all_call
#'@aliases get_all_call
#'@param FocalCall
#'@param ScanCall
#'@return a tibble
#'@import DBI
#'@import dplyr
#'@import lubridate
#'@import tibble
#''@export
#'@examples
#' con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#' dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#' FocalCall <- get_focal_call(con)
#' ScanCall <- get_scan_call(con)
#' AllCall <- get_all_call(FocalCall, ScanCall)
#'

get_all_call <- function(FocalCall, ScanCall) {
  FocalCall %>%
    bind_rows(ScanCall) %>%
    mutate(ObsTime=hms::as.hms(ObsTime)) %>% #for whatever reasons the lubridate hms() cannot be arranged. Ask Colin
    #GROUP BY OBSERVATION TYPE AND REF TO RANDOMIZE OBS WITHIN OBS
    group_by(ObsRef,ObsType) %>%
    sample_frac(size = 1, replace = FALSE) %>%
    ungroup() %>%
    #UNGROUP
    arrange(Date,ObsTime) %>%
    mutate(InterractionRef = 1:n())
}

##################################################################################################
#' get_sex
#'
#' return a tibble with the sex of the ID
#'
#'@name get_sex
#'@aliases get_sex
#'@param con a connection to the database
#'@return a tibble
#'@import DBI
#'@import dplyr
#'@export
#'@examples
#' con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#' dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#' sex_DF <- get_sex(con)

get_sex <- function(con) {
  con %>%
    dbGetQuery("SELECT *
FROM Moleratdatabase.tblSex
") %>%
    select(AnimalID,Sex)
}

##################################################################################################
#' get_weight
#'
#' return a tibble with the weight of the ID
#'
#'@name get_weight
#'@aliases get_weight
#'@param con a connection to the database
#'@return a tibble
#'@import DBI
#'@import dplyr
#'@export
#'@examples
#' con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#' dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#' weight_DF <- get_weight(con)
get_weight <- function(con) {
  con %>%
    dbGetQuery("SELECT * FROM user_philippev.Weight_AnimalID") %>%
    mutate(WeightDate=ymd(WeightDate)) %>%
    select(AnimalID,WeightDate,Weight,WeightType) %>%
    filter(!(is.na(Weight)))
}


##################################################################################################
#' get_characteristics
#'
#' return a tibble with the characteristics of the ID
#'
#'@name get_characteristics
#'@aliases get_characteristics
#'@param con a connection to the database
#'@return a tibble
#'@import DBI
#'@import dplyr
#'@export
#'@examples
#' con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#' dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#' characteristics_DF <- get_characteristics(con)
#'
get_characteristics <- function(con){
  con %>%
    dbGetQuery("SELECT * FROM user_philippev.ID_Characteristic") %>%
    mutate(BirthDate=ymd(BirthDate),DeathDate=ymd(DeathDate),Mother_FirstLitter=ymd(Mother_FirstLitter),Father_FirstLitter=ymd(Father_FirstLitter)) %>%
    select(AnimalID,Sex,Wildcaught,WildcaughtQueen,BirthDate,LitterRef,Mother_FirstLitter,Father_FirstLitter,DeathDate)
}

##################################################################################################
#' get_colony
#'
#'add the colony for the all call df
#'
#'@name get_colony
#'@aliases get_colony
#'@param con a connection to the database
#'@return a tibble
#'@import dplyr
#'@export
#'@examples
#' con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#' dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#' Membership <- get_characteristics(con)
#' FocalCall <- get_focal_call(con)
#' ScanCall <- get_scan_call(con)
#' AllCall <- get_all_call(FocalCall, ScanCall)
#' AllCall_colony <- get_colony(AllCall, Membership)
get_colony <- function(AllCall, Membership) {
inner_join(AllCall %>% distinct (AnimalID,Date) #one only wants one colony for each day as individual cannot be measure in two colonies simultaneousls
           , Membership, by = "AnimalID") %>%
  filter(Date >= MemberFrom & Date <= MemberTo)%>%
  select(-c(MemberFrom,MemberTo,AnimalRef))
}

