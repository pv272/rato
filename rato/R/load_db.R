#' Load database tables
#'
#' * load_db_XXX will load a specific table from the database.
#'
#' These functions can be used with a connection to the server of the Kalhari
#' research programme using the function [connect_db]. For user level function
#' check [create_db_tbl_family]
#'
#' @name load_db_family
#' @aliases load_db_family
#' @param con A connection to the database
#' @return This function returns nothing directly, but it creates a tibble of
#'   tables and table names stored in a hidden environment.
#' @examples
#'
NULL

################################################################################

#' connect_db
#'
#' This function connect to the database
#'@name connect_db
#'@param username A character string specifing the username.
#'@param local A boolean indicating if the connection is local or remote.
#'@return a db connection
#'@export
#'@examples
#'\dontrun{
#'con <- connect_db(username = 'philippev',local=FALSE)
#'DBI::dbDisconnect(con)
#'}
connect_db <- function(username, local = FALSE){

  if(local) hostname <- "192.168.11.6" else hostname <- "kalahariresearch.org"

  RMySQL::dbConnect(RMySQL::MySQL(), user = username, password = getPass::getPass(),
                 dbname = 'Moleratdatabase', host = hostname)

}

###########  life history ######################################################

#' @describeIn load_db_family load MembershipBetweenV2
#' @export
#' @examples
#' con <- connect_db(username = 'philippev',local=FALSE)
#' load_db_MembershipBetweenV2(con)
load_db_MembershipBetweenV2 <- function(con) {
  con %>%
  DBI::dbGetQuery ("SELECT
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
  dplyr::mutate(MemberFrom = lubridate::ymd(.data$MemberFrom),
                MemberTo = lubridate::ymd(.data$MemberTo)) %>%
  dplyr::select(.data$AnimalRef, .data$AnimalID, .data$MemberFrom, .data$MemberTo, .data$Colony, .data$ColonyLocation)
}

#' @describeIn load_db_family load tblAnimalID
#' @export
#' @examples
#' load_db_tblAnimalID(con)
load_db_tblAnimalID <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM Moleratdatabase.tblAnimalID")
}

#' @describeIn load_db_family load tblSex
#' @export
#' @examples
#' load_db_tblSex(con)
load_db_tblSex <-function(con) {
  con %>%
  DBI::dbGetQuery("SELECT * FROM Moleratdatabase.tblSex") %>%
  dplyr::select(.data$AnimalID, .data$Sex)
}

#' @describeIn load_db_family load tblWeights
#' @export
#' @examples
#' load_db_tblWeights(con)
load_db_tblWeights <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM user_philippev.Weight_AnimalID") %>%
  dplyr::mutate(WeightDate = lubridate::ymd(.data$WeightDate)) %>%
  dplyr::select(.data$AnimalID, .data$WeightDate, .data$Weight, .data$WeightType) %>%
  dplyr::filter(!(is.na(.data$Weight)))
}

#' @describeIn load_db_family load qry_BirthDate
#' @export
#' @examples
#' load_db_qry_BirthDate(con)
load_db_qry_BirthDate <- function(con){
  con %>%
  DBI::dbGetQuery ("SELECT * FROM MoleratViews.qry_BirthDate") %>%
  dplyr::rename(BirthDate = .data$LabBirthdate) %>%
  dplyr::mutate(BirthDate = lubridate::ymd(.data$BirthDate))
}

#' @describeIn load_db_family load qry_DeathDate
#' @export
#' @examples
#' load_db_qry_DeathDate(con)
load_db_qry_DeathDate <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MoleratViews.qry_DeathDate") %>%
  dplyr::mutate(DeathDate = lubridate::ymd(.data$DeathDate))
}

#' @describeIn load_db_family load tblPairing
#' @export
#' @examples
#' load_db_tblPairing(con)
load_db_tblPairing <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_MainData.tblPairing") %>%
  dplyr::mutate(PairingDate =
                  dplyr::case_when(.data$AnimalID == "DRF015" & .data$Colony == "Roms" | .data$AnimalID == "LAM015" & .data$Colony == "Roms" ~ "2019-03-14",
                                 .data$AnimalID == "WEM011" & .data$Colony == "Vunit" ~ "2019-05-04",
                                 TRUE ~ as.character(.data$PairingDate))) %>%
  dplyr::mutate(PairingDate = lubridate::ymd(.data$PairingDate))
}


#' @describeIn load_db_family load tblColonyCodes
#' @export
#' @examples
#' load_db_tblColonyCodes(con)
load_db_tblColonyCodes <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM Moleratdatabase.tblColonyCodes") %>%
  dplyr::select(.data$Colony, .data$ColonyOrigin, .data$ColonyLocation)
}

#' @describeIn load_db_family load tblLitterCode
#' @export
#' @examples
#' load_db_tblLitterCode(con)
load_db_tblLitterCode <- function(con){
  con %>%
  DBI::dbGetQuery ("SELECT LitterRef, MotherID, DATE(DateOfBirth) AS BirthDate,
                    Exact FROM Moleratdatabase.tblLitterCode") %>%
  dplyr::mutate(BirthDate = lubridate::ymd(.data$BirthDate)) %>%
  dplyr::filter(!(.data$LitterRef %in% c(31,139))) %>%  #Field population animals
  dplyr::filter(!(.data$LitterRef %in% c(383,384,392)))
}

#' @describeIn load_db_family load Escape
#' @export
#' @examples
#' load_db_Escape(con)
load_db_Escape <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM user_philippev.Escape") %>%
  dplyr::mutate(Escape_Date = lubridate::ymd(.data$Escape_Date)) %>%
  dplyr::rename(EscapeDate = .data$Escape_Date) %>%
  dplyr::arrange(.data$EscapeDate, .data$AnimalID) %>%
  dplyr::mutate(EscapeRef = dplyr::row_number())
}

#' @describeIn load_db_family load Experiments_SubjectID
#' @export
#' @examples
#' load_db_Experiments_SubjectID(con)
load_db_Experiments_SubjectID <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM user_philippev.Experiments_SubjectID") %>%
  dplyr::mutate(ExperimentStartDate = lubridate::ymd(.data$ExperimentStartDate))
}

#' @describeIn load_db_family load PV_Parentage
#' @export
#' @examples
#' load_db_PV_Parentage(con)
load_db_PV_Parentage <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM user_philippev.PV_Parentage") %>%
  dplyr::mutate(FatherID = dplyr::case_when(.data$FatherID == "Z1M007 " ~ "Z1M007",
                                            .data$FatherID == "G7M028 " ~ "G7M028",
                                            .data$LitterRef == 15 ~ "Z3M010",
                                            .data$MotherID == "HEF003" ~ "Unknown",
                                            .data$LitterRef == 565 ~ "JA1M003",
                                            TRUE ~ .data$FatherID)) #To correct a wrong entry, will have to be corrected in next update
}

#' @describeIn load_db_family load FirstColony
#' @export
#' @examples
#' load_db_FirstColony(con)
load_db_FirstColony <- function(con){
  con %>%
  DBI::dbGetQuery ("SELECT AnimalID,
                    Colony AS FirstColony
                    FROM MoleratViews_Pending.FirstColony")
}

################# scan #########################################################

#' @describeIn load_db_family load ScanBehaviour_Instant_List
#' @export
#' @examples
#' \dontrun{
#' load_db_ScanBehaviour_Instant_List(con)
#' }
load_db_ScanBehaviour_Instant_List <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_MainData.tblCodeList
                  WHERE CodeRef = 'ScanInstantBehav'")
}

#' @describeIn load_db_family load ScanBehaviour_Cont_List
#' @export
#' @examples
#' \dontrun{
#' load_db_ScanBehaviour_Cont_List(con)
#' }
load_db_ScanBehaviour_Cont_List <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM user_philippev.Scan_BehavModif_Cont")
}

#' @describeIn load_db_family load tblScanSessionDetails
#' @export
#' @examples
#' \dontrun{
#' load_db_tblScanSessionDetails(con)
#' }
load_db_tblScanSessionDetails <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT tblScanSessionDetails.*,
              tblColonyCodes.Colony,
              DATE(StartDate) AS ObsDate
              FROM Moleratdatabase.tblScanSessionDetails
              LEFT JOIN Moleratdatabase.tblColonyCodes ON tblScanSessionDetails.ColonyRef = tblColonyCodes.ColonyRef") %>%
  dplyr::rename(ObsRef = .data$ScanRef) %>%
  dplyr::mutate(ObsDate = lubridate::ymd(.data$ObsDate),
         ObsType = "Scan")
}

#' @describeIn load_db_family load qry_ScanSession_Mrdb
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanSession_Mrdb(con)
#' }
load_db_qry_ScanSession_Mrdb <-function(con){
  con %>%
  DBI::dbGetQuery("SELECT qry_ScanSession_Mrdb.*,
             tblColonyCodes.Colony,
             DATE(StartDate) AS ObsDate,
             TIME(StartTime) AS ObsTime
             FROM MR_RawData.qry_ScanSession_Mrdb
             LEFT JOIN Moleratdatabase.tblColonyCodes ON qry_ScanSession_Mrdb.ColonyRef = tblColonyCodes.ColonyRef")
}

#' @describeIn load_db_family load qry_ScanSession_MrRaw
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanSession_MrRaw(con)
#' }
load_db_qry_ScanSession_MrRaw <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT qry_ScanSession_MrRaw.*,
              tblColonyCodes.Colony,
              DATE(StartDate) AS ObsDate,
              TIME(StartTime) AS ObsTime
              FROM MR_RawData.qry_ScanSession_MrRaw
              LEFT JOIN Moleratdatabase.tblColonyCodes ON qry_ScanSession_MrRaw.ColonyRef = tblColonyCodes.ColonyRef") %>%
  dplyr::distinct()
}

#' @describeIn load_db_family load qry_ScanSession_All
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanSession_All(con)
#' }
load_db_qry_ScanSession_All <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT qry_ScanSession_All.*,
                  tblColonyCodes.Colony,
                  DATE(StartDate) AS ObsDate,
                  TIME(StartTime) AS ObsTime
                  FROM MR_RawData.qry_ScanSession_All
                  LEFT JOIN Moleratdatabase.tblColonyCodes ON qry_ScanSession_All.ColonyRef = tblColonyCodes.ColonyRef") %>%
  dplyr::distinct()
}

#' @describeIn load_db_family load qry_ScanInstantSummaryNoModifier_Mrdb
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanInstantSummaryNoModifier_Mrdb(con)
#' }
load_db_qry_ScanInstantSummaryNoModifier_Mrdb <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_RawData.qry_ScanInstantSummaryNoModifier_Mrdb")
}

#' @describeIn load_db_family load qry_ScanInstant_MrRaw
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanInstant_MrRaw(con)
#' }
load_db_qry_ScanInstant_MrRaw <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_RawData.qry_ScanInstant_MrRaw")
}

#' @describeIn load_db_family load qry_ScanInstantSummary_MrRaw
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanInstantSummary_MrRaw(con)
#' }
load_db_qry_ScanInstantSummary_MrRaw <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_RawData.qry_ScanInstantSummary_MrRaw")
}

#' @describeIn load_db_family load qry_ScanInstantSummaryNoModifier_MrRaw
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanInstantSummaryNoModifier_MrRaw(con)
#' }
load_db_qry_ScanInstantSummaryNoModifier_MrRaw <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_RawData.qry_ScanInstantSummaryNoModifier_MrRaw")
}

#' @describeIn load_db_family load qry_ScanContSummary_Mrdb
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanContSummary_Mrdb(con)
#' }
load_db_qry_ScanContSummary_Mrdb <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_RawData.qry_ScanContSummary_Mrdb")
}

#' @describeIn load_db_family load qry_ScanCont_MrRaw
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanCont_MrRaw(con)
#' }
load_db_qry_ScanCont_MrRaw <-  function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_RawData.qry_ScanCont_MrRaw")
}

#' @describeIn load_db_family load qry_ScanContSummary_MrRaw
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_ScanContSummary_MrRaw(con)
#' }
load_db_qry_ScanContSummary_MrRaw <-  function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_RawData.qry_ScanContSummary_MrRaw")
}

################# focal ########################################################

#' @describeIn load_db_family load FocalBehaviour_List_All
#' @export
#' @examples
#' \dontrun{
#' load_db_FocalBehaviour_List_All(con)
#' }
load_db_FocalBehaviour_List_All <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT temp.*,
                   usys_FocalBehaviourCorrection.BehaviourRaw,
                   usys_FocalBehaviourCorrection.ReceivedValue
                   FROM
                   (SELECT CodeRef, Label, Value
                   FROM MR_MainData.tblCodeList
                   WHERE CodeRef = 'FocalPointBehav'
                   OR CodeRef = 'FocalStateBehav') AS temp
                   LEFT JOIN MR_RawData.usys_FocalBehaviourCorrection ON temp.Value = usys_FocalBehaviourCorrection.BehaviourValue") %>%
  dplyr::mutate(Value = as.integer(.data$Value)) %>%
  dplyr::arrange(.data$CodeRef, .data$Value)
}

#' @describeIn load_db_family load qry_FocalSession_Mrdb
#' @export
#' @examples
#' \dontrun{
#' load_db_qry_FocalSession_Mrdb(con)
#' }
load_db_qry_FocalSession_Mrdb <- function(con){
  con %>%
  DBI::dbGetQuery("SELECT * FROM MR_RawData.qry_FocalSession_Mrdb") %>%
  dplyr::rename(ObsRef = .data$FocalRef,
         ObsDate = .data$FocalDate) %>%
  dplyr::mutate(ObsDate = lubridate::ymd(.data$ObsDate),
         ObsType = "Focal")
}
