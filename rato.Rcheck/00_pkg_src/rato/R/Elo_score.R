#' get_membership
#'
#' return the membership table from the database
#'
#'@name get_membership
#'@aliases get_membership
#'@param con a connection to the database
#'@return a tibble
#'@export
#'@import DBI
#'@import dplyr
#'@import lubridate
#'@examples
#'  con <- dbConnect(MySQL(), user = username, password = getPass(),
#'  dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#'  get_membership(con)

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
    dplyr::mutate(MemberFrom=lubridate::ymd(MemberFrom),MemberTo=lubidate::ymd(MemberTo)) %>%
    dplyr::select(AnimalRef,AnimalID,MemberFrom,MemberTo,QueriedColony,ColonyOrigin)
}
