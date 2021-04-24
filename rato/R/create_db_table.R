#' Create database tables
#'
#' The tables loaded by these functions will be stored in a hidden
#' environment. We do that so that other functions have access to it without the
#' user being tempted to interfere with it.
#' * [create_db_tbl_life_history] will load the tables from the database relative to life_history
#' * [create_db_tbl_behavior_scan] will load the tables from the database relative to scan behavior
#' * [create_db_tbl_behavior_focal] will load the tables from the database relative to focal behavior
#'
#' These functions can be used with a connection to the server of the Kalhari
#' research programme using the function [connect_db].
#'
#' @name create_db_tbl_family
#' @aliases create_db_tbl
#' @param con A connection to the database
#' @return This function returns a tibble
#'         of 2 columns. tbl_name and a list-col storing the table.
#' @examples
#'
NULL


################################################################################
#### grouping function
################################################################################
#' Load multiple tables based on their names
#'
#' Internal function
#' @name .load_db_mutiple_tbl
#' @param tbl_names A character vector of tbl_names to load. Tbl need to be on the database and a dedicated loading function.
#' @param con A connection to the database.
#' @export
#' @return A tibble of 2 columns. tbl_name and a list-col storing the table.
#' @examples
#' names <- c("tblSex", "tblWeights")
#' con <- connect_db(username = 'philippev',local=FALSE)
#' .load_db_multiple_tbl(tbl_names = names, con = con)

.load_db_multiple_tbl <- function(tbl_names, con){
  tibble::tibble(name = tbl_names,
                 fun = paste0("load_db_", .data$name),
                 tbl = purrr::map(.x = .data$fun, .f = do.call, list(con = con))) %>%
    dplyr::select(-.data$fun)
}


#' @describeIn create_db_tbl_family Create Life history db_tables
#' @export
#' @examples
#' \dontrun{
#' load_db_tbl_life_history(con)
#' }
create_db_tbl_life_history <- function(con){

  tbl_names <- c("MembershipBetweenV2",
                 "tblAnimalID",
                 "tblSex",
                 "tblWeights",
                 "qry_BirthDate",
                 "qry_DeathDate",
                 "tblPairing",
                 "tblColonyCodes",
                 "tblLitterCode",
                 "Escape",
                 "Experiments_SubjectID",
                 "PV_Parentage",
                 "FirstColony")

  .load_db_multiple_tbl(tbl_names, con)
}

#' @describeIn create_db_tbl_family create behavior db_tables for scan behavior
#' @export
#' @examples
#' \dontrun{
#' create_db_tbl_behavior_scan(con)
#' }
#'
create_db_tbl_behavior_scan <- function(con){

  tbl_names <- c("ScanBehaviour_Instant_List",
                 "ScanBehaviour_Cont_List",
                 "tblScanSessionDetails",
                 "qry_ScanSession_Mrdb",
                 "qry_ScanSession_MrRaw",
                 "qry_ScanSession_All",
                 "qry_ScanInstantSummaryNoModifier_Mrdb",
                 "qry_ScanInstant_MrRaw",
                 "qry_ScanInstantSummary_MrRaw",
                 "qry_ScanInstantSummaryNoModifier_MrRaw",
                 "qry_ScanContSummary_Mrdb",
                 "qry_ScanCont_MrRaw",
                 "qry_ScanContSummary_MrRaw")

  .load_db_multiple_tbl(tbl_names, con)
}

#' @describeIn create_db_tbl_family create behavior db_tables for focal behavior
#' @export
#' @examples
#' \dontrun{
#' create_db_tbl_behavior_focal(con)
#' }
#'
create_db_tbl_behavior_focal <- function(con){

  tbl_names <- c("FocalBehaviour_List_All",
                 "qry_FocalSession_Mrdb")

  .load_db_multiple_tbl(tbl_names, con)
}

