pkgname <- "rato"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "rato-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('rato')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("compute_elo_fixed_K")
### * compute_elo_fixed_K

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compute_elo_fixed_K
### Title: compute_elo_fixed_K
### Aliases: compute_elo_fixed_K

### ** Examples

con <- con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
AllCall_Tidy <- get_all_call_tidy(con)
Elo_Data <- filter_all_data(AllCall_Tidy, 15)
test <- compute_elo_fixed_K(Elo_Data, K = 4)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compute_elo_fixed_K", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("filter_all_data")
### * filter_all_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: filter_all_data
### Title: filter_all_data
### Aliases: filter_all_data

### ** Examples

con <- con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
AllCall_Tidy <- get_all_call_tidy(con)
Elo_Data <- filter_all_data(AllCall_Tidy, 15)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("filter_all_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_all_call")
### * get_all_call

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_all_call
### Title: get_all_call
### Aliases: get_all_call

### ** Examples

con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
FocalCall <- get_focal_call(con)
ScanCall <- get_scan_call(con)
AllCall <- get_all_call(FocalCall, ScanCall)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_all_call", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_all_call_tidy")
### * get_all_call_tidy

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_all_call_tidy
### Title: get_all_call_tidy
### Aliases: get_all_call_tidy

### ** Examples

con <- con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
AllCall_Tidy <- get_all_call_tidy(con)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_all_call_tidy", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_characteristics")
### * get_characteristics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_characteristics
### Title: get_characteristics
### Aliases: get_characteristics

### ** Examples

con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
characteristics_DF <- get_characteristics(con)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_characteristics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_colony")
### * get_colony

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_colony
### Title: get_colony
### Aliases: get_colony

### ** Examples

con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
Membership <- get_membership(con)
FocalCall <- get_focal_call(con)
ScanCall <- get_scan_call(con)
AllCall <- get_all_call(FocalCall, ScanCall)
AllCall_colony <- get_colony(AllCall%>%
 select(Winner,Date)%>%
 rename(AnimalID=Winner),Membership)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_colony", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_focal_calls")
### * get_focal_calls

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_focal_calls
### Title: get_focal_calls
### Aliases: get_focal_calls get_focal_call

### ** Examples

con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
focal_call <- get_focal_call(con)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_focal_calls", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_membership")
### * get_membership

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_membership
### Title: get_membership
### Aliases: get_membership

### ** Examples

 con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
 dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
 Membership <- get_membership(con)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_membership", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_scan_calls")
### * get_scan_calls

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_scan_calls
### Title: get_scan_call
### Aliases: get_scan_calls get_scan_call

### ** Examples

con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
scan_call <- get_scan_call(con)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_scan_calls", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_sex")
### * get_sex

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_sex
### Title: get_sex
### Aliases: get_sex

### ** Examples

con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
sex_DF <- get_sex(con)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_sex", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_weight")
### * get_weight

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_weight
### Title: get_weight
### Aliases: get_weight

### ** Examples

con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
weight_DF <- get_weight(con)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_weight", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
