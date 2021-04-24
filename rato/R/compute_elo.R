##################################################################################################
#' compute_elo_fixed_K
#'
#' return a nested data frame by colony. with interaction and elo
#' scores before and after the interaction.
#'
#'@name compute_elo_fixed_K
#'@aliases compute_elo_fixed_K
#'@param elo_DF a data frame created by the function get_all_call_tidy()
#'and filter_all_data().
#'@param K the log of the K value
#'@param nested_output logical to return a nested df by colony default = FALSE
#'@param burn_in burn_in for each colony
#'@return a tibble
#'@import purrr
#'@import dplyr
#'@import EloOptimized
#'@export
#'@examples
#'\dontrun{
#'con <- DBI::dbConnect(RMySQL::MySQL(), user = 'philippev', password = getPass::getPass(),
#'dbname = 'Moleratdatabase', host = 'Kalahariresearch.org')
#'AllCall_Tidy <- get_all_call_tidy(con)
#'Elo_Data <- filter_all_data(AllCall_Tidy, 15)
#'test <- Elo_Data %>% mutate(new_elo = list(compute_elo_fixed_K(., K = 4, nested_output = FALSE)))
#' }
#'
compute_elo_fixed_K <- function(elo_DF, K, nested_output = FALSE, burn_in = 10){
  Colony <- data  <- new_df <- NULL

  d2 <- elo_DF %>% nest(-Colony) %>%
   mutate(all_ids = purrr::map(data, ~ unique(c(.x$Winner, .x$Loser))))

out1 <- d2 %>%
  mutate(new_df = purrr::pmap(list(X = d2$data, Y = d2$all_ids, Z = K), function(X, Y, Z) {

          out <- EloOptimized::elo.model1(par = Z,
                            burn_in = burn_in,
                            init_elo = 1000,
                            IA_data = X,
                            all_ids = Y,
                            return_likelihood =F)
          }
     ))

### specify the output
if (nested_output) {

  final_df <- out1

} else {
  out1 <- out1 %>% mutate(new_df = purrr::map2(new_df, Colony, ~ .x %>% mutate(Colony = .y)))

  final_df <- bind_rows(out1$new_df)
}
return(final_df)
}

##################################################################################################


