#' Model output in wide format
#' 
#' this takes the long data and puts it into wide format for comparing with kc_data
#' that is columns for diag, hosp, death, testpos, and testneg, each by age
#' where they are the daily values (and also cumulative)
#' note that the data is not truncated, but this can be done with a join on date
#' 
#' @param data_long model output in long format
#'
#' @return model output in wide format
#' @import dplyr
#' @importFrom tidyr unite spread
#' @export
shape_data_wide = function(data_long)
{
  # calculate daily values
  data = data_long %>%
    filter(state %in% c("cum_diag", "cum_exp", "cum_hosp", "cum_death", "cum_testpos", "cum_testneg")) %>%
    mutate(state = recode(state, "cum_diag" = "diag", "cum_exp" = "inf", "cum_hosp" = "hosp", "cum_death" = "death", "cum_testpos" = "testpos", "cum_testneg" = "testneg")) %>%
    group_by(time, date, state, age) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    unite(col = state, state, age, sep = "-") %>%
    spread(state, value) %>%
    mutate_at(vars(-c('date', 'time')), diff_col) %>%
    mutate(tot_diag = rowSums(dplyr::select(.,starts_with("diag"))),
           tot_inf = rowSums(dplyr::select(.,starts_with("inf"))),
           tot_hosp = rowSums(dplyr::select(.,starts_with("hosp"))),
           tot_death = rowSums(dplyr::select(.,starts_with("death"))),
           tot_testpos = rowSums(dplyr::select(.,starts_with("testpos"))),
           tot_testneg = rowSums(dplyr::select(.,starts_with("testneg"))))
  
  # also keep cumulative values
  cum_data = data_long %>%
    filter(state %in% c("cum_diag", "cum_exp", "cum_hosp", "cum_death", "cum_testpos", "cum_testneg")) %>%
    group_by(time, date, state, age) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    unite(col = state, state, age, sep = "-") %>%
    spread(state, value) %>%
    mutate(cum_tot_diag = rowSums(dplyr::select(.,starts_with("cum_diag"))),
           cum_tot_inf = rowSums(dplyr::select(.,starts_with("cum_exp"))),
           cum_tot_hosp = rowSums(dplyr::select(.,starts_with("cum_hosp"))),
           cum_tot_death = rowSums(dplyr::select(.,starts_with("cum_death"))),
           cum_tot_testpos = rowSums(dplyr::select(.,starts_with("cum_testpos"))),
           cum_tot_testneg = rowSums(dplyr::select(.,starts_with("cum_testneg"))))
  
  data = left_join(data, cum_data, by = c("date", "time"))
  
  return(data)
}
