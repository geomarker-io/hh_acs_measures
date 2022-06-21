
library(tidyverse)
library(sf)
library(tidycensus)
census_api_key("CENSUS_API_KEY")
library(tigris)
library(readxl)
options(tigris_use_cache=TRUE)
options(tigris_class='sf')
library(pastecs)
library(ggsci)

#### get 2010-2020 5-year ACS tract-level variables -----------------------------------

states_needed <- tigris::fips_codes %>%
  select(state_code, state_name) %>%
  filter(! state_name %in% c('American Samoa', 'Guam', 'Northern Mariana Islands',
                             'Puerto Rico', 'U.S. Minor Outlying Islands',
                             'U.S. Virgin Islands')) %>%
  unique() %>%
  pull(state_code)


# get acs data by year
get_acs_data <- function(censusYr){

  # fraction_poverty
  # income in past 12 months below poverty level
  acs_poverty <- get_acs(geography = 'tract',
                         variables = 'B17001_002',
                         summary_var = 'B17001_001',
                         state = states_needed,
                         moe_level = 95,
                         survey = "acs5",
                         year = censusYr) %>%
    transmute(GEOID = GEOID,
              fraction_poverty = estimate / summary_est,
              fraction_poverty_moe = moe_prop(estimate, summary_est, moe, summary_moe),
              n_pop = summary_est, 
              n_pop_moe = summary_moe)
  
  # Number of children under age 18 
  acs_n_child_lt18 <- get_acs(geography = 'tract',
                 variables = c(paste0('B01001_00', 3:6),
                               paste0('B01001_0', 27:30)),
                 state = states_needed,
                 moe_level = 95,
                 survey = "acs5",
                 year = censusYr) %>% 
    group_by(GEOID) %>%
    summarize(n_children_lt18 = sum(estimate),
              n_children_lt18_moe = moe_sum(moe, estimate))
  
  # number of household with children under age 18
  # total number of household
  acs_n_hh_lt18 <- get_acs(geography = 'tract',
                         variables = 'B11005_002',
                         summary_var = 'B11005_001',
                         state = states_needed,
                         moe_level = 95,
                         survey = "acs5",
                         year = censusYr) %>%
    select(GEOID, 
           n_household_lt18 = estimate, 
           n_household_lt18_moe = moe,
           n_household = summary_est,
           n_household_moe = summary_moe)
  
  # Fraction insured (2012-2020)
  if (censusYr >= 2012){
    acs_perc_insured <- get_acs(geography = 'tract',
                                variables = c(paste0('B27001_00', c(4,7)),
                                              paste0('B27001_0', seq(10, 28, by=3)),
                                              paste0('B27001_0', seq(32, 56, by=3))),
                                summary_var = 'B27001_001',
                                state = states_needed,
                                moe_level = 95,
                                survey = "acs5",
                                year = censusYr) %>% 
      group_by(GEOID) %>%
      summarize(n_insured = sum(estimate),
                n_insured_moe = moe_sum(moe, estimate),
                n_total = unique(summary_est),
                n_total_moe = unique(summary_moe)) %>% 
      transmute(GEOID = GEOID,
                fraction_insured = n_insured / n_total,
                fraction_insured_moe = moe_prop(n_insured, n_total, n_insured_moe, n_total_moe)) 
  }
  
  # fraction family household with no spouse
  acs_perc_fam_nospouse <- get_acs(geography = 'tract',
                         variables = 'B11001_004',
                         summary_var = 'B11001_001',
                         state = states_needed,
                         moe_level = 95,
                         survey = "acs5",
                         year = censusYr) %>%
    transmute(GEOID = GEOID,
              fraction_fam_nospouse = estimate / summary_est,
              fraction_fam_nospouse_moe = moe_prop(estimate, summary_est, moe, summary_moe)) 
  
  # fraction of population 16 years and Over in labor force (2011-2020)
  if (censusYr >= 2011){
    acs_perc_laborforce <- get_acs(geography = 'tract',
                                   variables = 'B23025_002',
                                   summary_var = 'B23025_001',
                                   state = states_needed,
                                   moe_level = 95,
                                   survey = "acs5",
                                   year = censusYr) %>%
      transmute(GEOID = GEOID,
                fraction_laborforce = estimate / summary_est,
                fraction_laborforce_moe = moe_prop(estimate, summary_est, moe, summary_moe)) 
  }
  
  # fraction received food stamps
  acs_perc_foodstamp <- get_acs(geography = 'tract',
                                variables = 'B22003_002',
                                summary_var = 'B22003_001',
                                state = states_needed,
                                moe_level = 95,
                                survey = "acs5",
                                year = censusYr) %>%
    transmute(GEOID = GEOID,
              fraction_foodstamp = estimate / summary_est,
              fraction_foodstamp_moe = moe_prop(estimate, summary_est, moe, summary_moe)) 
  
  # median monthly housing cost
  acs_median_housingcost <- get_acs(geography = 'tract',
                                    variables = 'B25105_001',
                                    state = states_needed,
                                    moe_level = 95,
                                    survey = "acs5",
                                    year = censusYr) %>%
    transmute(GEOID = GEOID,
              median_housingcost = estimate,
              median_housingcost_moe = moe)
  
  # median gross rent 
  acs_median_grossrent <- get_acs(geography = 'tract',
                                    variables = 'B25113_001',
                                    state = states_needed,
                                    moe_level = 95,
                                    survey = "acs5",
                                    year = censusYr) %>%
    transmute(GEOID = GEOID,
              median_rent = estimate,
              median_rent_moe = moe)
  
  # fraction selected condition
  acs_perc_condition <- get_acs(geography = 'tract',
                                variables = c(paste0('B25123_00', 3:6),
                                              'B25123_009',
                                              paste0('B25123_0', 10:12)),
                                summary_var = 'B25123_001',
                                state = states_needed,
                                moe_level = 95,
                                survey = "acs5",
                                year = censusYr) %>%
    group_by(GEOID) %>%
    summarize(n_condition = sum(estimate),
              n_condition_moe = moe_sum(moe, estimate),
              n_total = unique(summary_est),
              n_total_moe = unique(summary_moe)) %>% 
    transmute(GEOID=GEOID,
                fraction_condition = n_condition / n_total,
                fraction_condition_moe = moe_prop(n_condition, n_total, n_condition_moe, n_total_moe)) 
  
  # fraction built before 1970
  acs_perc_bf1970 <- get_acs(geography = 'tract',
                                variables = c(paste0('B25034_00', 7:9),
                                              'B25034_010'),
                                summary_var = 'B25034_001',
                                state = states_needed,
                                moe_level = 95,
                                survey = "acs5",
                                year = censusYr) %>%
    group_by(GEOID) %>%
    summarize(n_bf1970 = sum(estimate),
              n_bf1970_moe = moe_sum(moe, estimate),
              n_total = unique(summary_est),
              n_total_moe = unique(summary_moe)) %>% 
    transmute(GEOID=GEOID,
              fraction_bf1970 = n_bf1970 / n_total,
              fraction_bf1970_moe = moe_prop(n_bf1970, n_total, n_bf1970_moe, n_total_moe)) 
  
  # fraction vacant housing
  acs_perc_vacant <- get_acs(geography = 'tract',
                             variables = 'B25002_003',
                             summary_var = 'B25002_001',
                             state = states_needed,
                             moe_level = 95,
                             survey = "acs5",
                             year = censusYr) %>%
    transmute(GEOID=GEOID,
              fraction_vacant = estimate / summary_est,
              fraction_vacant_moe = moe_prop(estimate, summary_est, moe, summary_moe)) 
  
  # join
  if (censusYr >= 2012){
    d <- reduce(.x = list(acs_poverty, acs_n_child_lt18, acs_n_hh_lt18,
                          acs_perc_fam_nospouse, acs_perc_foodstamp, acs_median_housingcost,
                          acs_median_grossrent, acs_perc_condition, acs_perc_bf1970, acs_perc_vacant,
                          acs_perc_insured, acs_perc_laborforce),
                .f = function(.x, .y) left_join(.x, .y, by='GEOID')) %>%
        mutate(survey = "acs5",
               dataYr = censusYr)
  } else if (censusYr == 2011){
    d <- reduce(.x = list(acs_poverty, acs_n_child_lt18, acs_n_hh_lt18,
                          acs_perc_fam_nospouse, acs_perc_foodstamp, acs_median_housingcost,
                          acs_median_grossrent, acs_perc_condition, acs_perc_bf1970, acs_perc_vacant,
                          acs_perc_laborforce),
                .f = function(.x, .y) left_join(.x, .y, by='GEOID')) %>%
      mutate(survey = "acs5",
             dataYr = censusYr)
  } else if (censusYr == 2010){
    d <- reduce(.x = list(acs_poverty, acs_n_child_lt18, acs_n_hh_lt18,
                          acs_perc_fam_nospouse, acs_perc_foodstamp, acs_median_housingcost,
                          acs_median_grossrent, acs_perc_condition, acs_perc_bf1970, acs_perc_vacant),
                .f = function(.x, .y) left_join(.x, .y, by='GEOID')) %>%
      mutate(survey = "acs5",
             dataYr = censusYr)
  }
  
  return(d) 
  
}
### end of get_acs_data()


# Create data lists to hold data from acs 5-year data 
# Each data list contains data sets from years 2010-2020

data_acs5 <- list()
for(i in 2010:2020){
  dataname <- paste0("data", i)
  data_acs5[[dataname]] <- get_acs_data(i)
}

save(data_acs5, file="data/data_acs5_list.RData")

# load("data/data_acs5_06152022.RData")

data_acs5_tb <- bind_rows(data_acs5)

saveRDS(data_acs5_tb, "data/data_acs5.rds")

Avondale_tracts <- data_acs5_tb %>% 
  filter(GEOID %in% c('39061006600', '39061006800', '39061006900'))

########### plots

plot_est_moe <- function(data, estimate, moe, title){
  
  p <- ggplot(data, aes(x=factor(dataYr), y=!!sym(estimate), group=GEOID, color=GEOID)) +
    geom_errorbar(aes(ymin = !!sym(estimate) - !!sym(moe), 
                      ymax = !!sym(estimate) + !!sym(moe)), 
                  width=0.5, position=position_dodge(0.1)) +
    geom_line(size=1) + 
    geom_point(size=3) +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
    labs(title = title,
         subtitle = "2010-2020 Five-year American Community Survey",
         x = "ACS 5-Year survey end year",
         y = paste0("ACS estimate \n(bars represent margin of error)")) + 
    scale_color_nejm() + 
    theme_bw()
  
  return(p)
}

plot_est_moe_fraction <- function(data, estimate, moe, title){
  
  est_var <- sym(estimate)
  moe_var <- sym(moe)
  
  p <- ggplot(data, aes(x=factor(dataYr), y=!!sym(estimate), group=GEOID, color=GEOID)) +
    geom_errorbar(aes(ymin = !!sym(estimate) - !!sym(moe), 
                      ymax = !!sym(estimate) + !!sym(moe)), 
                  width=0.5, position=position_dodge(0.1)) +
    geom_line(size=1) + 
    geom_point(size=3) +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
    labs(title = title,
         subtitle = "2010-2020 Five-year American Community Survey",
         x = "ACS 5-Year survey end year",
         y = paste0("ACS estimate \n(bars represent margin of error)")) + 
    scale_color_nejm() + 
    theme_bw() +
    ylim(0,1)
  
  return(p)
}


est_fraction <- c("fraction_poverty", "fraction_fam_nospouse", "fraction_foodstamp",
                  "fraction_condition","fraction_bf1970", "fraction_vacant",
                  "fraction_laborforce","fraction_insured")
moe_fraction <- c("fraction_poverty_moe", "fraction_fam_nospouse_moe", "fraction_foodstamp_moe",
                  "fraction_condition_moe", "fraction_bf1970_moe", "fraction_vacant_moe",
                  "fraction_laborforce_moe","fraction_insured_moe")
des_fraction <- c("Fraction of population with income in past 12 months below poverty level",
                  "Fraction of family household with no spouse",
                  "Fraction of households received food stamps in the past 12 months",
                  "Fraction of tenure with one or more selected physical and financial conditions",
                  "Fraction of housing units built before 1970",
                  "Fraction of vacant housing units",
                  "Fraction of population 16 years and over in labor force",
                  "Fraction of civilian noninstituionalized popultion with health insurance coverage")
est_n <- c("n_pop", "n_children_lt18", "n_household_lt18", "n_household", "median_housingcost",
           "median_rent")
moe_n <- c("n_pop_moe", "n_children_lt18_moe", "n_household_lt18_moe", "n_household_moe", "median_housingcost_moe",
           "median_rent_moe","fraction_insured_moe")
des_n <- c("Total population",
           "Number of children under 18",
           "Number of household with children under 18",
           "Total households",
           "Median monthly housing cost",
           "Median gross rent (total)")

pdf ("Output/Plots_comparison_in_Avondale_06162022.pdf", width=9, height=7)

  for (i in 1:6){
    p <- plot_est_moe_fraction(Avondale_tracts, est_fraction[i], moe_fraction[i], des_fraction[i])
    print(p)
  } 
  i=7
  p <- plot_est_moe_fraction(subset(Avondale_tracts, dataYr >= 2011), est_fraction[i], moe_fraction[i], des_fraction[i])
  print(p)
  i=8
  p <- plot_est_moe_fraction(subset(Avondale_tracts, dataYr >= 2012), est_fraction[i], moe_fraction[i], des_fraction[i])
  print(p)
  
  for (i in 1:6){
    p <- plot_est_moe(Avondale_tracts, est_n[i], moe_n[i], des_n[i])
    print(p)
  } 

dev.off()



