# plot avondale tracts ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggsci)
## library(tidyverse)
## library(sf)
library(readxl)
library(pastecs)
library(ggsci)



Avondale_tracts <- data_acs5_tb %>%
  filter(GEOID %in% c("39061006600", "39061006800", "39061006900"))

plot_est_moe <- function(data, estimate, moe, title) {
  p <- ggplot(data, aes(x = factor(dataYr), y = !!sym(estimate), group = GEOID, color = GEOID)) +
    geom_errorbar(aes(
      ymin = !!sym(estimate) - !!sym(moe),
      ymax = !!sym(estimate) + !!sym(moe)
    ),
    width = 0.5, position = position_dodge(0.1)
    ) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
    labs(
      title = title,
      subtitle = "2010-2020 Five-year American Community Survey",
      x = "ACS 5-Year survey end year",
      y = paste0("ACS estimate \n(bars represent margin of error)")
    ) +
    scale_color_nejm() +
    theme_bw()

  return(p)
} # end of plot_est_moe()

plot_est_moe_fraction <- function(data, estimate, moe, title) {
  est_var <- sym(estimate)
  moe_var <- sym(moe)

  p <- ggplot(data, aes(x = factor(dataYr), y = !!sym(estimate), group = GEOID, color = GEOID)) +
    geom_errorbar(aes(
      ymin = !!sym(estimate) - !!sym(moe),
      ymax = !!sym(estimate) + !!sym(moe)
    ),
    width = 0.5, position = position_dodge(0.1)
    ) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
    labs(
      title = title,
      subtitle = "2010-2020 Five-year American Community Survey",
      x = "ACS 5-Year survey end year",
      y = paste0("ACS estimate \n(bars represent margin of error)")
    ) +
    scale_color_nejm() +
    theme_bw() +
    ylim(0, 1)

  return(p)
} # end of plot_est_moe_fraction()

est_fraction <- c(
  "fraction_poverty", "fraction_fam_nospouse", "fraction_foodstamp",
  "fraction_condition", "fraction_bf1970", "fraction_vacant",
  "fraction_laborforce", "fraction_insured"
)
moe_fraction <- c(
  "fraction_poverty_moe", "fraction_fam_nospouse_moe", "fraction_foodstamp_moe",
  "fraction_condition_moe", "fraction_bf1970_moe", "fraction_vacant_moe",
  "fraction_laborforce_moe", "fraction_insured_moe"
)
des_fraction <- c(
  "Fraction of population with income in past 12 months below poverty level",
  "Fraction of family household with no spouse",
  "Fraction of households received food stamps in the past 12 months",
  "Fraction of tenure with one or more selected physical and financial conditions",
  "Fraction of housing units built before 1970",
  "Fraction of vacant housing units",
  "Fraction of population 16 years and over in labor force",
  "Fraction of civilian noninstituionalized popultion with health insurance coverage"
)
est_n <- c(
  "n_pop", "n_children_lt18", "n_household_lt18", "n_household", "median_housingcost",
  "median_rent"
)
moe_n <- c(
  "n_pop_moe", "n_children_lt18_moe", "n_household_lt18_moe", "n_household_moe", "median_housingcost_moe",
  "median_rent_moe", "fraction_insured_moe"
)
des_n <- c(
  "Total population",
  "Number of children under 18",
  "Number of household with children under 18",
  "Total households",
  "Median monthly housing cost",
  "Median gross rent (total)"
)

pdf("Output/Plots_comparison_in_Avondale_06162022.pdf", width = 9, height = 7)

for (i in 1:6) {
  p <- plot_est_moe_fraction(Avondale_tracts, est_fraction[i], moe_fraction[i], des_fraction[i])
  print(p)
}
i <- 7
p <- plot_est_moe_fraction(subset(Avondale_tracts, dataYr >= 2011), est_fraction[i], moe_fraction[i], des_fraction[i])
print(p)
i <- 8
p <- plot_est_moe_fraction(subset(Avondale_tracts, dataYr >= 2012), est_fraction[i], moe_fraction[i], des_fraction[i])
print(p)

for (i in 1:6) {
  p <- plot_est_moe(Avondale_tracts, est_n[i], moe_n[i], des_n[i])
  print(p)
}

dev.off()
