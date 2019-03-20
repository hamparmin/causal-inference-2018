# @knitr descriptives
FM_quake <- read.csv("dis_quakes_final_merge_inflation_corrected.csv")
japan <- subset(FM_quake, dis_country == "Japan" & dis_year_start == 2000)
usa <- subset(FM_quake, dis_country == "United States" & dis_year_start == 2000)

china <- subset(FM_quake, dis_country == "China")
india <- subset(FM_quake, dis_country == "India")

# @knitr silent_load_packages
suppressWarnings(suppressPackageStartupMessages(library(sampleSelection)))
suppressWarnings(suppressPackageStartupMessages(library(stargazer)))
suppressWarnings(suppressPackageStartupMessages(library(countrycode)))
suppressWarnings(suppressPackageStartupMessages(library(lattice)))
suppressWarnings(suppressPackageStartupMessages(library(foreign)))

# @knitr quake_estimation
FM_quake <- read.csv("dis_quakes_final_merge_inflation_corrected.csv")
h_quake <- heckit(selection = observed ~ lnprop_t_gs32_magnitude  + loggdppc_final + loggdp_final,
                  outcome = lndis_loss_overall_usd1995 ~  lnt_gs32_magnitude +
                    lnprop_t_gs32_magnitude + loggdppc_final + loggdp_final,
                  data = FM_quake,
                  method = "2step")
# summary(h_quake)

# @knitr cyclone_estimation
FM_cyclone <- read.csv("dis_cyclones_final_merge_inflation_corrected.csv")
h_cyclone <- heckit(selection = observed ~ lnprop_t3_top_wind_speed + loggdppc_final + loggdp_final,
                    outcome = lndis_loss_overall_usd1995 ~  lnt3_top_wind_speed + 
                      lnprop_t3_top_wind_speed + loggdppc_final + loggdp_final,
                    data = FM_cyclone, method = "2step")

# summary(h_cyclone)

# @knitr flood_estimation
FM_flood <- read.csv("dis_floods_final_merge_inflation_corrected.csv")
h_flood <- heckit(selection = observed ~ lnprop_sum_precip_abs_pos + loggdppc_final + loggdp_final,
                  outcome = lndis_loss_overall_usd1995 ~  lnsum_precip_abs_pos + 
                    lnprop_sum_precip_abs_pos + loggdppc_final + loggdp_final,
                  data = FM_flood, method = "2step")

# summary(h_flood)

# @knitr table_selection
stargazer(h_quake, h_cyclone, h_flood, selection.equation = TRUE, digits = 2,
          header = FALSE,
          title = "Economic loss from natural disasters - Selection equation",
          label = "selection-equation",
          dep.var.caption = "Dependent variable: selection probability",
          dep.var.labels = "Disaster type:",
          column.labels = c("quakes", "cyclones", "floods"),
          model.numbers = FALSE,
          covariate.labels = c("ln quake propensity",
                               
                               "ln tropical cyclone propensity",
                               
                               "ln flood propensity",
                               
                               "ln per capita income of country",
                               "ln gross domestic product of country",
                               
                               "Constant")
)

# @knitr table_outcome
stargazer(h_quake, h_cyclone, h_flood, selection.equation = FALSE, digits = 2,
          header = FALSE,
          title = "Economic loss from natural disasters - Outcome equation",
          label = "outcome-equation",
          dep.var.caption = "Dependent variable: natural log of disaster loss",
          dep.var.labels = "Disaster type:",
          column.labels = c("quakes", "cyclones", "floods"),
          model.numbers = FALSE,
          covariate.labels = c("ln quake hazard magnitude",
                               "ln quake propensity",
                               
                               "ln tropical cyclone hazard magnitude",
                               "ln tropical cyclone propensity",
                               
                               "ln flood hazard magnitude",
                               "ln flood propensity",
                               
                               "ln per capita income of country",
                               "ln gross domestic product of country",
                               
                               "Constant")
)
