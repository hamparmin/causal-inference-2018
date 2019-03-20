##-----Variable names:
# dis_country: country name
# dis_year_start: year indicator
# dis_subevent: type of disaster (gs: quake, tc: tropical cyclone, gf: flood)
# lndis_loss_overall_usd1995: log losses from natural disaster in 1995 dollars
# lnt_gs32_magnitude: log quake hazard magnitude
# lnprop_t_gs32_magnitude: log quake propensity
# lnt3_top_wind_speed: log tropical cyclone hazard magnitude
# lnprop_t3_top_wind_speed: log tropical cyclone propensity
# lnsum_precip_abs_pos: log flood hazard magnitude
# lnprop_sum_precip_abs_pos: log flood propensity
# lngdppc: log per capita income of country, from Neumayer et al.
# lngdp: log gross domestic product of country, from Neumayer et al.
# lngdppc_final: log per capita income of country, merged with World Bank data
# lngdp_final: log gross domestic product of country, merged with World Bank data

##----- Load packages

library(foreign)
library(data.table)
library(sampleSelection)

##----- Controls from https://data.un.org/

GDP <- fread("UNdata_GDP.csv")[, V4 := NULL]
setnames(GDP, "Country or Area", "dis_country")
setnames(GDP, "Year", "dis_year_start")
setnames(GDP, "Value", "gdp")
# Convert to 1995 dollars: http://www.multpl.com/gdp-deflator/table
GDP[, loggdp := log(as.numeric(gdp) * 100 / 75.86)]
GDP[, dis_year_start := as.character(dis_year_start)]
GDP[, gdp := NULL]

GDPPC <- fread("UNdata_GDP_Per_Capita.csv")
setnames(GDPPC, "Country or Area", "dis_country")
setnames(GDPPC, "Year", "dis_year_start")
setnames(GDPPC, "Value", "gdppc")
# Convert to 1995 dollars: http://www.multpl.com/gdp-deflator/table
GDPPC[, loggdppc := log(as.numeric(gdppc) * 100 / 75.86)]
GDPPC[, gdppc := NULL]
GDPPC[, dis_year_start := as.character(dis_year_start)]
GDPPC[, Item := NULL]

setkeyv(GDP, c("dis_country", "dis_year_start"))
setkeyv(GDPPC, c("dis_country", "dis_year_start"))

MM <- merge(GDP, GDPPC, by = c("dis_country", "dis_year_start"))

##----- Same data as Neumayer et al (2014)

dataset <- read.dta("Article for GEC (political economy of disaster damage).dta")
Dataset <- data.table(dataset)
Dataset <- unique(data.table(dataset)[, list(dis_country, dis_year_start, lngdppc, lngdp)],
                  by = c("dis_country", "dis_year_start"))

##----- Subset by disaster type

dis_quake <- subset(dataset, dis_subevent == "gs")
dis_cyclone <- subset(dataset, dis_subevent == "tc")
dis_flood <- subset(dataset, dis_subevent == "gf")

##----- Quakes

dis_quake_sub <- dis_quake[, c("dis_country", "dis_year_start",
                               "lndis_loss_overall_usd1995",
                               "lnt_gs32_magnitude", "lnprop_t_gs32_magnitude")]
Dis_quake_sub <- data.table(dis_quake_sub)
D_quake <- data.table(dis_quake_sub)
U_quake <- unique(D_quake[!is.na(lnprop_t_gs32_magnitude)], by = c("dis_country"))
Countries_quake <- unique(U_quake, by = c("dis_country"))$dis_country

## Create a balanced panel

Balanced_quake <- data.table(expand.grid(Countries_quake, 1980:2008, 0))
setnames(Balanced_quake, "Var1", "dis_country")
setnames(Balanced_quake, "Var2", "dis_year_start")
setnames(Balanced_quake, "Var3", "observed")
Balanced_quake <- cbind(Balanced_quake, U_quake$lnprop_t_gs32_magnitude)
setnames(Balanced_quake, "V2", "lnprop_t_gs32_magnitude")
setkeyv(Balanced_quake, c("dis_country", "dis_year_start"))
setkeyv(D_quake, c("dis_country", "dis_year_start"))
D_quake$lnprop_t_gs32_magnitude <- NULL
M_quake <- merge(Balanced_quake, D_quake, by = c("dis_country", "dis_year_start"), all = TRUE)
M_quake[, observed := !is.na(lndis_loss_overall_usd1995)]

## Create a balanced panel

N_quake <- merge(M_quake, Dataset, by = c("dis_country", "dis_year_start"), all = TRUE, allow.cartesian = TRUE)
N_quake <- unique(N_quake, by = c("dis_country", "dis_year_start"))
N_quake[, observed := !is.na(lndis_loss_overall_usd1995)]
N_quake[, dis_year_start := as.character(dis_year_start)]

## Merge with controls from https://data.un.org/

FM_quake <- merge(N_quake, MM, by = c("dis_country", "dis_year_start"), all.x = TRUE)
FM_quake[, loggdp_final := mean(c(lngdp, loggdp), na.rm = TRUE), by = c("dis_country", "dis_year_start")]
FM_quake[, loggdppc_final := mean(c(lngdppc, loggdppc), na.rm = TRUE), by = c("dis_country", "dis_year_start")]

write.csv(FM_quake, "dis_quakes_final_merge_inflation_corrected.csv")

##----- Cyclones

dis_cyclone_sub <- dis_cyclone[, c("dis_country", "dis_year_start",
                                   "lndis_loss_overall_usd1995",
                                   "lnt3_top_wind_speed", "lnprop_t3_top_wind_speed")]
Dis_cyclone_sub <- data.table(dis_cyclone_sub)
D_cyclone <- data.table(dis_cyclone_sub)
U_cyclone <- unique(D_cyclone[!is.na(lnprop_t3_top_wind_speed)], by = c("dis_country"))
Countries_cyclone <- unique(U_cyclone, by = c("dis_country"))$dis_country

## Create a balanced panel

Balanced_cyclone <- data.table(expand.grid(Countries_cyclone, 1980:2008, 0))
setnames(Balanced_cyclone, "Var1", "dis_country")
setnames(Balanced_cyclone, "Var2", "dis_year_start")
setnames(Balanced_cyclone, "Var3", "observed")

Balanced_cyclone <- cbind(Balanced_cyclone, U_cyclone$lnprop_t3_top_wind_speed)
setnames(Balanced_cyclone, "V2", "lnprop_t3_top_wind_speed")
setkeyv(Balanced_cyclone, c("dis_country", "dis_year_start"))
setkeyv(D_cyclone, c("dis_country", "dis_year_start"))
D_cyclone$lnprop_t3_top_wind_speed <- NULL
M_cyclone <- merge(Balanced_cyclone, D_cyclone, by = c("dis_country", "dis_year_start"), all = TRUE)
M_cyclone[, observed := !is.na(lndis_loss_overall_usd1995)]

## Heckmann correction model

N_cyclone <- merge(M_cyclone, Dataset, by = c("dis_country", "dis_year_start"), all = TRUE, allow.cartesian = TRUE)
N_cyclone <- unique(N_cyclone, by = c("dis_country", "dis_year_start"))
N_cyclone[, observed := !is.na(lndis_loss_overall_usd1995)]
N_cyclone[, dis_year_start := as.character(dis_year_start)]

## Merge with controls from https://data.un.org/

FM_cyclone <- merge(N_cyclone, MM, by = c("dis_country", "dis_year_start"), all.x = TRUE)
FM_cyclone[, loggdp_final := mean(c(lngdp, loggdp), na.rm = TRUE), by = c("dis_country", "dis_year_start")]
FM_cyclone[, loggdppc_final := mean(c(lngdppc, loggdppc), na.rm = TRUE), by = c("dis_country", "dis_year_start")]

write.csv(FM_cyclone, "dis_cyclones_final_merge_inflation_corrected.csv")

##----- Floods

dis_flood_sub <- dis_flood[, c("dis_country", "dis_year_start",
                               "lndis_loss_overall_usd1995",
                               "lnsum_precip_abs_pos", "lnprop_sum_precip_abs_pos")]
Dis_flood_sub <- data.table(dis_flood_sub)
D_flood <- data.table(dis_flood_sub)
U_flood <- unique(D_flood[!is.na(lnprop_sum_precip_abs_pos)], by = c("dis_country"))
Countries_flood <- unique(U_flood, by = c("dis_country"))$dis_country

## Create a balanced panel

Balanced_flood <- data.table(expand.grid(Countries_flood, 1980:2008, 0))
setnames(Balanced_flood, "Var1", "dis_country")
setnames(Balanced_flood, "Var2", "dis_year_start")
setnames(Balanced_flood, "Var3", "observed")

Balanced_flood <- cbind(Balanced_flood, U_flood$lnprop_sum_precip_abs_pos)
setnames(Balanced_flood, "V2", "lnprop_sum_precip_abs_pos")
setkeyv(Balanced_flood, c("dis_country", "dis_year_start"))
setkeyv(D_flood, c("dis_country", "dis_year_start"))
D_flood$lnprop_sum_precip_abs_pos <- NULL
M_flood <- merge(Balanced_flood, D_flood, by = c("dis_country", "dis_year_start"), all = TRUE)
M_flood[, observed := !is.na(lndis_loss_overall_usd1995)]

## Heckmann correction model

N_flood <- merge(M_flood, Dataset, by = c("dis_country", "dis_year_start"), all = TRUE, allow.cartesian = TRUE)
N_flood <- unique(N_flood, by = c("dis_country", "dis_year_start"))
N_flood[, observed := !is.na(lndis_loss_overall_usd1995)]
N_flood[, dis_year_start := as.character(dis_year_start)]

## Merge with controls from https://data.un.org/

FM_flood <- merge(N_flood, MM, by = c("dis_country", "dis_year_start"), all.x = TRUE)
FM_flood[, loggdp_final := mean(c(lngdp, loggdp), na.rm = TRUE), by = c("dis_country", "dis_year_start")]
FM_flood[, loggdppc_final := mean(c(lngdppc, loggdppc), na.rm = TRUE), by = c("dis_country", "dis_year_start")]

write.csv(FM_flood, "dis_floods_final_merge_inflation_corrected.csv")

