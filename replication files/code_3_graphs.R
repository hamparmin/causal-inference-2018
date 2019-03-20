# @knitr lattice_plot
library(countrycode)
library(lattice)
library(foreign)
library(methods)
library(data.table)
# install.packages("devtools")
# library(devtools)
# install_github('IQSS/Zelig')
library(Zelig) # require Zelig 5 from GitHub

d <- read.dta("Article for GEC (political economy of disaster damage).dta")
d$code <- countrycode(d$dis_country, "country.name", "iso3c")
d$dis_subevent[d$dis_subevent == "gs"] <- "quakes"
d$dis_subevent[d$dis_subevent == "gf"] <- "floods"
d$dis_subevent[d$dis_subevent == "tc"] <- "cyclones"

# pdf("plot_lattice_plot.pdf")
# xyplot(lndis_loss_overall_usd1995 ~ dis_year_start | code,
#        data = d,
#        group = dis_subevent,
#        auto.key = TRUE,
#        xlab = "Year",
#        ylab = "Natural log of disaster loss")
# dev.off()

sort(unique(d$dis_country))
d1 <- subset(d, code %in% sort(unique(d$code)[1:100]))
d2 <- subset(d, code %in% sort(unique(d$code)[101:201]))
D <- data.table(d)
setkeyv(D, c("code", "dis_year_start"))
D[code %in% sort(unique(d$code))[1:100]]
D[code %in% sort(unique(d$code))[101:201]]

pdf("plot_lattice_plot_1.pdf")
xyplot(lndis_loss_overall_usd1995 ~ dis_year_start | code,
       data = D[code %in% sort(unique(d$code))[1:100]],
       group = dis_subevent,
       auto.key = TRUE,
       xlab = "Year",
       ylab = "Natural log of disaster loss")
dev.off()

pdf("plot_lattice_plot_2.pdf")
xyplot(lndis_loss_overall_usd1995 ~ dis_year_start | code,
       data = D[code %in% sort(unique(d$code))[101:201]],
       group = dis_subevent,
       auto.key = TRUE,
       xlab = "Year",
       ylab = "Natural log of disaster loss")
dev.off()

# # @knitr lattice_plot_sub
pdf("plot_lattice_plot_sub.pdf")
xyplot(lndis_loss_overall_usd1995 ~ dis_year_start | code,
       data = subset(d, code %in% c("USA", "IND", "CHN", "JPN")), 
       type = c("p", "r"),
       group = dis_subevent,
       auto.key = TRUE,
       xlab = "Year",
       ylab = "Natural log of disaster loss")
dev.off()

# @knitr zelig_sample_selection
# We modify the defaut linear regression in Zelig to take into account the
# variance-covariance matrix from 'heckit' step 2
zsampleselection <- setRefClass("Zelig-sample-selection",
                                contains = "Zelig-ls")
zsampleselection$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, par2, cov2))
  }
)

##----- Quakes

# @knitr zelig_quake_step1_1
cov1 <- vcov(h_quake)[1:4, 1:4]
cov2 <- vcov(h_quake)[5:10, 5:10]
par1 <- coef(h_quake)[1:4]
par2 <- coef(h_quake)[5:10]
# Step 1: probit - the error structure is the same as 'heckit'
z1 <- zprobit$new()
z1$zelig(observed ~ lnprop_t_gs32_magnitude  + loggdppc_final + loggdp_final,
         data = FM_quake)
predicted1 <- predict(z1$zelig.out$z.out[[1]], FM_quake, type = "link")
FM_quake$invmills <- dnorm(predicted1) / pnorm(predicted1)
z1$setrange(loggdp_final = quantile(FM_quake$loggdp_final, 0.1, na.rm = TRUE),
            lnprop_t_gs32_magnitude = round(summary(FM_quake$lnprop_t_gs32_magnitude)[2][[1]]):
              round(summary(FM_quake$lnprop_t_gs32_magnitude)[5][[1]]))
z1$sim()
pdf("plot_zelig_quake_step1_1.pdf")
plot.ci(z1, ylim = c(0, 0.7),
        xlab = "ln quake propensity",
        ylab = "Selection probability")
dev.off()
# @knitr zelig_quake_step1_2
z1$setrange(loggdp_final = quantile(FM_quake$loggdp_final, 0.9, na.rm = TRUE),
            lnprop_t_gs32_magnitude = round(summary(FM_quake$lnprop_t_gs32_magnitude)[2][[1]]):
              round(summary(FM_quake$lnprop_t_gs32_magnitude)[5][[1]]))
z1$sim()
pdf("plot_zelig_quake_step1_2.pdf")
plot.ci(z1, ylim = c(0, 0.7),
        xlab = "ln quake propensity",
        ylab = "")
dev.off()

# @knitr zelig_quake_step2_1
z2 <- zsampleselection$new()
z2$zelig(lndis_loss_overall_usd1995 ~  lnt_gs32_magnitude +
              lnprop_t_gs32_magnitude + loggdppc_final + loggdp_final + invmills,
            data = FM_quake)
# z2$setx()
z2$setrange(loggdp_final = quantile(FM_quake$loggdp_final, 0.1, na.rm = TRUE),
            lnprop_t_gs32_magnitude = round(summary(FM_quake$lnprop_t_gs32_magnitude)[2][[1]]):
              round(summary(FM_quake$lnprop_t_gs32_magnitude)[5][[1]]))
z2$sim()
pdf("plot_zelig_quake_step2_1.pdf")
plot.ci(z2, ylim = c(-12, 4),
        xlab = "ln quake propensity",
        ylab = "Natural log of expected disaster loss")
dev.off()
# @knitr zelig_quake_step2_2
z2$setrange(loggdp_final = quantile(FM_quake$loggdp_final, 0.9, na.rm = TRUE),
            lnprop_t_gs32_magnitude = round(summary(FM_quake$lnprop_t_gs32_magnitude)[2][[1]]):
              round(summary(FM_quake$lnprop_t_gs32_magnitude)[5][[1]]))
z2$sim()
pdf("plot_zelig_quake_step2_2.pdf")
plot.ci(z2, ylim = c(-12, 4),
        xlab = "ln quake propensity",
        ylab = "")
dev.off()

##----- Cyclones

# @knitr zelig_cyclone_step1_1
cov1 <- vcov(h_cyclone)[1:4, 1:4]
cov2 <- vcov(h_cyclone)[5:10, 5:10]
par1 <- coef(h_cyclone)[1:4]
par2 <- coef(h_cyclone)[5:10]
# Step 1: probit - the error structure is the same as 'heckit'
z1 <- zprobit$new()
z1$zelig(observed ~ lnprop_t3_top_wind_speed + loggdppc_final + loggdp_final,
         data = FM_cyclone)
predicted1 <- predict(z1$zelig.out$z.out[[1]], FM_cyclone, type = "link")
FM_cyclone$invmills <- dnorm(predicted1) / pnorm(predicted1)
z1$setrange(loggdp_final = quantile(FM_cyclone$loggdp_final, 0.1, na.rm = TRUE),
            lnprop_t3_top_wind_speed = 
              round(summary(FM_cyclone$lnprop_t3_top_wind_speed))[2][[1]]:
              round(summary(FM_cyclone$lnprop_t3_top_wind_speed)[5][[1]]))
z1$sim()
pdf("plot_zelig_cyclone_step1_1.pdf")
plot.ci(z1, ylim = c(0, 0.7),
        xlab = "ln cyclone propensity",
        ylab = "Selection probability")
dev.off()
# @knitr zelig_cyclone_step1_2
z1$setrange(loggdp_final = quantile(FM_cyclone$loggdp_final, 0.9, na.rm = TRUE),
            lnprop_t3_top_wind_speed = 
              round(summary(FM_cyclone$lnprop_t3_top_wind_speed)[2][[1]]):
              round(summary(FM_cyclone$lnprop_t3_top_wind_speed)[5][[1]]))
z1$sim()
pdf("plot_zelig_cyclone_step1_2.pdf")
plot.ci(z1, ylim = c(0, 0.7),
        xlab = "ln cyclone propensity",
        ylab = "")
dev.off()

# @knitr zelig_cyclone_step2_1
z2 <- zsampleselection$new()
z2$zelig(lndis_loss_overall_usd1995 ~  lnt3_top_wind_speed + 
           lnprop_t3_top_wind_speed + loggdppc_final + loggdp_final + invmills,
         data = FM_cyclone)
# z2$setx()
z2$setrange(loggdp_final = quantile(FM_cyclone$loggdp_final, 0.1, na.rm = TRUE),
            lnprop_t3_top_wind_speed = 
              round(summary(FM_cyclone$lnprop_t3_top_wind_speed)[2][[1]]):
              round(summary(FM_cyclone$lnprop_t3_top_wind_speed)[5][[1]]))
z2$sim()
pdf("plot_zelig_cyclone_step2_1.pdf")
plot.ci(z2, ylim = c(-8, 4),
        xlab = "ln cyclone propensity",
        ylab = "Natural log of expected disaster loss")
dev.off()
# @knitr zelig_cyclone_step2_2
z2$setrange(loggdp_final = quantile(FM_cyclone$loggdp_final, 0.9, na.rm = TRUE),
            lnprop_t3_top_wind_speed = 
              round(summary(FM_cyclone$lnprop_t3_top_wind_speed)[2][[1]]):
              round(summary(FM_cyclone$lnprop_t3_top_wind_speed)[5][[1]]))
z2$sim()
pdf("plot_zelig_cyclone_step2_2.pdf")
plot.ci(z2, ylim = c(-8, 4),
        xlab = "ln cyclone propensity",
        ylab = "")
dev.off()

##----- Floods

# @knitr zelig_flood_step1_1
cov1 <- vcov(h_flood)[1:4, 1:4]
cov2 <- vcov(h_flood)[5:10, 5:10]
par1 <- coef(h_flood)[1:4]
par2 <- coef(h_flood)[5:10]
# Step 1: probit - the error structure is the same as 'heckit'
z1 <- zprobit$new()
z1$zelig(observed ~ lnprop_sum_precip_abs_pos + loggdppc_final + loggdp_final,
         data = FM_flood)
predicted1 <- predict(z1$zelig.out$z.out[[1]], FM_flood, type = "link")
FM_flood$invmills <- dnorm(predicted1) / pnorm(predicted1)
z1$setrange(loggdp_final = quantile(FM_flood$loggdp_final, 0.1, na.rm = TRUE),
            lnprop_sum_precip_abs_pos = 
              round(summary(FM_flood$lnprop_sum_precip_abs_pos)[2][[1]]):
              round(summary(FM_flood$lnprop_sum_precip_abs_pos)[5][[1]]))
z1$sim()
pdf("plot_zelig_flood_step1_1.pdf")
plot.ci(z1, ylim = c(0, 0.9),
        xlab = "ln flood propensity",
        ylab = "Selection probability")
dev.off()
# @knitr zelig_flood_step1_2
z1$setrange(loggdp_final = quantile(FM_flood$loggdp_final, 0.9, na.rm = TRUE),
            lnprop_sum_precip_abs_pos = 
              round(summary(FM_flood$lnprop_sum_precip_abs_pos)[2][[1]]):
              round(summary(FM_flood$lnprop_sum_precip_abs_pos)[5][[1]]))
z1$sim()
pdf("plot_zelig_flood_step1_2.pdf")
plot.ci(z1, ylim = c(0, 0.9),
        xlab = "ln flood propensity",
        ylab = "")
dev.off()

# @knitr zelig_flood_step2_1
z2 <- zsampleselection$new()
z2$zelig(lndis_loss_overall_usd1995 ~  lnsum_precip_abs_pos + 
           lnprop_sum_precip_abs_pos + loggdppc_final + loggdp_final + invmills,
         data = FM_flood)
# z2$setx()
z2$setrange(loggdp_final = 22,
            lnprop_sum_precip_abs_pos = 
              round(summary(FM_flood$lnprop_sum_precip_abs_pos)[2][[1]]):
              round(summary(FM_flood$lnprop_sum_precip_abs_pos)[5][[1]]))
z2$sim()
pdf("plot_zelig_flood_step2_1.pdf")
plot.ci(z2, ylim = c(-7, -3),
        xlab = "ln flood propensity",
        ylab = "Natural log of expected disaster loss")
dev.off()
# @knitr zelig_flood_step2_2
z1$setrange(loggdp_final = 26,
            lnprop_sum_precip_abs_pos = 
              round(summary(FM_flood$lnprop_sum_precip_abs_pos)[2][[1]]):
              round(summary(FM_flood$lnprop_sum_precip_abs_pos)[5][[1]]))
z2$sim()
pdf("plot_zelig_flood_step2_2.pdf")
plot.ci(z2, ylim = c(-7, -3),
        xlab = "ln flood propensity",
        ylab = "")
dev.off()
