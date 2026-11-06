# analyze_climate_doy
# charles thrift
# 1 oct 2026

setwd("~/ground_nest/scripts/")

library(tidyverse)

data <- read_csv("../data/occ_climate_all_df_1oct2026.csv")

data <- data |>  
  mutate(ppt_year = rowSums(across(c(ppt_m.prev7,ppt_m.prev8,ppt_m.prev9,ppt_m.prev10,ppt_m.prev11,ppt_m.prev12,
                       ppt_m.1,ppt_m.2,ppt_m.3,ppt_m.4,ppt_m.5))),
         tmean_year = rowMeans(across(c(tmean_m.prev7,tmean_m.prev8,tmean_m.prev9,tmean_m.prev10,
                            tmean_m.prev11,tmean_m.prev12,
                            tmean_m.1,tmean_m.2,tmean_m.3,tmean_m.4,tmean_m.5))),
         tmax_year = rowMeans(across(c(tmax_m.prev7,tmax_m.prev8,tmax_m.prev9,tmax_m.prev10,
                             tmax_m.prev11,tmax_m.prev12,
                             tmax_m.1,tmax_m.2,tmax_m.3,tmax_m.4,tmax_m.5))),
         tmean_fall = rowMeans(across(c(tmean_m.prev9,tmean_m.prev10,tmean_m.prev11))), # sept, oct, nov
         tmean_winter = rowMeans(across(c(tmean_m.prev12,tmean_m.1,tmean_m.2))), # decemb, jan, feb
         tmean_spring = rowMeans(across(c(tmean_m.3,tmean_m.4,tmean_m.5))), # march, april, may
         ppt_fall = rowSums(across(c(ppt_m.prev9,ppt_m.prev10,ppt_m.prev11))),
         ppt_winter = rowSums(across(c(ppt_m.prev12,ppt_m.1,ppt_m.2))),
         ppt_spring = rowSums(across(c(ppt_m.3,ppt_m.4,ppt_m.5))),
         tmax_fall = rowMeans(across(c(tmax_m.prev9,tmax_m.prev10,tmax_m.prev11))),
         tmax_winter = rowMeans(across(c(tmax_m.prev12,tmax_m.1,tmax_m.2))),
         tmax_spring = rowMeans(across(c(tmax_m.3,tmax_m.4,tmax_m.5))))  |>  
  rowwise() |>  
  mutate(tmax_all_max = max(c_across(c(tmax_m.prev7,tmax_m.prev8,tmax_m.prev9,tmax_m.prev10,
                            tmax_m.prev11,tmax_m.prev12,
                            tmax_m.1,tmax_m.2,tmax_m.3,tmax_m.4,tmax_m.5,tmax_m.6)))) |> 
  ungroup()

## create locations (sites) for each point based on lat/long
data <- data |> 
  mutate(decLat1 = round(decimalLatitude, digits = 1),
         decLat0 = round(decimalLatitude, digits = 0),
         decLong1 = round(decimalLongitude, digits = 1),
         decLong0 = round(decimalLongitude, digits = 0))

data <- data |> 
  mutate(site2 = paste0("Site2_", dense_rank(interaction(decimalLatitude, decimalLongitude))),
         site1 = paste0("Site1_", dense_rank(interaction(decLat1, decLong1))),
         site0 = paste0("Site2_", dense_rank(interaction(decLat0, decLong0))))


# lmer to test doy~clim w/ random effect for location
# model #1: temperature
mod_temp <- lmerTest::lmer(doy~tmean_year + (1|site0),
                          data = data)
plot(DHARMa::simulateResiduals(mod_temp))
summary(mod_temp)
performance::r2(mod_temp)

# model #2: precipitation
mod_precip <- lmerTest::lmer(doy~ppt_year + (1|site0),
                              data = data)
plot(DHARMa::simulateResiduals(mod_precip))
summary(mod_precip)
performance::r2(mod_precip)

# get model predictions for each model
mod_temp_predict <- 
  ggeffects::ggpredict(mod_temp, terms = "tmean_year")
mod_precip_pred <- 
  ggeffects::ggpredict(mod_precip,terms = "ppt_year")

# plot each relationship with model predictions
figA <- ggplot()+
  geom_ribbon(data = mod_temp_predict,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.4)+
  geom_line(data = mod_temp_predict,
            aes(x = x,
                y = predicted),
            linewidth = 1)+
  geom_point(data=data, aes(tmean_year,doy),
             alpha = 0.5,
             color = "red4")+
  theme_classic()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("Apr","May","Jun","Jul",
                                "Aug", "Sep", "Oct", "Nov"))+
  labs(y = "emergence",
       x = "mean temperature (°C)")

figB <- ggplot()+
  geom_ribbon(data = mod_precip_pred,
              aes(x = x,
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.4)+
  geom_line(data = mod_precip_pred,
            aes(x = x,
                y = predicted),
            linetype = "dashed",
            linewidth = 1)+
  geom_point(data = data, aes(ppt_year,doy),
             alpha = 0.5,
             color = "cornflowerblue")+
  theme_classic()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("","","","",
                                "", "", "", ""))+
  labs(y = "",
       x = "cumulative precipitation (mm)")


cowplot::plot_grid(figA,NULL,figB,nrow = 1,
                   rel_widths = c(1,-0.04,1))
ggsave("../figures/doy_by_temp_ppt.png",
       bg = "white",
       width = 16,
       height = 8,
       units = "cm")