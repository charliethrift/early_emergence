# analyze_climate_doy
# charles thrift
# 1 oct 2026

setwd("~/ground_nest/scripts/")

library(tidyverse)

data <- read_csv("../data/occ_climate_all_df_1oct2026.csv")

data <- data %>% 
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
         tmax_spring = rowMeans(across(c(tmax_m.3,tmax_m.4,tmax_m.5))))  %>% 
  rowwise() %>% 
  mutate(tmax_all_max = max(c_across(c(tmax_m.prev7,tmax_m.prev8,tmax_m.prev9,tmax_m.prev10,
                            tmax_m.prev11,tmax_m.prev12,
                            tmax_m.1,tmax_m.2,tmax_m.3,tmax_m.4,tmax_m.5,tmax_m.6)))) %>% 
  ungroup()


# test for relationship between doy and climate variables

models <- list(
  mean_temp_year   = lm(doy ~ tmean_year, data = data),
  mean_temp_fall   = lm(doy ~ tmean_fall, data = data),
  mean_temp_winter = lm(doy ~ tmean_winter, data = data),
  mean_temp_spring = lm(doy ~ tmean_spring, data = data),
  
  max_temp_year   = lm(doy ~ tmax_year, data = data),
  max_temp_fall   = lm(doy ~ tmax_fall, data = data),
  max_temp_winter = lm(doy ~ tmax_winter, data = data),
  max_temp_spring = lm(doy ~ tmax_spring, data = data),
  
  ppt_year   = lm(doy ~ ppt_year, data = data),
  ppt_fall   = lm(doy ~ ppt_fall, data = data),
  ppt_winter = lm(doy ~ ppt_winter, data = data),
  ppt_spring = lm(doy ~ ppt_spring, data = data)
)

results <- purrr::map_dfr(models, 
                          broom::tidy, 
                          .id = "variable") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    p_adjust_bonferroni = p.adjust(p.value, method = "bonferroni"),
    p_adjust_fdr        = p.adjust(p.value, method = "BH")
  ) %>%
  left_join(
    purrr::map_dfr(
      models,
      ~ broom::glance(.x),
      .id = "variable"
    ) %>%
      select(variable, r.squared, adj.r.squared),
    by = "variable"
  )

results$adj.r.squared.round <- round(results$adj.r.squared, digits = 2)


# only the five previous years at the lagoon
lago <- data %>% 
  filter(catalogNumber %in% 
           c("UCSB-IZC_fillerCT1oct26_c", # 2022
             "UCSB-IZC00056001", # 2023
             "UCSB-IZC_fillerCT1oct26_b", # 2024
             "UCSB-IZC_fillerCT1oct26_a", # 2025
             "UCSB-IZC00084222" # 2026
           ))

summary(lm(doy~tmean_year*ppt_year, data = data))
summary(lm(doy~tmean_year+ppt_year, data = data))
summary(lm(doy~tmean_year, data = data))

post70 <- data %>% filter(year > 1969)

ggplot(data, aes(tmax_year,doy))+
  geom_smooth(method = "lm", color = "black")+
  geom_point(alpha = 0.5, color = "red4")+
  theme_bw()+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("Apr","May","Jun","Jul",
                                "Aug", "Sep", "Oct", "Nov"))+
  labs(y = "emergence",
       x = "max temp (°C)")
ggplot(data, aes(ppt_year,doy))+
  geom_smooth(method = "lm", color = "black", linetype = "dashed")+
  geom_point(alpha = 0.5, color = "cornflowerblue")+
  theme_bw()+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("Apr","May","Jun","Jul",
                                "Aug", "Sep", "Oct", "Nov"))+
  labs(y = "emergence",
       x = "cumulative precip (mm)")

gmean <- ggplot(data, aes(tmean_year,doy))+
  geom_smooth(method = "lm", color = "black")+
  geom_point(alpha = 0.5, color = "orange3")+
  theme_classic()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("Apr","May","Jun","Jul",
                                "Aug", "Sep", "Oct", "Nov"))+
  labs(y = "emergence",
       x = "mean temp (°C)")

gmax <- ggplot(data, aes(tmax_year,doy))+
  geom_smooth(method = "lm", color = "black")+
  geom_point(alpha = 0.5, color = "red4")+
  theme_bw()+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("Apr","May","Jun","Jul",
                                "Aug", "Sep", "Oct", "Nov"))+
  labs(y = "emergence",
       x = "max temp (°C)")

gppt <- ggplot(data, aes(ppt_year,doy))+
  geom_smooth(method = "lm", color = "black", linetype = "dashed")+
  geom_point(alpha = 0.5, color = "cornflowerblue")+
  theme_bw()+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("Apr","May","Jun","Jul",
                                "Aug", "Sep", "Oct", "Nov"))+
  labs(y = "emergence",
       x = "cumulative precip (mm)")
  

cowplot::plot_grid(gmean,gmax,gppt,
                   nrow = 1)

gmax1 <- ggplot(data, aes(tmax_year,doy))+
  geom_smooth(method = "lm", color = "black")+
  geom_point(alpha = 0.5, color = "red4")+
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("","","","",
                                "", "", "", ""))+
  labs(y = "",
       x = "max temp (°C)")

gppt1 <- ggplot(data, aes(ppt_year,doy))+
  geom_smooth(method = "lm", color = "black", linetype = "dashed")+
  geom_point(alpha = 0.5, color = "cornflowerblue")+
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("","","","",
                                "", "", "", ""))+
  labs(y = "",
       x = "cumulative precip (mm)")


cowplot::plot_grid(gmean,NULL,gmax1,NULL,gppt1,
                   nrow = 1,rel_widths = c(1,-0.04,1,-0.04,1))

summary(data$doy)


ggplot(lago, aes(tmean_year, doy))+
  geom_point()+
  geom_text(aes(label = year), nudge_x = 0.1, check_overlap = TRUE)+
  theme_classic()
ggplot(lago, aes(ppt_year, doy))+
  geom_point()+
  geom_text(aes(label = year), nudge_y = 5, check_overlap = TRUE)+
  theme_classic()

ggplot(data, aes(ppt_fall,ppt_winter))+
  geom_point()

hist((data$ppt_year)^(1/3))

data <- data %>% 
  mutate(cube_ppt_year = ppt_year^(1/3))

# how correlated are values?
cor_matrix <- data %>% 
  select(ppt_fall,ppt_winter,ppt_spring,ppt_year,
         tmean_fall,tmean_winter,tmean_spring,tmean_year,
         tmax_fall,tmax_winter,tmax_spring,tmax_year) %>% 
  rstatix::cor_mat()


ggplot(new_df, aes(tr_ppt_winter,tr_ppt_spring))+
  geom_point()+
  geom_smooth(method = "lm")

summary(lm(tr_ppt_winter~tr_ppt_fall,data = new_df))


# running two models
# check residuals?
mod_temp <- lm(doy~tmean_year, data = data)
mod_precip <- lm(doy~cube_ppt_year, data = data)

sim_res_t <- DHARMa::simulateResiduals(fittedModel = mod_temp, n = 250)
sim_res_p <- DHARMa::simulateResiduals(fittedModel = mod_precip, n = 250)

plot(sim_res_t)
plot(sim_res_p)

ggplot(data, aes(cube_ppt_year,doy))+
  geom_point()+
  geom_smooth(method = "lm")

summary(mod_temp)
summary(mod_precip)
0.083*2

fig3a <- ggplot(data, aes(tmean_year,doy))+
  geom_smooth(method = "lm", color = "black")+
  geom_point(alpha = 0.5, color = "red4")+
  theme_classic()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("Apr","May","Jun","Jul",
                                "Aug", "Sep", "Oct", "Nov"))+
  labs(y = "emergence",
       x = "mean temp (°C)")
fig3b <- ggplot(data, aes(cube_ppt_year,doy))+
  geom_smooth(method = "lm", color = "black", linetype = "dashed")+
  geom_point(alpha = 0.5, color = "cornflowerblue")+
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(90,305),
                     breaks = c(91,121,152,
                                182,213,244,274,305),
                     labels = c("","","","",
                                "", "", "", ""))+
  scale_x_continuous(breaks = c(4,6,8,10),
                     labels = c(64,216,512,1000))+
  labs(y = "",
       x = "cumulative precip (mm)")

cowplot::plot_grid(fig3a,fig3b,nrow = 1)

