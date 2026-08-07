# early emergence - weather
# charles thrift
# 3 august 2026

#######
# 0. Set working directory and load libraries
setwd("~/ground_nest/scripts")
library(tidyverse)
library(lubridate)

#######
# 1. Read in climate data, accessed via PRISM, https://prism.oregonstate.edu/explorer/
target <- read_csv("../data/prism_2025-26_data.csv") # weather experienced by the bees which emerged in 2026

climate <- read_csv("../data/prism_1991_2020_daily.csv") # weather at this site through time, 1991 to 2021
climate <- climate %>% 
  mutate(ymd = ymd(Date),
         doy = yday(ymd),
         year = year(Date)) %>% 
  filter(year < 2022) 

#######
# 2. Calculate 7-day rolling mean values of each weather parameter in the target year
target <- target %>%
  arrange(doy_custom) %>%
  mutate(tmax_roll7 = 
           slider::slide_dbl(
             tmax_degC,mean,.before = 3,.after = 3,.complete = TRUE),
         tmin_roll7 = 
           slider::slide_dbl(
             tmin_degC,mean,.before = 3,.after = 3,.complete = TRUE),
         tmean_roll7 = 
           slider::slide_dbl(
             tmean_degC,mean,.before = 3,.after = 3,.complete = TRUE),
         ppt_sum7 = 
           slider::slide_dbl(
             ppt_cm,sum,.before = 3,.after = 3,.complete = TRUE)
           )

target <- target %>% 
  filter(Date > "2025-06-30",
         Date < "2026-04-21")

#######
# 3. Calculate 30-year normals (7-day rolling mean values of each weather parameter through all years)
climate <- climate %>%
  arrange(ymd) %>%
  mutate(
    tmax_roll7 = 
      slider::slide_dbl(
        tmax_degC,mean,.before = 3,.after = 3,.complete = TRUE),
    tmin_roll7 = 
      slider::slide_dbl(
        tmin_degC,mean,.before = 3,.after = 3,.complete = TRUE),
    tmean_roll7 = 
      slider::slide_dbl(
        tmean_degC,mean,.before = 3,.after = 3,.complete = TRUE),
    ppt_sum7 = 
      slider::slide_dbl(
        ppt_cm,sum,.before = 3,.after = 3,.complete = TRUE)
  ) 

climate <- climate %>%
  mutate(
    water_year = if_else(month(ymd) >= 7,
                         year(ymd) + 1L,
                         year(ymd))
  ) %>%
  arrange(ymd) %>%
  group_by(water_year) %>%
  mutate(
    cumulative_ppt = cumsum(coalesce(ppt_cm, 0)),
    doy_custom = row_number()
  ) %>%
  ungroup()

climate <- climate %>% # filter to the days of interest
  filter(ymd > "1991-06-30",
         ymd < "2021-07-01")

#######
# 4. Compare 2025-26 observed values to 30-year normals
target_comp <- target
target_comp$tmax_percentile <-
  map2_dbl(
    target_comp$doy,
    target_comp$tmax_roll7,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$tmax_roll7)(x)
    }
  )
target_comp$tmin_percentile <-
  map2_dbl(
    target_comp$doy,
    target_comp$tmin_roll7,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$tmin_roll7)(x)
    }
  )
target_comp$tmean_percentile <-
  map2_dbl(
    target_comp$doy,
    target_comp$tmean_roll7,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$tmean_roll7)(x)
    }
  )
target_comp$pptSum_percentile <-
  map2_dbl(
    target_comp$doy,
    target_comp$ppt_sum7,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$ppt_sum7)(x)
    }
  )
target_comp$cumPpt_percentile <-
  map2_dbl(
    target_comp$doy,
    target_comp$cumPpt_cm,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$cumulative_ppt)(x)
    }
  )

# calculate sustained periods of high percentiles
make_rects <- function(data, percentile_col, threshold = 0.95, min_days = 5) {
  
  data %>%
    mutate(
      high = {{ percentile_col }} > threshold,
      group = cumsum(high != lag(high, default = FALSE))
    ) %>%
    filter(high) %>%
    group_by(group) %>%
    summarise(
      n = n(),
      xmin = min(doy_custom1) - 0.5,
      xmax = max(doy_custom1) + 0.5,
      .groups = "drop"
    ) %>%
    filter(n >= min_days)
  
}

ppt_rects   <- make_rects(target_comp, cumPpt_percentile)
tmax_rects  <- make_rects(target_comp, tmax_percentile)
tmin_rects  <- make_rects(target_comp, tmin_percentile)
tmean_rects <- make_rects(target_comp, tmean_percentile)

#######
# 5. Plot 2025-26 observed values and percent of 30-year normals
## Plot 1: Daily Max Temp
gmax <- ggplot()+
  geom_rect(data = tmax_rects,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.2) +
  geom_line(data = target_comp, aes(x = doy_custom1, y = tmax_roll7))+
  geom_point(data = target_comp, aes(x = doy_custom1, y = tmax_roll7,
                                     color = tmax_percentile>.95))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  theme_classic()+
  scale_x_continuous(limits = c(1,294),
                     breaks = c(1,32,63,93,124,154,185,216,244,275))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Daily max temp (\u00B0C)")

## Plot 2: Daily Min Temp
gmin <- ggplot()+
  geom_rect(data = tmin_rects,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.2) +
  geom_line(data = target_comp, aes(x = doy_custom1, y = tmin_roll7))+
  geom_point(data = target_comp, aes(x = doy_custom1, y = tmin_roll7,
                                     color = tmin_percentile>.95))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"))+
  theme_classic()+
  scale_x_continuous(limits = c(1,294),
                     breaks = c(1,32,63,93,124,154,185,216,244,275))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank())+
  labs(y = "Daily min temp (\u00B0C)")

## Plot 3: Daily Mean Temp
gmean <- ggplot()+
  geom_rect(data = tmean_rects,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.2) +
  geom_line(data = target_comp, aes(x = doy_custom1, y = tmean_roll7))+
  geom_point(data = target_comp, aes(x = doy_custom1, y = tmean_roll7,
                              color = tmean_percentile>.95))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"))+
  theme_classic()+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(1,294),
                     breaks = c(1,32,63,93,124,154,185,216,244,275),
                     labels = c("Jul","Aug","Sep",
                                "Oct","Nov","Dec",
                                "Jan","Feb","Mar",
                                "Apr"))+
  labs(y = "Daily mean temp (\u00B0C)",
       x = "Date")

## Plot 4: Cumulative Precipitation
gppt <- ggplot()+
  geom_rect(data = ppt_rects,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "blue",
            alpha = 0.2) +
  geom_line(data = climate,
            aes(x = doy_custom, 
                y = cumulative_ppt, 
                group = water_year),
            alpha = 0.3)+
  geom_line(data = target_comp, 
            aes(x = doy_custom1,
                y = cumPpt_cm),
            color = "blue",
            linewidth = 1)+
  labs(y = "Cum precip (cm)",
       x = "Date")+
  scale_x_continuous(limits = c(1,294),
                     breaks = c(1,32,63,93,124,154,185,216,244,275),
                     labels = c("Jul","Aug","Sep",
                                "Oct","Nov","Dec",
                                "Jan","Feb","Mar",
                                "Apr"))+
  theme_classic()


cowplot::plot_grid(gmax,gmin,gmean,gppt,
                   ncol = 2, nrow = 2)


ggsave("../figures/temp_ppt.png",
       height = 12,
       width = 20,
       units = "cm")

# count days above the 95% percentile
cd_dailymax <- target_comp %>% 
  filter(tmax_percentile > 0.95) %>% 
  summarise(count = n()) # 41
cd_dailymin <- target_comp %>% 
  filter(tmin_percentile > 0.95) %>% 
  summarise(count = n()) # 70
cd_dailymean <- target_comp %>% 
  filter(tmean_percentile > 0.95) %>% 
  summarise(count = n()) # 72
cd_cumprecip <- target_comp %>% 
  filter(cumPpt_percentile > 0.95) %>% 
  summarise(count = n()) # 68
