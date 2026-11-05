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

target_comp <- target_comp %>% 
  mutate(cumPpt_percentile = 
           if_else(cumPpt_cm < 0.01, 0.00, cumPpt_percentile))

  
# calculate sustained periods of high percentiles (change threshold to change percentile cutoff)
make_rects <- function(data, percentile_col, threshold = 0.90, min_days = 5) {
  
  data %>%
    mutate(
      high = {{ percentile_col }} > threshold,
      group = cumsum(high != lag(high, default = FALSE))
    ) %>%
    filter(high) %>%
    group_by(group) %>%
    summarise(
      n = n(),
      xmin = min(doy_custom) - 0.5,
      xmax = max(doy_custom) + 0.5,
      .groups = "drop"
    ) %>%
    filter(n >= min_days)
  
}

ppt_rects   <- make_rects(target_comp, cumPpt_percentile)
tmax_rects  <- make_rects(target_comp, tmax_percentile)
tmin_rects  <- make_rects(target_comp, tmin_percentile)
tmean_rects <- make_rects(target_comp, tmean_percentile)

# count days above the 90% percentile
cd_dailymax <- target_comp %>% 
  filter(tmax_percentile > 0.90) %>% 
  summarise(count = n()) # 63
cd_dailymin <- target_comp %>% 
  filter(tmin_percentile > 0.90) %>% 
  summarise(count = n()) # 90
cd_dailymean <- target_comp %>% 
  filter(tmean_percentile > 0.90) %>% 
  summarise(count = n()) # 102
cd_cumprecip <- target_comp %>% 
  filter(cumPpt_percentile > 0.90) %>% 
  summarise(count = n()) # 119

#######
# 6. Repeat above steps for the 2022, 23, 24, and 25 emergence years
target_early <- read_csv("../data/prism_1991_2020_daily.csv") # weather at this site through time, 1991 to 2021
target_early <- target_early %>% 
  mutate(ymd = ymd(Date),
         doy = yday(ymd),
         year = year(Date)) %>% 
  filter(year > 2020) 

target_early <- target_early %>%
  mutate(water_year = if_else(month(ymd) >= 7,
                         year(ymd) + 1L,
                         year(ymd)),
          doy_custom_early = row_number() # doesn't reset
  ) %>%
  arrange(ymd) %>%
  group_by(water_year) %>%
  mutate(
    cumulative_ppt = cumsum(coalesce(ppt_cm, 0)),
    doy_custom = row_number() # resets each water year
  ) %>%
  ungroup() 

target_early <- target_early %>%
  arrange(doy_custom_early) %>%
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

target_early <- target_early %>% 
  filter(Date > "2021-06-30",
         Date < "2025-06-11")

#######
# 7. Compare 2022-25 observed values to 30-year normals
target_comp_e <- target_early
target_comp_e$tmax_percentile <-
  map2_dbl(
    target_comp_e$doy,
    target_comp_e$tmax_roll7,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$tmax_roll7)(x)
    }
  )
target_comp_e$tmin_percentile <-
  map2_dbl(
    target_comp_e$doy,
    target_comp_e$tmin_roll7,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$tmin_roll7)(x)
    }
  )
target_comp_e$tmean_percentile <-
  map2_dbl(
    target_comp_e$doy,
    target_comp_e$tmean_roll7,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$tmean_roll7)(x)
    }
  )
target_comp_e$pptSum_percentile <-
  map2_dbl(
    target_comp_e$doy,
    target_comp_e$ppt_sum7,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$ppt_sum7)(x)
    }
  )
target_comp_e$cumPpt_percentile <-
  map2_dbl(
    target_comp_e$doy,
    target_comp_e$cumulative_ppt,
    function(d, x){
      ref <- climate %>%
        filter(abs(doy - d) <= 7)
      ecdf(ref$cumulative_ppt)(x)
    }
  )


#######
# 8. Split up by emergence year
test22 <- target_comp_e %>% 
  filter(water_year == 2022,
         Date < "2022-05-18") #day after emergence
test23 <- target_comp_e %>% 
  filter(water_year == 2023,
         Date < "2023-05-26") #day after emergence
test24 <- target_comp_e %>% 
  filter(water_year == 2024,
         Date < "2024-05-30") #day after emergence
test25 <- target_comp_e %>% 
  filter(water_year == 2025,
         Date < "2025-06-11") #day after emergence

# for 2022: calculate sustained periods of high percentiles
ppt_rects_22 <- make_rects(test22, cumPpt_percentile) # note, nothing is above the percentile threshold for this variable this year
tmax_rects_22 <- make_rects(test22, tmax_percentile)
tmin_rects_22 <- make_rects(test22, tmin_percentile)
tmean_rects_22 <- make_rects(test22, tmean_percentile)

# for 2023: calculate sustained periods of high percentiles
ppt_rects_23 <- make_rects(test23, cumPpt_percentile)
tmax_rects_23 <- make_rects(test23, tmax_percentile)
tmin_rects_23 <- make_rects(test23, tmin_percentile)
tmean_rects_23 <- make_rects(test23, tmean_percentile)

# for 2024: calculate sustained periods of high percentiles
ppt_rects_24 <- make_rects(test24, cumPpt_percentile)
tmax_rects_24 <- make_rects(test24, tmax_percentile)
tmin_rects_24 <- make_rects(test24, tmin_percentile)
tmean_rects_24 <- make_rects(test24, tmean_percentile)

# for 2025: calculate sustained periods of high percentiles
ppt_rects_25 <- make_rects(test25, cumPpt_percentile)
tmax_rects_25 <- make_rects(test25, tmax_percentile)
tmin_rects_25 <- make_rects(test25, tmin_percentile)
tmean_rects_25 <- make_rects(test25, tmean_percentile)

# count the number of days above 90% for each year
# ppt:
count22 <- test22 %>% filter(cumPpt_percentile > 0.9)
count23 <- test23 %>% filter(cumPpt_percentile > 0.9)
count24 <- test24 %>% filter(cumPpt_percentile > 0.9)
count25 <- test25 %>% filter(cumPpt_percentile > 0.9)
count26 <- target_comp %>% filter(cumPpt_percentile > 0.9)

count22t <- test22 %>% filter(tmax_percentile > 0.9)
count23t <- test23 %>% filter(tmax_percentile > 0.9)
count24t <- test24 %>% filter(tmax_percentile > 0.9)
count25t <- test25 %>% filter(tmax_percentile > 0.9)
count26t <- target_comp %>% filter(tmax_percentile > 0.9)

compare_days <- data.frame(
  year = c(2022,2023,2024,2025,2026),
  days_90_temp = c(nrow(count22t),nrow(count23t),nrow(count24t),
                   nrow(count25t),nrow(count26t)),
  days_90_ppt = c(nrow(count22),nrow(count23),nrow(count24),
                  nrow(count25),nrow(count26)),
  emerge_doy = c(137,145,150,160,110)
                           )

ggplot(compare_days, aes(days_90_temp,emerge_doy))+
  geom_point()+
  geom_text(aes(label = year), nudge_y = 5, check_overlap = TRUE)+
  geom_smooth(method = "lm")

summary(lm(emerge_doy ~ days_90_temp, data = compare_days))

# growing days
t22_gdd <- test22 |> filter(doy_custom > 184) |> summarise(cstemp = sum(tmean_degC))
t23_gdd <- test23|> filter(doy_custom > 184) |> summarise(cstemp = sum(tmean_degC))
t24_gdd <- test24|> filter(doy_custom > 184) |> summarise(cstemp = sum(tmean_degC))
t25_gdd <- test25|> filter(doy_custom > 184) |> summarise(cstemp = sum(tmean_degC))
t26_gdd <- target_comp|> filter(doy_custom > 184) |> summarise(cstemp = sum(tmean_degC))

#######
# 9. Plot observed values (years 2022-25) and percent of 30-year normals
## Plot 1: Daily Max Temp
gmax22 <- ggplot()+
  geom_rect(data = tmax_rects_22,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.2) +
  geom_line(data = test22, aes(x = doy_custom, y = tmax_roll7))+
  geom_point(data = test22, aes(x = doy_custom, y = tmax_roll7,
                                     color = tmax_percentile>.90))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     limits = c(11,35),
                     breaks = c(15,20,25,30))+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334),
                     labels = c("Jul","Aug","Sep",
                                "Oct","Nov","Dec",
                                "Jan","Feb","Mar",
                                "Apr","May","Jun"))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

gmax23 <- ggplot()+
  geom_rect(data = tmax_rects_23,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.2) +
  geom_line(data = test23, aes(x = doy_custom, y = tmax_roll7))+
  geom_point(data = test23, aes(x = doy_custom, y = tmax_roll7,
                                color = tmax_percentile>.90))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     limits = c(11,35),
                     breaks = c(15,20,25,30))+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

gmax24 <- ggplot()+
  geom_rect(data = tmax_rects_24,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.2) +
  geom_line(data = test24, aes(x = doy_custom, y = tmax_roll7))+
  geom_point(data = test24, aes(x = doy_custom, y = tmax_roll7,
                                color = tmax_percentile>.90))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     limits = c(11,35),
                     breaks = c(15,20,25,30))+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

gmax25 <- ggplot()+
  geom_rect(data = tmax_rects_25,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.2) +
  geom_line(data = test25, aes(x = doy_custom, y = tmax_roll7))+
  geom_point(data = test25, aes(x = doy_custom, y = tmax_roll7,
                                color = tmax_percentile>.90))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     limits = c(11,35),
                     breaks = c(15,20,25,30))+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

## Plot 4: Cumulative Precipitation
# first, transform ppt from cm to mm
climate$cumulative_ppt <- 10*(climate$cumulative_ppt)
test22$cumulative_ppt <- 10*(test22$cumulative_ppt)
test23$cumulative_ppt <- 10*(test23$cumulative_ppt)
test24$cumulative_ppt <- 10*(test24$cumulative_ppt)
test25$cumulative_ppt <- 10*(test25$cumulative_ppt)
target_comp$cumPpt_cm <- 10*(target_comp$cumPpt_cm)

gppt22 <- ggplot()+
  geom_rect(data = ppt_rects_22, #####
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "blue",
            alpha = 0.2) +
  geom_line(data = climate,
            aes(x = doy_custom, 
                y = cumulative_ppt, 
                group = water_year),
            alpha = 0.3)+
  geom_line(data = test22, ########
            aes(x = doy_custom,
                y = cumulative_ppt),
            color = "blue",
            linewidth = 1)+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334),
                     labels = c("Jul","Aug","Sep",
                                "Oct","Nov","Dec",
                                "Jan","Feb","Mar",
                                "Apr","May","Jun"))+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


gppt23 <- ggplot()+
  geom_rect(data = ppt_rects_23,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "blue",
            alpha = 0.2) +
  geom_line(data = climate,
            aes(x = doy_custom, 
                y = cumulative_ppt, 
                group = water_year),
            alpha = 0.3)+
  geom_line(data = test23,
            aes(x = doy_custom,
                y = cumulative_ppt),
            color = "blue",
            linewidth = 1)+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

gppt24 <- ggplot()+
  geom_rect(data = ppt_rects_24,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "blue",
            alpha = 0.2) +
  geom_line(data = climate,
            aes(x = doy_custom, 
                y = cumulative_ppt, 
                group = water_year),
            alpha = 0.3)+
  geom_line(data = test24,
            aes(x = doy_custom,
                y = cumulative_ppt),
            color = "blue",
            linewidth = 1)+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

gppt25 <- ggplot()+
  geom_rect(data = ppt_rects_25,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "blue",
            alpha = 0.2) +
  geom_line(data = climate,
            aes(x = doy_custom, 
                y = cumulative_ppt, 
                group = water_year),
            alpha = 0.3)+
  geom_line(data = test25,
            aes(x = doy_custom,
                y = cumulative_ppt),
            color = "blue",
            linewidth = 1)+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334))+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())




#######
# 10. Plot observed values (years 2022-25) with 2026 year
gmax26 <- ggplot()+
  geom_rect(data = tmax_rects,
            aes(xmin = xmin,xmax = xmax,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE,
            fill = "red",
            alpha = 0.2) +
  geom_line(data = target_comp, aes(x = doy_custom, y = tmax_roll7))+
  geom_point(data = target_comp, aes(x = doy_custom, y = tmax_roll7,
                                     color = tmax_percentile>.90))+
  scale_color_manual(values = c("TRUE" = "red",
                                "FALSE" = "black"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     limits = c(11,35),
                     breaks = c(15,20,25,30))+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334),
                     labels = c("Jul","Aug","Sep",
                                "Oct","Nov","Dec",
                                "Jan","Feb","Mar",
                                "Apr","May","Jun"))+
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  labs(x = "date")

gppt26 <- ggplot()+
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
            aes(x = doy_custom,
                y = cumPpt_cm),
            color = "blue",
            linewidth = 1)+
  theme_classic()+
  scale_x_continuous(limits = c(1,365),
                     breaks = c(1,32,63,93,124,154,185,216,244,275,
                                305,334),
                     labels = c("Jul","Aug","Sep",
                                "Oct","Nov","Dec",
                                "Jan","Feb","Mar",
                                "Apr","May","Jun"))+
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  labs(x = "date")


emerge_box <- ggplot() +
  annotate(
    "text",
    x = 0.5, y = 0.5,
    label = "emergence",
    size = 7,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0
  ) +
  theme_void()
temp_box <- ggplot() +
  annotate(
    "text",
    x = 0.5, y = 0.5,
    label = "daily max temp (°C)",
    size = 7,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0
  ) +
  theme_void()
ppt_box <- ggplot() +
  annotate(
    "text",
    x = 0.5, y = 0.5,
    label = "cumulative precip (mm)",
    size = 7,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0
  ) +
  theme_void()
text_box22 <- ggplot() +
  annotate(
    "text",
    x = 0.5, y = 0.35,
    label = "2022",
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.65,
    label = "May 17",
    size = 6
  ) +
  xlim(0, .6) +
  ylim(0, 1) +
  theme_void()
text_box23 <- ggplot() +
  annotate(
    "text",
    x = 0.5, y = 0.35,
    label = "2023",
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.65,
    label = "May 25",
    size = 6
  ) +
  xlim(0, .6) +
  ylim(0, 1) +
  theme_void()
text_box24 <- ggplot() +
  annotate(
    "text",
    x = 0.5, y = 0.35,
    label = "2024",
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.65,
    label = "May 29",
    size = 6
  ) +
  xlim(0, .6) +
  ylim(0, 1) +
  theme_void()
text_box25 <- ggplot() +
  annotate(
    "text",
    x = 0.5, y = 0.35,
    label = "2025",
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.65,
    label = "June 10",
    size = 6
  ) +
  xlim(0, .6) +
  ylim(0, 1) +
  theme_void()
text_box26 <- ggplot() +
  annotate(
    "text",
    x = 0.5, y = 0.35,
    label = "2026",
    size = 4,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.65,
    label = "April 20",
    size = 6
  ) +
  xlim(0, .6) +
  ylim(0, 1) +
  theme_void()


cowplot::plot_grid(emerge_box,temp_box,ppt_box,
                   text_box25,gmax25,gppt25,
                   text_box24,gmax24,gppt24,
                   text_box23,gmax23,gppt23,
                   text_box22,gmax22,gppt22,
                   text_box26,gmax26,gppt26,
                   ncol = 3,nrow=6,
                   rel_heights = c(.5, 1, 1, 1, 1, 1.3),
                   rel_widths = c(.5,1,1))


ggsave("../figures/5years_lago.png",
       bg = "white",
       height = 20,
       width = 30,
       units = "cm")


