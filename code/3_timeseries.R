library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(kableExtra)
library(fable)
library(tsibble)
library(lubridate)
library(ggpubr)

crimes <- read_excel("../VC basic data.xlsx",sheet = 1,skip = 1) %>% 
  select(`By RPT DT`,Murder:`Petit Larceny`) %>%
  select(-`Assault 3/Related`) %>% 
  na.omit() %>% 
  mutate(ym = yearmonth(`By RPT DT`)) %>% 
  as_tsibble(key=NULL, index = ym) %>% 
  setNames(.,c("date","murder","rape","robbery","felony_assault",
               "burglary","grand_larceny","GTA","petit_larceny","ym")) %>% 
  select(-date)

shootings <- read_excel("VC basic data.xlsx",sheet = 2,skip = 3) %>% 
  pivot_longer(!"...1") %>% 
  setNames(.,c("month","year","value")) %>% 
  mutate(ym = yearmonth(paste(month, year)),
         name = "Shootings") %>% 
  arrange(ym) %>% 
  select(ym,value,name)

shooting_arrests <- read_excel("VC basic data.xlsx",sheet = 4) %>% 
  select(`ARREST_DATE...6`, `weapons possession 1&2`) %>% 
  na.omit() %>% 
  setNames(.,c("date","value")) %>% 
  mutate(ym = yearmonth(date),
         name = "Weapon possession arrests") %>% 
  as_tsibble(key=NULL, index = ym) %>% 
  select(ym,value,name)

shootings_all <- rbind(shootings, shooting_arrests) %>% 
  na.omit() %>% 
  as_tsibble(key=name, index = ym)

crimes_long <- crimes %>% 
  pivot_longer(!ym)

get_trend_and_predict <- function(crime_type){
  #message(crime_type)
  
  dataset <- crimes_long %>% 
    filter(name == !!crime_type) 
  crime_model <- dataset %>% 
    model(ets = ETS(value)) 
  
  trend <- crime_model %>% 
    components() %>% 
    select(ym, level)
  
  future <- crime_model %>% 
    forecast(h = "15 months") %>% 
    hilo(level = c(95)) %>% 
    unpack_hilo("95%") %>% 
    select(name, ym, .mean, `95%_lower`, `95%_upper`) %>% 
    setNames(.,c("name","ym","predicted","predicted_lower","predicted_upper"))
  
  result <- full_join(trend, future, by = "ym")
  result <- full_join(result,
                      dataset %>% select(ym,value),
                      by = "ym")
  
  result <- result %>% 
    as.data.frame() %>% 
    filter(!is.na(value) | !is.na(predicted)) %>% 
    mutate(name = crime_type)
  
  return(result)
}

predictions <- lapply(c("murder","rape","robbery","felony_assault",
                        "burglary","grand_larceny","GTA","petit_larceny"),
                      get_trend_and_predict)

predictions_all <- data.table::rbindlist(predictions)

predictions_all$value[
  is.na(predictions_all$value)
] <- predictions_all$predicted[
  is.na(predictions_all$value)]

predictions_all <- predictions_all %>% 
  mutate(name = case_when(
    name == "murder" ~ "Murder",
    name == "rape" ~ "Rape",
    name == "robbery" ~ "Robbery",
    name == "felony_assault" ~ "Felony assault",
    name == "burglary" ~ "Burglary",
    name == "grand_larceny" ~ "Grand larceny",
    name == "GTA" ~ "Motor vehicle theft",
    name == "petit_larceny" ~ "Petit larceny"
  ))

fig_levels <- ggplot(data = predictions_all %>%
                       filter(ym > yearmonth("2015 September") &
                                ym < yearmonth("2023 January")  ),
                     aes(x = ym, y = level, group = 1)) +
  geom_line() +
  cowplot::theme_minimal_grid() + 
  facet_wrap(.~name,
             ncol = 1, scales = "free_y")+
  labs(x = "", y= "Deseasonalized count") +
  scale_x_yearmonth(date_breaks = "1 year",date_labels = "%Y")

fig_predictions <- ggplot(data = predictions_all %>% 
                            filter(ym > yearmonth("2022 September")),
                          aes(x = ym, y = value, group = 1)) +
  geom_line() +
  geom_ribbon(aes(ymin=predicted_lower, ymax=predicted_upper), alpha=0.2) +
  cowplot::theme_minimal_grid() + 
  facet_wrap(.~name,
             ncol = 1, scales = "free_y") +
  labs(x = "", y= "Predicted count")

p <- cowplot::plot_grid(fig_levels, fig_predictions,
                        labels = c('Deseasonalized trend', 'Predictions for 2023'))

ggsave("code/images/trends.png",p, bg = "white", width = 14, height = 16)


### Shootings

crimes_long <- shootings_all

predictions_sh <- lapply(c("Shootings","Weapon possession arrests"),
                      get_trend_and_predict)

predictions_sh_all <- data.table::rbindlist(predictions_sh)

p2 <- ggplot(data = predictions_sh_all %>%
                       filter(ym > yearmonth("2015 September") &
                                ym < yearmonth("2023 January")  ),
                     aes(x = ym, y = level, group = 1)) +
  geom_line() +
  cowplot::theme_minimal_grid() + 
  facet_wrap(.~name,
             ncol = 1, scales = "free_y")+
  labs(x = "", y= "Deseasonalized count") +
  scale_x_yearmonth(date_breaks = "1 year",date_labels = "%Y")

ggsave("code/images/trends_shootings.png",p2, bg = "white", width = 10, height = 6)
