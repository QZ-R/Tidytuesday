pacman::p_load(tidyverse, tidylog, lubridate)
hablar::set_wd_to_script_path()

covid_19_raw <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid_19 <- covid_19_raw %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), 
               names_to = "date", 
               values_to = "confirmed_n") %>%
  select(-c(Lat, Long)) %>% 
  rename(province_state = `Province/State`,
         country_region = `Country/Region`) %>% 
  mutate(date = mdy(date)) %>% 
  group_by(country_region, date) %>% 
  summarise(confirmed_n = sum(confirmed_n)) %>% 
  ungroup()

  covid_19 %>% 
    filter(country_region == "US") %>% 
    ggplot(aes(x = date, y = confirmed_n)) +
    geom_line()
# if you look at the plot, it is total number of confirmed cases, what about new cases of every day?
# use lag() functions, 
 covid_19_daynew <- covid_19 %>% 
    arrange(date) %>%           # this step is to let all the data following the order of time so that we can calculate the new cases
    group_by(country_region) %>% 
    mutate(new_cases_n = confirmed_n - lag(confirmed_n, default = 0)) %>% 
    ungroup()
    
#plot
 covid_19_daynew %>%
    filter(country_region == "US") %>% 
    ggplot(aes(x = date, y = new_cases_n)) +
    geom_line() +
    scale_x_date(date_breaks = "2 week", date_labels = "%b" ) +
    scale_y_continuous() +
    theme_minimal()

# what if we want to compare US and China?
 covid_19_daynew %>%
   filter(country_region %in% c("China", "US")) %>% 
   ggplot(aes(x = date, y = new_cases_n, color = country_region)) +
   geom_line() +       #show.legend = FALSE
   scale_x_date(date_breaks = "2 week", date_labels = "%b" ) +
   scale_y_continuous() +
   #facet_wrap(~ country_region, ncol = 1, scales = "free_y") +
   labs(x = "Date", y = "Daily new cases", title = "New confirmed COVID-19 cases in China and US") +
   theme_minimal()
