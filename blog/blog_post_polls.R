library(pacman)
p_load(rvest,tidyverse,janitor,lubridate)
options(digits = 2)


setwd("C:/Users/Ivar/Desktop/school/EC_523/blog")

#Read in polling data
polls_2016 <- read.csv("polls_2016/presidential_polls.csv")[,c(1,6:10,14,15)] %>% 
  tibble() %>% 
  mutate(
    startdate=as.Date(startdate,format ="%m/%d/%Y"),
    enddate=as.Date(enddate,format ="%m/%d/%Y"),
    fte_grade = grade, 
    Clinton = rawpoll_clinton,
    Trump = rawpoll_trump,
    grade = NULL,
    pollster = NULL,
    rawpoll_clinton = NULL,
    rawpoll_trump = NULL
    ) %>%
  filter(
    state %in% state.name,
    9<month(startdate),
    2016==year(startdate)
    ) 

polls_2020 <- read.csv("polls_2020/president_polls.csv")[,c(2:4,12,20,21,36,38)] %>% 
  mutate(
    startdate=as.Date(start_date,format ="%m/%d/%y"),
    enddate=as.Date(end_date,format ="%m/%d/%y"),
    start_date = NULL,
    end_date=NULL
  ) %>%
  .[,c(1:3,7:8,4:6)] %>%
  tibble() %>%
  filter(
    candidate_name %in% c("Joseph R. Biden Jr.","Donald Trump"), 
    state %in% state.name,
    9<month(startdate),
    2020==year(startdate)
    ) %>% 
  group_by(poll_id,cycle,state,startdate,enddate,fte_grade) %>% 
  summarise(Biden = pct[candidate_name=="Joseph R. Biden Jr."],Trump = pct[candidate_name=="Donald Trump"]) %>%
  mutate(trump_ratio = Trump/(Biden+Trump))
  

results_2016_url <- "https://en.wikipedia.org/wiki/2016_United_States_presidential_election"
results_2016_source <- read_html(results_2016_url) %>% 
  html_node("#mw-content-text > div.mw-parser-output > div:nth-child(221) > table") %>%
  html_table(fill = TRUE) 

colnames(results_2016_source) <- paste(names(results_2016_source),results_2016_source[1,])

results_2016 <- results_2016_source[-c(1,10,22,23,32:34,59),-c(9,10)] %>%
  clean_names() %>% tibble() %>% 
  mutate(state = c(state.name,"Total")) %>%
  .[c(22,2,5,3,6)]

results_2016[,c(2,3)] <- lapply(results_2016[,c(2,3)],function(x) {as.integer(gsub("[^[:digit:]]","",x))})
results_2016[,c(4,5)] <- lapply(results_2016[,c(4,5)],function(x) {as.numeric(gsub("%","",x))})

results_2020_url <- "https://en.wikipedia.org/wiki/2020_United_States_presidential_election"
results_2020_source <- read_html(results_2020_url) %>% 
  html_node("#mw-content-text > div.mw-parser-output > div:nth-child(216) > table") %>%
  html_table(fill = TRUE)
