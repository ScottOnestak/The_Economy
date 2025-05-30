# The Economy
# 1 Recession
# Scott Onestak

#packages
library(dplyr)
library(tidyr)
library(fredr)
library(h2o)
library(lubridate)

options(scipen=999)

#Get Economic Variables
fredr_set_key("1e7ed343d3ccb82af108e43174bf8f1f")

unemployment = fredr(series_id = "UNRATE",
                     observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                     frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(unemployment=value)

tres_30y = fredr(series_id = "DGS30",
                 observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                 frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_30y=value)

tres_20y = fredr(series_id = "DGS20",
                 observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                 frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_20y=value)

tres_10y = fredr(series_id = "DGS10",
                 observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                 frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_10y=value)  

tres_5y = fredr(series_id = "DGS5",
                observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_5y=value)  

tres_2y = fredr(series_id = "DGS2",
                observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_2y=value)  

tres_1y = fredr(series_id = "DGS1",
                observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_1y=value)  

tres_6m = fredr(series_id = "DGS6MO",
                observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_6m=value) 

tres_3m = fredr(series_id = "DGS3MO",
                observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_3m=value) 

tres_1m = fredr(series_id = "DGS1MO",
                observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_1m=value) 

sentiment = fredr(series_id = "UMCSENT",
                  observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                  frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(sentiment=value) 

lfpr = fredr(series_id = "CIVPART",
             observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
             frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(lfpr=value) 

production = fredr(series_id = "INDPRO",
                   observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                   frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(production=value)

fed_funds = fredr(series_id = "DFF",
                  observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                  frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(fed_funds=value)

m2 = fredr(series_id = "M2SL",
           observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
           frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(m2=value)

GDP = fredr(series_id = "GDP",
            observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
            frequency = "q") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(GDP=value)

initial_claims = fredr(series_id = "ICSA",
                       observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                       frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(initial_claims=value)

leverage = fredr(series_id = "NFCILEVERAGE",
                 observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                 frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(leverage_index=value)

credit = fredr(series_id = "NFCICREDIT",
               observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
               frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(credit_index=value)

risk = fredr(series_id = "NFCIRISK",
             observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
             frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(risk_index=value)

recession = fredr(series_id = "USREC",
                  observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                  frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(recession=value)

tres_30y_c = unlist(fredr(series_id = "DGS30",
                          observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                          frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                      filter(date == max(date)) %>% select(value))

tres_20y_c = unlist(fredr(series_id = "DGS20",
                          observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                          frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                      filter(date == max(date)) %>% select(value))

tres_10y_c = unlist(fredr(series_id = "DGS10",
                          observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                          frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                      filter(date == max(date)) %>% select(value))  

tres_5y_c = unlist(fredr(series_id = "DGS5",
                         observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                         frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                     filter(date == max(date)) %>% select(value))  

tres_2y_c = unlist(fredr(series_id = "DGS2",
                         observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                         frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                     filter(date == max(date)) %>% select(value)) 

tres_1y_c = unlist(fredr(series_id = "DGS1",
                         observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                         frequency = "d") %>% select(date,value) %>%
                     filter(date == max(date)) %>% select(value))  

tres_6m_c = unlist(fredr(series_id = "DGS6MO",
                         observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                         frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                     filter(date == max(date)) %>% select(value))

tres_3m_c = unlist(fredr(series_id = "DGS3MO",
                         observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                         frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                     filter(date == max(date)) %>% select(value))

tres_1m_c = unlist(fredr(series_id = "DGS1MO",
                         observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                         frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                     filter(date == max(date)) %>% select(value))

fed_funds_c = unlist(fredr(series_id = "DFF",
                           observation_start = as.Date(as.character(Sys.Date()-5),format="%Y-%m-%d"),
                           frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>%
                       filter(date == max(date)) %>% select(value))

leverage_index_c = unlist(fredr(series_id = "NFCILEVERAGE",
                                observation_start = as.Date(as.character(Sys.Date()-14),format="%Y-%m-%d"),
                                frequency = "w") %>% select(date,value) %>% filter(!is.na(value)) %>%
                            filter(date == max(date)) %>% select(value))

credit_index_c = unlist(fredr(series_id = "NFCICREDIT",
                              observation_start = as.Date(as.character(Sys.Date()-14),format="%Y-%m-%d"),
                              frequency = "w") %>% select(date,value) %>% filter(!is.na(value)) %>%
                          filter(date == max(date)) %>% select(value))

risk_index_c = unlist(fredr(series_id = "NFCIRISK",
                            observation_start = as.Date(as.character(Sys.Date()-14),format="%Y-%m-%d"),
                            frequency = "w") %>% select(date,value) %>% filter(!is.na(value)) %>%
                        filter(date == max(date)) %>% select(value))

tres_30y_all = fredr(series_id = "DGS30",
                     observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                     frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_30y=value)

tres_1y_all = fredr(series_id = "DGS1",
                    observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                    frequency = "d") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(tres_1y=value)

#Create treasury 30 year - 1 year spread data for .csv export
tres_30y_1yr_spread = tres_30y_all %>% inner_join(.,tres_1y_all,by="date") %>%
  mutate(tres_30y_1yr_spread = tres_30y - tres_1y,
         join = paste(substr(date,1,8),"01",sep="")) %>%
  left_join(.,recession %>% mutate(join = as.character(date)) %>% select(-date),by="join") %>%
  select(-join) %>%
  mutate(recession = ifelse(is.na(recession),0,recession))
write.csv(tres_30y_1yr_spread,"Data/Recession Probability/tres_30y_1yr_spread.csv",row.names=F)

#Create current yield curve dataset
yield_curve = as.data.frame(cbind(c(1,3,6,12,24,60,120,240,360),
                                  c(tres_1m_c,tres_3m_c,tres_6m_c,tres_1y_c,tres_2y_c,tres_5y_c,
                                    tres_10y_c,tres_20y_c,tres_30y_c),
                                  c(unlist(tres_1m %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_1m)),
                                    unlist(tres_3m %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_3m)),
                                    unlist(tres_6m %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_6m)),
                                    unlist(tres_1y %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_1y)),
                                    unlist(tres_2y %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_2y)),
                                    unlist(tres_5y %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_5y)),
                                    unlist(tres_10y %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_10y)),
                                    unlist(tres_20y %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_20y)),
                                    unlist(tres_30y %>% filter(date==floor_date(Sys.Date(),"month") - months(3)) %>% select(tres_30y))),
                                  c(unlist(tres_1m %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_1m)),
                                    unlist(tres_3m %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_3m)),
                                    unlist(tres_6m %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_6m)),
                                    unlist(tres_1y %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_1y)),
                                    unlist(tres_2y %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_2y)),
                                    unlist(tres_5y %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_5y)),
                                    unlist(tres_10y %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_10y)),
                                    unlist(tres_20y %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_20y)),
                                    unlist(tres_30y %>% filter(date==floor_date(Sys.Date(),"month") - months(6)) %>% select(tres_30y))),
                                  c(unlist(tres_1m %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_1m)),
                                    unlist(tres_3m %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_3m)),
                                    unlist(tres_6m %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_6m)),
                                    unlist(tres_1y %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_1y)),
                                    unlist(tres_2y %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_2y)),
                                    unlist(tres_5y %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_5y)),
                                    unlist(tres_10y %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_10y)),
                                    unlist(tres_20y %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_20y)),
                                    unlist(tres_30y %>% filter(date==floor_date(Sys.Date(),"month") - months(12)) %>% select(tres_30y))),
                                  c(unlist(tres_1m %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_1m)),
                                    unlist(tres_3m %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_3m)),
                                    unlist(tres_6m %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_6m)),
                                    unlist(tres_1y %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_1y)),
                                    unlist(tres_2y %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_2y)),
                                    unlist(tres_5y %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_5y)),
                                    unlist(tres_10y %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_10y)),
                                    unlist(tres_20y %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_20y)),
                                    unlist(tres_30y %>% filter(date==floor_date(Sys.Date(),"month") - months(24)) %>% select(tres_30y)))))
colnames(yield_curve) = c("Bond Duration (in Months)","Current Yield","Yield 3 Months Previous","Yield 6 Months Previous",
                          "Yield 1 Year Previous","Yield 2 Years Previous")
write.csv(yield_curve,"Data/Recession Probability/yield_curve.csv",row.names=F)

#Join together to build dataset
theData = unemployment %>%
  left_join(.,tres_30y,by="date") %>%
  left_join(.,tres_20y,by="date") %>%
  left_join(.,tres_10y,by="date") %>%
  left_join(.,tres_5y,by="date") %>%
  left_join(.,tres_2y,by="date") %>%
  left_join(.,tres_1y,by="date") %>%
  left_join(.,tres_6m,by="date") %>%
  left_join(.,tres_3m,by="date") %>%
  left_join(.,tres_1m,by="date") %>%
  left_join(.,sentiment,by="date") %>%
  left_join(.,lfpr,by="date") %>%
  left_join(.,production,by="date") %>%
  left_join(.,fed_funds,by="date") %>%
  left_join(.,m2,by="date") %>%
  left_join(.,GDP,by="date") %>%
  left_join(.,initial_claims,by="date") %>%
  left_join(.,leverage,by="date") %>%
  left_join(.,credit,by="date") %>%
  left_join(.,risk,by="date") %>%
  left_join(.,recession,by="date") %>%
  arrange(date)

#Fill in GDP since it's monthly and sentiment when it was quarterly
GDP_temp = NA
counter = 1

for(i in seq(from=1,to=dim(theData)[1],by=1)){
  if(!is.na(theData[i,"GDP"])){
    GDP_temp = theData[i,"GDP"]
    counter = 1
  } else {
    if(!is.na(GDP_temp)){
      if(counter <= 2){
        theData[i,"GDP"] = GDP_temp
        counter = counter + 1
      } else {
        GDP_temp = NA
        counter = 1
      }
    }
  }
}

sent_temp = NA
counter2 = 1
for(i in seq(from=1,to=dim(theData)[1],by=1)){
  if(!is.na(theData[i,"sentiment"])){
    sent_temp = theData[i,"sentiment"]
    counter2 = 1
  } else {
    if(!is.na(sent_temp)){
      if(counter <= 2){
        theData[i,"sentiment"] = sent_temp
        counter2 = counter2 + 1
      } else {
        sent_temp = NA
        counter2 = 1
      }
    }
  }
}

theData$time = as.numeric(row.names(theData))
#test = theData

#Build recession dependent variable time periods
recession_3m = theData %>% select(time) %>% mutate(time_max = time + 3) %>% rename(time_min = time) %>%
  full_join(.,theData %>% select(time,recession),by=character()) %>%
  filter(time > time_min & time <= time_max) %>%
  group_by(time_min) %>%
  summarise(recession = max(recession),
            count = n()) %>%
  filter(count >= 3) %>%
  select(time_min,recession) %>%
  rename(time = time_min, recession_3m = recession)

recession_6m = theData %>% select(time) %>% mutate(time_max = time + 6) %>% rename(time_min = time) %>%
  full_join(.,theData %>% select(time,recession),by=character()) %>%
  filter(time > time_min & time <= time_max) %>%
  group_by(time_min) %>%
  summarise(recession = max(recession),
            count = n()) %>%
  filter(count >= 6) %>%
  select(time_min,recession) %>%
  rename(time = time_min, recession_6m = recession)

recession_12m = theData %>% select(time) %>% mutate(time_max = time + 12) %>% rename(time_min = time) %>%
  full_join(.,theData %>% select(time,recession),by=character()) %>%
  filter(time > time_min & time <= time_max) %>%
  group_by(time_min) %>%
  summarise(recession = max(recession),
            count = n()) %>%
  filter(count >= 12) %>%
  select(time_min,recession) %>%
  rename(time = time_min, recession_12m = recession)

recession_18m = theData %>% select(time) %>% mutate(time_max = time + 18) %>% rename(time_min = time) %>%
  full_join(.,theData %>% select(time,recession),by=character()) %>%
  filter(time > time_min & time <= time_max) %>%
  group_by(time_min) %>%
  summarise(recession = max(recession),
            count = n()) %>%
  filter(count >= 18) %>%
  select(time_min,recession) %>%
  rename(time = time_min, recession_18m = recession)

#Build additional variables
theData = theData %>% mutate(tres_30y_20y_spread = tres_30y - tres_20y,
                             tres_30y_10y_spread = tres_30y - tres_10y,
                             tres_30y_5y_spread = tres_30y - tres_5y,
                             tres_30y_2y_spread = tres_30y - tres_2y,
                             tres_30y_1y_spread = tres_30y - tres_1y,
                             tres_30y_6m_spread = tres_30y - tres_6m,
                             tres_30y_3m_spread = tres_30y - tres_3m,
                             tres_30y_1m_spread = tres_30y - tres_1m,
                             tres_20y_10y_spread = tres_20y - tres_10y,
                             tres_20y_5y_spread = tres_20y - tres_5y,
                             tres_20y_2y_spread = tres_20y - tres_2y,
                             tres_20y_1y_spread = tres_20y - tres_1y,
                             tres_20y_6m_spread = tres_20y - tres_6m,
                             tres_20y_3m_spread = tres_20y - tres_3m,
                             tres_20y_1m_spread = tres_20y - tres_1m,
                             tres_10y_5y_spread = tres_10y - tres_5y,
                             tres_10y_2y_spread = tres_10y - tres_2y,
                             tres_10y_1y_spread = tres_10y - tres_1y,
                             tres_10y_6m_spread = tres_10y - tres_6m,
                             tres_10y_3m_spread = tres_10y - tres_3m,
                             tres_10y_1m_spread = tres_10y - tres_1m,
                             tres_5y_2y_spread = tres_5y - tres_2y,
                             tres_5y_1y_spread = tres_5y - tres_1y,
                             tres_5y_6m_spread = tres_5y - tres_6m,
                             tres_5y_3m_spread = tres_5y - tres_3m,
                             tres_5y_1m_spread = tres_5y - tres_1m,
                             tres_2y_1y_spread = tres_2y - tres_1y,
                             tres_2y_6m_spread = tres_2y - tres_6m,
                             tres_2y_3m_spread = tres_2y - tres_3m,
                             tres_2y_1m_spread = tres_2y - tres_1m,
                             tres_1y_6m_spread = tres_1y - tres_6m,
                             tres_1y_3m_spread = tres_1y - tres_3m,
                             tres_1y_1m_spread = tres_1y - tres_1m,
                             tres_6m_3m_spread = tres_6m - tres_3m,
                             tres_6m_1m_spread = tres_6m - tres_1m,
                             tres_3m_1m_spread = tres_3m - tres_1m,
                             m2_velocity = GDP / m2)

theData = theData %>% left_join(.,theData %>% 
                                  select(time,unemployment) %>% 
                                  mutate(time = time + 3) %>% 
                                  rename(unemployment3 = unemployment),by="time") %>%
  left_join(.,theData %>% 
              select(time,unemployment) %>% 
              mutate(time = time + 6) %>% 
              rename(unemployment6 = unemployment),by="time") %>%
  left_join(.,theData %>% 
              select(time,unemployment) %>% 
              mutate(time = time + 12) %>% 
              rename(unemployment12 = unemployment),by="time") %>%
  left_join(.,theData %>% select(time,tres_30y_20y_spread,tres_30y_10y_spread,tres_30y_5y_spread,
                                 tres_30y_2y_spread,tres_30y_1y_spread,tres_30y_6m_spread,
                                 tres_30y_3m_spread,tres_30y_1m_spread,tres_20y_10y_spread,
                                 tres_20y_5y_spread,tres_20y_2y_spread,tres_20y_1y_spread,
                                 tres_20y_6m_spread,tres_20y_3m_spread,tres_20y_1m_spread,
                                 tres_10y_5y_spread,tres_10y_2y_spread,tres_10y_1y_spread,
                                 tres_10y_6m_spread,tres_10y_3m_spread,tres_10y_1m_spread,
                                 tres_5y_2y_spread,tres_5y_1y_spread,tres_5y_6m_spread,
                                 tres_5y_3m_spread,tres_5y_1m_spread,tres_2y_1y_spread,
                                 tres_2y_6m_spread,tres_2y_3m_spread,tres_2y_1m_spread,
                                 tres_1y_6m_spread,tres_1y_3m_spread,tres_1y_1m_spread,
                                 tres_6m_3m_spread,tres_6m_1m_spread,tres_3m_1m_spread,
                                 tres_30y,tres_20y,tres_10y,tres_5y,tres_2y,tres_1y,
                                 tres_6m,tres_3m,tres_1m) %>%
              mutate(time = time + 3) %>%
              rename(tres_30y_20y_spread_lead_3m = tres_30y_20y_spread,
                     tres_30y_10y_spread_lead_3m = tres_30y_10y_spread,
                     tres_30y_5y_spread_lead_3m = tres_30y_5y_spread,
                     tres_30y_2y_spread_lead_3m = tres_30y_2y_spread,
                     tres_30y_1y_spread_lead_3m = tres_30y_1y_spread,
                     tres_30y_6m_spread_lead_3m = tres_30y_6m_spread,
                     tres_30y_3m_spread_lead_3m = tres_30y_3m_spread,
                     tres_30y_1m_spread_lead_3m = tres_30y_1m_spread,
                     tres_20y_10y_spread_lead_3m = tres_20y_10y_spread,
                     tres_20y_5y_spread_lead_3m = tres_20y_5y_spread,
                     tres_20y_2y_spread_lead_3m = tres_20y_2y_spread,
                     tres_20y_1y_spread_lead_3m = tres_20y_1y_spread,
                     tres_20y_6m_spread_lead_3m = tres_20y_6m_spread,
                     tres_20y_3m_spread_lead_3m = tres_20y_3m_spread,
                     tres_20y_1m_spread_lead_3m = tres_20y_1m_spread,
                     tres_10y_5y_spread_lead_3m = tres_10y_5y_spread,
                     tres_10y_2y_spread_lead_3m = tres_10y_2y_spread,
                     tres_10y_1y_spread_lead_3m = tres_10y_1y_spread,
                     tres_10y_6m_spread_lead_3m = tres_10y_6m_spread,
                     tres_10y_3m_spread_lead_3m = tres_10y_3m_spread,
                     tres_10y_1m_spread_lead_3m = tres_10y_1m_spread,
                     tres_5y_2y_spread_lead_3m = tres_5y_2y_spread,
                     tres_5y_1y_spread_lead_3m = tres_5y_1y_spread,
                     tres_5y_6m_spread_lead_3m = tres_5y_6m_spread,
                     tres_5y_3m_spread_lead_3m = tres_5y_3m_spread,
                     tres_5y_1m_spread_lead_3m = tres_5y_1m_spread,
                     tres_2y_1y_spread_lead_3m = tres_2y_1y_spread,
                     tres_2y_6m_spread_lead_3m = tres_2y_6m_spread,
                     tres_2y_3m_spread_lead_3m = tres_2y_3m_spread,
                     tres_2y_1m_spread_lead_3m = tres_2y_1m_spread,
                     tres_1y_6m_spread_lead_3m = tres_1y_6m_spread,
                     tres_1y_3m_spread_lead_3m = tres_1y_3m_spread,
                     tres_1y_1m_spread_lead_3m = tres_1y_1m_spread,
                     tres_6m_3m_spread_lead_3m = tres_6m_3m_spread,
                     tres_6m_1m_spread_lead_3m = tres_6m_1m_spread,
                     tres_3m_1m_spread_lead_3m = tres_3m_1m_spread,
                     tres_30y_lead3 = tres_30y, tres_20y_lead3 = tres_20y,
                     tres_10y_lead3 = tres_10y, tres_5y_lead3 = tres_5y,
                     tres_2y_lead3 = tres_2y, tres_1y_lead3 = tres_1y,
                     tres_6m_lead3 = tres_6m, tres_3m_lead3 = tres_3m,
                     tres_1m_lead3 = tres_1m),by="time") %>%
  left_join(.,theData %>% select(time,tres_30y_20y_spread,tres_30y_10y_spread,tres_30y_5y_spread,
                                 tres_30y_2y_spread,tres_30y_1y_spread,tres_30y_6m_spread,
                                 tres_30y_3m_spread,tres_30y_1m_spread,tres_20y_10y_spread,
                                 tres_20y_5y_spread,tres_20y_2y_spread,tres_20y_1y_spread,
                                 tres_20y_6m_spread,tres_20y_3m_spread,tres_20y_1m_spread,
                                 tres_10y_5y_spread,tres_10y_2y_spread,tres_10y_1y_spread,
                                 tres_10y_6m_spread,tres_10y_3m_spread,tres_10y_1m_spread,
                                 tres_5y_2y_spread,tres_5y_1y_spread,tres_5y_6m_spread,
                                 tres_5y_3m_spread,tres_5y_1m_spread,tres_2y_1y_spread,
                                 tres_2y_6m_spread,tres_2y_3m_spread,tres_2y_1m_spread,
                                 tres_1y_6m_spread,tres_1y_3m_spread,tres_1y_1m_spread,
                                 tres_6m_3m_spread,tres_6m_1m_spread,tres_3m_1m_spread,
                                 tres_30y,tres_20y,tres_10y,tres_5y,tres_2y,tres_1y,
                                 tres_6m,tres_3m,tres_1m) %>%
              mutate(time = time + 6) %>%
              rename(tres_30y_20y_spread_lead_6m = tres_30y_20y_spread,
                     tres_30y_10y_spread_lead_6m = tres_30y_10y_spread,
                     tres_30y_5y_spread_lead_6m = tres_30y_5y_spread,
                     tres_30y_2y_spread_lead_6m = tres_30y_2y_spread,
                     tres_30y_1y_spread_lead_6m = tres_30y_1y_spread,
                     tres_30y_6m_spread_lead_6m = tres_30y_6m_spread,
                     tres_30y_3m_spread_lead_6m = tres_30y_3m_spread,
                     tres_30y_1m_spread_lead_6m = tres_30y_1m_spread,
                     tres_20y_10y_spread_lead_6m = tres_20y_10y_spread,
                     tres_20y_5y_spread_lead_6m = tres_20y_5y_spread,
                     tres_20y_2y_spread_lead_6m = tres_20y_2y_spread,
                     tres_20y_1y_spread_lead_6m = tres_20y_1y_spread,
                     tres_20y_6m_spread_lead_6m = tres_20y_6m_spread,
                     tres_20y_3m_spread_lead_6m = tres_20y_3m_spread,
                     tres_20y_1m_spread_lead_6m = tres_20y_1m_spread,
                     tres_10y_5y_spread_lead_6m = tres_10y_5y_spread,
                     tres_10y_2y_spread_lead_6m = tres_10y_2y_spread,
                     tres_10y_1y_spread_lead_6m = tres_10y_1y_spread,
                     tres_10y_6m_spread_lead_6m = tres_10y_6m_spread,
                     tres_10y_3m_spread_lead_6m = tres_10y_3m_spread,
                     tres_10y_1m_spread_lead_6m = tres_10y_1m_spread,
                     tres_5y_2y_spread_lead_6m = tres_5y_2y_spread,
                     tres_5y_1y_spread_lead_6m = tres_5y_1y_spread,
                     tres_5y_6m_spread_lead_6m = tres_5y_6m_spread,
                     tres_5y_3m_spread_lead_6m = tres_5y_3m_spread,
                     tres_5y_1m_spread_lead_6m = tres_5y_1m_spread,
                     tres_2y_1y_spread_lead_6m = tres_2y_1y_spread,
                     tres_2y_6m_spread_lead_6m = tres_2y_6m_spread,
                     tres_2y_3m_spread_lead_6m = tres_2y_3m_spread,
                     tres_2y_1m_spread_lead_6m = tres_2y_1m_spread,
                     tres_1y_6m_spread_lead_6m = tres_1y_6m_spread,
                     tres_1y_3m_spread_lead_6m = tres_1y_3m_spread,
                     tres_1y_1m_spread_lead_6m = tres_1y_1m_spread,
                     tres_6m_3m_spread_lead_6m = tres_6m_3m_spread,
                     tres_6m_1m_spread_lead_6m = tres_6m_1m_spread,
                     tres_3m_1m_spread_lead_6m = tres_3m_1m_spread,
                     tres_30y_lead6 = tres_30y, tres_20y_lead6 = tres_20y,
                     tres_10y_lead6 = tres_10y, tres_5y_lead6 = tres_5y,
                     tres_2y_lead6 = tres_2y, tres_1y_lead6 = tres_1y,
                     tres_6m_lead6 = tres_6m, tres_3m_lead6 = tres_3m,
                     tres_1m_lead6 = tres_1m),by="time") %>%
  left_join(.,theData %>% select(time,tres_30y_20y_spread,tres_30y_10y_spread,tres_30y_5y_spread,
                                 tres_30y_2y_spread,tres_30y_1y_spread,tres_30y_6m_spread,
                                 tres_30y_3m_spread,tres_30y_1m_spread,tres_20y_10y_spread,
                                 tres_20y_5y_spread,tres_20y_2y_spread,tres_20y_1y_spread,
                                 tres_20y_6m_spread,tres_20y_3m_spread,tres_20y_1m_spread,
                                 tres_10y_5y_spread,tres_10y_2y_spread,tres_10y_1y_spread,
                                 tres_10y_6m_spread,tres_10y_3m_spread,tres_10y_1m_spread,
                                 tres_5y_2y_spread,tres_5y_1y_spread,tres_5y_6m_spread,
                                 tres_5y_3m_spread,tres_5y_1m_spread,tres_2y_1y_spread,
                                 tres_2y_6m_spread,tres_2y_3m_spread,tres_2y_1m_spread,
                                 tres_1y_6m_spread,tres_1y_3m_spread,tres_1y_1m_spread,
                                 tres_6m_3m_spread,tres_6m_1m_spread,tres_3m_1m_spread,
                                 tres_30y,tres_20y,tres_10y,tres_5y,tres_2y,tres_1y,
                                 tres_6m,tres_3m,tres_1m) %>%
              mutate(time = time + 12) %>%
              rename(tres_30y_20y_spread_lead_12m = tres_30y_20y_spread,
                     tres_30y_10y_spread_lead_12m = tres_30y_10y_spread,
                     tres_30y_5y_spread_lead_12m = tres_30y_5y_spread,
                     tres_30y_2y_spread_lead_12m = tres_30y_2y_spread,
                     tres_30y_1y_spread_lead_12m = tres_30y_1y_spread,
                     tres_30y_6m_spread_lead_12m = tres_30y_6m_spread,
                     tres_30y_3m_spread_lead_12m = tres_30y_3m_spread,
                     tres_30y_1m_spread_lead_12m = tres_30y_1m_spread,
                     tres_20y_10y_spread_lead_12m = tres_20y_10y_spread,
                     tres_20y_5y_spread_lead_12m = tres_20y_5y_spread,
                     tres_20y_2y_spread_lead_12m = tres_20y_2y_spread,
                     tres_20y_1y_spread_lead_12m = tres_20y_1y_spread,
                     tres_20y_6m_spread_lead_12m = tres_20y_6m_spread,
                     tres_20y_3m_spread_lead_12m = tres_20y_3m_spread,
                     tres_20y_1m_spread_lead_12m = tres_20y_1m_spread,
                     tres_10y_5y_spread_lead_12m = tres_10y_5y_spread,
                     tres_10y_2y_spread_lead_12m = tres_10y_2y_spread,
                     tres_10y_1y_spread_lead_12m = tres_10y_1y_spread,
                     tres_10y_6m_spread_lead_12m = tres_10y_6m_spread,
                     tres_10y_3m_spread_lead_12m = tres_10y_3m_spread,
                     tres_10y_1m_spread_lead_12m = tres_10y_1m_spread,
                     tres_5y_2y_spread_lead_12m = tres_5y_2y_spread,
                     tres_5y_1y_spread_lead_12m = tres_5y_1y_spread,
                     tres_5y_6m_spread_lead_12m = tres_5y_6m_spread,
                     tres_5y_3m_spread_lead_12m = tres_5y_3m_spread,
                     tres_5y_1m_spread_lead_12m = tres_5y_1m_spread,
                     tres_2y_1y_spread_lead_12m = tres_2y_1y_spread,
                     tres_2y_6m_spread_lead_12m = tres_2y_6m_spread,
                     tres_2y_3m_spread_lead_12m = tres_2y_3m_spread,
                     tres_2y_1m_spread_lead_12m = tres_2y_1m_spread,
                     tres_1y_6m_spread_lead_12m = tres_1y_6m_spread,
                     tres_1y_3m_spread_lead_12m = tres_1y_3m_spread,
                     tres_1y_1m_spread_lead_12m = tres_1y_1m_spread,
                     tres_6m_3m_spread_lead_12m = tres_6m_3m_spread,
                     tres_6m_1m_spread_lead_12m = tres_6m_1m_spread,
                     tres_3m_1m_spread_lead_12m = tres_3m_1m_spread,
                     tres_30y_lead12 = tres_30y, tres_20y_lead12 = tres_20y,
                     tres_10y_lead12 = tres_10y, tres_5y_lead12 = tres_5y,
                     tres_2y_lead12 = tres_2y, tres_1y_lead12 = tres_1y,
                     tres_6m_lead12 = tres_6m, tres_3m_lead12 = tres_3m,
                     tres_1m_lead12 = tres_1m),by="time") %>%
  left_join(.,theData %>% select(time,tres_30y_20y_spread,tres_30y_10y_spread,tres_30y_5y_spread,
                                 tres_30y_2y_spread,tres_30y_1y_spread,tres_30y_6m_spread,
                                 tres_30y_3m_spread,tres_30y_1m_spread,tres_20y_10y_spread,
                                 tres_20y_5y_spread,tres_20y_2y_spread,tres_20y_1y_spread,
                                 tres_20y_6m_spread,tres_20y_3m_spread,tres_20y_1m_spread,
                                 tres_10y_5y_spread,tres_10y_2y_spread,tres_10y_1y_spread,
                                 tres_10y_6m_spread,tres_10y_3m_spread,tres_10y_1m_spread,
                                 tres_5y_2y_spread,tres_5y_1y_spread,tres_5y_6m_spread,
                                 tres_5y_3m_spread,tres_5y_1m_spread,tres_2y_1y_spread,
                                 tres_2y_6m_spread,tres_2y_3m_spread,tres_2y_1m_spread,
                                 tres_1y_6m_spread,tres_1y_3m_spread,tres_1y_1m_spread,
                                 tres_6m_3m_spread,tres_6m_1m_spread,tres_3m_1m_spread,
                                 tres_30y,tres_20y,tres_10y,tres_5y,tres_2y,tres_1y,
                                 tres_6m,tres_3m,tres_1m) %>%
              mutate(time = time + 18) %>%
              rename(tres_30y_20y_spread_lead_18m = tres_30y_20y_spread,
                     tres_30y_10y_spread_lead_18m = tres_30y_10y_spread,
                     tres_30y_5y_spread_lead_18m = tres_30y_5y_spread,
                     tres_30y_2y_spread_lead_18m = tres_30y_2y_spread,
                     tres_30y_1y_spread_lead_18m = tres_30y_1y_spread,
                     tres_30y_6m_spread_lead_18m = tres_30y_6m_spread,
                     tres_30y_3m_spread_lead_18m = tres_30y_3m_spread,
                     tres_30y_1m_spread_lead_18m = tres_30y_1m_spread,
                     tres_20y_10y_spread_lead_18m = tres_20y_10y_spread,
                     tres_20y_5y_spread_lead_18m = tres_20y_5y_spread,
                     tres_20y_2y_spread_lead_18m = tres_20y_2y_spread,
                     tres_20y_1y_spread_lead_18m = tres_20y_1y_spread,
                     tres_20y_6m_spread_lead_18m = tres_20y_6m_spread,
                     tres_20y_3m_spread_lead_18m = tres_20y_3m_spread,
                     tres_20y_1m_spread_lead_18m = tres_20y_1m_spread,
                     tres_10y_5y_spread_lead_18m = tres_10y_5y_spread,
                     tres_10y_2y_spread_lead_18m = tres_10y_2y_spread,
                     tres_10y_1y_spread_lead_18m = tres_10y_1y_spread,
                     tres_10y_6m_spread_lead_18m = tres_10y_6m_spread,
                     tres_10y_3m_spread_lead_18m = tres_10y_3m_spread,
                     tres_10y_1m_spread_lead_18m = tres_10y_1m_spread,
                     tres_5y_2y_spread_lead_18m = tres_5y_2y_spread,
                     tres_5y_1y_spread_lead_18m = tres_5y_1y_spread,
                     tres_5y_6m_spread_lead_18m = tres_5y_6m_spread,
                     tres_5y_3m_spread_lead_18m = tres_5y_3m_spread,
                     tres_5y_1m_spread_lead_18m = tres_5y_1m_spread,
                     tres_2y_1y_spread_lead_18m = tres_2y_1y_spread,
                     tres_2y_6m_spread_lead_18m = tres_2y_6m_spread,
                     tres_2y_3m_spread_lead_18m = tres_2y_3m_spread,
                     tres_2y_1m_spread_lead_18m = tres_2y_1m_spread,
                     tres_1y_6m_spread_lead_18m = tres_1y_6m_spread,
                     tres_1y_3m_spread_lead_18m = tres_1y_3m_spread,
                     tres_1y_1m_spread_lead_18m = tres_1y_1m_spread,
                     tres_6m_3m_spread_lead_18m = tres_6m_3m_spread,
                     tres_6m_1m_spread_lead_18m = tres_6m_1m_spread,
                     tres_3m_1m_spread_lead_18m = tres_3m_1m_spread,
                     tres_30y_lead18 = tres_30y, tres_20y_lead18 = tres_20y,
                     tres_10y_lead18 = tres_10y, tres_5y_lead18 = tres_5y,
                     tres_2y_lead18 = tres_2y, tres_1y_lead18 = tres_1y,
                     tres_6m_lead18 = tres_6m, tres_3m_lead18 = tres_3m,
                     tres_1m_lead18 = tres_1m),by="time") %>%
  left_join(.,theData %>% select(time,tres_30y_20y_spread,tres_30y_10y_spread,tres_30y_5y_spread,
                                 tres_30y_2y_spread,tres_30y_1y_spread,tres_30y_6m_spread,
                                 tres_30y_3m_spread,tres_30y_1m_spread,tres_20y_10y_spread,
                                 tres_20y_5y_spread,tres_20y_2y_spread,tres_20y_1y_spread,
                                 tres_20y_6m_spread,tres_20y_3m_spread,tres_20y_1m_spread,
                                 tres_10y_5y_spread,tres_10y_2y_spread,tres_10y_1y_spread,
                                 tres_10y_6m_spread,tres_10y_3m_spread,tres_10y_1m_spread,
                                 tres_5y_2y_spread,tres_5y_1y_spread,tres_5y_6m_spread,
                                 tres_5y_3m_spread,tres_5y_1m_spread,tres_2y_1y_spread,
                                 tres_2y_6m_spread,tres_2y_3m_spread,tres_2y_1m_spread,
                                 tres_1y_6m_spread,tres_1y_3m_spread,tres_1y_1m_spread,
                                 tres_6m_3m_spread,tres_6m_1m_spread,tres_3m_1m_spread,
                                 tres_30y,tres_20y,tres_10y,tres_5y,tres_2y,tres_1y,
                                 tres_6m,tres_3m,tres_1m) %>%
              mutate(time = time + 24) %>%
              rename(tres_30y_20y_spread_lead_24m = tres_30y_20y_spread,
                     tres_30y_10y_spread_lead_24m = tres_30y_10y_spread,
                     tres_30y_5y_spread_lead_24m = tres_30y_5y_spread,
                     tres_30y_2y_spread_lead_24m = tres_30y_2y_spread,
                     tres_30y_1y_spread_lead_24m = tres_30y_1y_spread,
                     tres_30y_6m_spread_lead_24m = tres_30y_6m_spread,
                     tres_30y_3m_spread_lead_24m = tres_30y_3m_spread,
                     tres_30y_1m_spread_lead_24m = tres_30y_1m_spread,
                     tres_20y_10y_spread_lead_24m = tres_20y_10y_spread,
                     tres_20y_5y_spread_lead_24m = tres_20y_5y_spread,
                     tres_20y_2y_spread_lead_24m = tres_20y_2y_spread,
                     tres_20y_1y_spread_lead_24m = tres_20y_1y_spread,
                     tres_20y_6m_spread_lead_24m = tres_20y_6m_spread,
                     tres_20y_3m_spread_lead_24m = tres_20y_3m_spread,
                     tres_20y_1m_spread_lead_24m = tres_20y_1m_spread,
                     tres_10y_5y_spread_lead_24m = tres_10y_5y_spread,
                     tres_10y_2y_spread_lead_24m = tres_10y_2y_spread,
                     tres_10y_1y_spread_lead_24m = tres_10y_1y_spread,
                     tres_10y_6m_spread_lead_24m = tres_10y_6m_spread,
                     tres_10y_3m_spread_lead_24m = tres_10y_3m_spread,
                     tres_10y_1m_spread_lead_24m = tres_10y_1m_spread,
                     tres_5y_2y_spread_lead_24m = tres_5y_2y_spread,
                     tres_5y_1y_spread_lead_24m = tres_5y_1y_spread,
                     tres_5y_6m_spread_lead_24m = tres_5y_6m_spread,
                     tres_5y_3m_spread_lead_24m = tres_5y_3m_spread,
                     tres_5y_1m_spread_lead_24m = tres_5y_1m_spread,
                     tres_2y_1y_spread_lead_24m = tres_2y_1y_spread,
                     tres_2y_6m_spread_lead_24m = tres_2y_6m_spread,
                     tres_2y_3m_spread_lead_24m = tres_2y_3m_spread,
                     tres_2y_1m_spread_lead_24m = tres_2y_1m_spread,
                     tres_1y_6m_spread_lead_24m = tres_1y_6m_spread,
                     tres_1y_3m_spread_lead_24m = tres_1y_3m_spread,
                     tres_1y_1m_spread_lead_24m = tres_1y_1m_spread,
                     tres_6m_3m_spread_lead_24m = tres_6m_3m_spread,
                     tres_6m_1m_spread_lead_24m = tres_6m_1m_spread,
                     tres_3m_1m_spread_lead_24m = tres_3m_1m_spread,
                     tres_30y_lead24 = tres_30y, tres_20y_lead24 = tres_20y,
                     tres_10y_lead24 = tres_10y, tres_5y_lead24 = tres_5y,
                     tres_2y_lead24 = tres_2y, tres_1y_lead24 = tres_1y,
                     tres_6m_lead24 = tres_6m, tres_3m_lead24 = tres_3m,
                     tres_1m_lead24 = tres_1m),by="time") %>%
  left_join(.,theData %>% 
              select(time,sentiment) %>% 
              mutate(time = time + 3) %>% 
              rename(sentiment3 = sentiment),by="time") %>%
  left_join(.,theData %>% 
              select(time,sentiment) %>% 
              mutate(time = time + 6) %>% 
              rename(sentiment6 = sentiment),by="time") %>%
  left_join(.,theData %>% 
              select(time,sentiment) %>% 
              mutate(time = time + 12) %>% 
              rename(sentiment12 = sentiment),by="time") %>%
  left_join(.,theData %>% 
              select(time,lfpr) %>% 
              mutate(time = time + 3) %>% 
              rename(lfpr3 = lfpr),by="time") %>%
  left_join(.,theData %>% 
              select(time,lfpr) %>% 
              mutate(time = time + 6) %>% 
              rename(lfpr6 = lfpr),by="time") %>%
  left_join(.,theData %>% 
              select(time,lfpr) %>% 
              mutate(time = time + 12) %>% 
              rename(lfpr12 = lfpr),by="time") %>%
  left_join(.,theData %>% 
              select(time,production) %>% 
              mutate(time = time + 3) %>% 
              rename(production3 = production),by="time") %>%
  left_join(.,theData %>% 
              select(time,production) %>% 
              mutate(time = time + 6) %>% 
              rename(production6 = production),by="time") %>%
  left_join(.,theData %>% 
              select(time,production) %>% 
              mutate(time = time + 12) %>% 
              rename(production12 = production),by="time") %>%
  left_join(.,theData %>% 
              select(time,fed_funds) %>% 
              mutate(time = time + 3) %>% 
              rename(fed_funds3 = fed_funds),by="time") %>%
  left_join(.,theData %>% 
              select(time,fed_funds) %>% 
              mutate(time = time + 6) %>% 
              rename(fed_funds6 = fed_funds),by="time") %>%
  left_join(.,theData %>% 
              select(time,fed_funds) %>% 
              mutate(time = time + 12) %>% 
              rename(fed_funds12 = fed_funds),by="time") %>%
  left_join(.,theData %>% 
              select(time,m2_velocity) %>% 
              mutate(time = time + 3) %>% 
              rename(m2_velocity3 = m2_velocity),by="time") %>%
  left_join(.,theData %>% 
              select(time,m2_velocity) %>% 
              mutate(time = time + 6) %>% 
              rename(m2_velocity6 = m2_velocity),by="time") %>%
  left_join(.,theData %>% 
              select(time,m2_velocity) %>% 
              mutate(time = time + 12) %>% 
              rename(m2_velocity12 = m2_velocity),by="time") %>%
  left_join(.,theData %>% 
              select(time,initial_claims) %>% 
              mutate(time = time + 3) %>% 
              rename(initial_claims3 = initial_claims),by="time") %>%
  left_join(.,theData %>% 
              select(time,initial_claims) %>% 
              mutate(time = time + 6) %>% 
              rename(initial_claims6 = initial_claims),by="time") %>%
  left_join(.,theData %>% 
              select(time,initial_claims) %>% 
              mutate(time = time + 12) %>% 
              rename(initial_claims12 = initial_claims),by="time") %>%
  left_join(.,theData %>% 
              select(time,leverage_index) %>% 
              mutate(time = time + 3) %>% 
              rename(leverage_index3 = leverage_index),by="time") %>%
  left_join(.,theData %>% 
              select(time,leverage_index) %>% 
              mutate(time = time + 6) %>% 
              rename(leverage_index6 = leverage_index),by="time") %>%
  left_join(.,theData %>% 
              select(time,leverage_index) %>% 
              mutate(time = time + 12) %>% 
              rename(leverage_index12 = leverage_index),by="time") %>%
  left_join(.,theData %>% 
              select(time,credit_index) %>% 
              mutate(time = time + 3) %>% 
              rename(credit_index3 = credit_index),by="time") %>%
  left_join(.,theData %>% 
              select(time,credit_index) %>% 
              mutate(time = time + 6) %>% 
              rename(credit_index6 = credit_index),by="time") %>%
  left_join(.,theData %>% 
              select(time,credit_index) %>% 
              mutate(time = time + 12) %>% 
              rename(credit_index12 = credit_index),by="time") %>%
  left_join(.,theData %>% 
              select(time,risk_index) %>% 
              mutate(time = time + 3) %>% 
              rename(risk_index3 = risk_index),by="time") %>%
  left_join(.,theData %>% 
              select(time,risk_index) %>% 
              mutate(time = time + 6) %>% 
              rename(risk_index6 = risk_index),by="time") %>%
  left_join(.,theData %>% 
              select(time,risk_index) %>% 
              mutate(time = time + 12) %>% 
              rename(risk_index12 = risk_index),by="time") %>%
  mutate(unemployment_raw_chg_3 = unemployment - unemployment3,
         unemployment_prct_chg_3 = (unemployment - unemployment3)/unemployment3,
         unemployment_raw_chg_6 = unemployment - unemployment6,
         unemployment_prct_chg_6 = (unemployment - unemployment6)/unemployment6,
         unemployment_raw_chg_12 = unemployment - unemployment12,
         unemployment_prct_chg_12 = (unemployment - unemployment12)/unemployment12,
         tres_30y_raw_chg_3 = tres_30y - tres_30y_lead3,
         tres_30y_prct_chg_3 = (tres_30y - tres_30y_lead3)/tres_30y_lead3,
         tres_30y_raw_chg_6 = tres_30y - tres_30y_lead6,
         tres_30y_prct_chg_6 = (tres_30y - tres_30y_lead6)/tres_30y_lead6,
         tres_30y_raw_chg_12 = tres_30y - tres_30y_lead12,
         tres_30y_prct_chg_12 = (tres_30y - tres_30y_lead12)/tres_30y_lead12,
         tres_30y_raw_chg_18 = tres_30y - tres_30y_lead18,
         tres_30y_prct_chg_18 = (tres_30y - tres_30y_lead18)/tres_30y_lead18,
         tres_30y_raw_chg_24 = tres_30y - tres_30y_lead24,
         tres_30y_prct_chg_24 = (tres_30y - tres_30y_lead24)/tres_30y_lead24,
         tres_20y_raw_chg_3 = tres_20y - tres_20y_lead3,
         tres_20y_prct_chg_3 = (tres_20y - tres_20y_lead3)/tres_20y_lead3,
         tres_20y_raw_chg_6 = tres_20y - tres_20y_lead6,
         tres_20y_prct_chg_6 = (tres_20y - tres_20y_lead6)/tres_20y_lead6,
         tres_20y_raw_chg_12 = tres_20y - tres_20y_lead12,
         tres_20y_prct_chg_12 = (tres_20y - tres_20y_lead12)/tres_20y_lead12,
         tres_20y_raw_chg_18 = tres_20y - tres_20y_lead18,
         tres_20y_prct_chg_18 = (tres_20y - tres_20y_lead18)/tres_20y_lead18,
         tres_20y_raw_chg_24 = tres_20y - tres_20y_lead24,
         tres_20y_prct_chg_24 = (tres_20y - tres_20y_lead24)/tres_20y_lead24,
         tres_10y_raw_chg_3 = tres_10y - tres_10y_lead3,
         tres_10y_prct_chg_3 = (tres_10y - tres_10y_lead3)/tres_10y_lead3,
         tres_10y_raw_chg_6 = tres_10y - tres_10y_lead6,
         tres_10y_prct_chg_6 = (tres_10y - tres_10y_lead6)/tres_10y_lead6,
         tres_10y_raw_chg_12 = tres_10y - tres_10y_lead12,
         tres_10y_prct_chg_12 = (tres_10y - tres_10y_lead12)/tres_10y_lead12,
         tres_10y_raw_chg_18 = tres_10y - tres_10y_lead18,
         tres_10y_prct_chg_18 = (tres_10y - tres_10y_lead18)/tres_10y_lead18,
         tres_10y_raw_chg_24 = tres_10y - tres_10y_lead24,
         tres_10y_prct_chg_24 = (tres_10y - tres_10y_lead24)/tres_10y_lead24,
         tres_5y_raw_chg_3 = tres_5y - tres_5y_lead3,
         tres_5y_prct_chg_3 = (tres_5y - tres_5y_lead3)/tres_5y_lead3,
         tres_5y_raw_chg_6 = tres_5y - tres_5y_lead6,
         tres_5y_prct_chg_6 = (tres_5y - tres_5y_lead6)/tres_5y_lead6,
         tres_5y_raw_chg_12 = tres_5y - tres_5y_lead12,
         tres_5y_prct_chg_12 = (tres_5y - tres_5y_lead12)/tres_5y_lead12,
         tres_5y_raw_chg_18 = tres_5y - tres_5y_lead18,
         tres_5y_prct_chg_18 = (tres_5y - tres_5y_lead18)/tres_5y_lead18,
         tres_5y_raw_chg_24 = tres_5y - tres_5y_lead24,
         tres_5y_prct_chg_24 = (tres_5y - tres_5y_lead24)/tres_5y_lead24,
         tres_2y_raw_chg_3 = tres_2y - tres_2y_lead3,
         tres_2y_prct_chg_3 = (tres_2y - tres_2y_lead3)/tres_2y_lead3,
         tres_2y_raw_chg_6 = tres_2y - tres_2y_lead6,
         tres_2y_prct_chg_6 = (tres_2y - tres_2y_lead6)/tres_2y_lead6,
         tres_2y_raw_chg_12 = tres_2y - tres_2y_lead12,
         tres_2y_prct_chg_12 = (tres_2y - tres_2y_lead12)/tres_2y_lead12,
         tres_2y_raw_chg_18 = tres_2y - tres_2y_lead18,
         tres_2y_prct_chg_18 = (tres_2y - tres_2y_lead18)/tres_2y_lead18,
         tres_2y_raw_chg_24 = tres_2y - tres_2y_lead24,
         tres_2y_prct_chg_24 = (tres_2y - tres_2y_lead24)/tres_2y_lead24,
         tres_1y_raw_chg_3 = tres_1y - tres_1y_lead3,
         tres_1y_prct_chg_3 = (tres_1y - tres_1y_lead3)/tres_1y_lead3,
         tres_1y_raw_chg_6 = tres_1y - tres_1y_lead6,
         tres_1y_prct_chg_6 = (tres_1y - tres_1y_lead6)/tres_1y_lead6,
         tres_1y_raw_chg_12 = tres_1y - tres_1y_lead12,
         tres_1y_prct_chg_12 = (tres_1y - tres_1y_lead12)/tres_1y_lead12,
         tres_1y_raw_chg_18 = tres_1y - tres_1y_lead18,
         tres_1y_prct_chg_18 = (tres_1y - tres_1y_lead18)/tres_1y_lead18,
         tres_1y_raw_chg_24 = tres_1y - tres_1y_lead24,
         tres_1y_prct_chg_24 = (tres_1y - tres_1y_lead24)/tres_1y_lead24,
         tres_6m_raw_chg_3 = tres_6m - tres_6m_lead3,
         tres_6m_prct_chg_3 = (tres_6m - tres_6m_lead3)/tres_6m_lead3,
         tres_6m_raw_chg_6 = tres_6m - tres_6m_lead6,
         tres_6m_prct_chg_6 = (tres_6m - tres_6m_lead6)/tres_6m_lead6,
         tres_6m_raw_chg_12 = tres_6m - tres_6m_lead12,
         tres_6m_prct_chg_12 = (tres_6m - tres_6m_lead12)/tres_6m_lead12,
         tres_6m_raw_chg_18 = tres_6m - tres_6m_lead18,
         tres_6m_prct_chg_18 = (tres_6m - tres_6m_lead18)/tres_6m_lead18,
         tres_6m_raw_chg_24 = tres_6m - tres_6m_lead24,
         tres_6m_prct_chg_24 = (tres_6m - tres_6m_lead24)/tres_6m_lead24,
         tres_3m_raw_chg_3 = tres_3m - tres_3m_lead3,
         tres_3m_prct_chg_3 = (tres_3m - tres_3m_lead3)/tres_3m_lead3,
         tres_3m_raw_chg_6 = tres_3m - tres_3m_lead6,
         tres_3m_prct_chg_6 = (tres_3m - tres_3m_lead6)/tres_3m_lead6,
         tres_3m_raw_chg_12 = tres_3m - tres_3m_lead12,
         tres_3m_prct_chg_12 = (tres_3m - tres_3m_lead12)/tres_3m_lead12,
         tres_3m_raw_chg_18 = tres_3m - tres_3m_lead18,
         tres_3m_prct_chg_18 = (tres_3m - tres_3m_lead18)/tres_3m_lead18,
         tres_3m_raw_chg_24 = tres_3m - tres_3m_lead24,
         tres_3m_prct_chg_24 = (tres_3m - tres_3m_lead24)/tres_3m_lead24,
         tres_1m_raw_chg_3 = tres_1m - tres_1m_lead3,
         tres_1m_prct_chg_3 = (tres_1m - tres_1m_lead3)/tres_1m_lead3,
         tres_1m_raw_chg_6 = tres_1m - tres_1m_lead6,
         tres_1m_prct_chg_6 = (tres_1m - tres_1m_lead6)/tres_1m_lead6,
         tres_1m_raw_chg_12 = tres_1m - tres_1m_lead12,
         tres_1m_prct_chg_12 = (tres_1m - tres_1m_lead12)/tres_1m_lead12,
         tres_1m_raw_chg_18 = tres_1m - tres_1m_lead18,
         tres_1m_prct_chg_18 = (tres_1m - tres_1m_lead18)/tres_1m_lead18,
         tres_1m_raw_chg_24 = tres_1m - tres_1m_lead24,
         tres_1m_prct_chg_24 = (tres_1m - tres_1m_lead24)/tres_1m_lead24,
         sentiment_raw_chg_3 = sentiment - sentiment3,
         sentiment_prct_chg_3 = (sentiment - sentiment3)/sentiment3,
         sentiment_raw_chg_6 = sentiment - sentiment6,
         sentiment_prct_chg_6 = (sentiment - sentiment6)/sentiment6,
         sentiment_raw_chg_12 = sentiment - sentiment12,
         sentiment_prct_chg_12 = (sentiment - sentiment12)/sentiment12,
         lfpr_raw_chg_3 = lfpr - lfpr3,
         lfpr_prct_chg_3 = (lfpr - lfpr3)/lfpr3,
         lfpr_raw_chg_6 = lfpr - lfpr6,
         lfpr_prct_chg_6 = (lfpr - lfpr6)/lfpr6,
         lfpr_raw_chg_12 = lfpr - lfpr12,
         lfpr_prct_chg_12 = (lfpr - lfpr12)/lfpr12,
         production_raw_chg_3 = production - production3,
         production_prct_chg_3 = (production - production3)/production3,
         production_raw_chg_6 = production - production6,
         production_prct_chg_6 = (production - production6)/production6,
         production_raw_chg_12 = production - production12,
         production_prct_chg_12 = (production - production12)/production12,
         fed_funds_raw_chg_3 = fed_funds - fed_funds3,
         fed_funds_prct_chg_3 = (fed_funds - fed_funds3)/fed_funds3,
         fed_funds_raw_chg_6 = fed_funds - fed_funds6,
         fed_funds_prct_chg_6 = (fed_funds - fed_funds6)/fed_funds6,
         fed_funds_raw_chg_12 = fed_funds - fed_funds12,
         fed_funds_prct_chg_12 = (fed_funds - fed_funds12)/fed_funds12,
         m2_velocity_raw_chg_3 = m2_velocity - m2_velocity3,
         m2_velocity_prct_chg_3 = (m2_velocity - m2_velocity3)/m2_velocity3,
         m2_velocity_raw_chg_6 = m2_velocity - m2_velocity6,
         m2_velocity_prct_chg_6 = (m2_velocity - m2_velocity6)/m2_velocity6,
         m2_velocity_raw_chg_12 = m2_velocity - m2_velocity12,
         m2_velocity_prct_chg_12 = (m2_velocity - m2_velocity12)/m2_velocity12,
         initial_claims_raw_chg_3 = initial_claims - initial_claims3,
         initial_claims_prct_chg_3 = (initial_claims - initial_claims3)/initial_claims3,
         initial_claims_raw_chg_6 = initial_claims - initial_claims6,
         initial_claims_prct_chg_6 = (initial_claims - initial_claims6)/initial_claims6,
         initial_claims_raw_chg_12 = initial_claims - initial_claims12,
         initial_claims_prct_chg_12 = (initial_claims - initial_claims12)/initial_claims12,
         leverage_index_raw_chg_3 = leverage_index - leverage_index3,
         leverage_index_prct_chg_3 = (leverage_index - leverage_index3)/leverage_index3,
         leverage_index_raw_chg_6 = leverage_index - leverage_index6,
         leverage_index_prct_chg_6 = (leverage_index - leverage_index6)/leverage_index6,
         leverage_index_raw_chg_12 = leverage_index - leverage_index12,
         leverage_index_prct_chg_12 = (leverage_index - leverage_index12)/leverage_index12,
         credit_index_raw_chg_3 = credit_index - credit_index3,
         credit_index_prct_chg_3 = (credit_index - credit_index3)/credit_index3,
         credit_index_raw_chg_6 = credit_index - credit_index6,
         credit_index_prct_chg_6 = (credit_index - credit_index6)/credit_index6,
         credit_index_raw_chg_12 = credit_index - credit_index12,
         credit_index_prct_chg_12 = (credit_index - credit_index12)/credit_index12,
         risk_index_raw_chg_3 = risk_index - risk_index3,
         risk_index_prct_chg_3 = (risk_index - risk_index3)/risk_index3,
         risk_index_raw_chg_6 = risk_index - risk_index6,
         risk_index_prct_chg_6 = (risk_index - risk_index6)/risk_index6,
         risk_index_raw_chg_12 = risk_index - risk_index12,
         risk_index_prct_chg_12 = (risk_index - risk_index12)/risk_index12) %>%
  select(-c("unemployment3","unemployment6","unemployment12",
            "tres_30y_lead3","tres_30y_lead6","tres_30y_lead12","tres_30y_lead18","tres_30y_lead24",
            "tres_20y_lead3","tres_20y_lead6","tres_20y_lead12","tres_20y_lead18","tres_20y_lead24",
            "tres_10y_lead3","tres_10y_lead6","tres_10y_lead12","tres_10y_lead18","tres_10y_lead24",
            "tres_5y_lead3","tres_5y_lead6","tres_5y_lead12","tres_5y_lead18","tres_5y_lead24",
            "tres_2y_lead3","tres_2y_lead6","tres_2y_lead12","tres_2y_lead18","tres_2y_lead24",
            "tres_1y_lead3","tres_1y_lead6","tres_1y_lead12","tres_1y_lead18","tres_1y_lead24",
            "tres_6m_lead3","tres_6m_lead6","tres_6m_lead12","tres_6m_lead18","tres_6m_lead24",
            "tres_3m_lead3","tres_3m_lead6","tres_3m_lead12","tres_3m_lead18","tres_3m_lead24",
            "tres_1m_lead3","tres_1m_lead6","tres_1m_lead12","tres_1m_lead18","tres_1m_lead24",
            "sentiment3","sentiment6","sentiment12",
            "lfpr3","lfpr6","lfpr12",
            "production3","production6","production12",
            "fed_funds3","fed_funds6","fed_funds12",
            "m2_velocity3","m2_velocity6","m2_velocity12",
            "initial_claims3","initial_claims6","initial_claims12",
            "leverage_index3","leverage_index6","leverage_index12",
            "credit_index3","credit_index6","credit_index12",
            "risk_index3","risk_index6","risk_index12")) %>%
  left_join(.,recession_3m,by="time") %>%
  left_join(.,recession_6m,by="time") %>%
  left_join(.,recession_12m,by="time") %>%
  left_join(.,recession_18m,by="time") %>%
  filter(time >= 78) #Start data in June 1976 when 2-year treasury data starts

#Build current... take most recent values populated as inputs
current_test = as.data.frame(cbind(as.character(Sys.Date()),as.numeric(unemployment[dim(unemployment)[1],"unemployment"]),
                                   tres_30y_c,tres_20y_c,tres_10y_c,tres_5y_c,tres_2y_c,tres_1y_c,tres_6m_c,tres_3m_c,tres_1m_c,
                                   as.numeric(sentiment[dim(sentiment)[1],"sentiment"]),as.numeric(lfpr[dim(lfpr)[1],"lfpr"]),
                                   as.numeric(production[dim(production)[1],"production"]),fed_funds_c,
                                   as.numeric(m2[dim(m2)[1],"m2"]),as.numeric(GDP[dim(GDP)[1],"GDP"]),
                                   as.numeric(initial_claims[dim(initial_claims)[1],"initial_claims"]),
                                   leverage_index_c,credit_index_c,risk_index_c,
                                   as.numeric(recession[dim(recession)[1],"recession"]),
                                   as.numeric(theData[dim(theData)[1],"time"])))
colnames(current_test) = colnames(theData)[1:23]

for(i in seq(from=2,to=dim(current_test)[2],by=1)){
  current_test[,i] = as.numeric(current_test[,i])
}

current_test = current_test %>% mutate(tres_30y_20y_spread = tres_30y - tres_20y,
                                       tres_30y_10y_spread = tres_30y - tres_10y,
                                       tres_30y_5y_spread = tres_30y - tres_5y,
                                       tres_30y_2y_spread = tres_30y - tres_2y,
                                       tres_30y_1y_spread = tres_30y - tres_1y,
                                       tres_30y_6m_spread = tres_30y - tres_6m,
                                       tres_30y_3m_spread = tres_30y - tres_3m,
                                       tres_30y_1m_spread = tres_30y - tres_1m,
                                       tres_20y_10y_spread = tres_20y - tres_10y,
                                       tres_20y_5y_spread = tres_20y - tres_5y,
                                       tres_20y_2y_spread = tres_20y - tres_2y,
                                       tres_20y_1y_spread = tres_20y - tres_1y,
                                       tres_20y_6m_spread = tres_20y - tres_6m,
                                       tres_20y_3m_spread = tres_20y - tres_3m,
                                       tres_20y_1m_spread = tres_20y - tres_1m,
                                       tres_10y_5y_spread = tres_10y - tres_5y,
                                       tres_10y_2y_spread = tres_10y - tres_2y,
                                       tres_10y_1y_spread = tres_10y - tres_1y,
                                       tres_10y_6m_spread = tres_10y - tres_6m,
                                       tres_10y_3m_spread = tres_10y - tres_3m,
                                       tres_10y_1m_spread = tres_10y - tres_1m,
                                       tres_5y_2y_spread = tres_5y - tres_2y,
                                       tres_5y_1y_spread = tres_5y - tres_1y,
                                       tres_5y_6m_spread = tres_5y - tres_6m,
                                       tres_5y_3m_spread = tres_5y - tres_3m,
                                       tres_5y_1m_spread = tres_5y - tres_1m,
                                       tres_2y_1y_spread = tres_2y - tres_1y,
                                       tres_2y_6m_spread = tres_2y - tres_6m,
                                       tres_2y_3m_spread = tres_2y - tres_3m,
                                       tres_2y_1m_spread = tres_2y - tres_1m,
                                       tres_1y_6m_spread = tres_1y - tres_6m,
                                       tres_1y_3m_spread = tres_1y - tres_3m,
                                       tres_1y_1m_spread = tres_1y - tres_1m,
                                       tres_6m_3m_spread = tres_6m - tres_3m,
                                       tres_6m_1m_spread = tres_6m - tres_1m,
                                       tres_3m_1m_spread = tres_3m - tres_1m,
                                       m2_velocity = GDP / m2)

current_test = cbind(current_test,theData[dim(theData)[1],c(61:dim(theData)[2])])

#Create folds
theData = theData %>% mutate(folds = ifelse(time <= 190, 0,
                                            ifelse(time <= 303, 1,
                                                   ifelse(time <= 416, 2,
                                                          ifelse(time <= 530, 3, 4)))))

#Define independent and dependent variables to use in model
x_vars = setdiff(colnames(theData),c("date","production","m2","GDP","initial_claims","time",
                                     "recession","recession_3m","recession_6m","recession_12m","recession_18m","folds"))
y1 = "recession"
y2 = "recession_3m"
y3 = "recession_6m"
y4 = "recession_12m"
y5 = "recession_18m"


#Build final models
max_time = max(theData$time) - 12 #takes a while for a recession to be determined... don't use the last year in the model
model_dataset_c = theData %>% filter(!is.na(recession) & time < max_time)
model_dataset_3 = theData %>% filter(!is.na(recession_3m) & time < max_time)
model_dataset_6 = theData %>% filter(!is.na(recession_6m) & time < max_time)
model_dataset_12 = theData %>% filter(!is.na(recession_12m) & time < max_time)
model_dataset_18 = theData %>% filter(!is.na(recession_18m) & time < max_time)

# theData_subset = theData %>% filter(folds != 4 & folds != 3) #drop prior to 2004-08-01 for a test
# model_dataset_c = theData_subset %>% filter(!is.na(recession))
# model_dataset_3 = theData_subset %>% filter(!is.na(recession_3m))
# model_dataset_6 = theData_subset %>% filter(!is.na(recession_6m))
# model_dataset_12 = theData_subset %>% filter(!is.na(recession_12m))
# model_dataset_18 = theData_subset %>% filter(!is.na(recession_18m))

#start H2O
h2o.init(max_mem_size = "16g")

train_data_c = as.h2o(model_dataset_c)
train_data_3 = as.h2o(model_dataset_3)
train_data_6 = as.h2o(model_dataset_6)
train_data_12 = as.h2o(model_dataset_12)
train_data_18 = as.h2o(model_dataset_18)
test = as.h2o(theData)
test_current = as.h2o(current_test)

model_c =  h2o.gbm(x=x_vars,
                   y=y1,
                   fold_column = "folds",
                   training_frame = train_data_c,
                   col_sample_rate = 0.8,
                   min_split_improvement = 0.01,
                   learn_rate = 0.1,
                   max_depth = 10,
                   min_rows = 20,
                   ntrees = 25,
                   sample_rate = 1,
                   seed=412)
varimp_c = h2o.varimp(model_c)
r2_c = h2o.r2(model_c)
recession_pred_c = as.vector(h2o.predict(model_c,test))
recession_pred_c_current = as.vector(h2o.predict(model_c,test_current))

# Variable Importances Current: 
#                       variable relative_importance scaled_importance percentage
# 1       unemployment_raw_chg_3          105.869011          1.000000   0.428220
# 2      unemployment_prct_chg_6           32.964207          0.311368   0.133334
# 3               leverage_index           15.337646          0.144874   0.062038
# 4    initial_claims_prct_chg_3           11.688690          0.110407   0.047279
# 5      unemployment_prct_chg_3           10.614173          0.100258   0.042932
# 6                 credit_index            7.411032          0.070002   0.029976
# 7    initial_claims_prct_chg_6            6.507182          0.061464   0.026320
# 8          fed_funds_raw_chg_3            4.122522          0.038940   0.016675
# 9     initial_claims_raw_chg_6            4.088338          0.038617   0.016537
# 10 tres_30y_2y_spread_lead_18m            3.974154          0.037538   0.016075
# 11      m2_velocity_prct_chg_3            3.421572          0.032319   0.013840
# 12  initial_claims_prct_chg_12            2.615944          0.024709   0.010581
# 13           tres_5y_6m_spread            2.338479          0.022088   0.009459
# 14 tres_10y_5y_spread_lead_18m            2.311739          0.021836   0.009351
# 15   tres_2y_6m_spread_lead_6m            2.225687          0.021023   0.009002
# 16   initial_claims_raw_chg_12            2.180534          0.020597   0.008820
# 17                  risk_index            2.097718          0.019814   0.008485
# 18 tres_10y_2y_spread_lead_18m            1.911369          0.018054   0.007731
# 19 tres_30y_3m_spread_lead_12m            1.863707          0.017604   0.007538
# 20       production_prct_chg_3            1.774052          0.016757   0.007176


model_3 =  h2o.gbm(x=x_vars,
                   y=y2,
                   fold_column = "folds",
                   training_frame = train_data_3,
                   col_sample_rate = 1.0,
                   min_split_improvement = 0.0001,
                   learn_rate = 0.1,
                   max_depth = 10,
                   min_rows = 15,
                   ntrees = 50,
                   sample_rate = 1,
                   seed=412)
varimp_3 = h2o.varimp(model_3)
r2_3 = h2o.r2(model_3)
recession_pred_3 = as.vector(h2o.predict(model_3,test))
recession_pred_3_current = as.vector(h2o.predict(model_3,test_current))

# Variable Importances 3: 
#                       variable relative_importance scaled_importance percentage
# 1               leverage_index          103.242287          1.000000   0.332947
# 2     initial_claims_raw_chg_6           33.114204          0.320743   0.106790
# 3                 credit_index           30.982180          0.300092   0.099915
# 4  tres_30y_1y_spread_lead_12m           27.984278          0.271054   0.090247
# 5    initial_claims_prct_chg_6           12.169145          0.117870   0.039244
# 6    tres_1y_6m_spread_lead_6m           11.920887          0.115465   0.038444
# 7  tres_30y_5y_spread_lead_18m           11.339216          0.109831   0.036568
# 8    tres_1y_3m_spread_lead_6m            7.935839          0.076866   0.025592
# 9    tres_2y_1y_spread_lead_6m            7.587055          0.073488   0.024468
# 10       production_prct_chg_3            4.882402          0.047291   0.015745
# 11       sentiment_prct_chg_12            4.117835          0.039885   0.013280
# 12        sentiment_raw_chg_12            3.816041          0.036962   0.012306
# 13  tres_5y_2y_spread_lead_24m            3.283036          0.031799   0.010587
# 14        risk_index_raw_chg_6            2.970069          0.028768   0.009578
# 15        tres_30y_prct_chg_18            2.868530          0.027784   0.009251
# 16 tres_30y_2y_spread_lead_18m            2.807823          0.027196   0.009055
# 17     credit_index_raw_chg_12            2.797441          0.027096   0.009021
# 18   initial_claims_prct_chg_3            2.544188          0.024643   0.008205
# 19                  risk_index            1.904061          0.018443   0.006140
# 20 tres_20y_10y_spread_lead_3m            1.895227          0.018357   0.006112

model_6 =  h2o.gbm(x=x_vars,
                   y=y3,
                   fold_column = "folds",
                   training_frame = train_data_6,
                   col_sample_rate = 0.8,
                   min_split_improvement = 0.01,
                   learn_rate = 0.1,
                   max_depth = 3,
                   min_rows = 10,
                   ntrees = 25,
                   sample_rate = 1,
                   seed=412)
varimp_6 = h2o.varimp(model_6)
r2_6 = h2o.r2(model_6)
recession_pred_6 = as.vector(h2o.predict(model_6,test))
recession_pred_6_current = as.vector(h2o.predict(model_6,test_current))

# Variable Importances 6: 
#                       variable relative_importance scaled_importance percentage
# 1                 credit_index          129.219177          1.000000   0.349991
# 2  tres_30y_1y_spread_lead_12m           42.296497          0.327324   0.114560
# 3               leverage_index           22.691885          0.175608   0.061461
# 4    initial_claims_prct_chg_3           20.784246          0.160845   0.056294
# 5    tres_2y_6m_spread_lead_6m           18.714428          0.144827   0.050688
# 6     initial_claims_raw_chg_3           17.924276          0.138712   0.048548
# 7    tres_1y_3m_spread_lead_6m           13.007820          0.100665   0.035232
# 8    tres_1y_6m_spread_lead_6m           11.022657          0.085302   0.029855
# 9        production_prct_chg_3            8.365668          0.064740   0.022658
# 10  tres_20y_6m_spread_lead_6m            8.266606          0.063974   0.022390
# 11    initial_claims_raw_chg_6            7.791986          0.060301   0.021105
# 12   tres_1y_6m_spread_lead_3m            7.383777          0.057141   0.019999
# 13        production_raw_chg_3            5.512210          0.042658   0.014930
# 14   tres_1y_3m_spread_lead_3m            4.935004          0.038191   0.013366
# 15          tres_1m_raw_chg_18            4.123746          0.031913   0.011169
# 16 tres_30y_2y_spread_lead_12m            3.822191          0.029579   0.010352
# 17           tres_1y_raw_chg_3            3.408396          0.026377   0.009232
# 18  tres_20y_3m_spread_lead_6m            3.308673          0.025605   0.008962
# 19                  risk_index            3.189390          0.024682   0.008638
# 20           tres_2y_6m_spread            2.609517          0.020194   0.007068

# H2ORegressionMetrics: gbm
# ** Reported on training data. **
#   
# R2: 0.95

model_12 = h2o.gbm(x=x_vars,
                   y=y4,
                   fold_column = "folds",
                   training_frame = train_data_12,
                   col_sample_rate = 1.0,
                   min_split_improvement = 0.01,
                   learn_rate = 0.1,
                   max_depth = 10,
                   min_rows = 20,
                   ntrees = 25,
                   sample_rate = 1,
                   seed=412)
varimp_12 = h2o.varimp(model_12)
r2_12 = h2o.r2(model_12)
recession_pred_12 = as.vector(h2o.predict(model_12,test))
recession_pred_12_current = as.vector(h2o.predict(model_12,test_current))

# Variable Importances 12: 
#                        variable relative_importance scaled_importance percentage
# 1    tres_30y_1y_spread_lead_6m          134.211166          1.000000   0.283290
# 2                  credit_index           74.214134          0.552965   0.156650
# 3   tres_30y_1y_spread_lead_12m           56.952713          0.424352   0.120215
# 4     tres_5y_2y_spread_lead_6m           29.569592          0.220321   0.062415
# 5     tres_2y_1y_spread_lead_6m           19.379778          0.144398   0.040906
# 6    tres_10y_1y_spread_lead_6m           17.179672          0.128005   0.036263
# 7    tres_20y_1y_spread_lead_3m           10.219440          0.076144   0.021571
# 8   tres_20y_3m_spread_lead_24m            9.116129          0.067924   0.019242
# 9                leverage_index            8.308866          0.061909   0.017538
# 10           tres_20y_3m_spread            6.427370          0.047890   0.013567
# 11 tres_30y_10y_spread_lead_12m            6.174187          0.046004   0.013032
# 12    tres_2y_3m_spread_lead_6m            5.798767          0.043206   0.012240
# 13  tres_30y_3m_spread_lead_12m            5.388378          0.040149   0.011374
# 14                 unemployment            5.134423          0.038256   0.010838
# 15   tres_2y_1y_spread_lead_24m            5.115286          0.038114   0.010797
# 16  tres_20y_1y_spread_lead_12m            4.977180          0.037085   0.010506
# 17        production_prct_chg_3            4.284276          0.031922   0.009043
# 18   tres_20y_3m_spread_lead_3m            3.479399          0.025925   0.007344
# 19   tres_20y_2y_spread_lead_3m            3.288568          0.024503   0.006941
# 20    tres_5y_1y_spread_lead_6m            3.002712          0.022373   0.006338

# H2ORegressionMetrics: gbm
# ** Reported on training data. **
#   
# R2: 0.94

model_18 = h2o.gbm(x=x_vars,
                   y=y5,
                   fold_column = "folds",
                   training_frame = train_data_18,
                   col_sample_rate = 1.0,
                   min_split_improvement = 0.001,
                   learn_rate = 0.1,
                   max_depth = 3,
                   min_rows = 10,
                   ntrees = 25,
                   sample_rate = 1,
                   seed=412)
varimp_18 = h2o.varimp(model_18)
r2_18 = h2o.r2(model_18)
recession_pred_18 = as.vector(h2o.predict(model_18,test))
recession_pred_18_current = as.vector(h2o.predict(model_18,test_current))

# Variable Importances 18: 
#                        variable relative_importance scaled_importance percentage
# 1    tres_30y_1y_spread_lead_3m          197.583298          1.000000   0.352142
# 2    tres_30y_1y_spread_lead_6m          101.320549          0.512799   0.180578
# 3                  credit_index           53.769920          0.272138   0.095831
# 4   tres_20y_10y_spread_lead_6m           44.332584          0.224374   0.079012
# 5                leverage_index           20.543831          0.103976   0.036614
# 6           tres_30y_raw_chg_18           12.636505          0.063955   0.022521
# 7   tres_20y_1y_spread_lead_24m           11.132140          0.056342   0.019840
# 8   tres_10y_1y_spread_lead_24m           10.398430          0.052628   0.018533
# 9    tres_20y_1y_spread_lead_6m            9.251683          0.046824   0.016489
# 10                 unemployment            6.438619          0.032587   0.011475
# 11   tres_1y_6m_spread_lead_24m            6.027426          0.030506   0.010742
# 12 tres_20y_10y_spread_lead_12m            5.796154          0.029335   0.010330
# 13    tres_2y_1y_spread_lead_6m            5.471296          0.027691   0.009751
# 14  tres_20y_6m_spread_lead_12m            5.367116          0.027164   0.009566
# 15           tres_1m_raw_chg_18            5.312881          0.026889   0.009469
# 16  tres_20y_3m_spread_lead_12m            4.980070          0.025205   0.008876
# 17           tres_30y_1y_spread            4.766774          0.024125   0.008496
# 18     initial_claims_raw_chg_6            4.452953          0.022537   0.007936
# 19    tres_2y_6m_spread_lead_6m            4.107395          0.020788   0.007320
# 20   tres_5y_6m_spread_lead_12m            3.894927          0.019713   0.006942

# H2ORegressionMetrics: gbm
# ** Reported on training data. **
#   
# R2: 0.97

h2o.shutdown(prompt=FALSE)



#Build the current model results
current_test$date = as.character(current_test$date)
current_temp = current_test %>% select(date)
current_temp$recession_pred_c = recession_pred_c_current
current_temp$recession_pred_3 = recession_pred_3_current
current_temp$recession_pred_6 = recession_pred_6_current
current_temp$recession_pred_12 = recession_pred_12_current
current_temp$recession_pred_18 = recession_pred_18_current
current_temp = current_temp %>% mutate(recession_pred_c = ifelse(recession_pred_c > 1,1,ifelse(recession_pred_c < 0,0,recession_pred_c)),
                                       recession_pred_3 = ifelse(recession_pred_3 > 1,1,ifelse(recession_pred_3 < 0,0,recession_pred_3)),
                                       recession_pred_6 = ifelse(recession_pred_6 > 1,1,ifelse(recession_pred_6 < 0,0,recession_pred_6)),
                                       recession_pred_12 = ifelse(recession_pred_12 > 1,1,ifelse(recession_pred_12 < 0,0,recession_pred_12)),
                                       recession_pred_18 = ifelse(recession_pred_18 > 1,1,ifelse(recession_pred_18 < 0,0,recession_pred_18)))

#Read in previous models
current_inputs = read.csv("Data/Recession Probability/recession_inputs.csv",header = T,stringsAsFactors = F)
current_pred = read.csv("Data/Recession Probability/recession_prediction.csv",header = T,stringsAsFactors = F)

#Write backups
write.csv(current_pred,"Data/Recession Probability/Backup/recession_prediction.csv",row.names=F)
write.csv(current_inputs,"Data/Recession Probability/Backup/recession_inputs.csv",row.names=F)

#stack the currents model results onto previous
current_inputs = rbind(current_inputs,current_test)
current_pred = rbind(current_pred,current_temp)

#create view of change in predictions
rec_pred_chg = as.data.frame(cbind(c("Currently Recession","Recession within 12 Months"),
                                   c(current_pred[dim(current_pred)[1],"recession_pred_c"],current_pred[dim(current_pred)[1],"recession_pred_12"]),
                                   c(current_pred[dim(current_pred)[1]-4,"recession_pred_c"],current_pred[dim(current_pred)[1]-4,"recession_pred_12"]),
                                   c(current_pred[dim(current_pred)[1]-13,"recession_pred_c"],current_pred[dim(current_pred)[1]-13,"recession_pred_12"])))
colnames(rec_pred_chg) = c("Name","Current","Month Prior","Quarter Prior")
rec_pred_chg$Current = as.numeric(rec_pred_chg$Current)
rec_pred_chg$`Month Prior` = as.numeric(rec_pred_chg$`Month Prior`)
rec_pred_chg$`Quarter Prior` = as.numeric(rec_pred_chg$`Quarter Prior`)
rec_pred_chg$`Change from Month Prior` = rec_pred_chg$Current - rec_pred_chg$`Month Prior`
rec_pred_chg$`Change from Quarter Prior` = rec_pred_chg$Current - rec_pred_chg$`Quarter Prior`

#write out new stacked files
write.csv(current_pred,"Data/Recession Probability/recession_prediction.csv",row.names=F)
write.csv(current_inputs,"Data/Recession Probability/recession_inputs.csv",row.names=F)
write.csv(rec_pred_chg,"Data/Recession Probability/recession_prediction_changes.csv",row.names=F)


# temp = theData %>% select(date,time,recession) 
# temp$recession_pred_c = recession_pred_c
# temp$recession_pred_3 = recession_pred_3
# temp$recession_pred_6 = recession_pred_6
# temp$recession_pred_12 = recession_pred_12
# temp$recession_pred_18 = recession_pred_18
# temp = temp %>% mutate(recession_pred_c = round(ifelse(recession_pred_c > 1, 1, ifelse(recession_pred_c < 0, 0, recession_pred_c)),3),
#                        recession_pred_3 = round(ifelse(recession_pred_3 > 1, 1, ifelse(recession_pred_3 < 0, 0, recession_pred_3)),3),
#                        recession_pred_6 = round(ifelse(recession_pred_6 > 1, 1, ifelse(recession_pred_6 < 0, 0, recession_pred_6)),3),
#                        recession_pred_12 = round(ifelse(recession_pred_12 > 1, 1, ifelse(recession_pred_12 < 0, 0, recession_pred_12)),3),
#                        recession_pred_18 = round(ifelse(recession_pred_18 > 1, 1, ifelse(recession_pred_18 < 0, 0, recession_pred_18)),3))
# write.csv(temp,"Data/Recession Probability/recession.csv",row.names=F)



#Hyperparamater Tuning 
# max_depth_opt = c(3,5,10)
# min_rows_opt = c(10,15,20)
# ntrees_opt = c(10,25,50)
# col_sample_rt_opt = c(0.8,1.0)
# min_split_improvement_opt = c(0.01,0.001,0.0001)
# hyper_params = list(max_depth = max_depth_opt,
#                     min_rows=min_rows_opt,
#                     col_sample_rate = col_sample_rt_opt,
#                     ntrees=ntrees_opt,
#                     min_split_improvement=min_split_improvement_opt)
# 
# search_criteria = list(strategy = "RandomDiscrete",
#                        stopping_tolerance = 0.001,
#                        stopping_rounds = 10,
#                        max_runtime_secs = 4*60*60)

#current model
# model_dataset = theData %>% filter(!is.na(recession))
# model_subset = theData_subset %>% filter(!is.na(recession))
# 
#start H2O
# h2o.init(max_mem_size = "16g")
# 
# train_data = as.h2o(model_dataset)
# 
# grid = h2o.grid(algorithm = "gbm",
#                 grid_id = "gbm_grid",
#                 x = x_vars,
#                 y = y1,
#                 training_frame = train_data,
#                 fold_column = "folds",
#                 hyper_params = hyper_params,
#                 search_criteria=search_criteria,
#                 seed = 412)
# 
#view grid results
# grid_performance = h2o.getGrid(grid_id = "gbm_grid",
#                                sort_by = "residual_deviance",
#                                decreasing = FALSE)
# theperformance = as.data.frame(grid_performance@summary_table)
# print(theperformance)
# 
# h2o.shutdown(prompt=FALSE)

#     col_sample_rate max_depth min_rows min_split_improvement ntrees          model_ids residual_deviance
# 1               0.8        10       20                0.0100     25 gbm_grid_model_135        0.05276513
# 2               0.8        10       20                0.0010     25 gbm_grid_model_111        0.05287830
# 3               0.8        10       20                0.0001     25  gbm_grid_model_90        0.05287971
# 4               0.8         5       20                0.0001     25 gbm_grid_model_148        0.05336566
# 5               0.8         5       20                0.0100     25  gbm_grid_model_31        0.05336566
# 6               0.8         5       20                0.0010     25  gbm_grid_model_97        0.05336566
# 7               0.8        10       15                0.0100     25  gbm_grid_model_27        0.05441185
# 8               0.8        10       15                0.0001     25 gbm_grid_model_161        0.05443957
# 9               0.8        10       15                0.0010     25 gbm_grid_model_105        0.05444295
# 10              0.8        10       20                0.0010     50  gbm_grid_model_35        0.05451984
# 11              1.0         5       15                0.0100     25 gbm_grid_model_102        0.05466840
# 12              1.0         5       15                0.0001     25  gbm_grid_model_88        0.05466845
# 13              1.0         5       15                0.0010     25   gbm_grid_model_9        0.05466845
# 14              0.8        10       20                0.0001     50  gbm_grid_model_46        0.05468198
# 15              1.0        10       20                0.0010     25  gbm_grid_model_61        0.05487871
# 16              1.0        10       20                0.0001     25  gbm_grid_model_62        0.05487871
# 17              0.8         5       20                0.0010     50 gbm_grid_model_137        0.05498572
# 18              0.8         5       20                0.0001     50  gbm_grid_model_84        0.05498572
# 19              0.8         5       20                0.0100     50  gbm_grid_model_20        0.05498607
# 20              1.0        10       20                0.0100     25  gbm_grid_model_92        0.05499693
# 21              0.8         5       20                0.0010     10  gbm_grid_model_39        0.05503504
# 22              0.8         5       20                0.0100     10  gbm_grid_model_86        0.05503504
# 23              0.8         5       20                0.0001     10  gbm_grid_model_95        0.05503504
# 24              0.8        10       20                0.0100     50  gbm_grid_model_15        0.05503992
# 25              0.8        10       20                0.0100     10  gbm_grid_model_17        0.05505280
# 26              0.8        10       20                0.0001     10  gbm_grid_model_33        0.05505336
# 27              0.8        10       20                0.0010     10  gbm_grid_model_48        0.05505336
# 28              1.0         5       20                0.0100     25   gbm_grid_model_2        0.05521100
# 29              0.8         3       20                0.0001     10 gbm_grid_model_145        0.05529519
# 30              0.8         3       20                0.0010     10  gbm_grid_model_60        0.05529519
# 31              0.8         3       20                0.0100     10  gbm_grid_model_71        0.05529519
# 32              1.0         5       20                0.0010     25 gbm_grid_model_142        0.05547881
# 33              1.0         5       20                0.0001     25  gbm_grid_model_23        0.05547881
# 34              1.0        10       20                0.0100     50  gbm_grid_model_96        0.05566700
# 35              0.8        10       15                0.0100     50 gbm_grid_model_151        0.05569412
# 36              0.8         3       20                0.0010     25 gbm_grid_model_156        0.05571164
# 37              0.8         3       20                0.0100     25  gbm_grid_model_68        0.05571164
# 38              0.8         3       20                0.0001     25  gbm_grid_model_80        0.05571164
# 39              1.0        10       20                0.0001     50 gbm_grid_model_141        0.05574076
# 40              1.0        10       20                0.0010     50 gbm_grid_model_140        0.05574116
# 41              1.0         5       20                0.0010     10 gbm_grid_model_104        0.05606319
# 42              1.0         5       20                0.0001     10  gbm_grid_model_28        0.05606319
# 43              1.0         5       20                0.0100     10 gbm_grid_model_128        0.05606834
# 44              1.0        10       20                0.0001     10  gbm_grid_model_59        0.05609106
# 45              1.0        10       20                0.0010     10  gbm_grid_model_56        0.05609106
# 46              1.0        10       20                0.0100     10  gbm_grid_model_51        0.05609621
# 47              1.0        10       15                0.0100     25 gbm_grid_model_120        0.05650645
# 48              0.8        10       15                0.0001     50  gbm_grid_model_99        0.05653163
# 49              0.8        10       15                0.0001     10 gbm_grid_model_114        0.05655748
# 50              0.8        10       15                0.0010     10 gbm_grid_model_103        0.05655750
# 51              0.8        10       15                0.0100     10  gbm_grid_model_93        0.05655796
# 52              1.0        10       15                0.0001     25 gbm_grid_model_107        0.05666430
# 53              1.0        10       15                0.0010     25  gbm_grid_model_50        0.05666431
# 54              0.8        10       15                0.0010     50   gbm_grid_model_3        0.05669351
# 55              1.0         3       20                0.0100     10 gbm_grid_model_101        0.05671187
# 56              1.0         3       20                0.0001     10 gbm_grid_model_115        0.05671187
# 57              1.0         3       20                0.0010     10  gbm_grid_model_19        0.05671187
# 58              0.8         5       15                0.0010     10  gbm_grid_model_22        0.05679484
# 59              0.8         5       15                0.0001     10  gbm_grid_model_44        0.05679484
# 60              0.8         5       15                0.0100     10  gbm_grid_model_70        0.05679484
# 61              0.8         5       15                0.0010     50 gbm_grid_model_158        0.05684013
# 62              0.8         5       15                0.0001     50  gbm_grid_model_52        0.05684013
# 63              0.8         5       15                0.0100     50  gbm_grid_model_25        0.05684171
# 64              0.8         3       15                0.0100     10   gbm_grid_model_1        0.05714208
# 65              0.8         3       15                0.0010     10  gbm_grid_model_55        0.05714208
# 66              0.8         3       15                0.0001     10  gbm_grid_model_78        0.05714208
# 67              0.8         5       15                0.0001     25   gbm_grid_model_6        0.05726767
# 68              0.8         5       15                0.0010     25  gbm_grid_model_76        0.05726767
# 69              0.8         5       15                0.0100     25  gbm_grid_model_30        0.05726770
# 70              1.0         5       15                0.0100     50  gbm_grid_model_91        0.05727668
# 71              1.0         5       20                0.0100     50  gbm_grid_model_57        0.05731982
# 72              1.0         5       15                0.0001     50   gbm_grid_model_4        0.05763755
# 73              1.0         5       15                0.0010     50  gbm_grid_model_72        0.05763755
# 74              1.0         5       20                0.0001     50  gbm_grid_model_10        0.05764873
# 75              1.0         5       20                0.0010     50 gbm_grid_model_150        0.05764873
# 76              0.8         3       20                0.0010     50 gbm_grid_model_118        0.05793572
# 77              0.8         3       20                0.0001     50  gbm_grid_model_38        0.05793572
# 78              0.8         3       20                0.0100     50  gbm_grid_model_49        0.05793572
# 79              1.0         3       15                0.0001     10 gbm_grid_model_125        0.05833133
# 80              1.0         3       15                0.0100     10 gbm_grid_model_130        0.05833133
# 81              1.0         3       15                0.0010     10  gbm_grid_model_21        0.05833133
# 82              0.8         3       15                0.0001     25 gbm_grid_model_106        0.05838620
# 83              0.8         3       15                0.0010     25 gbm_grid_model_121        0.05838620
# 84              0.8         3       15                0.0100     25 gbm_grid_model_153        0.05838620
# 85              1.0        10       15                0.0001     10 gbm_grid_model_126        0.05880319
# 86              1.0        10       15                0.0010     10 gbm_grid_model_129        0.05880323
# 87              1.0        10       15                0.0100     10  gbm_grid_model_77        0.05880394
# 88              1.0        10       15                0.0001     50  gbm_grid_model_18        0.05884004
# 89              1.0        10       15                0.0100     50  gbm_grid_model_73        0.05890374
# 90              1.0        10       15                0.0010     50  gbm_grid_model_54        0.05900087
# 91              0.8         3       15                0.0001     50 gbm_grid_model_119        0.05915411
# 92              0.8         3       15                0.0010     50  gbm_grid_model_34        0.05915411
# 93              0.8         3       15                0.0100     50  gbm_grid_model_11        0.05916613
# 94              1.0         3       20                0.0010     25 gbm_grid_model_160        0.05928791
# 95              1.0         3       20                0.0001     25  gbm_grid_model_58        0.05928791
# 96              1.0         3       20                0.0100     25  gbm_grid_model_81        0.05928791
# 97              1.0         5       15                0.0001     10 gbm_grid_model_136        0.05934675
# 98              1.0         5       15                0.0010     10  gbm_grid_model_29        0.05934675
# 99              1.0         5       15                0.0100     10  gbm_grid_model_69        0.05934675
# 100             1.0         3       15                0.0100     25 gbm_grid_model_147        0.05940441
# 101             1.0         3       15                0.0001     25  gbm_grid_model_41        0.05940441
# 102             1.0         3       15                0.0010     25  gbm_grid_model_45        0.05940441
# 103             0.8        10       10                0.0100     10  gbm_grid_model_43        0.06096403
# 104             0.8         3       10                0.0010     10 gbm_grid_model_116        0.06100406
# 105             0.8         3       10                0.0100     10 gbm_grid_model_133        0.06100406
# 106             0.8         3       10                0.0001     10 gbm_grid_model_138        0.06100406
# 107             0.8        10       10                0.0001     10  gbm_grid_model_32        0.06105550
# 108             0.8        10       10                0.0010     10  gbm_grid_model_24        0.06105565
# 109             1.0         3       20                0.0100     50 gbm_grid_model_109        0.06116238
# 110             1.0         3       20                0.0010     50 gbm_grid_model_113        0.06116238
# 111             1.0         3       20                0.0001     50 gbm_grid_model_117        0.06116238
# 112             0.8         5       10                0.0100     10 gbm_grid_model_144        0.06122725
# 113             0.8         5       10                0.0010     10 gbm_grid_model_146        0.06131865
# 114             0.8         5       10                0.0001     10  gbm_grid_model_36        0.06131865
# 115             1.0         3       10                0.0100     10  gbm_grid_model_13        0.06158025
# 116             1.0         3       10                0.0010     10 gbm_grid_model_134        0.06158025
# 117             1.0         3       10                0.0001     10  gbm_grid_model_47        0.06158025
# 118             1.0        10       10                0.0001     25  gbm_grid_model_65        0.06179402
# 119             1.0        10       10                0.0001     10  gbm_grid_model_16        0.06196343
# 120             1.0        10       10                0.0010     10  gbm_grid_model_66        0.06196507
# 121             1.0        10       10                0.0100     10 gbm_grid_model_132        0.06196728
# 122             1.0         5       10                0.0100     10  gbm_grid_model_12        0.06197564
# 123             1.0         5       10                0.0001     10  gbm_grid_model_63        0.06197564
# 124             1.0         5       10                0.0010     10  gbm_grid_model_94        0.06197564
# 125             1.0        10       10                0.0001     50 gbm_grid_model_100        0.06231965
# 126             1.0        10       10                0.0100     25  gbm_grid_model_79        0.06257600
# 127             1.0        10       10                0.0010     25 gbm_grid_model_152        0.06277295
# 128             1.0         3       15                0.0100     50 gbm_grid_model_162        0.06288811
# 129             1.0         3       15                0.0010     50  gbm_grid_model_75        0.06290522
# 130             1.0         3       15                0.0001     50   gbm_grid_model_8        0.06290522
# 131             1.0        10       10                0.0100     50 gbm_grid_model_149        0.06319907
# 132             1.0         5       10                0.0100     25 gbm_grid_model_112        0.06336024
# 133             1.0         5       10                0.0010     25  gbm_grid_model_14        0.06336024
# 134             1.0         5       10                0.0001     25  gbm_grid_model_87        0.06336024
# 135             1.0        10       10                0.0010     50  gbm_grid_model_26        0.06349907
# 136             0.8         5       10                0.0100     25  gbm_grid_model_40        0.06395707
# 137             0.8         5       10                0.0001     25  gbm_grid_model_67        0.06418227
# 138             0.8         5       10                0.0010     25  gbm_grid_model_74        0.06418227
# 139             1.0         5       10                0.0010     50 gbm_grid_model_139        0.06430682
# 140             1.0         5       10                0.0100     50  gbm_grid_model_53        0.06430682
# 141             1.0         5       10                0.0001     50  gbm_grid_model_98        0.06430682
# 142             0.8        10       10                0.0100     25 gbm_grid_model_157        0.06465076


#3-months model
# model_dataset = theData %>% filter(!is.na(recession_3m))
# model_subset = theData_subset %>% filter(!is.na(recession_3m))

#start H2O
# h2o.init(max_mem_size = "16g")
# 
# train_data = as.h2o(model_dataset)
# 
# grid = h2o.grid(algorithm = "gbm",
#                 grid_id = "gbm_grid",
#                 x = x_vars,
#                 y = y2,
#                 training_frame = train_data,
#                 fold_column = "folds",
#                 hyper_params = hyper_params,
#                 search_criteria=search_criteria,
#                 seed = 412)
# 
#view grid results
# grid_performance = h2o.getGrid(grid_id = "gbm_grid",
#                                sort_by = "residual_deviance",
#                                decreasing = FALSE)
# theperformance = as.data.frame(grid_performance@summary_table)
# print(theperformance)
# 
# h2o.shutdown(prompt=FALSE)


#     col_sample_rate max_depth min_rows min_split_improvement ntrees          model_ids residual_deviance
# 1               1.0        10       15                0.0001     50 gbm_grid_model_108        0.06719820
# 2               1.0        10       15                0.0010     50  gbm_grid_model_76        0.06719978
# 3               1.0        10       15                0.0100     50 gbm_grid_model_111        0.06739899
# 4               1.0         5       15                0.0001     50 gbm_grid_model_130        0.06764381
# 5               1.0         5       15                0.0010     50  gbm_grid_model_81        0.06764381
# 6               1.0         5       15                0.0100     50  gbm_grid_model_34        0.06771249
# 7               1.0        10       15                0.0100     25   gbm_grid_model_8        0.06788212
# 8               0.8        10       15                0.0010     50 gbm_grid_model_102        0.06838306
# 9               0.8        10       10                0.0100     50  gbm_grid_model_56        0.06844835
# 10              0.8        10       15                0.0100     50  gbm_grid_model_45        0.06862677
# 11              0.8        10       10                0.0010     50  gbm_grid_model_86        0.06875290
# 12              0.8        10       15                0.0001     50  gbm_grid_model_68        0.06883431
# 13              1.0         5       15                0.0100     25 gbm_grid_model_117        0.06896340
# 14              1.0         5       15                0.0001     25  gbm_grid_model_55        0.06896340
# 15              1.0         5       15                0.0010     25  gbm_grid_model_61        0.06896340
# 16              1.0         3       20                0.0100     50  gbm_grid_model_58        0.06907267
# 17              1.0         5       20                0.0100     50  gbm_grid_model_31        0.06919469
# 18              1.0         3       20                0.0001     50 gbm_grid_model_105        0.06926837
# 19              1.0         3       20                0.0010     50  gbm_grid_model_19        0.06927499
# 20              1.0        10       20                0.0010     50  gbm_grid_model_20        0.06940254
# 21              1.0         3       20                0.0010     25  gbm_grid_model_26        0.06940496
# 22              1.0         3       20                0.0100     25  gbm_grid_model_69        0.06940496
# 23              1.0        10       20                0.0001     50  gbm_grid_model_57        0.06979398
# 24              0.8        10       15                0.0010     25 gbm_grid_model_129        0.07015592
# 25              0.8         5       15                0.0100     50  gbm_grid_model_66        0.07023873
# 26              0.8        10       15                0.0100     25  gbm_grid_model_77        0.07028406
# 27              0.8        10       15                0.0001     25 gbm_grid_model_127        0.07029480
# 28              0.8        10       10                0.0100     25  gbm_grid_model_15        0.07031912
# 29              0.8        10       10                0.0010     25   gbm_grid_model_3        0.07038566
# 30              1.0         5       20                0.0001     50  gbm_grid_model_33        0.07040680
# 31              1.0         5       20                0.0010     50  gbm_grid_model_85        0.07040680
# 32              0.8         5       20                0.0100     50 gbm_grid_model_114        0.07046403
# 33              0.8         5       15                0.0100     25  gbm_grid_model_35        0.07046729
# 34              0.8         3       15                0.0001     25 gbm_grid_model_115        0.07052100
# 35              0.8         3       15                0.0010     25 gbm_grid_model_126        0.07052100
# 36              0.8         3       15                0.0100     25  gbm_grid_model_39        0.07052100
# 37              0.8        10       20                0.0100     50  gbm_grid_model_91        0.07056833
# 38              1.0        10       20                0.0010     25  gbm_grid_model_38        0.07057108
# 39              0.8         5       15                0.0010     25  gbm_grid_model_25        0.07063800
# 40              1.0        10       20                0.0001     25  gbm_grid_model_87        0.07065375
# 41              0.8         5       20                0.0001     50   gbm_grid_model_2        0.07069877
# 42              0.8         5       20                0.0010     50  gbm_grid_model_67        0.07069877
# 43              0.8         3       15                0.0100     50  gbm_grid_model_52        0.07079526
# 44              0.8         3       15                0.0010     50  gbm_grid_model_92        0.07094033
# 45              0.8         5       15                0.0010     50  gbm_grid_model_23        0.07100048
# 46              0.8         5       15                0.0001     50  gbm_grid_model_78        0.07100048
# 47              0.8         5       20                0.0010     25 gbm_grid_model_122        0.07100756
# 48              0.8         5       20                0.0001     25  gbm_grid_model_13        0.07100756
# 49              1.0         5       20                0.0100     25  gbm_grid_model_18        0.07109289
# 50              1.0        10       20                0.0100     50   gbm_grid_model_5        0.07122957
# 51              0.8         5       10                0.0100     50   gbm_grid_model_7        0.07146802
# 52              1.0         5       20                0.0010     25  gbm_grid_model_10        0.07154627
# 53              1.0         5       20                0.0001     25 gbm_grid_model_103        0.07154627
# 54              1.0        10       20                0.0100     25  gbm_grid_model_62        0.07160541
# 55              0.8        10       20                0.0001     50 gbm_grid_model_112        0.07164500
# 56              0.8         3       20                0.0100     50  gbm_grid_model_21        0.07209504
# 57              0.8        10       20                0.0100     25 gbm_grid_model_110        0.07210031
# 58              1.0         3       15                0.0100     25 gbm_grid_model_104        0.07224007
# 59              1.0         3       15                0.0001     25 gbm_grid_model_116        0.07224007
# 60              1.0         3       15                0.0010     25   gbm_grid_model_9        0.07224007
# 61              1.0         3       15                0.0100     50  gbm_grid_model_16        0.07228188
# 62              1.0         3       15                0.0010     50 gbm_grid_model_100        0.07228459
# 63              1.0         3       15                0.0001     50   gbm_grid_model_4        0.07228459
# 64              1.0         3       10                0.0010     50 gbm_grid_model_123        0.07234544
# 65              1.0         3       10                0.0001     50  gbm_grid_model_70        0.07234544
# 66              0.8         3       10                0.0100     25 gbm_grid_model_120        0.07236301
# 67              0.8         3       10                0.0001     25 gbm_grid_model_124        0.07236301
# 68              0.8         3       10                0.0010     25  gbm_grid_model_64        0.07236301
# 69              1.0         3       10                0.0001     25  gbm_grid_model_32        0.07246900
# 70              0.8         3       20                0.0010     50 gbm_grid_model_118        0.07263705
# 71              0.8         3       20                0.0001     50  gbm_grid_model_12        0.07263705
# 72              0.8         3       20                0.0010     25 gbm_grid_model_119        0.07280720
# 73              0.8         3       20                0.0100     25  gbm_grid_model_97        0.07280720
# 74              0.8        10       20                0.0001     25  gbm_grid_model_72        0.07306539
# 75              0.8         5       10                0.0010     25  gbm_grid_model_41        0.07382650
# 76              0.8         5       10                0.0001     25  gbm_grid_model_65        0.07382650
# 77              0.8         5       10                0.0100     25  gbm_grid_model_99        0.07382650
# 78              0.8         3       10                0.0100     50 gbm_grid_model_106        0.07461338
# 79              0.8         3       10                0.0010     50  gbm_grid_model_46        0.07461338
# 80              0.8         3       10                0.0001     50  gbm_grid_model_74        0.07461338
# 81              0.8        10       10                0.0001     17 gbm_grid_model_134        0.07626213
# 82              1.0        10       10                0.0001     50  gbm_grid_model_51        0.07725435
# 83              1.0        10       10                0.0100     50  gbm_grid_model_53        0.07750286
# 84              1.0         5       15                0.0010     10  gbm_grid_model_14        0.07849139
# 85              1.0         5       15                0.0001     10  gbm_grid_model_17        0.07849139
# 86              1.0         5       15                0.0100     10  gbm_grid_model_22        0.07849139
# 87              0.8         3       10                0.0001     10 gbm_grid_model_131        0.07851222
# 88              0.8         3       10                0.0010     10  gbm_grid_model_93        0.07851222
# 89              1.0        10       10                0.0100     25  gbm_grid_model_50        0.07851579
# 90              1.0        10       10                0.0001     25  gbm_grid_model_73        0.07853685
# 91              0.8         5       10                0.0010     10 gbm_grid_model_132        0.07869855
# 92              0.8         5       10                0.0001     10  gbm_grid_model_30        0.07869855
# 93              0.8         3       15                0.0100     10 gbm_grid_model_113        0.07875906
# 94              0.8         3       15                0.0001     10  gbm_grid_model_43        0.07875906
# 95              1.0         3       15                0.0001     10  gbm_grid_model_40        0.07879668
# 96              1.0         3       15                0.0010     10  gbm_grid_model_47        0.07879668
# 97              1.0         3       15                0.0100     10  gbm_grid_model_80        0.07879668
# 98              0.8        10       10                0.0001     10  gbm_grid_model_83        0.07899427
# 99              0.8        10       10                0.0010     10  gbm_grid_model_94        0.07899452
# 100             0.8         5       15                0.0010     10   gbm_grid_model_1        0.07936634
# 101             0.8         5       15                0.0100     10  gbm_grid_model_84        0.07936634
# 102             1.0        10       15                0.0100     10  gbm_grid_model_36        0.07936674
# 103             1.0         5       10                0.0001     50 gbm_grid_model_128        0.07939306
# 104             1.0         5       10                0.0010     50  gbm_grid_model_82        0.07943222
# 105             0.8        10       15                0.0010     10  gbm_grid_model_89        0.07947023
# 106             0.8        10       15                0.0001     10 gbm_grid_model_125        0.07947026
# 107             0.8        10       15                0.0100     10  gbm_grid_model_44        0.07947077
# 108             0.8         5       20                0.0010     10  gbm_grid_model_88        0.07996801
# 109             0.8         5       20                0.0100     10  gbm_grid_model_96        0.07996801
# 110             1.0         3       20                0.0010     10  gbm_grid_model_24        0.08016001
# 111             1.0         3       20                0.0001     10  gbm_grid_model_49        0.08016001
# 112             1.0         3       20                0.0100     10  gbm_grid_model_71        0.08016001
# 113             0.8        10       20                0.0100     10  gbm_grid_model_79        0.08039989
# 114             0.8        10       20                0.0010     10  gbm_grid_model_42        0.08040010
# 115             0.8        10       20                0.0001     10 gbm_grid_model_101        0.08040010
# 116             0.8         3       20                0.0010     10 gbm_grid_model_121        0.08070440
# 117             0.8         3       20                0.0100     10  gbm_grid_model_27        0.08070440
# 118             0.8         3       20                0.0001     10  gbm_grid_model_60        0.08070440
# 119             1.0         5       10                0.0100     25  gbm_grid_model_98        0.08099178
# 120             1.0         5       10                0.0010     25  gbm_grid_model_54        0.08100330
# 121             1.0         5       10                0.0001     25  gbm_grid_model_90        0.08100330
# 122             1.0         3       10                0.0010     10  gbm_grid_model_11        0.08138722
# 123             1.0         3       10                0.0100     10 gbm_grid_model_133        0.08138722
# 124             1.0         5       20                0.0010     10  gbm_grid_model_28        0.08159171
# 125             1.0         5       20                0.0100     10  gbm_grid_model_63        0.08159171
# 126             1.0        10       20                0.0001     10   gbm_grid_model_6        0.08163992
# 127             1.0        10       20                0.0010     10 gbm_grid_model_107        0.08164125
# 128             1.0        10       20                0.0100     10  gbm_grid_model_37        0.08166975
# 129             1.0         5       10                0.0001     10 gbm_grid_model_109        0.08257220
# 130             1.0         5       10                0.0010     10  gbm_grid_model_29        0.08257220
# 131             1.0         5       10                0.0100     10  gbm_grid_model_48        0.08257220
# 132             1.0        10       10                0.0001     10  gbm_grid_model_75        0.08333757
# 133             1.0        10       10                0.0010     10  gbm_grid_model_95        0.08333757
# 134             1.0        10       10                0.0100     10  gbm_grid_model_59        0.08336151
# 135             0.8         3       10                0.0100      2 gbm_grid_model_135        0.09611763

#6-months model
# model_dataset = theData %>% filter(!is.na(recession_6m))
# model_subset = theData_subset %>% filter(!is.na(recession_6m))
# 
#start H2O
# h2o.init(max_mem_size = "16g")
# 
# train_data = as.h2o(model_dataset)
# 
# grid = h2o.grid(algorithm = "gbm",
#                 grid_id = "gbm_grid",
#                 x = x_vars,
#                 y = y3,
#                 training_frame = train_data,
#                 fold_column = "folds",
#                 hyper_params = hyper_params,
#                 search_criteria=search_criteria,
#                 seed = 412)
# 
#view grid results
# grid_performance = h2o.getGrid(grid_id = "gbm_grid",
#                                sort_by = "residual_deviance",
#                                decreasing = FALSE)
# theperformance = as.data.frame(grid_performance@summary_table)
# print(theperformance)
# 
# h2o.shutdown(prompt=FALSE)

#     col_sample_rate max_depth min_rows min_split_improvement ntrees          model_ids residual_deviance
# 1               0.8         3       10                0.0100     25 gbm_grid_model_102        0.08265996
# 2               0.8         3       10                0.0001     25   gbm_grid_model_3        0.08265996
# 3               0.8         3       10                0.0010     25  gbm_grid_model_83        0.08265996
# 4               1.0         3       20                0.0001     50  gbm_grid_model_46        0.08357573
# 5               1.0         3       20                0.0010     50  gbm_grid_model_75        0.08357573
# 6               1.0         3       20                0.0100     50  gbm_grid_model_96        0.08373104
# 7               0.8         3       10                0.0010     10 gbm_grid_model_159        0.08379832
# 8               0.8         3       10                0.0001     10  gbm_grid_model_21        0.08379832
# 9               0.8         3       10                0.0100     10  gbm_grid_model_92        0.08379832
# 10              1.0         3       10                0.0100     25 gbm_grid_model_135        0.08393186
# 11              1.0         3       10                0.0001     25  gbm_grid_model_51        0.08393186
# 12              1.0         3       10                0.0010     25  gbm_grid_model_87        0.08393186
# 13              0.8         3       20                0.0100     25  gbm_grid_model_37        0.08430294
# 14              0.8         3       20                0.0001     25  gbm_grid_model_66        0.08430294
# 15              0.8         3       20                0.0010     25  gbm_grid_model_77        0.08430294
# 16              0.8        10       10                0.0100     25 gbm_grid_model_101        0.08432944
# 17              0.8        10       10                0.0001     25 gbm_grid_model_117        0.08446730
# 18              0.8        10       10                0.0010     25 gbm_grid_model_143        0.08446730
# 19              1.0         3       10                0.0010     50 gbm_grid_model_129        0.08464738
# 20              1.0         3       10                0.0100     50 gbm_grid_model_144        0.08464738
# 21              1.0         3       10                0.0001     50  gbm_grid_model_86        0.08464738
# 22              1.0         3       20                0.0001     25 gbm_grid_model_113        0.08532928
# 23              1.0         3       20                0.0010     25 gbm_grid_model_138        0.08532928
# 24              1.0         3       20                0.0100     25  gbm_grid_model_23        0.08532928
# 25              0.8        10       10                0.0100     10  gbm_grid_model_69        0.08541295
# 26              0.8        10       10                0.0010     10 gbm_grid_model_126        0.08541307
# 27              0.8        10       10                0.0001     10  gbm_grid_model_15        0.08541307
# 28              0.8        10       15                0.0100     25   gbm_grid_model_9        0.08561901
# 29              0.8         3       20                0.0010     50  gbm_grid_model_10        0.08594355
# 30              0.8         3       20                0.0100     50  gbm_grid_model_24        0.08594355
# 31              0.8         3       20                0.0001     50  gbm_grid_model_56        0.08594355
# 32              0.8        10       15                0.0001     25 gbm_grid_model_150        0.08597032
# 33              0.8        10       10                0.0100     50 gbm_grid_model_136        0.08598870
# 34              0.8        10       15                0.0010     25 gbm_grid_model_134        0.08601604
# 35              0.8         3       10                0.0010     50   gbm_grid_model_1        0.08617927
# 36              0.8         3       10                0.0100     50  gbm_grid_model_42        0.08617927
# 37              0.8         3       10                0.0001     50  gbm_grid_model_85        0.08617927
# 38              0.8        10       10                0.0010     50 gbm_grid_model_121        0.08625823
# 39              0.8        10       10                0.0001     50  gbm_grid_model_50        0.08625823
# 40              1.0        10       20                0.0010     50  gbm_grid_model_11        0.08626032
# 41              1.0        10       20                0.0010     25  gbm_grid_model_19        0.08646625
# 42              1.0        10       20                0.0001     50 gbm_grid_model_118        0.08660486
# 43              1.0         5       20                0.0100     50   gbm_grid_model_8        0.08661890
# 44              1.0         5       20                0.0001     25  gbm_grid_model_90        0.08662162
# 45              1.0         5       20                0.0010     25  gbm_grid_model_99        0.08662162
# 46              0.8         5       20                0.0001     25  gbm_grid_model_91        0.08663702
# 47              0.8         5       20                0.0010     25  gbm_grid_model_94        0.08663702
# 48              1.0         5       20                0.0100     25  gbm_grid_model_70        0.08674139
# 49              0.8         3       15                0.0100     25  gbm_grid_model_84        0.08678166
# 50              1.0        10       20                0.0100     50 gbm_grid_model_100        0.08687325
# 51              0.8         5       20                0.0100     25  gbm_grid_model_79        0.08690201
# 52              1.0        10       20                0.0001     25   gbm_grid_model_4        0.08691829
# 53              0.8         3       15                0.0001     25 gbm_grid_model_108        0.08702301
# 54              0.8         3       15                0.0010     25  gbm_grid_model_13        0.08702301
# 55              0.8        10       20                0.0100     25  gbm_grid_model_88        0.08703187
# 56              0.8         5       10                0.0010     25   gbm_grid_model_2        0.08711801
# 57              0.8         5       10                0.0001     25  gbm_grid_model_48        0.08711801
# 58              0.8         5       10                0.0100     25  gbm_grid_model_76        0.08711801
# 59              1.0        10       20                0.0100     25 gbm_grid_model_148        0.08739187
# 60              0.8        10       20                0.0001     25  gbm_grid_model_58        0.08774372
# 61              0.8         5       15                0.0001     25  gbm_grid_model_25        0.08782348
# 62              0.8         5       15                0.0010     25  gbm_grid_model_33        0.08782348
# 63              0.8         5       20                0.0010     50 gbm_grid_model_145        0.08805383
# 64              0.8         5       20                0.0001     50  gbm_grid_model_78        0.08805383
# 65              0.8        10       20                0.0010     25  gbm_grid_model_32        0.08809799
# 66              1.0         3       10                0.0010     10 gbm_grid_model_110        0.08812606
# 67              1.0         3       10                0.0100     10 gbm_grid_model_153        0.08812606
# 68              1.0         3       10                0.0001     10  gbm_grid_model_54        0.08812606
# 69              0.8         5       15                0.0100     25  gbm_grid_model_97        0.08820839
# 70              0.8        10       20                0.0100     50 gbm_grid_model_104        0.08834315
# 71              0.8        10       15                0.0001     10 gbm_grid_model_111        0.08835185
# 72              0.8        10       15                0.0010     10 gbm_grid_model_115        0.08835185
# 73              1.0         5       20                0.0010     50  gbm_grid_model_40        0.08839367
# 74              1.0         5       20                0.0001     50  gbm_grid_model_98        0.08839367
# 75              0.8        10       15                0.0100     10  gbm_grid_model_35        0.08841362
# 76              0.8        10       20                0.0001     50  gbm_grid_model_73        0.08853177
# 77              1.0        10       15                0.0001     25  gbm_grid_model_28        0.08861721
# 78              1.0        10       15                0.0010     25  gbm_grid_model_53        0.08861721
# 79              0.8         5       10                0.0010     50 gbm_grid_model_154        0.08866395
# 80              0.8         5       10                0.0001     50  gbm_grid_model_38        0.08866395
# 81              1.0        10       15                0.0100     25  gbm_grid_model_57        0.08866407
# 82              1.0        10       10                0.0010     25 gbm_grid_model_155        0.08876497
# 83              1.0        10       10                0.0001     25  gbm_grid_model_93        0.08877412
# 84              1.0        10       20                0.0100     10  gbm_grid_model_18        0.08879229
# 85              1.0        10       20                0.0010     10 gbm_grid_model_140        0.08879670
# 86              1.0        10       20                0.0001     10  gbm_grid_model_27        0.08879670
# 87              1.0         3       20                0.0010     10 gbm_grid_model_120        0.08880591
# 88              1.0         3       20                0.0001     10 gbm_grid_model_142        0.08880591
# 89              1.0         3       20                0.0100     10  gbm_grid_model_29        0.08880591
# 90              0.8        10       20                0.0010     50  gbm_grid_model_31        0.08888231
# 91              0.8         5       20                0.0100     50 gbm_grid_model_151        0.08889917
# 92              0.8         5       10                0.0100     50 gbm_grid_model_124        0.08890220
# 93              0.8         3       15                0.0001     10 gbm_grid_model_125        0.08890960
# 94              0.8         3       15                0.0100     10 gbm_grid_model_131        0.08890960
# 95              0.8         3       15                0.0010     10 gbm_grid_model_158        0.08890960
# 96              0.8         5       10                0.0100     10 gbm_grid_model_156        0.08893370
# 97              0.8         5       10                0.0010     10  gbm_grid_model_30        0.08893370
# 98              0.8         5       10                0.0001     10  gbm_grid_model_47        0.08893370
# 99              1.0        10       10                0.0100     25 gbm_grid_model_132        0.08899968
# 100             1.0         5       10                0.0001     25 gbm_grid_model_162        0.08912247
# 101             1.0         5       10                0.0010     25  gbm_grid_model_39        0.08912247
# 102             1.0         5       10                0.0100     25  gbm_grid_model_49        0.08912247
# 103             1.0         3       15                0.0001     25 gbm_grid_model_105        0.08925720
# 104             1.0         3       15                0.0100     25  gbm_grid_model_64        0.08925720
# 105             1.0         3       15                0.0010     25  gbm_grid_model_82        0.08925720
# 106             1.0        10       10                0.0010     50  gbm_grid_model_36        0.08939521
# 107             0.8         5       15                0.0001     10 gbm_grid_model_139        0.08940314
# 108             0.8         5       15                0.0010     10  gbm_grid_model_59        0.08940314
# 109             0.8         5       15                0.0100     10  gbm_grid_model_14        0.08940334
# 110             1.0        10       10                0.0001     50 gbm_grid_model_149        0.08943248
# 111             1.0         5       20                0.0100     10 gbm_grid_model_133        0.08949676
# 112             1.0         5       20                0.0001     10 gbm_grid_model_137        0.08949676
# 113             1.0         5       20                0.0010     10  gbm_grid_model_43        0.08949676
# 114             1.0         5       15                0.0100     25  gbm_grid_model_44        0.08968082
# 115             1.0         5       15                0.0001     25  gbm_grid_model_52        0.08971107
# 116             1.0         5       15                0.0010     25  gbm_grid_model_95        0.08971107
# 117             1.0        10       10                0.0100     50 gbm_grid_model_152        0.08971215
# 118             0.8         5       15                0.0001     50 gbm_grid_model_106        0.09027589
# 119             0.8         5       15                0.0010     50  gbm_grid_model_26        0.09027589
# 120             1.0        10       10                0.0010     10  gbm_grid_model_65        0.09042414
# 121             1.0        10       10                0.0001     10  gbm_grid_model_80        0.09042649
# 122             1.0         5       10                0.0010     10 gbm_grid_model_127        0.09048139
# 123             1.0         5       10                0.0100     10 gbm_grid_model_160        0.09048139
# 124             1.0         5       10                0.0001     10  gbm_grid_model_55        0.09048139
# 125             1.0        10       10                0.0100     10  gbm_grid_model_68        0.09063234
# 126             0.8        10       15                0.0001     50   gbm_grid_model_7        0.09071767
# 127             0.8         3       15                0.0100     50  gbm_grid_model_67        0.09071799
# 128             1.0         5       15                0.0001     10 gbm_grid_model_130        0.09073718
# 129             1.0         5       15                0.0010     10 gbm_grid_model_147        0.09073718
# 130             1.0         5       15                0.0100     10  gbm_grid_model_71        0.09073718
# 131             0.8        10       15                0.0100     50 gbm_grid_model_157        0.09074577
# 132             1.0        10       15                0.0100     50  gbm_grid_model_61        0.09085201
# 133             1.0        10       15                0.0100     10  gbm_grid_model_60        0.09105293
# 134             0.8         5       20                0.0100     10  gbm_grid_model_41        0.09118891
# 135             0.8         5       20                0.0001     10  gbm_grid_model_45        0.09118891
# 136             0.8         5       20                0.0010     10   gbm_grid_model_6        0.09118891
# 137             0.8        10       20                0.0001     10  gbm_grid_model_72        0.09124356
# 138             0.8        10       20                0.0010     10  gbm_grid_model_34        0.09124356
# 139             1.0         5       10                0.0001     50 gbm_grid_model_122        0.09124370
# 140             1.0         5       10                0.0010     50  gbm_grid_model_17        0.09124370
# 141             0.8        10       20                0.0100     10  gbm_grid_model_63        0.09124385
# 142             0.8         3       20                0.0100     10  gbm_grid_model_12        0.09128631

#12-months model
# model_dataset = theData %>% filter(!is.na(recession_12m))
# model_subset = theData_subset %>% filter(!is.na(recession_12m))

#start H2O
# h2o.init(max_mem_size = "16g")
# 
# train_data = as.h2o(model_dataset)
# 
# grid = h2o.grid(algorithm = "gbm",
#                grid_id = "gbm_grid",
#                x = x_vars,
#                y = y4,
#                training_frame = train_data,
#                fold_column = "folds",
#                hyper_params = hyper_params,
#                search_criteria=search_criteria,
#                seed = 412)
# 
#view grid results
# grid_performance = h2o.getGrid(grid_id = "gbm_grid",
#                                sort_by = "residual_deviance",
#                                decreasing = FALSE)
# theperformance = as.data.frame(grid_performance@summary_table)
# print(theperformance)
# 
# h2o.shutdown(prompt=FALSE)

#     col_sample_rate max_depth min_rows min_split_improvement ntrees          model_ids residual_deviance
# 1               1.0        10       20                0.0100     25 gbm_grid_model_123         0.1018203
# 2               1.0        10       20                0.0010     25  gbm_grid_model_10         0.1019107
# 3               1.0        10       20                0.0001     25  gbm_grid_model_14         0.1019248
# 4               1.0         5       20                0.0010     25 gbm_grid_model_100         0.1020683
# 5               1.0         5       20                0.0001     25  gbm_grid_model_22         0.1020683
# 6               0.8         3       20                0.0100     25 gbm_grid_model_121         0.1021098
# 7               1.0         5       20                0.0100     25  gbm_grid_model_11         0.1021377
# 8               0.8         5       10                0.0100     25  gbm_grid_model_97         0.1023732
# 9               0.8         3       20                0.0001     25 gbm_grid_model_124         0.1025984
# 10              0.8         3       20                0.0010     25  gbm_grid_model_86         0.1025984
# 11              0.8         5       15                0.0001     25  gbm_grid_model_49         0.1029625
# 12              0.8         5       10                0.0010     25  gbm_grid_model_31         0.1030133
# 13              0.8         5       10                0.0001     25 gbm_grid_model_117         0.1030174
# 14              0.8         5       15                0.0010     25 gbm_grid_model_112         0.1030772
# 15              0.8        10       10                0.0100     25 gbm_grid_model_155         0.1030897
# 16              1.0         3       20                0.0001     25 gbm_grid_model_127         0.1031263
# 17              1.0         3       20                0.0100     25  gbm_grid_model_84         0.1031263
# 18              1.0         3       20                0.0010     25  gbm_grid_model_98         0.1031263
# 19              0.8         5       15                0.0100     25 gbm_grid_model_120         0.1031980
# 20              0.8        10       10                0.0001     25  gbm_grid_model_53         0.1032685
# 21              0.8         5       10                0.0001     50   gbm_grid_model_3         0.1032860
# 22              0.8         5       10                0.0100     50  gbm_grid_model_23         0.1033009
# 23              0.8         5       10                0.0010     50  gbm_grid_model_17         0.1033575
# 24              0.8        10       10                0.0010     25  gbm_grid_model_95         0.1036169
# 25              1.0         3       15                0.0010     25 gbm_grid_model_139         0.1037972
# 26              1.0         3       15                0.0001     25   gbm_grid_model_2         0.1037972
# 27              1.0         3       15                0.0100     25  gbm_grid_model_64         0.1037972
# 28              0.8         5       20                0.0100     25  gbm_grid_model_47         0.1040423
# 29              0.8        10       10                0.0100     50  gbm_grid_model_82         0.1040514
# 30              0.8        10       10                0.0001     50  gbm_grid_model_67         0.1041121
# 31              0.8         3       20                0.0100     50 gbm_grid_model_137         0.1041631
# 32              0.8        10       10                0.0010     50 gbm_grid_model_154         0.1046219
# 33              1.0         3       20                0.0001     50  gbm_grid_model_15         0.1046914
# 34              1.0         3       20                0.0010     50  gbm_grid_model_87         0.1046914
# 35              0.8         3       20                0.0001     50 gbm_grid_model_150         0.1048137
# 36              0.8         3       20                0.0010     50  gbm_grid_model_68         0.1048137
# 37              0.8        10       20                0.0010     25  gbm_grid_model_21         0.1048276
# 38              0.8        10       20                0.0100     25 gbm_grid_model_152         0.1048290
# 39              0.8        10       20                0.0001     25 gbm_grid_model_128         0.1048627
# 40              1.0         3       20                0.0100     50  gbm_grid_model_94         0.1048681
# 41              0.8         5       20                0.0010     25 gbm_grid_model_149         0.1049030
# 42              0.8         5       20                0.0001     25  gbm_grid_model_50         0.1049030
# 43              0.8         5       15                0.0100     50   gbm_grid_model_7         0.1050390
# 44              0.8         5       15                0.0010     50 gbm_grid_model_132         0.1052087
# 45              0.8         5       15                0.0001     50 gbm_grid_model_125         0.1052921
# 46              0.8        10       15                0.0010     25  gbm_grid_model_69         0.1056805
# 47              0.8        10       15                0.0001     25  gbm_grid_model_78         0.1056805
# 48              1.0         3       15                0.0100     50 gbm_grid_model_126         0.1057362
# 49              1.0         3       15                0.0010     50  gbm_grid_model_33         0.1057362
# 50              1.0         3       15                0.0001     50  gbm_grid_model_83         0.1057362
# 51              1.0        10       10                0.0100     50  gbm_grid_model_46         0.1057970
# 52              0.8        10       15                0.0100     25 gbm_grid_model_104         0.1059474
# 53              1.0        10       10                0.0100     25  gbm_grid_model_99         0.1060532
# 54              0.8        10       15                0.0100     50 gbm_grid_model_145         0.1064292
# 55              1.0        10       10                0.0010     25  gbm_grid_model_77         0.1064976
# 56              0.8        10       20                0.0001     50  gbm_grid_model_38         0.1065597
# 57              1.0        10       10                0.0010     50   gbm_grid_model_9         0.1065892
# 58              1.0         5       15                0.0100     25  gbm_grid_model_18         0.1065983
# 59              1.0        10       10                0.0001     25 gbm_grid_model_159         0.1066356
# 60              1.0         5       15                0.0001     25   gbm_grid_model_5         0.1067220
# 61              1.0         5       15                0.0010     25  gbm_grid_model_70         0.1067220
# 62              1.0        10       20                0.0100     50  gbm_grid_model_79         0.1067843
# 63              0.8         3       15                0.0100     25  gbm_grid_model_90         0.1069292
# 64              0.8         3       15                0.0001     25 gbm_grid_model_113         0.1069669
# 65              0.8         3       15                0.0010     25  gbm_grid_model_85         0.1069669
# 66              0.8        10       20                0.0010     50  gbm_grid_model_56         0.1071844
# 67              1.0        10       10                0.0001     50  gbm_grid_model_27         0.1072657
# 68              1.0        10       20                0.0010     50  gbm_grid_model_66         0.1073267
# 69              1.0        10       20                0.0001     50 gbm_grid_model_115         0.1073793
# 70              0.8        10       15                0.0001     50 gbm_grid_model_151         0.1075450
# 71              0.8        10       15                0.0010     50  gbm_grid_model_34         0.1075450
# 72              1.0         5       10                0.0001     50 gbm_grid_model_116         0.1075872
# 73              1.0         5       10                0.0100     50  gbm_grid_model_52         0.1075872
# 74              1.0         5       10                0.0010     50  gbm_grid_model_80         0.1075872
# 75              0.8        10       20                0.0100     50  gbm_grid_model_63         0.1078022
# 76              0.8         5       20                0.0010     50 gbm_grid_model_129         0.1080654
# 77              0.8         5       20                0.0001     50 gbm_grid_model_158         0.1080654
# 78              0.8         3       15                0.0010     50  gbm_grid_model_65         0.1080859
# 79              0.8         3       15                0.0001     50  gbm_grid_model_73         0.1080859
# 80              1.0         5       10                0.0001     25 gbm_grid_model_144         0.1081191
# 81              1.0         5       10                0.0010     25 gbm_grid_model_162         0.1081191
# 82              1.0         5       10                0.0100     25  gbm_grid_model_45         0.1081191
# 83              0.8         5       20                0.0100     50  gbm_grid_model_51         0.1081276
# 84              1.0         5       20                0.0100     50 gbm_grid_model_146         0.1082019
# 85              1.0         5       20                0.0010     50 gbm_grid_model_131         0.1082084
# 86              1.0         5       20                0.0001     50  gbm_grid_model_36         0.1082084
# 87              0.8         3       15                0.0100     50 gbm_grid_model_143         0.1083068
# 88              1.0         5       15                0.0010     50  gbm_grid_model_24         0.1084436
# 89              1.0         5       15                0.0001     50  gbm_grid_model_37         0.1084436
# 90              0.8         3       10                0.0100     50 gbm_grid_model_110         0.1084629
# 91              0.8         3       10                0.0010     50 gbm_grid_model_142         0.1085192
# 92              0.8         3       10                0.0001     50  gbm_grid_model_76         0.1085192
# 93              0.8         3       10                0.0001     25 gbm_grid_model_114         0.1090025
# 94              0.8         3       10                0.0010     25  gbm_grid_model_16         0.1090025
# 95              0.8         3       10                0.0100     25   gbm_grid_model_6         0.1090025
# 96              1.0        10       15                0.0010     25 gbm_grid_model_157         0.1094930
# 97              1.0        10       15                0.0001     25  gbm_grid_model_29         0.1094930
# 98              1.0        10       15                0.0010     50 gbm_grid_model_102         0.1098339
# 99              1.0        10       15                0.0001     50  gbm_grid_model_92         0.1098339
# 100             1.0         5       15                0.0100     50 gbm_grid_model_103         0.1099619
# 101             1.0        10       15                0.0100     25  gbm_grid_model_60         0.1101197
# 102             1.0         3       10                0.0100     25 gbm_grid_model_101         0.1105113
# 103             1.0         3       10                0.0001     25  gbm_grid_model_40         0.1105974
# 104             1.0         3       10                0.0010     25  gbm_grid_model_42         0.1105974
# 105             1.0        10       20                0.0010     10  gbm_grid_model_54         0.1106659
# 106             1.0        10       20                0.0100     10 gbm_grid_model_136         0.1106659
# 107             1.0        10       20                0.0001     10 gbm_grid_model_111         0.1106660
# 108             1.0        10       15                0.0100     50  gbm_grid_model_41         0.1109809
# 109             1.0         5       20                0.0001     10  gbm_grid_model_12         0.1113908
# 110             1.0         5       20                0.0100     10 gbm_grid_model_148         0.1113908
# 111             1.0         5       20                0.0010     10 gbm_grid_model_156         0.1113908
# 112             1.0         3       10                0.0010     50 gbm_grid_model_138         0.1115407
# 113             1.0         3       10                0.0001     50  gbm_grid_model_25         0.1115407
# 114             0.8         3       20                0.0001     10 gbm_grid_model_109         0.1118602
# 115             0.8         3       20                0.0010     10 gbm_grid_model_119         0.1118602
# 116             0.8         3       20                0.0100     10  gbm_grid_model_96         0.1118602
# 117             1.0         3       10                0.0100     50  gbm_grid_model_39         0.1119252
# 118             1.0         3       20                0.0001     10 gbm_grid_model_107         0.1127205
# 119             1.0         3       20                0.0100     10  gbm_grid_model_30         0.1127205
# 120             1.0         3       20                0.0010     10  gbm_grid_model_48         0.1127205
# 121             0.8         5       15                0.0010     10 gbm_grid_model_133         0.1130918
# 122             0.8         5       15                0.0001     10 gbm_grid_model_141         0.1130918
# 123             0.8         5       15                0.0100     10  gbm_grid_model_72         0.1130918
# 124             0.8         5       10                0.0001     10  gbm_grid_model_43         0.1132215
# 125             0.8         5       10                0.0100     10  gbm_grid_model_62         0.1132215
# 126             0.8         5       10                0.0010     10  gbm_grid_model_89         0.1132215
# 127             0.8        10       10                0.0100     10 gbm_grid_model_130         0.1133012
# 128             0.8        10       10                0.0001     10   gbm_grid_model_1         0.1133012
# 129             0.8        10       10                0.0010     10  gbm_grid_model_61         0.1133014
# 130             0.8        10       15                0.0001     10 gbm_grid_model_161         0.1140409
# 131             0.8        10       15                0.0010     10  gbm_grid_model_26         0.1140409
# 132             0.8        10       15                0.0100     10 gbm_grid_model_105         0.1140410
# 133             0.8        10       20                0.0001     10  gbm_grid_model_55         0.1141449
# 134             0.8        10       20                0.0010     10  gbm_grid_model_71         0.1141449
# 135             0.8        10       20                0.0100     10  gbm_grid_model_20         0.1141502
# 136             1.0         3       15                0.0010     10 gbm_grid_model_108         0.1142369
# 137             1.0         3       15                0.0100     10  gbm_grid_model_35         0.1142369
# 138             1.0         3       15                0.0001     10  gbm_grid_model_75         0.1142369
# 139             0.8         5       20                0.0010     10  gbm_grid_model_28         0.1153163
# 140             0.8         5       20                0.0001     10  gbm_grid_model_57         0.1153163
# 141             0.8         5       20                0.0100     10   gbm_grid_model_8         0.1153163
# 142             0.8         3       15                0.0010     10 gbm_grid_model_122         0.1161686


#18-month model
# model_dataset = theData %>% filter(!is.na(recession_18m))
# model_subset = theData_subset %>% filter(!is.na(recession_18m))
# 
#start H2O
# h2o.init(max_mem_size = "16g")
# 
# train_data = as.h2o(model_dataset)
# 
# grid = h2o.grid(algorithm = "gbm",
#                 grid_id = "gbm_grid",
#                 x = x_vars,
#                 y = y5,
#                 training_frame = train_data,
#                 fold_column = "folds",
#                 hyper_params = hyper_params,
#                 search_criteria=search_criteria,
#                 seed = 412)
# 
#view grid results
# grid_performance = h2o.getGrid(grid_id = "gbm_grid",
#                                sort_by = "residual_deviance",
#                                decreasing = FALSE)
# theperformance = as.data.frame(grid_performance@summary_table)
# print(theperformance)
# 
# h2o.shutdown(prompt=FALSE)

#     col_sample_rate max_depth min_rows min_split_improvement ntrees          model_ids residual_deviance
# 1               1.0         3       10                0.0001     25   gbm_grid_model_7         0.1195875
# 2               1.0         3       10                0.0010     25  gbm_grid_model_73         0.1195875
# 3               1.0         3       10                0.0100     25  gbm_grid_model_75         0.1195875
# 4               1.0         3       10                0.0001     50  gbm_grid_model_23         0.1199073
# 5               1.0         3       10                0.0010     50  gbm_grid_model_52         0.1199073
# 6               1.0         3       10                0.0100     50  gbm_grid_model_13         0.1200821
# 7               0.8         5       10                0.0001     50 gbm_grid_model_107         0.1209957
# 8               0.8         5       10                0.0010     50  gbm_grid_model_62         0.1209957
# 9               0.8         5       10                0.0100     50  gbm_grid_model_15         0.1210618
# 10              0.8         3       10                0.0001     50 gbm_grid_model_124         0.1227543
# 11              0.8         3       10                0.0100     50 gbm_grid_model_157         0.1227543
# 12              0.8         3       10                0.0010     50  gbm_grid_model_89         0.1227543
# 13              0.8         3       10                0.0001     25 gbm_grid_model_113         0.1264055
# 14              0.8         3       10                0.0010     25 gbm_grid_model_148         0.1264055
# 15              0.8         3       10                0.0100     25   gbm_grid_model_6         0.1264055
# 16              0.8         5       15                0.0100     50 gbm_grid_model_120         0.1299863
# 17              1.0         3       15                0.0001     50 gbm_grid_model_133         0.1300812
# 18              1.0         3       15                0.0010     50 gbm_grid_model_139         0.1300812
# 19              1.0         3       15                0.0100     50  gbm_grid_model_59         0.1300812
# 20              0.8        10       15                0.0001     50  gbm_grid_model_71         0.1303664
# 21              0.8        10       15                0.0010     50  gbm_grid_model_92         0.1303664
# 22              0.8         5       10                0.0001     25  gbm_grid_model_56         0.1307179
# 23              0.8         5       10                0.0010     25  gbm_grid_model_58         0.1307179
# 24              0.8         5       10                0.0100     25  gbm_grid_model_57         0.1307182
# 25              1.0         3       15                0.0010     25  gbm_grid_model_10         0.1309587
# 26              1.0         3       15                0.0001     25 gbm_grid_model_112         0.1309587
# 27              1.0         3       15                0.0100     25  gbm_grid_model_90         0.1309587
# 28              0.8        10       10                0.0001     50  gbm_grid_model_39         0.1309794
# 29              0.8         5       15                0.0001     50 gbm_grid_model_122         0.1312330
# 30              0.8         5       15                0.0010     50  gbm_grid_model_91         0.1312330
# 31              0.8        10       15                0.0100     50  gbm_grid_model_35         0.1312556
# 32              0.8        10       20                0.0100     50 gbm_grid_model_143         0.1316345
# 33              0.8         3       20                0.0001     25   gbm_grid_model_1         0.1317891
# 34              0.8         3       20                0.0010     25 gbm_grid_model_127         0.1317891
# 35              0.8         3       20                0.0001     50 gbm_grid_model_151         0.1318879
# 36              0.8         3       20                0.0010     50  gbm_grid_model_83         0.1318879
# 37              0.8        10       10                0.0100     50  gbm_grid_model_34         0.1319147
# 38              1.0         5       10                0.0100     50 gbm_grid_model_106         0.1320246
# 39              1.0         5       10                0.0010     50 gbm_grid_model_135         0.1320290
# 40              1.0         5       10                0.0001     50  gbm_grid_model_60         0.1320290
# 41              0.8         3       20                0.0100     25  gbm_grid_model_17         0.1323006
# 42              0.8         5       20                0.0010     50 gbm_grid_model_152         0.1324044
# 43              0.8         5       20                0.0001     50  gbm_grid_model_47         0.1324044
# 44              0.8         5       20                0.0100     50  gbm_grid_model_40         0.1324549
# 45              0.8        10       20                0.0001     50 gbm_grid_model_103         0.1325295
# 46              0.8        10       20                0.0010     50  gbm_grid_model_50         0.1325295
# 47              0.8        10       10                0.0010     50 gbm_grid_model_126         0.1326430
# 48              0.8         3       15                0.0010     50 gbm_grid_model_140         0.1336823
# 49              0.8         3       15                0.0001     50  gbm_grid_model_31         0.1336823
# 50              0.8         5       15                0.0001     25 gbm_grid_model_137         0.1337033
# 51              0.8         5       15                0.0010     25 gbm_grid_model_141         0.1337033
# 52              0.8         5       15                0.0100     25 gbm_grid_model_150         0.1337033
# 53              0.8         3       15                0.0001     25 gbm_grid_model_117         0.1337175
# 54              0.8         3       15                0.0010     25  gbm_grid_model_49         0.1337175
# 55              0.8         3       15                0.0100     50 gbm_grid_model_147         0.1337833
# 56              0.8         3       15                0.0100     25   gbm_grid_model_4         0.1337960
# 57              0.8         3       20                0.0100     50  gbm_grid_model_82         0.1338085
# 58              0.8         5       20                0.0100     25 gbm_grid_model_119         0.1340348
# 59              0.8         5       20                0.0001     25  gbm_grid_model_14         0.1340348
# 60              0.8         5       20                0.0010     25  gbm_grid_model_45         0.1340348
# 61              1.0        10       10                0.0001     50  gbm_grid_model_16         0.1341908
# 62              1.0        10       10                0.0010     50  gbm_grid_model_65         0.1342014
# 63              1.0        10       20                0.0001     50 gbm_grid_model_138         0.1342564
# 64              1.0        10       10                0.0100     50  gbm_grid_model_84         0.1342955
# 65              0.8        10       20                0.0100     25   gbm_grid_model_8         0.1343559
# 66              1.0        10       20                0.0010     50  gbm_grid_model_11         0.1345352
# 67              1.0         3       20                0.0010     25 gbm_grid_model_142         0.1347263
# 68              1.0         3       20                0.0001     25  gbm_grid_model_51         0.1347263
# 69              1.0         3       20                0.0100     25 gbm_grid_model_104         0.1347571
# 70              0.8        10       20                0.0001     25 gbm_grid_model_134         0.1353099
# 71              0.8        10       20                0.0010     25  gbm_grid_model_64         0.1353099
# 72              1.0        10       20                0.0100     50 gbm_grid_model_161         0.1354208
# 73              1.0         3       20                0.0100     50 gbm_grid_model_108         0.1357112
# 74              1.0         3       20                0.0001     50  gbm_grid_model_80         0.1359009
# 75              1.0         3       20                0.0010     50  gbm_grid_model_96         0.1359009
# 76              1.0         5       15                0.0001     50 gbm_grid_model_123         0.1359763
# 77              1.0         5       15                0.0010     50   gbm_grid_model_9         0.1359763
# 78              1.0         5       10                0.0100     25 gbm_grid_model_159         0.1360916
# 79              1.0         5       10                0.0010     25 gbm_grid_model_130         0.1360916
# 80              1.0         5       10                0.0001     25 gbm_grid_model_158         0.1360916
# 81              1.0         5       15                0.0100     50 gbm_grid_model_162         0.1363472
# 82              0.8        10       15                0.0010     25 gbm_grid_model_118         0.1376486
# 83              0.8        10       15                0.0001     25 gbm_grid_model_153         0.1376486
# 84              1.0        10       20                0.0001     25  gbm_grid_model_74         0.1377746
# 85              1.0        10       20                0.0010     25 gbm_grid_model_131         0.1377883
# 86              0.8        10       10                0.0100     25 gbm_grid_model_109         0.1377957
# 87              1.0        10       15                0.0100     50  gbm_grid_model_48         0.1378714
# 88              1.0        10       15                0.0001     50  gbm_grid_model_30         0.1378963
# 89              0.8        10       10                0.0001     25 gbm_grid_model_125         0.1380336
# 90              1.0        10       10                0.0010     25  gbm_grid_model_27         0.1380743
# 91              1.0        10       15                0.0010     50  gbm_grid_model_32         0.1380989
# 92              1.0        10       10                0.0001     25 gbm_grid_model_145         0.1381179
# 93              1.0        10       10                0.0100     25 gbm_grid_model_101         0.1381382
# 94              1.0        10       20                0.0100     25  gbm_grid_model_95         0.1381996
# 95              0.8        10       10                0.0010     25 gbm_grid_model_156         0.1382165
# 96              0.8        10       15                0.0100     25  gbm_grid_model_46         0.1382200
# 97              1.0         5       15                0.0001     25  gbm_grid_model_79         0.1407557
# 98              1.0         5       15                0.0010     25  gbm_grid_model_94         0.1407557
# 99              1.0        10       15                0.0100     25 gbm_grid_model_144         0.1407869
# 100             1.0         5       15                0.0100     25  gbm_grid_model_28         0.1407942
# 101             1.0         5       20                0.0001     50 gbm_grid_model_111         0.1408199
# 102             1.0         5       20                0.0010     50  gbm_grid_model_99         0.1408199
# 103             1.0         5       20                0.0100     50 gbm_grid_model_154         0.1412840
# 104             1.0        10       15                0.0010     25  gbm_grid_model_55         0.1423561
# 105             1.0        10       15                0.0001     25 gbm_grid_model_110         0.1423780
# 106             0.8         3       10                0.0010     10 gbm_grid_model_121         0.1435941
# 107             0.8         3       10                0.0100     10  gbm_grid_model_19         0.1435941
# 108             0.8         3       10                0.0001     10   gbm_grid_model_3         0.1435941
# 109             1.0         5       20                0.0100     25  gbm_grid_model_36         0.1445860
# 110             0.8         3       20                0.0100     10 gbm_grid_model_129         0.1446004
# 111             0.8         3       20                0.0010     10 gbm_grid_model_160         0.1446004
# 112             0.8         3       20                0.0001     10  gbm_grid_model_85         0.1446004
# 113             1.0         5       20                0.0001     25 gbm_grid_model_102         0.1446176
# 114             1.0         5       20                0.0010     25 gbm_grid_model_114         0.1446176
# 115             0.8        10       20                0.0100     10  gbm_grid_model_61         0.1467616
# 116             0.8        10       20                0.0010     10   gbm_grid_model_5         0.1476465
# 117             0.8        10       20                0.0001     10  gbm_grid_model_86         0.1476465
# 118             0.8         3       15                0.0100     10 gbm_grid_model_115         0.1479993
# 119             0.8         3       15                0.0001     10 gbm_grid_model_149         0.1479993
# 120             0.8         3       15                0.0010     10  gbm_grid_model_38         0.1479993
# 121             0.8         5       20                0.0001     10  gbm_grid_model_21         0.1481744
# 122             0.8         5       20                0.0100     10  gbm_grid_model_42         0.1481744
# 123             0.8         5       20                0.0010     10  gbm_grid_model_93         0.1481744
# 124             1.0         3       10                0.0001     10  gbm_grid_model_12         0.1484205
# 125             1.0         3       10                0.0100     10  gbm_grid_model_43         0.1484205
# 126             1.0         3       10                0.0010     10  gbm_grid_model_68         0.1484205
# 127             0.8         5       10                0.0100     10 gbm_grid_model_128         0.1497802
# 128             0.8         5       10                0.0001     10  gbm_grid_model_77         0.1497802
# 129             0.8         5       10                0.0010     10  gbm_grid_model_88         0.1497802
# 130             0.8        10       10                0.0100     10 gbm_grid_model_100         0.1528093
# 131             1.0         3       15                0.0001     10 gbm_grid_model_132         0.1528142
# 132             1.0         3       15                0.0100     10  gbm_grid_model_20         0.1528142
# 133             1.0         3       15                0.0010     10  gbm_grid_model_33         0.1528142
# 134             0.8        10       10                0.0010     10  gbm_grid_model_63         0.1530368
# 135             0.8        10       10                0.0001     10  gbm_grid_model_70         0.1530368
# 136             0.8         5       15                0.0100     10  gbm_grid_model_22         0.1537385
# 137             0.8         5       15                0.0001     10  gbm_grid_model_69         0.1537385
# 138             0.8         5       15                0.0010     10  gbm_grid_model_87         0.1537385
# 139             1.0         5       15                0.0010     10 gbm_grid_model_116         0.1542093
# 140             1.0         5       15                0.0100     10 gbm_grid_model_136         0.1542093
# 141             1.0         5       15                0.0001     10 gbm_grid_model_146         0.1542093
# 142             1.0        10       15                0.0100     10  gbm_grid_model_78         0.1542291