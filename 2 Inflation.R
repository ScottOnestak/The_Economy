# The Economy
# 2 Inflation
# Scott Onestak

#packages
library(dplyr)
library(tidyr)
library(fredr)
library(h2o)

options(scipen=999)

#Get Economic Variables
fredr_set_key("1e7ed343d3ccb82af108e43174bf8f1f")

cpi = fredr(series_id = "CPIAUCSL",
            observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
            frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(cpi=value)

cpi_core = fredr(series_id = "CPILFESL",
                 observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                 frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(cpi_core=value)

pce = fredr(series_id = "PCEPI",
            observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
            frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(pce=value)

pce_core = fredr(series_id = "PCEPILFE",
                 observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                 frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(pce_core=value)

m2 = fredr(series_id = "M2SL",
           observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
           frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(m2=value)

ppi = fredr(series_id = "PPIACO",
            observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
            frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(ppi=value)

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

earnings = fredr(series_id = "LES1252881500Q",
                 observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                 frequency = "q") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(earnings=value)

lfpr = fredr(series_id = "CIVPART",
             observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
             frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(lfpr=value)

GDP = fredr(series_id = "GDP",
            observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
            frequency = "q") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(GDP=value)

PGDP = fredr(series_id = "NGDPPOT",
             observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
             frequency = "q") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(PGDP=value)

fed_funds = fredr(series_id = "DFF",
                  observation_start = as.Date("1970-01-01",format="%Y-%m-%d"),
                  frequency = "m") %>% select(date,value) %>% filter(!is.na(value)) %>% rename(fed_funds=value)


#Join the data together
theData = cpi %>% left_join(.,cpi_core,by="date") %>%
                  left_join(.,pce,by="date") %>%
                  left_join(.,pce_core,by="date") %>%
                  left_join(.,earnings,by="date") %>%
                  left_join(.,lfpr,by="date") %>% 
                  left_join(.,m2,by="date") %>%
                  left_join(.,ppi,by="date") %>%
                  left_join(.,unemployment,by="date") %>%
                  left_join(.,fed_funds,by="date") %>%
                  left_join(.,GDP,by="date") %>%
                  left_join(.,PGDP,by="date") %>%
                  left_join(.,tres_30y,by="date") %>%
                  left_join(.,tres_20y,by="date") %>%
                  left_join(.,tres_10y,by="date") %>%
                  left_join(.,tres_5y,by="date") %>%
                  left_join(.,tres_2y,by="date") %>%
                  left_join(.,tres_1y,by="date") %>%
                  left_join(.,tres_6m,by="date") %>%
                  left_join(.,tres_3m,by="date") %>%
                  left_join(.,tres_1m,by="date") %>%
                  arrange(date)

#Fill in GDP, PGDP, and earnings since the data is monthly and these values are quarterly
GDP_temp = NA
PGDP_temp = NA
earnings_temp = NA
counter1 = 1
counter2 = 1
counter3 = 1

for(i in seq(from=1,to=dim(theData)[1],by=1)){
  if(!is.na(theData[i,"GDP"])){
    GDP_temp = theData[i,"GDP"]
    counter1 = 1
  } else {
    if(!is.na(GDP_temp)){
      if(counter1 <= 2){
        theData[i,"GDP"] = GDP_temp
        counter1 = counter1 + 1
      } else {
        GDP_temp = NA
        counter1 = 1
      }
    }
  }
  
  if(!is.na(theData[i,"PGDP"])){
    PGDP_temp = theData[i,"PGDP"]
    counter2 = 1
  } else {
    if(!is.na(PGDP_temp)){
      if(counter2 <= 2){
        theData[i,"PGDP"] = PGDP_temp
        counter2 = counter2 + 1
      } else {
        PGDP_temp = NA
        counter2 = 1
      }
    }
  }
  
  if(!is.na(theData[i,"earnings"])){
    earnings_temp = theData[i,"earnings"]
    counter3 = 1
  } else {
    if(!is.na(PGDP_temp)){
      if(counter3 <= 2){
        theData[i,"earnings"] = earnings_temp
        counter3 = counter3 + 1
      } else {
        earnings_temp = NA
        counter3 = 1
      }
    }
  }
}

theData$time = as.numeric(row.names(theData))

#Filter data so that the ending cutoff has non-missing data and past point of revisions... a quarter
theData = theData %>% filter(time < (max(theData$time)-3))

#Build inflation percentage variables
cpi_1m = theData %>% select(time,cpi) %>% 
                     left_join(theData %>% select(time,cpi) %>% mutate(time = time + 1) %>% rename(pcpi=cpi),by="time") %>%
                     mutate(cpi_1m = (cpi - pcpi)/pcpi) %>% select(time,cpi_1m)

cpi_3m = theData %>% select(time,cpi) %>% 
                     left_join(theData %>% select(time,cpi) %>% mutate(time = time + 3) %>% rename(pcpi=cpi),by="time") %>%
                     mutate(cpi_3m = (cpi - pcpi)/pcpi) %>% select(time,cpi_3m)

cpi_6m = theData %>% select(time,cpi) %>% 
                     left_join(theData %>% select(time,cpi) %>% mutate(time = time + 6) %>% rename(pcpi=cpi),by="time") %>%
                     mutate(cpi_6m = (cpi - pcpi)/pcpi) %>% select(time,cpi_6m)

cpi_12m = theData %>% select(time,cpi) %>% 
                      left_join(theData %>% select(time,cpi) %>% mutate(time = time + 12) %>% rename(pcpi=cpi),by="time") %>%
                      mutate(cpi_12m = (cpi - pcpi)/pcpi) %>% select(time,cpi_12m)
  
cpi_core_1m = theData %>% select(time,cpi_core) %>% 
                          left_join(theData %>% select(time,cpi_core) %>% mutate(time = time + 1) %>% rename(pcpi_core=cpi_core),by="time") %>%
                          mutate(cpi_core_1m = (cpi_core - pcpi_core)/pcpi_core) %>% select(time,cpi_core_1m)

cpi_core_3m = theData %>% select(time,cpi_core) %>% 
                          left_join(theData %>% select(time,cpi_core) %>% mutate(time = time + 3) %>% rename(pcpi_core=cpi_core),by="time") %>%
                          mutate(cpi_core_3m = (cpi_core - pcpi_core)/pcpi_core) %>% select(time,cpi_core_3m)

cpi_core_6m = theData %>% select(time,cpi_core) %>% 
                          left_join(theData %>% select(time,cpi_core) %>% mutate(time = time + 6) %>% rename(pcpi_core=cpi_core),by="time") %>%
                          mutate(cpi_core_6m = (cpi_core - pcpi_core)/pcpi_core) %>% select(time,cpi_core_6m)

cpi_core_12m = theData %>% select(time,cpi_core) %>% 
                           left_join(theData %>% select(time,cpi_core) %>% mutate(time = time + 12) %>% rename(pcpi_core=cpi_core),by="time") %>%
                           mutate(cpi_core_12m = (cpi_core - pcpi_core)/pcpi_core) %>% select(time,cpi_core_12m)
  
pce_1m = theData %>% select(time,pce) %>% 
                     left_join(theData %>% select(time,pce) %>% mutate(time = time + 1) %>% rename(ppce=pce),by="time") %>%
                     mutate(pce_1m = (pce - ppce)/ppce) %>% select(time,pce_1m)

pce_3m = theData %>% select(time,pce) %>% 
                     left_join(theData %>% select(time,pce) %>% mutate(time = time + 3) %>% rename(ppce=pce),by="time") %>%
                     mutate(pce_3m = (pce - ppce)/ppce) %>% select(time,pce_3m)

pce_6m = theData %>% select(time,pce) %>% 
                     left_join(theData %>% select(time,pce) %>% mutate(time = time + 6) %>% rename(ppce=pce),by="time") %>%
                     mutate(pce_6m = (pce - ppce)/ppce) %>% select(time,pce_6m)

pce_12m = theData %>% select(time,pce) %>% 
                      left_join(theData %>% select(time,pce) %>% mutate(time = time + 12) %>% rename(ppce=pce),by="time") %>%
                      mutate(pce_12m = (pce - ppce)/ppce) %>% select(time,pce_12m)

pce_core_1m = theData %>% select(time,pce_core) %>% 
                          left_join(theData %>% select(time,pce_core) %>% mutate(time = time + 1) %>% rename(ppce_core=pce_core),by="time") %>%
                          mutate(pce_core_1m = (pce_core - ppce_core)/ppce_core) %>% select(time,pce_core_1m)

pce_core_3m = theData %>% select(time,pce_core) %>% 
                          left_join(theData %>% select(time,pce_core) %>% mutate(time = time + 3) %>% rename(ppce_core=pce_core),by="time") %>%
                          mutate(pce_core_3m = (pce_core - ppce_core)/ppce_core) %>% select(time,pce_core_3m)

pce_core_6m = theData %>% select(time,pce_core) %>% 
                          left_join(theData %>% select(time,pce_core) %>% mutate(time = time + 6) %>% rename(ppce_core=pce_core),by="time") %>%
                          mutate(pce_core_6m = (pce_core - ppce_core)/ppce_core) %>% select(time,pce_core_6m)

pce_core_12m = theData %>% select(time,pce_core) %>% 
                           left_join(theData %>% select(time,pce_core) %>% mutate(time = time + 12) %>% rename(ppce_core=pce_core),by="time") %>%
                           mutate(pce_core_12m = (pce_core - ppce_core)/ppce_core) %>% select(time,pce_core_12m)

#Build new variables
model = theData %>%
            left_join(.,cpi_1m,by="time") %>%
            left_join(.,cpi_3m,by="time") %>%
            left_join(.,cpi_6m,by="time") %>%
            left_join(.,cpi_12m,by="time") %>%
            left_join(.,cpi_core_1m,by="time") %>%
            left_join(.,cpi_core_3m,by="time") %>%
            left_join(.,cpi_core_6m,by="time") %>%
            left_join(.,cpi_core_12m,by="time") %>%
            left_join(.,pce_1m,by="time") %>%
            left_join(.,pce_3m,by="time") %>%
            left_join(.,pce_6m,by="time") %>%
            left_join(.,pce_12m,by="time") %>%
            left_join(.,pce_core_1m,by="time") %>%
            left_join(.,pce_core_3m,by="time") %>%
            left_join(.,pce_core_6m,by="time") %>%
            left_join(.,pce_core_12m,by="time") %>%
            mutate(tres_30y_20y_spread = tres_30y - tres_20y,
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
                   m2_velocity = GDP / m2,
                   output_gap = (GDP - PGDP)/PGDP*100,
                   taylor_rule_cpi = 2 + 0.5*((GDP - PGDP)/PGDP*100) + 0.5*(cpi_12m*100 - 2),
                   taylor_rule_to_fed_funds_cpi = (2 + 0.5*((GDP - PGDP)/PGDP*100) + 0.5*(cpi_12m*100 - 2))-fed_funds,
                   taylor_rule_cpi_core = 2 + 0.5*((GDP - PGDP)/PGDP*100) + 0.5*(cpi_core_12m*100 - 2),
                   taylor_rule_to_fed_funds_cpi_core = (2 + 0.5*((GDP - PGDP)/PGDP*100) + 0.5*(cpi_core_12m*100 - 2))-fed_funds,
                   taylor_rule_pce = 2 + 0.5*((GDP - PGDP)/PGDP*100) + 0.5*(pce_12m*100 - 2),
                   taylor_rule_to_fed_funds_pce = (2 + 0.5*((GDP - PGDP)/PGDP*100) + 0.5*(pce_12m*100 - 2))-fed_funds,
                   taylor_rule_pce_core = 2 + 0.5*((GDP - PGDP)/PGDP*100) + 0.5*(pce_core_12m*100 - 2),
                   taylor_rule_to_fed_funds_pce_core = (2 + 0.5*((GDP - PGDP)/PGDP*100) + 0.5*(pce_core_12m*100 - 2))-fed_funds)

model = model %>%
          left_join(.,model %>% select(time,earnings,lfpr,m2,ppi,unemployment,fed_funds,GDP,PGDP,m2_velocity,output_gap,
                                       taylor_rule_to_fed_funds_cpi,taylor_rule_to_fed_funds_cpi_core,
                                       taylor_rule_to_fed_funds_pce,taylor_rule_to_fed_funds_pce_core) %>%
                                mutate(time = time + 3) %>% 
                                rename(earnings3 = earnings,lfpr3 = lfpr,m23 = m2,ppi3 = ppi,unemployment3 = unemployment,
                                       fed_funds3 = fed_funds,GDP3 = GDP, PGDP3 = PGDP,m2_velocity3 = m2_velocity,
                                       output_gap3 = output_gap, 
                                       taylor_rule_to_fed_funds_cpi3 = taylor_rule_to_fed_funds_cpi,
                                       taylor_rule_to_fed_funds_cpi_core3 = taylor_rule_to_fed_funds_cpi_core,
                                       taylor_rule_to_fed_funds_pce3 = taylor_rule_to_fed_funds_pce,
                                       taylor_rule_to_fed_funds_pce_core3 = taylor_rule_to_fed_funds_pce_core),
                    by="time")

filter(time >= 78) %>% #start of 2-year treasury bill