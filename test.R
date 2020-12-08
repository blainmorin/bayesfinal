library(tidyverse)
library(rstan)
library(pscl)

fed = read_csv("fed_agency_capacity_autonomy.csv")

# Model for reference
# capacity =~ mission_pct + mission_pct^2 + LOSavg + AIN_pct + LST_pct + med_sal_ + gini
# Clean up the data a bit, get rid of agencies with med salary = 0 and employees less than 5
df = fed %>%
  filter(yr != 1973) %>%
  filter(yr >= 1974) %>%
  filter(yr < 2014) %>% 
  filter(med_sal_ > 0) %>% 
  filter(n > 5) %>%
  drop_na(med_sal_) %>%
  filter(AGYSUB != "TOTL")%>%
  filter(!grepl("NET", agy_full)) %>%
  mutate(logMA_pct = log(ma_pct + 1)) %>%
  mutate(logLST_pct = log(LST_pct + 1)) %>%
  mutate(logAIN_pct = log(AIN_pct + 1)) %>%
  mutate(logPHDpct = log(doc_pct + 1)) %>%
  mutate(logn = log(n)) 


df = df %>%
  filter(AGYSUB %in% c("AG00", "SB00", "DJ06", "DN00", "ED00", "EP00"))

agencies = c("AG00", "SB00", "DJ06", "DN00", "ED00", "EP00")
years = c(1980, 1985, 1990)

regressors = c("logMA_pct",
               "logn",
               "med_sal_",
               "LOSavg")

dff = df %>%
  filter(yr %in% years) %>%
  select(regressors, yr, AGYSUB, agy_typ) %>%
  mutate_at(regressors, scale) %>%
  mutate(AGYSUB = as.factor(AGYSUB)) %>%
  mutate(yr = as.factor(yr))




cap.data = list(M = length(agencies), J = length(years), y = dff$logn)

cap.data$ind_justice = rep(1:6, each = 3)

cap.data$ind_case = rep(1:3, 6)

cap.data$y = as.vector(dff$logn)

cap.data$N = length(cap.data$y)

test = stan(file = "cap.stan", data = cap.data)
summary(test)$summary

df = fed %>%
  filter(yr != 1973) %>%
  filter(yr >= 1974) %>%
  filter(yr < 2014) %>% 
  filter(med_sal_ > 0) %>% 
  filter(n > 5) %>%
  drop_na(med_sal_) %>%
  filter(AGYSUB != "TOTL")%>%
  filter(!grepl("NET", agy_full)) %>%
  mutate(logMA_pct = log(ma_pct + 1)) %>%
  mutate(logLST_pct = log(LST_pct + 1)) %>%
  mutate(logAIN_pct = log(AIN_pct + 1)) %>%
  mutate(logPHDpct = log(doc_pct + 1)) %>%
  mutate(logn = log(n)) 


df = df %>%
  filter(AGYSUB %in% c("AG00", "SB00", "DJ06", "DN00", "ED00", "EP00"))

agencies = c("AG00", "SB00", "DJ06", "DN00", "ED00", "EP00")
years = c(1980, 1985, 1990)

regressors = c("logMA_pct",
               "logn",
               "med_sal_",
               "LOSavg")

dff = df %>%
  filter(yr %in% years) %>%
  select(regressors, yr, AGYSUB, agy_typ) %>%
  mutate_at(regressors, scale) %>%
  mutate(AGYSUB = as.factor(AGYSUB)) %>%
  mutate(yr = as.factor(yr))




cap.data = list(M = length(agencies), J = length(years), y = dff$logn)

cap.data$ind_justice = rep(1:6, each = 3)

cap.data$ind_case = rep(1:3, 6)

cap.data$y = as.vector(dff$logn)

cap.data$N = length(cap.data$y)

test = stan(file = "cap.stan", data = cap.data)
summary(test)$summary

#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################

library(tidyverse)
library(rstan)
library(pscl)

fed = read_csv("fed_agency_capacity_autonomy.csv")

# Model for reference
# capacity =~ mission_pct + mission_pct^2 + LOSavg + AIN_pct + LST_pct + med_sal_ + gini
# Clean up the data a bit, get rid of agencies with med salary = 0 and employees less than 5
df = fed %>%
  filter(yr != 1973) %>%
  filter(yr >= 1974) %>%
  filter(yr < 2014) %>% 
  filter(med_sal_ > 0) %>% 
  filter(n > 5) %>%
  drop_na(med_sal_) %>%
  filter(AGYSUB != "TOTL")%>%
  filter(!grepl("NET", agy_full)) %>%
  mutate(logMA_pct = log(ma_pct + 1)) %>%
  mutate(logLST_pct = log(LST_pct + 1)) %>%
  mutate(logAIN_pct = log(AIN_pct + 1)) %>%
  mutate(logPHDpct = log(doc_pct + 1)) %>%
  mutate(logn = log(n)) 

regressors = c("logMA_pct",
               "logn",
               "med_sal_",
               "LOSavg")

agencies = length(unique(dff$AGYSUB))
years = 1980:1989

dff = df %>%
  filter(yr %in% years) %>%
  select(regressors, yr, AGYSUB, agy_typ) %>%
  mutate_at(regressors, scale) %>%
  mutate(AGYSUB = as.factor(AGYSUB)) %>%
  mutate(yr = as.factor(yr))




cap.data = list(M = agencies, J = length(years), y = as.matrix(dff[,c("logn", "med_sal_")]))

cap.data$ind_justice = as.integer(dff$AGYSUB)

cap.data$ind_case = as.integer(dff$yr)

cap.data$N = nrow(cap.data$y)

test = stan(file = "cap.stan", data = cap.data)
summary(test)$summary

df = fed %>%
  filter(yr != 1973) %>%
  filter(yr >= 1974) %>%
  filter(yr < 2014) %>% 
  filter(med_sal_ > 0) %>% 
  filter(n > 5) %>%
  drop_na(med_sal_) %>%
  filter(AGYSUB != "TOTL")%>%
  filter(!grepl("NET", agy_full)) %>%
  mutate(logMA_pct = log(ma_pct + 1)) %>%
  mutate(logLST_pct = log(LST_pct + 1)) %>%
  mutate(logAIN_pct = log(AIN_pct + 1)) %>%
  mutate(logPHDpct = log(doc_pct + 1)) %>%
  mutate(logn = log(n)) 


df = df %>%
  filter(AGYSUB %in% c("AG00", "SB00", "DJ06", "DN00", "ED00", "EP00"))

agencies = c("AG00", "SB00", "DJ06", "DN00", "ED00", "EP00")
years = c(1980, 1985, 1990)

regressors = c("logMA_pct",
               "logn",
               "med_sal_",
               "LOSavg")

dff = df %>%
  filter(yr %in% years) %>%
  select(regressors, yr, AGYSUB, agy_typ) %>%
  mutate_at(regressors, scale) %>%
  mutate(AGYSUB = as.factor(AGYSUB)) %>%
  mutate(yr = as.factor(yr))