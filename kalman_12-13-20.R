
library(knitr)
library(tidyverse)
library(rstan)
library(parallel)
library(pscl)
library(kableExtra)
library(shinystan)
library(ggcorrplot)

fed = read_csv("fed_agency_capacity_autonomy.csv")

# Model for reference
# capacity =~ mission_pct + mission_pct^2 + LOSavg + AIN_pct + LST_pct + med_sal_ + gini
# Clean up the data a bit, get rid of agencies with med salary = 0 and employees less than 5

set.seed(9876)
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

years = 1980:1984

setagency = sample(unique(df$AGYSUB), size = 50)

regressors = c("logn", "med_sal_", "LOSavg", "logMA_pct", "logAIN_pct", "logLST_pct", "gini")

dff = df %>%
  filter(yr %in% years) %>%
  filter(AGYSUB %in% setagency) %>%
  select(regressors, yr, AGYSUB, agy_typ) %>%
  mutate_at(regressors, scale) %>%
  mutate(AGYSUB = as.factor(AGYSUB)) %>%
  mutate(yr = as.factor(yr)) %>%
  drop_na()

dff = dff %>%
  mutate(year = as.integer(as.factor(yr))) %>%
  group_by(AGYSUB) %>%
  mutate(minyear = min(year)) %>%
  mutate(maxyear = max(year)) %>%
  ungroup()

dff = dff %>%
  arrange(AGYSUB)

dff = dff %>%
  group_by(AGYSUB) %>%
  mutate(nobs = n()) %>%
  ungroup()

dff = dff %>%
  mutate(missingyear = ifelse(nobs != maxyear - minyear + 1, 1, 0)) %>%
  filter(missingyear == 0) ### Removes 16 agencies 

dff = dff %>%
  complete(nesting(AGYSUB), year) %>%
  mutate(agency = as.integer(AGYSUB))

z = array(0, dim=c(length(unique(dff$AGYSUB)), length(years), length(regressors))) ### array[agency, time, indicator]

for (i in 1:length(unique(dff$AGYSUB))) {
  for (t in 1:length(years)) {
    for (j in 1:length(regressors)) {
      
      value = as.double(dff %>% filter(agency == i, year == t) %>% select(regressors[j]))
      z[i, t, j] = value
      
    }
    
  }
  
}

z[is.na(z)] = 0

cap.data = list(M = length(unique(dff$AGYSUB)), Time = length(years), z = z)


cap.data$N = nrow(cap.data$y)

cap.data$J = length(regressors)

cap.data$start_time = dff %>%
  group_by(AGYSUB) %>%
  drop_na() %>%
  slice(1) %>%
  ungroup() %>%
  select(minyear)

cap.data$start_time = cap.data$start_time$minyear

cap.data$end_time = dff %>%
  group_by(AGYSUB) %>%
  drop_na() %>%
  slice(1) %>%
  ungroup() %>%
  select(maxyear)

cap.data$end_time = cap.data$end_time$maxyear

options(mc.cores = detectCores())
kalman.test = stan(file = "kalman.stan", data = cap.data, iter = 10000)

check=as.data.frame(summary(kalman.test)$summary)
check$rowname = rownames(check)
check2 = check %>%
  filter(str_detect(rowname, "x_sam"))
check2 = check2[, c(1,4, 6, 8, 9, 10)]
check2$agency = rep(1:cap.data$M, each = cap.data$Time)
check2$time = rep(1:cap.data$Time, cap.data$M)

dff = dff %>%
  mutate(agency = as.integer(AGYSUB)) %>%
  mutate(time = as.integer(yr))

dfff = left_join(dff, check2) 
dfff = dfff %>%
  rename(Ave_Capacity = mean, lower = "2.5%", upper = "97.5%")

dfff %>%
  ggplot(aes(x = time, y = Ave_Capacity)) +
  geom_line(aes(color = agy_typ, group = AGYSUB), size = 1.4, alpha = .6) +
  facet_wrap(~agy_typ) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  ylab("Capacity") +
  xlab("Time") +
  theme_minimal()
