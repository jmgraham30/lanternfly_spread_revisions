# Jason M Graham
# August 24, 2023

# load essential packages
library(tidyverse)
library(mgcv)
library(gratia)
library(DHARMa)

# set plotting theme
theme_set(theme_minimal(base_size = 12))

# load the data

## 2021 only data
data_1 <- read_csv("data/data_1.csv")

data_1 <- data_1 %>%
  mutate(pop_log = log10(pop),
         infested = i21)

glimpse(data_1)

## all years
data_2_temporal <- read_csv("data/data_2_temporal.csv")

data_2_temporal <- data_2_temporal %>%
  mutate(pop_log = log10(pop),
         year_zero = year - min(year))

glimpse(data_2_temporal)

## projections data

data_to_map <- read_csv("data/data_to_map.csv")

data_to_map_166 <- data_to_map %>%
  filter(fips %in% data_2_temporal$fips)

glimpse(data_to_map_166)

### Some exploratory stuff

data_1 %>%
  select(nis,nps,gc,pop_log,infested) %>%
  GGally::ggpairs()

data_2_temporal %>%
  select(nis,nps,gc,pop_log,infested) %>%
  GGally::ggpairs()

data_2_temporal %>%
  ggplot(aes(x=pop_log,y=gc)) + 
  geom_point() + 
  geom_smooth() + 
  geom_smooth(method = "lm")

dftm <- data_2_temporal %>%
  filter(year == 2021) %>%
  select(infested,nps,nis,gc,pop_log,latitude_num,longitude_num,fips) %>%
  right_join(.,data_to_map_166,by="fips")

glimpse(dftm)

dftm %>%
  ggplot() +
  geom_polygon(mapping = aes(long, lat, group=group,fill=factor(infested)),color="black",
               linewidth=0.2)  +
  scale_fill_discrete(name="Infested",type=c("white","lightgreen")) + 
  geom_point(aes(longitude_num,latitude_num,color=nps),size=2) + 
  scale_color_continuous(low = "white", 
                        high = "darkblue",
                        limits = c(0,5)) + 
  labs(x="",y="",title = "2021 Infestations per NPS")

dftm %>%
  ggplot() +
  geom_polygon(mapping = aes(long, lat, group=group,fill=factor(infested)),color="black",
               linewidth=0.2)  +
  scale_fill_discrete(name="Infested",type=c("white","lightgreen")) + 
  geom_point(aes(longitude_num,latitude_num,color=nis),size=2) + 
  scale_color_continuous(low = "white", 
                         high = "darkblue",
                         limits = c(0,3)) + 
  labs(x="",y="",title = "2021 Infestations per NIS")

dftm %>%
  ggplot() +
  geom_polygon(mapping = aes(long, lat, group=group,fill=factor(infested)),color="black",
               linewidth=0.2)  +
  scale_fill_discrete(name="Infested",type=c("white","lightgreen")) + 
  geom_point(aes(longitude_num,latitude_num,color=gc),size=2) + 
  scale_color_continuous(low = "white", 
                         high = "darkblue",
                         limits = c(0,41)) + 
  labs(x="",y="",title = "2021 Infestations per GC")

dftm %>%
  ggplot() +
  geom_polygon(mapping = aes(long, lat, group=group,fill=factor(infested)),color="black",
               linewidth=0.2)  +
  scale_fill_discrete(name="Infested",type=c("white","lightgreen")) + 
  geom_point(aes(longitude_num,latitude_num,color=pop_log),size=2) + 
  scale_color_continuous(low = "white", 
                         high = "darkblue",
                         limits = c(3,7)) + 
  labs(x="",y="",title = "2021 Infestations per Pop (log)")


### GAM modeling

# full model formulation for all years
full_model_formulation <- infested ~ nis + nps + std_gc + pop_log + 
  s(year_zero,k=8) + te(latitude_num,longitude_num)

full_model_gam <- gam(full_model_formulation,data=data_2_temporal,
                      family = binomial(),
                      method = "REML")

# DHARMa residuals
full_res <- simulateResiduals(full_model_gam)

plot(full_res) # residuals look good

# check for temporal autocorrelation
performance::check_autocorrelation(full_model_gam)

acf(residuals(full_model_gam))

# aggregating residuals by time
res_agg <- recalculateResiduals(full_res, group = data_2_temporal$year)

testTemporalAutocorrelation(res_agg,time=unique(data_2_temporal$year))

draw(full_model_gam)

# nested reduced models for comparison
model_formulation_no_nis <- infested ~ nps + std_gc + pop_log + 
  s(year_zero,k=8) + te(latitude_num,longitude_num)

no_nis_gam <- gam(model_formulation_no_nis,data=data_2_temporal,
                              family = binomial(),
                              method = "REML") 

model_formulation_no_nps <- infested ~  nis + std_gc + pop_log + 
  s(year_zero,k=8) + te(latitude_num,longitude_num)

no_nps_gam <- gam(model_formulation_no_nps,data=data_2_temporal,
                              family = binomial(),
                              method = "REML")

model_formulation_no_gc <- infested ~  nis + nps + pop_log +  
  s(year_zero,k=8) + te(latitude_num,longitude_num)

no_gc_gam <- gam(model_formulation_no_gc,data=data_2_temporal,
                             family = binomial(),
                             method = "REML")

model_formulation_no_pop <- infested ~ nis + nps + std_gc +   
  s(year_zero,k=8) + te(latitude_num,longitude_num)

no_pop_gam <- gam(model_formulation_no_pop,data=data_2_temporal,
                              family = binomial(),
                              method = "REML")


anova(no_nis_gam,full_model_gam,test = "Chisq")

anova(no_nps_gam,full_model_gam,test = "Chisq")

anova(no_gc_gam,full_model_gam,test = "Chisq")

anova(no_pop_gam,full_model_gam,test = "Chisq") # you definitely don't want to drop population


############## Ignoring national parks ##################


# full model formulation for all years
full_model_formulation <- infested ~ nis + std_gc + pop_log +
  s(year_zero,k=8) + te(latitude_num,longitude_num)

full_model_gam <- gam(full_model_formulation,data=data_2_temporal,
                      family = binomial(),
                      method = "REML")

# DHARMa residuals
full_res <- simulateResiduals(full_model_gam)

plot(full_res) # residuals look good

# check for temporal autocorrelation
performance::check_autocorrelation(full_model_gam)

acf(residuals(full_model_gam))

# aggregating residuals by time
res_agg <- recalculateResiduals(full_res, group = data_2_temporal$year)

testTemporalAutocorrelation(res_agg,time=unique(data_2_temporal$year))

draw(full_model_gam)

# nested reduced models for comparison
model_formulation_no_nis <- infested ~ std_gc + pop_log + 
  s(year_zero,k=8) + te(latitude_num,longitude_num)

no_nis_gam <- gam(model_formulation_no_nis,data=data_2_temporal,
                  family = binomial(),
                  method = "REML") 

model_formulation_no_gc <- infested ~  nis + pop_log +  
  s(year_zero,k=8) + te(latitude_num,longitude_num)

no_gc_gam <- gam(model_formulation_no_gc,data=data_2_temporal,
                 family = binomial(),
                 method = "REML")

model_formulation_no_pop <- infested ~ nis + std_gc +   
  s(year_zero,k=8) + te(latitude_num,longitude_num)

no_pop_gam <- gam(model_formulation_no_pop,data=data_2_temporal,
                  family = binomial(),
                  method = "REML")


anova(no_nis_gam,full_model_gam,test = "Chisq")

anova(no_gc_gam,full_model_gam,test = "Chisq")

anova(no_pop_gam,full_model_gam,test = "Chisq") # you definitely don't want to drop population



## for 2021 only

full_model_formulation_21 <- infested ~ nis + nps + std_gc + pop_log + 
   te(latitude,longitude)

full_model_gam_21 <- gam(full_model_formulation_21,data=data_1,
                      family = binomial(),
                      method = "REML")

# DHARMa residuals
full_res <- simulateResiduals(full_model_gam_21)

plot(full_res) # residuals look good

# check for temporal autocorrelation
performance::check_autocorrelation(full_model_gam_21)

acf(residuals(full_model_gam_21))

draw(full_model_gam)

# nested reduced models for comparison
model_formulation_no_nis_21 <- infested ~ nps + std_gc + pop_log + 
  te(latitude,longitude)

no_nis_gam_21 <- gam(model_formulation_no_nis_21,data=data_1,
                  family = binomial(),
                  method = "REML") 

model_formulation_no_nps_21 <- infested ~  nis + std_gc + pop_log + 
  te(latitude,longitude)

no_nps_gam_21 <- gam(model_formulation_no_nps_21,data=data_1,
                  family = binomial(),
                  method = "REML")

model_formulation_no_gc_21 <- infested ~  nis + nps + pop_log +  
  te(latitude,longitude)

no_gc_gam_21 <- gam(model_formulation_no_gc_21,data=data_1,
                 family = binomial(),
                 method = "REML")

model_formulation_no_pop_21 <- infested ~ nis + nps + std_gc +   
  te(latitude,longitude)

no_pop_gam_21 <- gam(model_formulation_no_pop_21,data=data_1,
                  family = binomial(),
                  method = "REML")


anova(no_nis_gam_21,full_model_gam_21,test = "Chisq")

anova(no_nps_gam_21,full_model_gam_21,test = "Chisq")

anova(no_gc_gam_21,full_model_gam_21,test = "Chisq")

anova(no_pop_gam_21,full_model_gam_21,test = "Chisq") # you definitely don't want to drop population


