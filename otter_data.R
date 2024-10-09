# The purpose of this file is import and examine otter activity data from Terra Nova National Park (TNNP) and the Glovertown Newfoundland area.  Data provided by David Cote, 2024-09-23, from Chelsey Lawrence's thesis.

# see notes from 2024-09-20 (notebook) for questions

# Set up a project - see the below link for directions.
#https://happygitwithr.com/rstudio-git-github.html

# But basically:
# 1.	Set up a Git repo on GitHub.
# 2.	Create the project in R - New Project - VErsion Control - Git
# 3. type "git add -A" in the terminal
# 4.	Create a bunch of directories automatically (see below)
# 5. Copy git -ignore file

#Create a "name_dat.R" file
#put this file in the folder with the project and create the following subfolders
if(!dir.exists("archive"))dir.create("archive")
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("data_derived"))dir.create("data_derived")
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms") # manuscript
if(!dir.exists("report"))dir.create("report") #for rmd report
if(!dir.exists("refs"))dir.create("refs") #for rmd report


# Start----
#libraries
#library(readr)
library(readxl)
library(lubridate)
library(magrittr)
# library(dplyr)
#library(tidyr)
# library(ggplot2)
# library(plotly)
# library(purrr)
# library(magrittr)

options(dplyr.print_max = 1e9)

# Source files and objects
#source("simpleLRP_FUN.R")


# Data ----

## camera activity data----
## 
df_act <- read_excel("data/camera_activity_5-11-2014.xlsx")
str(df_act)

# most of the data are date/time/site; the rest is temp and otters

unique(df_act$season) # not sure if this needs to be changed
# deployment_date and retrieval date are in a terrible format and don't quite match with date.  There is also a doy, day of study, month, timeofday (h:mm:ss) time.of.day (? PosicX??), hour, year, day.night.
# hours and days seem to be how long the camera was out

unique(df_act$year) # 2012, 2013, 2014

# site has a number and name
unique(df_act$site_name)
df_act$site_name <- as.factor(df_act$site_name)

summary(df_act)

unique(df_act$month)


df_act$month[df_act$month == "Feburary"] <- "February"

unique(df_act$month)
barplot(prop.table(table(df_act$month)))
# probably want to order this by Month so change with lubridate I think- 

# no records of doy or day of study from 11169 onwards 
plot(density(df_act$doy, na.rm = T))
df_act[is.na(df_act$doy), 10]

# temperature appears to be in F and C


# Otters are detected and number
plot(density(df_act$numberofOtter))


## latrine distributions ----
### See ReadMe for explanation - had to pivot from wide to long
### see Lawrence thesis, Table 2.1 for metadata
df_lat <- read_excel("data/latrine_distributions_29-7-2014.xlsx", sheet = "original")
df_lat <- df_lat[-9, ] # remove blank row

# change from wide to long format
df_latT <- setNames(as.data.frame(t(df_lat[-1])), df_lat[[1]]) |> tibble::rownames_to_column(var = "site.name")
head(df_latT)
str(df_latT)

# change data types
unique(df_latT$site.name)
unique(df_latT$latrine.present)
unique(df_latT$site.location)
plot(density(as.numeric(df_latT$droad)))


library(dplyr)
df_latT <- df_latT %>%
  mutate_at(c("droad", "dcabin", "treeheight", "dlogging", "dstreammouth", "dfreshwater", "dforagearea"), as.numeric) 

df_latT$site.location <- as.factor(df_latT$site.location)

df_latT <- df_latT |>
  mutate(lat.pres = if_else(latrine.present == "Y", 1, 0))
str(df_latT)
View(df_latT)

# this is pretty useless as it always has been 
m1 <- glm(lat.pres ~ droad + site.location + dlogging, 
          family = binomial(link = "logit"),
          data = df_latT)
summary(m1)
coef(m1)
fitted(m1)
predict(m1)
residuals(m1)
residuals(m1, type = "working")

# # pretty useless
# par(mfrow=c(2,2))
# plot(m1) 
# graphics.off()
# 
# plot(predict(m1), resid(m1, type = "working"))
# 
# #https://sscc.wisc.edu/sscc/pubs/RegDiag-R/logistic-regression.html
# # linearity
# library(ggplot2)
# df_latT |> 
#   mutate(comp_res = coef(m1)["droad"]*droad + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = droad, y = comp_res)) +
#   geom_point() +
#   geom_smooth(color = "red", method = "lm", linetype = 2, se = F) +
#   geom_smooth(se = F)
# 
# 
# m2 <- glm(lat.pres ~ I(droad^3) + site.location, 
#           family = binomial(link = "logit"),
#           data = df_latT)
# summary(m2)
# 
# 
# # this didn't work at all
# df_latT |> 
#   mutate(comp_res = coef(m2)["I(droad^3)"]*droad^3 + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = droad^3, y = comp_res)) +
#   geom_point() +
#   geom_smooth(color = "red", method = "lm", linetype = 2, se = F) +
#   geom_smooth(se = F)
# 
# df_latT |> 
#   mutate(comp_res = as.numeric(site.location) + residuals(m1, type = "working")) |> 
#   ggplot(aes(x = site.location, y = comp_res)) +
#   geom_point() +
#   geom_hline(yintercept = 0, color = "red") +
#   stat_summary(geom = "line", fun = mean, color = "blue", size = 1.5)




#https://cran.r-project.org/web/packages/regressinator/vignettes/logistic-regression-diagnostics.html
# multicollinearity

library(car)
vif(m1)


# outliers



#DHARMA
## https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

## diagnostics ----
library(DHARMa)
m1_simres <- simulateResiduals(m1)
# str(bt_den.glmm1_simres,1)
plot(m1_simres)
# The normality is great; homogeneity OK


### temporal independence
# No temporal independence as these are just use/non-use sites

### spatial independence
# join coordinates with data set - may not be required
#bt.np <- left_join(bt.np, coords, by = c("Station_new" = "Station"))
#str(bt.np)
#bt.np[,c(2, 4, 6, 7, 11:13, 17, 21, 22)]

# just coords for non-pool
# coords.np <- as.data.frame(coords[c(1:4, 7:8, 10, 12:13, 15:18) ,]) 
# nrow(coords.np)

# recalculate resids with stations as the grouping variable
bt_den.glmm1_simres_recalcSpace <- recalculateResiduals(bt_den.glmm1_simres, group = as.factor(bt.np$Station_new))
unique(bt.np$Station_new) # OK - there are only 13 values in this because this is no pools and there are 4 pools + 1 destroyed pool so 18-5=13.
#str(bt_den.glmm1_simres_recalcSpace)

testSpatialAutocorrelation(bt_den.glmm1_simres_recalcSpace, x = unique(bt.np$X), y = unique(bt.np$Y))

spatialAutoCorrBase_fun(bt.np, bt_den.glmm1_simres_recalcSpace)  
bt.np.density.all <- spatialData_join(bt.np.density.station[-4,], bt_den.glmm1_simres_recalcSpace, coords.np)

spatialAutoCorrGG_fun(bt.np.density.all)




## camera functioning ----
### gives camera and dates deployed with number days out and days captured which I think is how many days of data.  NA means a "camera not set up" but in some cases, its blank.  Card filled seems to happen when days_capture << days_out.  But camera malfuction can also cause this problem - see Cote about this.
# "camera didn’t trigger after" - not sure what this means?

# should filter out records based on camera malfunction and otehr criteria?
# do we need to extract date or is this in df_act?
df_cam <- read_excel("data/Camera_functioning.xlsx")
str(df_cam)
#View(df_cam)
unique(df_cam$year)
df_cam$days_out <- as.numeric(df_cam$days_out)
summary(df_cam$days_out)




# set up some binary data
Pres <- c(rep(1, 40), rep(0, 40))
rnor <- function(x) rnorm(1, mean = ifelse(x == 1, 12.5, 7.5), sd = 2)
ExpVar <- sapply(Pres, rnor)

# linear model with binary data...
lm(Pres ~ ExpVar)