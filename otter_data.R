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
library(dplyr)
library(tidyr)
library(readr)
# library(ggplot2)
# library(plotly)
# library(purrr)
# library(magrittr)

options(dplyr.print_max = 1e9)

# Source files and objects
#source("simpleLRP_FUN.R")


# Data ----

## camera activity data----
## I wanted to avoid bringing this in as a csv to avoid the extra step but read_excel is not bringing the dates in correctly.  It seems to be doing the old excel thing with dates and I spent a LOT of time trying to fix it.  Bascially, its bringing in some as character and some as numbers - none of my usual tricks worked - abandon and bring in csv
# df_act <- read_excel("data/camera_activity_5-11-2014.xlsx")

df_act <- readr::read_csv("data/camera_activity_5-11-2014.csv")
str(df_act)
df_act$date <- strptime(df_act$date, format = "%m/%d/%Y")

# most of the data are date/time/site; the rest is temp and otters

unique(df_act$season) # not sure if this needs to be changed
# deployment_date and retrieval date are in a terrible format and don't quite match with date.  There is also a doy, day of study, month, timeofday (h:mm:ss) time.of.day (? PosicX??), hour, year, day.night.
# hours and days seem to be how long the camera was out

unique(df_act$year) # 2012, 2013, 2014

# site has a number and name
unique(df_act$site_name)
df_act$site_name <- as.factor(df_act$site_name)

summary(df_act)


# fix and convert months
unique(df_act$month)
class(df_act$month)
df_act$month[df_act$month == "Feburary"] <- "February"

str(df_act)
# can't figure out how to convert month to a data - use backdoor by extracting from date
df_act$month_test <- month(df_act$date, label = T)

barplot(prop.table(table(df_act$month_test)))

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
df_latT <- rename(df_latT, 
                  site.name.original = site.name.origional)
df_latT$dforagearea[df_latT$dforagearea == "N/A"] <- "NA"
df_latT$dforagearea <- as.numeric(df_latT$dforagearea)

df_latT <- df_latT %>%
  mutate_at(c("droad", "dcabin", "treeheight", "dlogging", "dstreammouth", "dfreshwater", "dforagearea"), as.numeric) 

df_latT$site.location <- as.factor(df_latT$site.location)

df_latT <- df_latT |>
  mutate(lat.pres = if_else(latrine.present == "Y", 1, 0))
str(df_latT)
#View(df_latT)
write.csv(df_latT, "data_derived/latrine_landscape.csv")

# make data for plotting violin plots
## but eliminate dlogging because all values for TN are 0
df_latPlot <- pivot_longer(df_latT[c(2:3, 6:7, 9:11)], cols = c(1:5))
str(df_latPlot)
head(df_latPlot)
df_latPlot$latrine.present <- factor(df_latPlot$latrine.present)
df_latPlot$name <- factor(df_latPlot$name)




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



# Lat-long ----
## TN-random ----
df_way_tnr <- read.csv("../original_Data/Slide Locations/park_random_sites.csv")
str(df_way_tnr)


length(df_way_tnr$name)
nrow(df_latT |> filter(site.location == "Terra Nova" & latrine.present == "N"))

df_latT[duplicated(df_latT$site.name) == "TRUE",]
df_way_tnr[duplicated(df_way_tnr$name) == "TRUE",]
df_way_tnr[df_way_tnr$name == "TNR26" | df_way_tnr$name == "TNR153", ]


# after looking at Google EArth, I suspect that the first TNR26 in df_way_tnr, i.e,. the western most is the right one and the second one (eastern most) is actually TNR27 as there is none in the kmz file but is in the spreadsheet.  I suspect that TNR153 is a duplicate iwth the second in teh spreadsheet (western most) being the right one as it is about equidistant from TNR152 and TNR 153 while the first TNR153 (eastern most) is very near TNR152.  All Lat longs match for these points.  So, rename the second TNR26 and eliminate TNR146.

df_way_tnr$name[35] <- "TNR27"
df_way_tnr <- df_way_tnr[-c(146),]
str(df_way_tnr)


#df_latT <- left_join(df_latT, df_way_tnr, by= c("site.name" = "name"))
str(df_latT)


## AB-random ----
df_way_ab <- read.csv("../original_Data/Slide Locations/AB_random_sites.csv")
str(df_way_ab)
str(df_latT)

length(df_way_ab$name)
nrow(df_latT |> filter(site.location == "Alexander Bay" & latrine.present == "N"))

# same length but SSR8 duplicated - no way to tell difference so eliminate
df_way_ab[duplicated(df_way_ab$name) == "TRUE",]
df_latT[df_latT$site.name.original == "SSR8", ]
df_way_ab[df_way_ab$name == "SSR8", ]

df_way_tnr <- df_way_tnr[-c(8,9),]

# df_latT has an SSR59 - unless everything has been misnumbered
# delete this point
anti_join(df_latT |> filter(site.location == "Alexander Bay" & latrine.present == "N"), df_way_ab, by = c("site.name.original" = "name"))
df_latT[df_latT$site.name.original == "SSR59",]
df_latT <- df_latT[-509,]


tmp_location <- bind_rows(df_way_tnr, df_way_ab)

df_latT <- left_join(df_latT, tmp_location, by= c("site.name.original" = "name"))

df_latT |>
  group_by(site.location, latrine.present) |>
  summarise(count = n(), na_count = sum(is.na(latitude)))


# END ----
# set up some binary data
# Pres <- c(rep(1, 40), rep(0, 40))
# rnor <- function(x) rnorm(1, mean = ifelse(x == 1, 12.5, 7.5), sd = 2)
# ExpVar <- sapply(Pres, rnor)
# 
# # linear model with binary data...
# lm(Pres ~ ExpVar)
# 
