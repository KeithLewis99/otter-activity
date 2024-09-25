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
# library(dplyr)
# library(tidyr)
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

# temperature appears to be in F and C

# Otters are detected and number
plot(density(df_act$numberofOtter))


## latrine distributions ----
df_lat <- read_excel("data/latrine_distributions_29-7-2014.xlsx", sheet = "original")
str(df_lat)

## camera functioning ----
df_cam <- read_excel("data/Camera_functioning.xlsx")
str(df_cam)
