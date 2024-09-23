# The purpose of this file is import and examine otter activity data from Terra Nova National Park (TNNP) and the Glovertown Newfoundland area.  Data provided by David Cote, 2024-09-23.

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
library(readr)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(plotly)
# library(purrr)
# library(magrittr)

options(dplyr.print_max = 1e9)

# Source files and objects
#source("simpleLRP_FUN.R")
