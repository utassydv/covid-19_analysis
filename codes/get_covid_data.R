########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 1            ##
## Get the data       ##
########################
# Clear memory and call packages
rm(list=ls())
library(WDI)
library(tidyverse)

# Download COVID cross-sectional data
date <- '11-05-2020'
covid_url <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/',
            date,'.csv')
covid_raw <- read.csv(covid_url)

# Download population data for 2019
pop_raw <- WDI(indicator=c('SP.POP.TOTL'), 
                country="all", start=2019, end=2019)

# Save the raw files
my_path <- "/Users/utassydv/Documents/workspaces/CEU/my_repos/covid-19_analysis/"
# covid data
write_csv(covid_raw, paste0(my_path,'data/raw/covid_11-05-2020_raw.csv'))
# population data
write_csv(pop_raw, paste0(my_path,'data/raw/pop_WDI_2019.csv'))


