These codes are for analysis pattern of association between registered death per capita and registered cases per capita in countries.

get_covid_data.R - downloads the raw data from https://github.com/CSSEGISandData/COVID-19 and  from World Development Indicators maintained by World Bank and saves it to data/raw folder.

clean_covid_data.R - loads the raw data and clean them: create a tidy table where each observation is a country.

covid_data_analysis.R - loads the clean data and executes simple linear regressions with visual inspections and quantitative analysis. It chooses model and then analyse the residuals.

covid_data_analysis.Rmd - replicates what covid_data_analysis.R does, but creates an html/ pdf documentation out of the analyses created there.  The visualizations part is different in the two files, as the previous one focuses on convenient coding, whereas the focus in this code is on the pretty output. 