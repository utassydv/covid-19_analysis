#######################
##   Analysis of     ##
##    Number of      ##
##  registered death ##
##    per capita     ##
##      AND          ##
##    Number of      ##
##  registered case  ##
##    per capita     ##
##                   ##
## Analysis of       ##
#       the data     ##
##                   ##
#######################

# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages("ggthemes")
library(ggthemes)

# Call the data from github
my_url <- "https://raw.githubusercontent.com/utassydv/covid-19_analysis/main/data/clean/covid_pop_11-05-2020_clean.csv"
df <- read_csv( my_url )


####
# 
# Quick check on all HISTOGRAMS
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()
# all the distributions look lognormal for the first blink, skewed with a right tail
summary( df )
# the summary shows this as well
#It can be seen, that we might face some issue with the number of death, as there are some values with 0.
#It will be an issue when we are taking the ln of it, as it will give us -Inf
df %>% filter( death == 0 )
#As we can see, these are usually areas where 
#- not really affected by covid-19 (according to the numbers), or 
#- places with small population or



######
# Create new variables: Total death per capita, registered cases per capita according to my assigment description
df <- df %>% mutate( death_per_capita = death/(population/1000000) )
df <- df %>% mutate( confirmed_per_capita = confirmed/(population/1000000) )


# Take Log of confirmed_per_capita and death_per_capita
df<- df %>% mutate( ln_death_per_capita = log( death_per_capita ),
                     ln_confirmed_per_capita = log( confirmed_per_capita ) )

# To check what happens if we replace -Inf values with a realy small number
df_replaced <- df %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), -100, x))

# On the main thread I will work with the dataframe in which countries with zero death are neglected 
# Countries with zero deaths can be neglected, as we can say, that they do not belong to the question, of this analyses, as they have no
# registered deaths from covid-19. However, of course some can argue with this statement. 
df <- df %>% filter( ln_death_per_capita != -Inf )

#To plot only meaningful variables at this point
df_to_plot <- df %>% select( -c(active, confirmed, death, population, recovered))

#checking the distribution of our x and y variable, it can be easy seen, that the ln transformed variables have a more symetric distribution
df_to_plot %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()
# all the distributions look lognormal for the first blink, skewed with a right tail
summary( df_to_plot )



# Where to use log-transformation? - level-level vs level-log vs log-level vs log-log
#

# The my task is to use Number of registered death per capita and Number of registered case per capita
# The question we can answer with the following scatterplots is where to use log-transformation? (level-level vs level-log vs log-level vs log-log)
# According to the x and y variable distributions, we should get the best result with log-log

# level-level
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case per 1 million capita",y = "Number of registered death per 1 million capita") 

# log-level
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case per 1 million capita",y = "Number of registered death per 1 million capita (ln scale)")  +
  scale_y_continuous( trans = log_trans() )

# level-log
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case per 1 million capita (ln scale)",y = "Number of registered death per 1 million capita")  +
  scale_x_continuous( trans = log_trans() )

# log-log
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case per 1 million capita (ln scale)",y = "Number of registered death per 1 million capita (ln scale)")  +
  scale_y_continuous( trans = log_trans() ) +
  scale_x_continuous( trans = log_trans() )

####
# Conclusions:
# taking log of both axes is making the association close to linear!
# For this reason for now on I will use the ln_confirmed_per_capita and ln_death_per_capita variables as x and y variables

############################################################################################################################################
############################################################################################################################################

######
# Make some models w ln_confirmed_per_capita and ln_death_per_capita
#     reg1: ln_death_per_capita = alpha + beta * ln_confirmed_per_capita
#     reg2: ln_death_per_capita = alpha + beta_1 * ln_confirmed_per_capita + beta_2 * ln_confirmed_per_capita^2
#
#     reg3: ln_death_per_capita = alpha + beta_1 * ln_confirmed_per_capita * 1(confirmed_per_capita < 1.0e-03) + beta_2 * ln_confirmed_per_capita * 1(confirmed_per_capita >= 1.0e-03)
#       #there is really no point to do this, as it is so linear, but that was the task...
#     reg4: ln_death_per_capita = alpha + beta * ln_confirmed_per_capita, weights: population

###
# Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( ln_confirmed_per_capita_sq = ln_confirmed_per_capita^2)



# Do the regressions

# First model:
reg1 <- lm_robust( ln_death_per_capita ~ ln_confirmed_per_capita , data = df , se_type = "HC2" )
summary( reg1 )
# Visual inspection:
ggplot( data = df, aes( x = ln_confirmed_per_capita, y = ln_death_per_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# Second model:
reg2 <- lm_robust( ln_death_per_capita ~ ln_confirmed_per_capita + ln_confirmed_per_capita_sq , data = df )
summary( reg2 )
ggplot( data = df, aes( x = ln_confirmed_per_capita, y = ln_death_per_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

# Third model:
cutoff <- 3000
cutoff_ln<- log( cutoff )

# Use simple regression with the lspline function
reg3 <- lm_robust(ln_death_per_capita ~ lspline( ln_confirmed_per_capita , cutoff_ln ), data = df )
summary( reg2 )
ggplot( data = df, aes( x = ln_confirmed_per_capita, y = ln_death_per_capita ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )


# Fourth model:
# Weighted-OLS: use reg1 setup and weight with population
reg4 <- lm_robust( ln_death_per_capita ~ ln_confirmed_per_capita , data = df , weights = population )
summary( reg4 )

ggplot(data = df, aes(x = ln_confirmed_per_capita, y = ln_death_per_capita)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red')+
  scale_size(range = c(1, 15))


#####
# Creating model summary with texreg
data_out <- "/Users/utassydv/Documents/workspaces/CEU/my_repos/covid-19_analysis/out"
htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.model.names = c("linear","quadratic","PLS", "weighted linear"),
         caption = "Modelling death per capita and number of confirmed cases per capita of countries",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)

######
# Based on model comparison our chosen model is reg1:
#     reg1: ln_death_per_capita = alpha + beta * ln_confirmed_per_capita
#   Substantive: - log-log interpretation works properly for countries
#                - magnitude of coefficients are meaningful
#   Statistical: - simple model, easy to interpret
#                - Comparatively high R2 and captures variation well
#   The next model, which is fairly good also, is the reg1 based model with population weight.
#   by that, we are taking greater countries into account with a bigger weight, however, as we are allready using per capita measures,
#   we don not need to take this into account again

#################################
## Testing hypothesis

# H0: beta=0
# HA: beta!=0
summary( reg1 )
#Multiple R-squared:  0.8216 ,	Adjusted R-squared:  0.8205 
#F-statistic: 564.9 on 1 and 168 DF,  p-value: < 2.2e-16
#p-value is almost zero which is <<< then 0.05 -> we can reject H0

#The same result in a nother way:
library(car)
res <- linearHypothesis( reg1 , "ln_confirmed_per_capita = 0")


######
# Residual analysis.


# Get the predicted y values from the model
df$reg1_y_pred <- reg1$fitted.values
# Calculate the errors of the model
df$reg1_res <- df$ln_death_per_capita - df$reg1_y_pred 

# Finding the best countries
# Find countries with largest negative errors
df %>% top_n( -5 , reg1_res ) %>% 
  select( country , ln_death_per_capita , reg1_y_pred , reg1_res )

#Finding the worst countries
# Find countries with largest positive errors
df %>% top_n( 5 , reg1_res ) %>% 
  select( country , ln_death_per_capita , reg1_y_pred , reg1_res )

