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

######
# Create new variables: Total death per capita, registered cases per capita
df <- df %>% mutate( death_per_capita = death/population )
df <- df %>% mutate( confirmed_per_capita = confirmed/population )


######
# Check basic scatter-plots!
#   Two competing models:
#     1) lifeexp = alpha + beta * gdptot
#     2) lifeexp = alpha + beta * gdppc
#
# Where to use log-transformation? - level-level vs level-log vs log-level vs log-log
#

# The my task is to use Number of registered death per capita and Number of registered case per capita
# The question we can answer with the following scatterplots is where to use log-transformation? (level-level vs level-log vs log-level vs log-log)

# level-level
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case per capita",y = "Number of registered death per capita") 

# log-level
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case per capita",y = "Number of registered death per capita (ln scale)")  +
  scale_y_continuous( trans = log_trans() )

# level-log
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case per capita (ln scale)",y = "Number of registered death per capita")  +
  scale_x_continuous( trans = log_trans() )


# log-log
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of registered case per capita (ln scale)",y = "Number of registered death per capita (ln scale)")  +
  scale_y_continuous( trans = log_trans() ) +
  scale_x_continuous( trans = log_trans() )

####
# Conclusions:
# taking log of both axes is making the association close to linear!


# Take Log of confirmed_per_capita and death_per_capita
df<- df %>% mutate( ln_death_per_capita = log( death_per_capita ),
                     ln_confirmed_per_capita = log( confirmed_per_capita ) )

############################################################################################################################################
############################################################################################################################################
# TODO: might drop 0 death countries
# TODO: all vallues are under 1, ln makes them negative? is it a problem?
######
# Make some models:
#   w ln_gdptot:
#     reg1: lifeexp = alpha + beta * ln_gdptot
#     reg2: lifeexp = alpha + beta_1 * ln_gdptot + beta_2 * ln_gdptot^2
#     reg3: lifeexp = alpha + beta_1 * ln_gdptot + beta_2 * ln_gdptot^2 + beta_3 * ln_gdptot^3
#   w ln_gdppc:
#     reg4: lifeexp = alpha + beta * ln_gdppc
#     reg5: lifeexp = alpha + beta_1 * ln_gdppc + beta_2 * ln_gdppc^2
#     reg6: lifeexp = alpha + beta_1 * ln_gdppc * 1(gdppc < 50) + beta_2 * ln_gdppc * 1(gdppc >= 50)
#   Extra: weighted-ols:
#     reg7: lifeexp = alpha + beta * ln_gdppc, weights: population

###
# Two ways to handle polynomials: 
#
# 1) Add powers of the variable(s) to the dataframe:
df <- df %>% mutate( ln_gdptot_sq = ln_gdptot^2,
                     ln_gdptot_cb = ln_gdptot^3,
                     ln_gdppc_sq = ln_gdppc^2 )
#
# 2) Use 'poly(x,n)' function, which creates polynomials of x up to order n
#     use this approach for graphs! may use it for models: 
#                   positive - simpler, less new variables, 
#                   negative - uglier names, harder to compare
#     Note: poly() creates rotates your variables automatically to get mean independent variables
#       use raw = TRUE if you dont want to rotate your variables.

# Do the regressions
#
# Built in regression in R
reg_b <- lm( lifeexp ~ ln_gdptot , data = df )
reg_b
summary( reg_b )
# formula: y ~ x1 + x2 + ..., note: intercept is automatically added
# drawback: no robust SE, only homoskedastic SEs...
# So instead of lm we use lm_robust from package estimatr

# First model:
reg1 <- lm_robust( lifeexp ~ ln_gdptot , data = df , se_type = "HC2" )
reg1
# Summary statistics
summary( reg1 )
# Visual inspection:
ggplot( data = df, aes( x = ln_gdptot, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# Second and third model with gdptot
reg2 <- lm_robust( lifeexp ~ ln_gdptot + ln_gdptot_sq , data = df )
summary( reg2 )
ggplot( data = df, aes( x = ln_gdptot, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

reg3 <- lm_robust( lifeexp ~ ln_gdptot + ln_gdptot_sq + ln_gdptot_cb , data = df )
ggplot( data = df, aes( x = ln_gdptot, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )


# Models with gdp per capita
reg4 <- lm_robust( lifeexp ~ ln_gdppc , data = df )
summary( reg4 )
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

reg5 <- lm_robust( lifeexp ~ ln_gdppc + ln_gdppc_sq , data = df )
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

# Regression with piecewise linear spline:
# 1st define the cutoff for gdp per capita
cutoff <- 50
# 2nd we use a log transformation -> cutoff needs to be transformed as well
cutoff_ln<- log( cutoff )
# Use simple regression with the lspline function
?lspline
reg6 <- lm_robust(lifeexp ~ lspline( ln_gdppc , cutoff_ln ), data = df )
summary( reg6 )
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )


# Weighted-OLS: use reg4 setup and weight with population
reg7 <- lm_robust(lifeexp ~ ln_gdppc, data = df , weights = population)
summary( reg7 )

ggplot(data = df, aes(x = ln_gdppc, y = lifeexp)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red')+
  scale_size(range = c(1, 15)) +
  coord_cartesian(ylim = c(50, 85)) +
  labs(x = "ln(GDP per capita, thousand US dollars) ",y = "Life expectancy  (years)")+
  annotate("text", x = 4, y = 80, label = "USA", size=5)+
  annotate("text", x = 2.7, y = 79, label = "China", size=5)+
  annotate("text", x = 2,  y = 68, label = "India", size=5)


#####
# Creating model summary with texreg
data_out <- "Documents/Egyetem/CEU/Teaching_2020/Coding_with_R/git_coding_1/ECBS-5208-Coding-1-Business-Analytics/Class_8/out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 , reg5 , reg6 , reg7),
         type = 'html',
         custom.model.names = c("GDP total - linear","GDP total - quadratic","GDP total - cubic",
                                "GDP/capita - linear","GDP/capita - quadratic","GDP/capita - PLS",
                                "GDP/capita - weighted linear"),
         caption = "Modelling life expectancy and different wealth measures of countries",
         file = paste0( data_out ,'model_comparison.html'), include.ci = FALSE)

######
# Based on model comparison our chosen model is reg4 - lifeexp ~ ln_gdppc
#   Substantive: - level-log interpretation works properly for countries
#                - magnitude of coefficients are meaningful
#   Statistical: - simple model, easy to interpret
#                - Comparatively high R2 and captures variation well


######
# Residual analysis.

# lm_robust output is an `object` or `list` with different elements
# Check the `Value` section
?lm_robust

# Get the predicted y values from the model
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_res <- df$lifeexp - df$reg4_y_pred 

# Find countries with largest negative errors
df %>% top_n( -5 , reg4_res ) %>% 
  select( country , lifeexp , reg4_y_pred , reg4_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg4_res ) %>% 
  select( country , lifeexp , reg4_y_pred , reg4_res )


#################################
## Testing hypothesis
#

##
# 1) Coefficient is equal to 0:
# Implemented by default...
summary( reg4 )

# 2) Coefficient is equal to your favorite value
library(car)
# Let test: H0: ln_gdppc = 5, HA: ln_gdppc neq 5
linearHypothesis( reg4 , "ln_gdppc = 5")

# 3) Or two coefficients are the same in one model: 
#   in piecewise linear spline, the two coefficients are the same
summary( reg6 )
#   H0: lspline(ln_gdppc, cutoff_ln)1 - lspline(ln_gdppc, cutoff_ln)2 = 0
#   HA: lspline(ln_gdppc, cutoff_ln)1 - lspline(ln_gdppc, cutoff_ln)2 neq 0
linearHypothesis( reg6 , "lspline(ln_gdppc, cutoff_ln)1 = lspline(ln_gdppc, cutoff_ln)2")



#################################
## Prediction uncertainty
#

# CI of predicted value/regression line is implemented in ggplot
ggplot( data = df, aes( x = ln_gdppc, y = lifeexp ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' , se = T )

##
# You can get them by predict function
#   interval can be any of c("none", "confidence", "prediction")
#   alpha = 0.05 (default) is the significance level
###
# CI of regression line
pred4_CI <- predict( reg4, newdata = df , interval ="confidence" , alpha = 0.05 )
pred4_CI

# If you want you can ask to calculate the SEs for each point:
# pred4_CI <- predict( reg4, newdata = df , se.fit=T,
#                  interval ="confidence" , alpha = 0.05 )

# Hand made CI for regression line
# 1) Add to datatset:
df <- df %>% mutate( CI_reg4_lower = pred4_CI$fit[,2],
                     CI_reg4_upper = pred4_CI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = ln_gdppc, y = lifeexp ) , color='blue') +
  geom_line( data = df, aes( x = ln_gdppc, y = reg4_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = df, aes( x = ln_gdppc, y = CI_reg4_lower ) , color = 'green' ,
             size = 1 , linetype = "dashed" ) +
  geom_line( data = df, aes( x = ln_gdppc, y = CI_reg4_upper ) , color = 'black' ,
             size = 1 , linetype = "dashed" ) +
  labs(x = "ln( GDP/capita, 2018 int. const. $, PPP)",y = "Life expectancy  (years)") 


##
# Now we change to get the prediction intervals!
#
pred4_PI <- predict( reg4, newdata = df , interval ="prediction" , alpha = 0.05 )

# Hand made Prediction Interval for regression line
# 1) Add to datatset (You can use the SE's as well if you wish...
#                        then alpha does not have any meaning)
df <- df %>% mutate( PI_reg4_lower = pred4_PI$fit[,2],
                     PI_reg4_upper = pred4_PI$fit[,3] )
# 2) Plot
ggplot(  ) + 
  geom_point( data = df, aes( x = ln_gdppc, y = lifeexp ) , color='blue') +
  geom_line( data = df, aes( x = ln_gdppc, y = reg4_y_pred ) , color = 'red' , size = 1 ) +
  geom_line( data = df, aes( x = ln_gdppc, y = PI_reg4_lower ) , color = 'green' ,
             size = 1 , linetype = "dotted" ) +
  geom_line( data = df, aes( x = ln_gdppc, y = PI_reg4_upper ) , color = 'black' ,
             size = 1 , linetype = "dotted" ) +
  labs(x = "ln( GDP/capita, 2018 int. const. $, PPP)",y = "Life expectancy  (years)") 



