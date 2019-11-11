##### Module 11 HW ###
#### BAE590 Fall 2019###
#### Ryan Phillips ### 

# rm(list=ls(all=TRUE))

library(tidyverse)
library(modelr)
library(broom)
library(corrr)

#Data Read-in and working data creation -----
#Read in all the data
gdp <- read_csv("data/gdp-2017.csv")
meat <- read_csv("data/meat-production-2017.csv")
pop <- read_csv("data/total-population-2017.csv")

#Confirm data came in 
gdp
meat
pop

#Determine meat production by country
meat <- meat %>%
  group_by(country) %>%
  summarise(meat_produced = sum(meat_produced, na.rm=TRUE))
# names(meat)[2]<-"meat_produced"
meat

#Join gdp and meat
gdp_meat <- meat %>%
  inner_join(gdp, by = "country", na.rm=TRUE)
gdp_meat

#Join everything together
gdp_meat_pop <- gdp_meat %>%
 inner_join(pop, by = "country", na.rm=TRUE)
gdp_meat_pop

#Rename
meat_data <- gdp_meat_pop

summary(meat_data)

#Data validation through viz ----
#plot it out

meat_data %>%
  ggplot(mapping = aes(x = gdp, y = meat_produced)) +
  geom_point(alpha = 0.5)+
  theme_bw()

#Normalize Function creation 
normalize <- function(x) {
  n <- (x - min(x)) / (max(x) - min(x))
  return(n)
}

meat_data %>%
  mutate(meat_produced = normalize(meat_produced),
         gdp = normalize(gdp),
         total_population = normalize(total_population)) -> meat_data_norm
meat_data_norm
summary(meat_data_norm)  

#Replot with normalized values
meat_data_norm %>%
  ggplot() +
  geom_point(mapping = aes(gdp, meat_produced), alpha = 0.5) + 
  theme_bw()
  
#Re-plot meat_produced vs gdp but zoomed in

meat_data_norm %>%
  ggplot() +
  geom_point(mapping = aes(gdp, meat_produced), alpha = 0.5) +
  lims(x = c(0, 0.05),
       y = c(0, 0.15)) +
  theme_bw()

#Plot meat vs pop with normalized values
meat_data_norm %>%
  ggplot() +
  geom_point(mapping = aes(total_population, meat_produced), alpha = 0.5) + 
  theme_bw()
## Correlations ----

meat_data_norm %>%
  select(-country) %>%
  correlate() %>%
  fashion()

meat_data_norm %>%
  select(-country) %>%
  correlate() %>%
  rplot()

#Creating models ----
#Meat_produced as function of gdp ----

meat_gdp_mod <- lm(meat_produced ~ gdp, data = meat_data_norm)
summary(meat_gdp_mod)
tidy(meat_gdp_mod)
glance(meat_gdp_mod)

#Add model predictions to new dataframe just for this model
meat_gdp_pred <- meat_data_norm %>%
  add_predictions(meat_gdp_mod) %>%  # Add predictions
  add_residuals(meat_gdp_mod)  # Add residuals
meat_gdp_pred

#Prediction plot
ggplot(data = meat_gdp_pred) +
  geom_point(mapping = aes(pred, meat_produced)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  lims(x = c(0, 0.15),
       y = c(0, 0.15)) +
  labs(title =  "Production as function of GDP") +
  theme_bw()
# Residual Plot
ggplot(meat_gdp_pred, aes(meat_produced,resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  labs(title = "prod as function of GDP")




#Meat_produced as function of total population ----
meat_pop_mod <- lm(meat_produced ~ total_population, data = meat_data_norm)
summary(meat_pop_mod)
tidy(meat_pop_mod)  
glance(meat_pop_mod)  

meat_pop_pred <- meat_data_norm %>%
  add_predictions(meat_pop_mod) %>%
  add_residuals(meat_pop_mod)
meat_pop_pred


#Prediction plot
ggplot(data = meat_pop_pred) +
  geom_point(mapping = aes(pred, meat_produced)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  lims(x = c(0, 0.15),
       y = c(0, 0.15)) +
  labs(title = "production as function of pop") +
  theme_bw()
# Residual Plot
ggplot(meat_pop_pred, aes(meat_produced,resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  labs(title =  "prod as funtion of pop")
  
#Meat produced as function of gdp, total pop, and the interaction  ----

interact_mod <- lm(meat_produced ~ gdp + total_population + gdp * total_population, data = meat_data_norm)
summary(interact_mod)  
tidy(interact_mod)
glance(interact_mod)

interact_pred <- meat_data_norm %>%
  add_predictions(interact_mod) %>%  #Add predictions
  add_residuals(interact_mod) # Add residuals
interact_pred

#Prediction plot
ggplot(data = interact_pred) +
  geom_point(mapping = aes(pred, meat_produced)) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  lims(x = c(0, 0.15),
       y = c(0, 0.15)) +
  labs(title = "interaction")+
  theme_bw()
# Residual Plot
ggplot(interact_pred, aes(meat_produced,resid)) +
  geom_ref_line(h = 0) +
  geom_point() + 
  labs(title = "interaction")



# Log(meat) as function of log(gdp) ----

log_norm <- meat_data_norm %>%
  mutate( lmeat = log(meat_produced, base = exp(1)),
          lgdp = log(gdp, base = exp(1)),
          lpop = log(total_population, base = exp(1)))
log_norm <- na.omit(log_norm)
log_norm
options(na.action="na.omit")

log_norm 

log_gdp_mod <- lm(log(meat_produced+0.0001) ~ log(gdp+0.0001), data = log_norm)
summary(log_gdp_mod)
summary(log_norm)

log_gdp_pred <- log_norm %>%
  add_predictions(log_gdp_mod) %>%
  add_residuals(log_gdp_mod)

log_gdp_pred

#Prediction plot
ggplot(data = log_gdp_pred) +
  geom_point(mapping = aes(pred, lmeat)) +
  geom_abline(intercept = 0, slope = 1, color = "red")+
  labs(title = "log prod log gdp") + 
  theme_bw()
# Residual Plot
ggplot(log_gdp_pred, aes(lmeat,resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  labs(title = "log prod log gdp")




# Log production as function of log(pop) ----


log_pop_mod <- lm(log(meat_produced + .0001) ~ log(total_population+.0001), data = log_norm)

summary(log_pop_mod)
log_pop_pred <- log_norm %>%
  add_predictions(log_pop_mod) %>%
  add_residuals(log_pop_mod)
log_pop_pred

#Prediction plot
ggplot(data = log_pop_pred) +
  geom_point(mapping = aes(pred, lmeat)) +
  geom_abline(intercept = 0, slope = 1, color = "red")+
  labs(title = "log prod log pop")+
  theme_bw()
# Residual Plot
ggplot(log_pop_pred, aes(lmeat,resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  labs(title = "log prod log pop")


# Answer ----

# The interaction model has the highest correlation coefficient of 0.91 which makes it seem like that would be the best model. When looking at the residual plot for this model though,there is a large cluster very small which probably increases the accuracy of this model. There is no randomness involved with this plot so although the correlation coefficient is high, it may not be accurate when you leave a very small value. The log pop model has the next highest correlation coefficient. The model seems to fit more of the points overall here, but on the lower end it still doesn't get everything. Since most countries are clustered so tightly and low, I think it is acceptable to use the interaction model for most countries when predicting meat production using a combination of gdp and population. 


















