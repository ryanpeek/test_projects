#PLS 298: Final project
#Parameter optimizastion for a deterministic water temperature model
#Ann Willis
#Thursday, December 8, 2016

library(ggplot2)
library(lme4)
library(dplyr)
library(sjPlot)
library(merTools)


#setwd("Z:/cws-eel/fissekis/Documents/PhD/Coursework/PLS298/Homework")

W3T <- read.csv("data/W3T_output.csv")

#Visualize data

plot(mean_bias~roughness, W3T)
plot(MAE~roughness, W3T)
plot(RMSE~roughness, W3T)

#Group data by location

plot.mb<-ggplot(data=W3T) + geom_point(aes(x=roughness, y=mean_bias, color = location, size = 0.3))
plot.mb + theme_bw() + scale_color_gradientn(colours = rainbow(5), breaks=c(1:9),labels=c(1:9))

plot.mae<-ggplot(data=W3T) + geom_point(aes(x=roughness, y=MAE, color = location, size = 0.3))
plot.mae + theme_bw() + scale_color_gradientn(colours = rainbow(5))

plot.rmse<-ggplot(data=W3T) + geom_point(aes(x=roughness, y=RMSE, color = location, size = 0.3))
plot.rmse + theme_bw() + scale_color_gradientn(colours = rainbow(5))

# When the data is grouped by location, the relationship between roughness and mean_bias appear to be polynomial. Though roughness is not in itself a predictor of mean_bias, it is one of the parameters that can be used to reduce mean_bias in numerical models. Next, I will fit both a linear regression model and second-order polynomial regression model, and compare them to see if one performs better than the other. Once I've determined which model performs the best, I will use it to solve for the roughness value that results in mean_bias = 0.

# Explore linear and polynomial models for mean_bias and roughness

#linear model
lm_mb <- lm(mean_bias~roughness, W3T)

summary(lm_mb)

ggplot(data = W3T, aes(x=roughness, y=mean_bias, size = 0.3)) + 
  geom_point() +
  stat_smooth(method = 'lm', formula = y~x)

#polynomial model
poly_mb <- lm(mean_bias ~ poly(roughness,2,raw=TRUE), data = W3T)

summary(poly_mb)

ggplot(data = W3T, aes(x=roughness, y=mean_bias, size = 0.3)) + 
  geom_point() +
  stat_smooth(method = 'lm', formula = y~poly(x,2))

#Compare AIC
AIC(lm_mb,poly_mb)


#The AIC score shows that the models are about the same, so either one would be appropriate if we're looking at all the mean_bias results as a group. Because roughness and mean_bias are strongly related by location, I will also try a random effects model to see if the model performance improves. 

lmer_mb <- lmer(mean_bias ~ roughness + (1|location), data = W3T, REML=FALSE)

summary (lmer_mb)

sjp.lmer(lmer_mb)

AIC(lm_mb,poly_mb,lmer_mb)

#The random effects model performs better than either the linear model or second-degree polynomial model. 

#For comparison, I'll solve the linear model and random effects model for mean_bias = 0 and see if the predictor value is comparable.

#Solve linear model for mean_bias = 0
cc_lm <- coef(lm_mb)
(xnew <- (0-cc_lm[1])/cc_lm[2])

#solve random effects model for mean_bias = 0
opt2 <- wiggle(data=W3T, var="roughness", values = seq(0.05,0.1,0.001))
sub.opt2 <- filter(opt2,mean_bias == "0")

# rerun model with new dataset
lmer_mb2 <- lmer(mean_bias ~ roughness + (1|location), data = sub.opt2, REML=FALSE)

opt.roughness <- draw(lmer_mb2, type = 'average')
opt.roughness

# plot
sjp.lmer(lmer_mb, type = 'ri.slope', varlist=sub.opt2)

# also see "get_model_data()"
plot_model(lmer_mb2, type="slope") # plots slope of coefficients for each single predictor against response
plot_model(lmer_mb, type="re", show.values=T) # plots random effects
plot_model(lmer_mb, type="pred", pred.type = "re", terms = "roughness", show.data = T) # plots predict. values for model term

plot_model(lmer_mb, type="pred", pred.type = "re", terms = "location", show.data = T) # plots predict. values for model term

plot_model(lmer_mb, type="est") # plots estimates for each predictor


