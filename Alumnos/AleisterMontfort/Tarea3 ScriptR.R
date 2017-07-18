install.packages("ggplot2") # solo si necesario... 
setwd("D://Documentos//Tareas Data Science")
install.packages("knitr")
install.packages("markdown")
data(package="ggplot2")	

library(ggplot2)
data(diamonds) 
head(diamonds)

dim(diamonds)		# Dimensions of the data frame. 		Syntax: dim(x)
head(diamonds)		# Shows first n rows. 					Syntax: head(x,n) 
tail(diamonds)		# Shows last n rows. 					Syntax: head(x,n)
str(diamonds)  		# Displays the structure of an object. 	Syntax: str(x)
summary(diamonds)	# Displays summary statistics.			Syntax: summary(x)
names(diamonds)		# Element names of an object.			Syntax: names(x)
colnames(diamonds)	# Column names of an object.			Syntax: colnames(x)
summary(diamonds)

#Create dummy variables for CUT
head(diamonds$cut, n=10)
diamonds$fair_C <- ifelse(diamonds$cut=="Fair", 1,0)
diamonds$vg_C <- ifelse(diamonds$cut=="Very Good", 1,0)
diamonds$ideal_C <- ifelse(diamonds$cut=="Ideal", 1,0)
diamonds$premium_C <- ifelse(diamonds$cut=="Premium", 1,0)
diamonds$good_C <- ifelse(diamonds$cut=="Good", 1,0)

#Create dummy variables for Color
head(diamonds$color, n=10)
diamonds$color_D <- ifelse(diamonds$color=="D", 1,0)
diamonds$color_E <- ifelse(diamonds$color=="E", 1,0)
diamonds$color_F <- ifelse(diamonds$color=="F", 1,0)
diamonds$color_G <- ifelse(diamonds$color=="G", 1,0)
diamonds$color_I <- ifelse(diamonds$color=="I", 1,0)
diamonds$color_J <- ifelse(diamonds$color=="J", 1,0)
diamonds$color_H <- ifelse(diamonds$color=="H", 1,0)


#Create dummy variables for clarity
head(diamonds$clarity, n=10)
diamonds$clarity_I1 <- ifelse(diamonds$color=="I1", 1,0)
diamonds$clarity_SI2 <- ifelse(diamonds$color=="SI2", 1,0)
diamonds$clarity_SI1 <- ifelse(diamonds$color=="SI1", 1,0)
diamonds$clarity_VS1 <- ifelse(diamonds$color=="VS1", 1,0)
diamonds$clarity_VS2 <- ifelse(diamonds$color=="VS2", 1,0)
diamonds$clarity_VVS2 <- ifelse(diamonds$color=="VVS2", 1,0)
diamonds$clarity_VVS1 <- ifelse(diamonds$color=="VVS1", 1,0)
diamonds$clarity_IF <- ifelse(diamonds$color=="IF", 1,0)

head(diamonds$clarity_IF, n=10)
summary(diamonds$clarity_I1)
summary(diamonds$clarity_SI1)
summary(diamonds$clarity_VS1)
summary(diamonds$clarity_VS2)
summary(diamonds$clarity_VVS2)
summary(diamonds$clarity_VVS1)
summary(diamonds$clarity_IF)

?summary

diamonds
model1 <- lm(price ~ carat+good_C+vg_C+premium_C+ideal_C +color_D+color_E+color_F+color_G
             +color_H+color_I+depth+table+x+y+z, data= diamonds)
summary(model1)
prediction.model1 <- predict.lm(model1,  # model
                                newdata = data.frame(educate=range.educate, 
                                                     age = mean(turnout$age), 
                                                     gender=  "Male"), # must be data frame
                                se.fit=TRUE,  # keep standard errors
                                interval="confidence") # provide confidence intervals to the mean

Table1 <- texreg(list(model1),	# List of models to compare
                 custom.model.names = c("Model 1"), # Model names
                 digits = 2,							# Number of digits
                 custom.note = "Notes for this table", # Adds notes
                 caption = "This is the texreg table", # Adds a caption
                 caption.above = TRUE,				# Caption position
                 label = "Table Texreg",				# Adds a label
                 float.pos = "h!"					# Table position
)

d$predicted <- predict(model1)   # Save the predicted values
d$residuals <- residuals(model1) # Save the residual values
library(dplyr)
d %>% select(mpg, predicted, residuals) %>% head()


library(MASS)
sresid <- studres(model1) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals", xlab="Residuals")
xfit<-seq(sresid) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

sresid <- studres(model1) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(-5),max(5),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
 
lrfit <- glm( price ~ carat+good_C+vg_C+premium_C+ideal_C +color_D+color_E+color_F+color_G
              +color_H+color_I+depth+table+x+y+z , family = gaussian, data=diamonds)

lrfit

?optim
plot (model1)
prediction.data <- data.frame(prediction.model1$fit) # Keep fitted values and confidence intervals
prediction.data
