install.packages("readxl")
library(readxl)
library(moments)

startup <- read.csv("C:/RAVI/Data science/Assignments/Module 7 Multiple linear regression/MLR Assignment Q1 Dataset/50_Startups.csv/50_Startups.csv")
View(startup)
attach(startup)

install.packages("plyr")
install.packages("dplyr")
library(dplyr)
library(plyr)



# To Transform the data from Character to Numeric
startup$State <- revalue(startup$State,
                         c("New York"="0","California"="1","Florida"="2"))
startup$State

#startup$State=as.numeric(startup$State)
#startup$State

View(startup)

startup <- as.data.frame(startup)
startup

unique(startup$State) #Checking how many city are in state


#EDA(Exploratory data analysis)
# 1. Measures of central tendency
mean(startup$R.D.Spend)
mean(startup$Administration)
mean(startup$Marketing.Spend)
mean(startup$Profit)
mean(startup$State)
median(startup$R.D.Spend)



#Mode
x <- c(startup$R.D.Spend)
Mode <- function(x){
  a=table(x)
  return(a[which.max(a)])
}
Mode(x)

Y <- c(startup$State)
Mode <- function(Y){
  a=table(Y)
  return(a[which.max(a)])
}
Mode(Y)


# 2. Measures of dispersion
var(startup$R.D.Spend)
sd(startup$R.D.Spend)
range(startup$R.D.Spend)

# 3. Third moment business decision
skewness(startup$R.D.Spend)
skewness(startup$Administration)
skewness(startup$Marketing.Spend)
skewness(startup$Profit)


# 4. Fourth moment business decision
kurtosis(startup$R.D.Spend)
kurtosis(startup$Administration)
kurtosis(startup$Marketing.Spend)
kurtosis(startup$Profit)

# 5. Probability distributions of variables 
#Normal Quantile-Quantile Plot
qqnorm(startup$R.D.Spend)
qqline(startup$R.D.Spend)

qqnorm(startup$Administration)
qqline(startup$Administration)

# 6. Graphical representations
#Histogram
hist(startup$R.D.Spend)
hist(startup$Administration)
hist(startup$Marketing.Spend)
hist(startup$Profit)

#Box plot
boxplot(startup$R.D.Spend)
boxplot(startup$Administration)
boxplot(startup$Marketing.Spend)
boxplot(startup$Profit)


# Dot plot

dotplot(startup$R.D.Spend)
dotplot(startup$Administration)

#Bar plot
barplot(startup$R.D.Spend)
barplot(startup$Administration)
barplot(startup$Marketing.Spend)
barplot(startup$Profit)

summary(startup)

#scatter plot/bi-variate analysis
plot(startup$R.D.Spend,startup$Profit)
plot(startup$Administration,startup$Profit)
plot(startup$State,startup$Profit)
plot(startup$Marketing.Spend,startup$Profit)

#make a combined scatter plot  
pairs(startup)  # Scatter plot for all pairs of variables
cor(startup) # correlation matrix

#cor between i/p and o/p seperatley
cor(startup$R.D.Spend,startup$Profit)   # 0.9729005
cor(startup$Administration,startup$Profit)  #0.2007166  
cor(startup$Marketing.Spend,startup$Profit) #0.7477657  
cor(startup$State,startup$Profit) #0.1017963    

# The Linear Model 
model.startup <- lm(startup$Profit ~ startup$R.D.Spend + startup$Administration + startup$Marketing.Spend + startup$State,data=startup) 
summary(model.startup)
#Coefficients are insignificant Administration=0.606 ,Marketing Spend=0.109, State=0.989


##################################################################
#if Coefficients are insignificant then check the model for individual inputs
model.startupAdmin <- lm(startup$Profit ~ startup$Administration)
summary(model.startupAdmin)

model.startupMS <- lm(startup$Profit ~ startup$Marketing.Spend)
summary(model.startupMS)

model.startupState <- lm(startup$Profit ~ startup$State)
summary(model.startupState)

model.startup1 <- lm(startup$Profit ~ startup$Administration + startup$Marketing.Spend + startup$State)
summary(model.startup1) #also one of the input state values are insignificant

####### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}

pairs(startup, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


###  Partial Correlation matrix  ###
install.packages("corpcor")
library(corpcor)

cor(startup)

cor2pcor(cor(startup))

# Diagnostic Plots
install.packages("car")
library(car)

plot(model.startup) # Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

qqPlot(model.startup, id.n=5) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(model.startup)
influenceIndexPlot(model.startup, id.n=3) # Index Plots of the influence measures
influencePlot(model.startup, id.n=3) # A user friendly representation of the above

## Regression after deleting the 46 47 and 49 50th observation
model.startup_del<-lm(startup$Profit ~ startup$R.D.Spend + startup$Administration + startup$Marketing.Spend + startup$State, data = startup[-c(46,47,49,50) ])
summary(model.startup_del)
####******avoid removing outiers/rows because there is no significance of values are getting

### Variance Inflation Factors
vif(model.startup)  # VIF is > 10 => collinearity


VIFState <- lm(startup$State ~ startup$R.D.Spend + startup$Administration + startup$Marketing.Spend)
VIFR.D.Spend <- lm(startup$R.D.Spend ~ startup$Administration + startup$Marketing.Spend + startup$State)
VIFAdministration <- lm(startup$Administration ~ startup$Marketing.Spend + startup$State + startup$R.D.Spend)
VIFMarketing.Spend <- lm(startup$Marketing.Spend ~ startup$R.D.Spend + startup$Administration + startup$State)

summary(VIFState)
summary(VIFR.D.Spend)
summary(VIFAdministration)
summary(VIFMarketing.Spend)


1/(1-0.01115)  #VIF 0f State


#### Added Variable Plots ######
avPlots(model.startup, id.n=2, id.cex=0.8, col="red")


#library("MASS")
model.final <- lm(startup$Profit ~ startup$R.D.Spend + startup$Administration + startup$Marketing.Spend , data=startup) #here state is removed cause of there is no effect on increase of state nO effect On profit
summary(model.final)

model.final1 <- lm(startup$Profit ~ startup$R.D.Spend + startup$Administration + startup$Marketing.Spend, data=startup[-c(46,47,49,50)])# here remove outlier
summary(model.final1)


model.final2 <- lm(startup$Profit ~ startup$R.D.Spend + startup$Marketing.Spend, data=startup[-c(46,47,49,50)])# here remove outlier
summary(model.final2)

avPlots(model.final2, id.n=2, id.cex=0.8, col="red")



model.final_startup <- lm(startup$Profit ~ startup$R.D.Spend + startup$Marketing.Spend,data = startup)
summary(model.final_startup)

#data partitioning
n=nrow(startup)
n
n1=n*0.7
n1
n2=n-n1
n2
train_ind=sample(1:n,n1)
train_ind
train=startup[train_ind, ]
train
test=startup[-train_ind, ]
test

#Train Data
model <- lm(startup$Profit ~ startup$R.D.Spend + startup$Marketing.Spend, data = train)
summary(model)


pred=predict(model,newdata = test)
actual=test$Profit
actual
error=actual-pred
error

test.rmse=sqrt(mean(error**2))
test.rmse

train.rmse = sqrt(mean(model$residuals**2))
train.rmse


