---
title: "CIA 4"
author: "Riya"
date: "2023-05-06"
output:
  word_document: default
  html_document: default
  pdf_document: default
---

```{r}
getOption("repos")
options(repos = "https://cran.r-project.org")
```

```{r}
install.packages("knitr")
```

```{r}
install.packages("contrib.url")

```
```{r}
install.packages("tidyverse")
```

```{r}
install.packages("dplyr")
```

```{r}

library(tidyverse)

```
```{r}
install.packages("readxl")
```

```{r}
library(readxl)
```

```{r}
setwd("C:/Users/Admin/OneDrive/Desktop/rcia 3")
data = read_excel("Crude oil Dataset.xlsx")
data

```

```{r}
library(MASS)
bc <- boxcox(my_variable)
lambda <- bc$x[which.max(bc$y)]
transformed_variable <- if (lambda != 0) (my_variable^lambda - 1)/lambda else log(my_variable)
```

```{r}
#clean the data
  na.omit(data) 

```

```{r}
install.packages("ggplot2")
```

# Load the ggplot2 package

```{r}
library(ggplot2)
```

# Univariate Analysis of Variable

```{r}
summary(data)
```

```{r}
table(data)
data
```

# Univariate Analysis of each VariableS with outliers

```{r}

boxplot(data$`Crude Oil Price`,col="#3090C7")
boxplot(data$`EuDolEx`,col="#3090C7")
boxplot(data$GPNY,col="#3090C7")
boxplot(data$GPUS,col="#3090C7")
boxplot(data$`Heating Oil`,col="#3090C7")
boxplot(data$VIX,col="#3090C7")
boxplot(data$SPX,col="#3090C7")
boxplot(data$Gold,col="#3090C7")
boxplot(data$`TMU USA`,col="#3090C7")
boxplot(data$`TEU USA`,col="#3090C7")
boxplot(data$`OPEC Basket Price`,col="#3090C7")
boxplot(data$GPRD,col="#3090C7")
boxplot(data$Transportation,col="#3090C7")

```

#remove the outliers from each variables and then plotting box plot

```{r}

#crude oil price
outliers2 <- boxplot(data$`Crude Oil Price`, plot = FALSE)$out
data_clean <- data$`Crude Oil Price`[!data$`Crude Oil Price`%in% outliers2]
boxplot(data_clean,col="#50C878")


#Eu-DolEx
outliers <- boxplot(data$`EuDolEx`, plot = FALSE)$out
data_clean <- data$`EuDolEx`[!data$`EuDolEx` %in% outliers]
boxplot(data_clean,col="#50C878")

#GPNY
outliers1 <- boxplot(data$GPNY, plot = FALSE)$out
data_clean1 <- data$GPNY[!data$GPNY %in% outliers1]
boxplot(data_clean1,col="#50C878")


#GPUS
outliers3 <- boxplot(data$GPUS, plot = FALSE)$out
data_clean <- data$GPUS[!data$GPUS %in% outliers3]
boxplot(data_clean,col="#50C878")

# `Heating Oil`
outliers4 <- boxplot(data$`Heating Oil`, plot = FALSE)$out
data_clean <- data$`Heating Oil`[!data$`Heating Oil` %in% outliers4]
boxplot(data_clean,col="#50C878")


# VIX
outliers5 <- boxplot(data$VIX, plot = FALSE)$out
data_clean <- data$VIX [!data$VIX %in% outliers5]
boxplot(data_clean,col="#50C878")


# SPX
outliers6 <- boxplot(data$SPX, plot = FALSE)$out
data_clean <- data$SPX[!data$SPX %in% outliers6]
boxplot(data_clean ,col="#50C878")


# Gold
outliers7 <- boxplot(data$Gold, plot = FALSE)$out
data_clean <- data$Gold[!data$Gold%in% outliers7]
boxplot(data_clean,col="#50C878")

# `TMU-USA`
outliers8 <- boxplot(data$`TMU USA`, plot = FALSE)$out
data_clean <- data$`TMU USA`[!data$`TMU USA`%in% outliers8]
boxplot(data_clean,col="#50C878")

# `TEU-USA`
outliers9 <- boxplot(data$`TEU USA`, plot = FALSE)$out
data_clean <- data$`TEU USA`[!data$`TEU USA`%in% outliers9]
boxplot(data_clean,col="#50C878")

# `OPEC Basket Price`
outliers10 <- boxplot(data$`OPEC Basket Price`, plot = FALSE)$out
data_clean <- data$`OPEC Basket Price`[!data$`OPEC Basket Price`%in% outliers10]
boxplot(data_clean,col="#50C878")

#GPRD
outliers11 <- boxplot(data$GPRD, plot = FALSE)$out
data_clean <- data$GPRD[!data$GPRD %in% outliers11]
boxplot(data_clean, col = "#50C878")


```

# Bivariate Analysis of each variables

```{r}

#create scatterplot of EuDolEx vs.Crude oil

plot(data$`EuDolEx`,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='Eu-DolEx vs.Crude oil',
     xlab= 'crude oil price', ylab='Eu-DolEx')


#create scatterplot of GPNY vs.Crude oil

plot(data$GPNY,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='GPNY vs.Crude oil',
     xlab= 'crude oil price', ylab='GPNY')

#create scatterplot of GPUS vs.Crude oil

plot(data$GPUS,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='GPUS vs.Crude oil',
     xlab= 'crude oil price', ylab='GPUS')

#create scatterplot of Heating Oil vs.Crude oil

plot(data$`Heating Oil`,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='Heating Oil vs.Crude oil',
     xlab= 'crude oil price', ylab='Heating Oil')


#create scatterplot of VIX vs.Crude oil

plot(data$VIX,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='VIX vs.Crude oil',
     xlab= 'crude oil price', ylab='VIX')


#create scatterplot of SPX vs.Crude oil

plot(data$SPX,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='SPX vs.Crude oil',
     xlab= 'crude oil price', ylab='SPX')


#create scatterplot of Gold vs.Crude oil

plot(data$Gold,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='Gold vs.Crude oil',
     xlab= 'crude oil price', ylab='Gold')


#create scatterplot of TMU-USA vs.Crude oil

plot(data$`TMU USA`,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='Twitter Market Uncertainity vs.Crude oil',
     xlab= 'crude oil price', ylab='TMU-USA')


#create scatterplot of TEU-USA vs.Crude oil

plot(data$`TEU USA`,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='Twitter Economic Uncertainity vs.Crude oil',
     xlab= 'crude oil price', ylab='TEU-USA')


#create scatterplot of OPEC Basket Price vs.Crude oil

plot(data$`OPEC Basket Price`,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='OPEC Basket Price vs.Crude oil',
     xlab= 'crude oil price', ylab='OPEC Basket Price')


#create scatterplot of GPRD vs.Crude oil

plot(data$GPRD,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='Geopolitical Risk Index vs.Crude oil',
     xlab= 'crude oil price', ylab='GPRD')



#create scatterplot of Transportation vs.Crude oil

plot(data$Transportation,data$`Crude Oil Price`, pch=20, col='#967BB6',
     main='Transportation vs.Crude oil',
     xlab= 'crude oil price', ylab='Transportation')


```

#fit simple linear regression model for Bivariate Analysis

```{r}
#fit simple linear regression model
fit=lm(EuDolEx ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(GPNY ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(GPUS ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(`Heating Oil` ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(VIX ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(SPX ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(Gold ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(`TMU USA` ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(`TEU USA` ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(`OPEC Basket Price` ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(GPRD ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)

#fit simple linear regression model
fit=lm(Transportation ~ `Crude Oil Price` , data=data)
#view summary of model
summary(fit)



```

# Cross Tab Analysis of two variabales

```{r}

#create data frame
df <- data.frame(data$VIX , data$`TEU USA`)

#produce cross tab 
# Create a contingency table using xtabs()
mytable <- xtabs(~ VIX +`TEU USA`, data = data)

# Print the contingency table
print(mytable)


```

# statistical analysis: mean, median, mode of continous Variables

```{r}
#crude Oil Price
summary(data$`Crude Oil Price`)
x = data$`Crude Oil Price`
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)

#EuDolEx
summary(data$EuDolEx)
x = data$EuDolEx
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#GPNY
summary(data$GPNY)
x = data$GPNY
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#GPUS
summary(data$GPUS)
x = data$GPUS
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#Heating Oil
summary(data$`Heating Oil`)
x = data$`Heating Oil`
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)

#VIX
summary(data$VIX)
x = data$VIX
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#SPX
summary(data$SPX)
x = data$SPX
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#Gold
summary(data$Gold)
x = data$Gold
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#TMU USA
summary(data$`TMU USA`)
x = data$`TMU USA`
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#TEU USA
summary(data$`TEU USA`)
x = data$`TEU USA`
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)

#OPEC Basket Price
summary(data$`OPEC Basket Price`)
x = data$`OPEC Basket Price`
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#GPRD
summary(data$GPRD)
x = data$GPRD
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)


#Transportation
summary(data$Transportation)
x = data$Transportation
mode_x = names(sort(table(x), decreasing = TRUE)[1])
# Print the mode
print(mode_x)

```

#standard deviation and variance of each variable

```{r}
sd= sd(data$`Crude Oil Price`)
var= var(data$`Crude Oil Price`)
cat("Standard deviation:", sd, "\n")
cat("Variance:", var, "\n")


sd1= sd(data$EuDolEx)
var1= var(data$EuDolEx)
cat("Standard deviation:", sd1, "\n")
cat("Variance:", var1, "\n")


sd2= sd(data$GPNY)
var2= var(data$GPNY)
cat("Standard deviation:", sd2, "\n")
cat("Variance:", var2, "\n")


sd3= sd(data$GPUS)
var3= var(data$GPUS)
cat("Standard deviation:", sd3, "\n")
cat("Variance:", var3, "\n")


sd4= sd(data$`Heating Oil`)
var4= var(data$`Heating Oil`)
cat("Standard deviation:", sd4, "\n")
cat("Variance:", var4, "\n")


sd5= sd(data$VIX)
var5= var(data$VIX)
cat("Standard deviation:", sd5, "\n")
cat("Variance:", var5, "\n")


sd6= sd(data$SPX)
var6= var(data$SPX)
cat("Standard deviation:", sd6, "\n")
cat("Variance:", var6, "\n")


sd7= sd(data$Gold)
var7= var(data$Gold)
cat("Standard deviation:", sd7, "\n")
cat("Variance:", var7, "\n")


sd8= sd(data$`TMU USA`)
var8= var(data$`TMU USA`)
cat("Standard deviation:", sd8, "\n")
cat("Variance:", var8, "\n")

sd9= sd(data$`TEU USA`)
var9= var(data$`TEU USA`)
cat("Standard deviation:", sd9, "\n")
cat("Variance:", var9, "\n")


sd10= sd(data$`OPEC Basket Price`)
var10= var(data$`OPEC Basket Price`)
cat("Standard deviation:", sd10, "\n")
cat("Variance:", var10, "\n")


sd11= sd(data$GPRD)
var11= var(data$GPRD)
cat("Standard deviation:", sd11, "\n")
cat("Variance:", var11, "\n")


sd12= sd(data$Transportation)
var12= var(data$Transportation)
cat("Standard deviation:", sd12, "\n")
cat("Variance:", var12, "\n")


```

# Undylying Hypothesis testing

# Anova of the Variables

```{r}
model <- aov(`Crude Oil Price` ~ EuDolEx, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ GPNY, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ GPUS, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ `Heating Oil`, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ VIX, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ SPX, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ Gold, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ `TMU USA`, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ `TEU USA`, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ `OPEC Basket Price`, data = data)
# Print the ANOVA table
summary(model)


model <- aov(`Crude Oil Price` ~ GPRD, data = data)
# Print the ANOVA table
summary(model)

model <- aov(`Crude Oil Price` ~ Transportation, data = data)
# Print the ANOVA table
summary(model)

```

```{r}

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$EuDolEx, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$GPNY, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$GPUS, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$`Heating Oil`, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$VIX, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$SPX, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$Gold, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$`TMU USA`, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$`TEU USA`, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$`OPEC Basket Price`, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$GPRD, var.equal = FALSE)

# Perform a Welch's t-test with unequal variances
t.test(data$`Crude Oil Price`,data$Transportation, var.equal = FALSE)



```

```{r}
install.packages("corrplot")
library("corrplot")

```

#Test for Multicollinearity with Variance Inflation Factors (VIF)

```{r}
install.packages("olsrr")
library("olsrr")

```

```{r}

# Fit a linear regression model
model <- lm(`Crude Oil Price` ~ EuDolEx + GPNY +VIX +SPX + Gold+GPUS+GPRD+ Transportation, data = data)
# Calculate VIF for each predictor
ols_vif_tol<- round(1 / (1 - summary(model)$r.squared), 2)

# Compute the variance inflation factors (VIF)
ols_vif_tol(model)


```

# correlation Analysis

```{r}
# Create the correlation matrix
corr_matrix <- cor(data[c("Crude Oil Price", "EuDolEx", "GPNY", "GPUS", "Heating Oil", "VIX", "SPX", "Gold", "TEU USA", "TMU USA", "OPEC Basket Price", "GPRD", "Transportation")])

# Create the correlation plot
corrplot(corr_matrix, method = "circle", type = "upper", tl.col = "#C21E56")


```

#FEATURE SELECTION

```{r}
base.mod <- lm(`Crude Oil Price` ~ EuDolEx + GPNY+`Heating Oil`+VIX +SPX +`TMU USA`+Gold+GPUS+GPRD+ Transportation+`TEU USA`+ `OPEC Basket Price`, data= data)  

# base intercept only model
all.mod <- lm(`Crude Oil Price` ~ EuDolEx + GPNY+`Heating Oil`+VIX +SPX +`TMU USA`+Gold+GPUS+GPRD+ Transportation+`TEU USA`+ `OPEC Basket Price`, data= data) # full model with all predictors

stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm

shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.

shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars)

```

```{r}
install.packages("MASS")
library(MASS)

```


#MODELLING
```{r}
install.packages("ggpubr")
library(ggpubr)

```


#Boxcox Transformation to Normalize the data
```{r}
# Find minimum value of response variable
min_value <- min(data$`Crude Oil Price`)


# Add constant value to response variable
data$`Crude Oil Price` <- data$`Crude Oil Price` + abs(min_value) + 0.1


# Fit a linear regression model with all four variables
model <- lm(`Crude Oil Price` ~ `OPEC Basket Price` + GPUS + GPRD +`Heating Oil`, data = data)
# Perform a Box-Cox transformation
BC=boxcox(model,lambda = seq(-2, 2, 0.1)) 


```

#linear regression

```{r}
m1 <- lm(`Crude Oil Price` ~ EuDolEx + GPNY+`Heating Oil`+VIX +SPX +`TMU USA`+Gold+GPUS+GPRD+ Transportation+`TEU USA`+ `OPEC Basket Price` , data = data)
summary(m1)

# Get the coefficients for the model
coef(m1)

```

# Residual Analysis

```{r}
residuals <- resid(m1)

# Plot the residuals against the fitted values
plot(fitted(m1), residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot")

# Add a horizontal line at y = 0 for reference
abline(h=0)
```

```{r}
install.packages("shiny")
library(shiny)
```

# Model Diagonistics

#shinyApp

```{r}
# Define UI
ui <- fluidPage(
  titlePanel("Predicted Crude Oil Price Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("wti_price", "WTI Crude Oil Price ($/bbl):", value = 50),
      numericInput("brent_price", "Brent Crude Oil Price ($/bbl):", value = 60),
      numericInput("usd_index", "USD Index:", value = 90),
      numericInput("inflation_rate", "Inflation Rate (%):", value=2),
      numericInput("gdp_growth_rate", "GDP Growth Rate (%):", value = 3),
      numericInput("world_oil_demand", "World Oil Demand (mb/d):", value = 100),
      numericInput("oil_supply", "Oil Supply (mb/d):", value = 105),
      numericInput("oil_inventories", "Oil Inventories (mb):", value = 500),
      actionButton("calculate", "Calculate Predicted Crude Oil Price")
    ),
    mainPanel(
      plotOutput("diagnostic_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive expression for predicted crude oil price
  predicted_price <- reactive({
    req(input$calculate)
    
    # Model based on input variables
    crude_oil_price <- input$wti_price * 0.7 + input$brent_price * 0.3 - input$usd_index * 0.2 +
      input$inflation_rate * 0.1 + input$gdp_growth_rate * 0.2 - input$world_oil_demand * 0.3 +
      input$oil_supply * 0.4 - input$oil_inventories * 0.1
    
    crude_oil_price
  })
  
  # Render diagnostic plot
  output$diagnostic_plot <- renderPlot({
    # Plot predicted crude oil price vs. input variables
    ggplot(data.frame(x = predicted_price(), y = c(input$wti_price, input$brent_price, input$usd_index,
                                                   input$inflation_rate, input$gdp_growth_rate, input$world_oil_demand,
                                                   input$oil_supply, input$oil_inventories)), 
           aes(x = x, y = y)) +
      geom_point() +
      labs(x = "Predicted Crude Oil Price ($/bbl)", y = "Input Variables")
  })
}

# Run the app
shinyApp(ui, server)
```
