# Commodity-Price - Predicting

## Introduction

This repository contains code and data for the CIA 4 project. The goal of the project is to analyze and model crude oil price based on various economic indicators and variables.

## Getting Started

### Installation

To run the code in this project, you will need to install the required R packages:

- knitr
- tidyverse
- readxl
- ggplot2
- corrplot
- MASS
- olsrr
- ggpubr
- shiny

## Data

The data used for this project is stored in the file "Crude oil Dataset.xlsx." You can load the data into R using the following code:

```R
setwd("C:/Users/Admin/OneDrive/Desktop/") # Change the path to your data directory
data <- read_excel("Crude oil Dataset.xlsx")
