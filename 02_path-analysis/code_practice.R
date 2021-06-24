
library(lavaan)
library(semPlot)
#library(OpenMx)
library(tidyverse)

# Organizing package information for table
mtcars
str(mtcars)

model <-'
mpg ~ hp + gear + cyl + disp + carb + am + wt
hp ~ cyl + disp + carb
'

fit <- cfa(model, data = mtcars)
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit, 'std', layout = 'circle')
