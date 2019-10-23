# Auditor

rm(list=ls())


library(auditor)
library(randomForest)
data(mtcars)

# fitting models
model_lm <- lm(mpg ~ ., data = mtcars)
set.seed(123)
model_rf <- randomForest(mpg ~ ., data = mtcars)

# creating objects with 'explain' function from the package DALEX
# that contains all necessary components required for further processing
exp_lm <- DALEX::explain(model_lm, data = mtcars, y = mtcars$mpg,  verbose = FALSE)
exp_rf <- DALEX::explain(model_rf, data = mtcars, y = mtcars$mpg, label = "rf", verbose = FALSE)

# create explanation  objects
mr_lm <- model_residual(exp_lm)
mr_rf <- model_residual(exp_rf)

# generating plots
plot_residual(mr_lm, mr_rf, variable = "wt", smooth = TRUE)
