#######################################################
# Create data to simulate validation data with predicted values
#######################################################

rm(list=ls())

# Correl: This is the correlation used to determine how correlated the variables are to 
# the target variable. Switch it up (between 0 and 1) to see how the charts below change.
Correl <- 0.85
data <- data.table::data.table(Target = runif(1000))
#  Mock independent variables - they are correlated variables with 
# various transformations so you can see different kinds of relationships 
# in the charts below

# Helper columns for creating simulated variables 
data[, x1 := qnorm(Target)]
data[, x2 := runif(1000)]

# Create one variable at a time
data[, Independent_Variable1 := log(pnorm(Correl * x1 +
                                            sqrt(1-Correl^2) * qnorm(x2)))]
data[, Independent_Variable2 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))]
data[, Independent_Variable3 := exp(pnorm(Correl * x1 +
                                            sqrt(1-Correl^2) * qnorm(x2)))]
data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 +
                                                sqrt(1-Correl^2) * qnorm(x2))))]
data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 +
                                             sqrt(1-Correl^2) * qnorm(x2)))]
data[, Independent_Variable6 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))^0.10]
data[, Independent_Variable7 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))^0.25]
data[, Independent_Variable8 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))^0.75]
data[, Independent_Variable9 := (pnorm(Correl * x1 +
                                         sqrt(1-Correl^2) * qnorm(x2)))^2]
data[, Independent_Variable10 := (pnorm(Correl * x1 +
                                          sqrt(1-Correl^2) * qnorm(x2)))^4]

data[, Independent_Variable11 := ifelse(Independent_Variable2 < 0.20, "A",
                                        ifelse(Independent_Variable2 < 0.40, "B",
                                               ifelse(Independent_Variable2 < 0.6, "C",
                                                      ifelse(Independent_Variable2 < 0.8, "D", "E"))))]

# We’ll use this as a mock predicted value
data[, Predict := (pnorm(Correl * x1 +
                           sqrt(1-Correl^2) * qnorm(x2)))]

# Remove the helper columns
data[, ':=' (x1 = NULL, x2 = NULL)]

# In the ParDepCalPlot() function below, note the Function argument - 
# we are using mean() to aggregate our values but you 
# can use quantile(x, probs = y) for quantile regression

# Partial Dependence Calibration Plot: 
p1 <- RemixAutoML::ParDepCalPlots(data,
                                  PredictionColName = "Predict",
                                  TargetColName = "Target",
                                  IndepVar = "Independent_Variable1",
                                  GraphType = "calibration",
                                  PercentileBucket = 0.05,
                                  FactLevels = 10,
                                  Function = function(x) mean(x, na.rm = TRUE))

# Partial Dependence Calibration BoxPlot:  note the GraphType argument
p2 <- RemixAutoML::ParDepCalPlots(data,
                                  PredictionColName = "Predict",
                                  TargetColName = "Target",
                                  IndepVar = "Independent_Variable1",
                                  GraphType = "boxplot",
                                  PercentileBucket = 0.05,
                                  FactLevels = 10,
                                  Function = function(x) mean(x, na.rm = TRUE))

# Partial Dependence Calibration Plot: 
p3 <- RemixAutoML::ParDepCalPlots(data,
                                  PredictionColName = "Predict",
                                  TargetColName = "Target",
                                  IndepVar = "Independent_Variable4",
                                  GraphType = "calibration",
                                  PercentileBucket = 0.05,
                                  FactLevels = 10,
                                  Function = function(x) mean(x, na.rm = TRUE))

# Partial Dependence Calibration BoxPlot for factor variables: 
p4 <- RemixAutoML::ParDepCalPlots(data,
                                  PredictionColName = "Predict",
                                  TargetColName  = "Target",
                                  IndepVar = "Independent_Variable11",
                                  GraphType = "calibration",
                                  PercentileBucket = 0.05,
                                  FactLevels = 10,
                                  Function = function(x) mean(x, na.rm = TRUE))

# Plot all the individual graphs in a single pane
RemixAutoML::multiplot(plotlist = list(p1,p2,p3,p4), cols = 2)

