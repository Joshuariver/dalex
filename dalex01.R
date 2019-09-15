# https://www.r-bloggers.com/tell-me-a-story-how-to-generate-textual-explanations-for-predictive-models/
# Tell Me a Story: How to Generate Textual Explanations for Predictive Models

# https://github.com/ModelOriented/DALEX


# the easiest way to get DALEX is to install it from CRAN:
# install.packages("DALEX")

# Or the the development version from GitHub:
# install.packages("devtools")
# devtools::install_github("ModelOriented/DALEX")


rm(list=ls())
library(DALEX)

data(apartments)
head(apartments)

apartments_lm_model <- lm(m2.price ~ construction.year + surface + floor + 
                            no.rooms + district, data = apartments)
summary(apartments_lm_model)


predicted_mi2_lm <- predict(apartments_lm_model, apartmentsTest)
sqrt(mean((predicted_mi2_lm - apartmentsTest$m2.price)^2))

explainer_lm <- explain(apartments_lm_model, 
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)


library("randomForest")
set.seed(59)

apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor + 
                                      no.rooms + district, data = apartments)
apartments_rf_model

predicted_mi2_rf <- predict(apartments_rf_model, apartmentsTest)
sqrt(mean((predicted_mi2_rf - apartmentsTest$m2.price)^2))

explainer_rf <- explain(apartments_rf_model, 
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)


mp_lm <- model_performance(explainer_lm)
mp_rf <- model_performance(explainer_rf)
mp_lm
mp_rf

plot(mp_lm, mp_rf)
plot(mp_lm, mp_rf, geom = "boxplot")


vi_rf <- variable_importance(explainer_rf, loss_function = loss_root_mean_square)
vi_rf

vi_lm <- variable_importance(explainer_lm, loss_function = loss_root_mean_square)
vi_lm

plot(vi_lm, vi_rf)


vi_lm <- variable_importance(explainer_lm, loss_function = loss_root_mean_square, type = "difference")
vi_rf <- variable_importance(explainer_rf, loss_function = loss_root_mean_square, type = "difference")
plot(vi_lm, vi_rf)


varImpPlot(apartments_rf_model)


library("forestmodel")
forest_model(apartments_lm_model)

library("sjPlot")
plot_model(apartments_lm_model, type = "est", sort.est = TRUE)




sv_rf  <- single_variable(explainer_rf, variable =  "construction.year", type = "pdp")
plot(sv_rf)

sv_lm  <- single_variable(explainer_lm, variable =  "construction.year", type = "pdp")
plot(sv_rf, sv_lm)

sva_rf  <- single_variable(explainer_rf, variable = "construction.year", type = "ale")
sva_lm  <- single_variable(explainer_lm, variable = "construction.year", type = "ale")

plot(sva_rf, sva_lm)


# install.packages("factorMerger")
library(factorMerger)

svd_rf  <- single_variable(explainer_rf, variable = "district", type = "factor")
svd_lm  <- single_variable(explainer_lm, variable = "district", type = "factor")

plot(svd_rf, svd_lm)
