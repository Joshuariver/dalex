# Ceteris Paribus Profiles
# https://pbiecek.github.io/DALEX_docs/5-ceterisParibus.html


rm(list=ls())

library("DALEX")
# Linear model trained on apartments data
model_lm <- lm(m2.price ~ construction.year + surface + floor + 
                 no.rooms + district, data = apartments)

library("randomForest")
set.seed(59)
# Random Forest model trained on apartments data
model_rf <- randomForest(m2.price ~ construction.year + surface + floor + 
                           no.rooms + district, data = apartments)

library("e1071")
# Support Vector Machinesr model trained on apartments data
model_svm <- svm(m2.price ~ construction.year + surface + floor + 
                   no.rooms + district, data = apartments)

explainer_lm <- explain(model_lm, 
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
explainer_rf <- explain(model_rf, 
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
explainer_svm <- explain(model_svm, 
                         data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

library("ceterisParibus")

aplevels <- levels(apartments$district)

new_apartment <- data.frame(construction.year = 2000, 
                            surface = 100,
                            floor = 1L,
                            no.rooms = 4,
                            district = factor("Bemowo", levels = aplevels))
new_apartment

predict(model_rf, new_apartment)

profile_rf <- ceteris_paribus(explainer_rf, observations = new_apartment)
profile_rf

plot(profile_rf, selected_variables = "construction.year")

plot(profile_rf)

neighbours <- select_neighbours(apartmentsTest, observation = new_apartment, n = 10)
head(neighbours)

profile_rf_neig  <- ceteris_paribus(explainer_rf,  
                                    observations = neighbours, 
                                    y = neighbours$m2.price)

plot(profile_rf_neig, 
     selected_variables = "surface", size_residuals = 2,
     color_residuals = "red", show_residuals = TRUE, show_observations = FALSE) 


plot(profile_rf_neig, 
     selected_variables = "surface", size_residuals = 2,
     color_residuals = "red", show_residuals = TRUE, show_observations = FALSE) +
  ceteris_paribus_layer(profile_rf, size = 3, alpha = 1, color = "blue",
                        selected_variables = "surface") 

plot(profile_rf_neig, 
     selected_variables = "surface", size_residuals = 2,
     color_residuals = "red", show_residuals = TRUE, show_observations = FALSE) +
  ceteris_paribus_layer(profile_rf, size = 3, alpha = 1, color = "blue",
                        selected_variables = "surface") +
  ceteris_paribus_layer(profile_rf_neig, size = 3, alpha = 1, color = "black",
                        aggregate_profiles = mean, show_observations = FALSE,
                        selected_variables = "surface")


profile_svm <- ceteris_paribus(explainer_svm, observations = new_apartment)
profile_lm  <- ceteris_paribus(explainer_lm, observations = new_apartment)
plot(profile_rf, profile_svm, profile_lm, color = "_label_")

