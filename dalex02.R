library("DALEX")
apartments_lm_model <- lm(m2.price ~ construction.year + surface + floor + 
                            no.rooms + district, data = apartments)
library("randomForest")
set.seed(59)
apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor + 
                                      no.rooms + district, data = apartments)

# First we need to prepare wrappers for these models. They are in explainer_lm and explainer_rf objects.

explainer_lm <- explain(apartments_lm_model, 
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)
explainer_rf <- explain(apartments_rf_model, 
                        data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

mp_rf <- model_performance(explainer_rf)

library("ggplot2")
ggplot(mp_rf, aes(observed, diff)) + geom_point() + 
  xlab("Observed") + ylab("Predicted - Observed") + 
  ggtitle("Diagnostic plot for the random forest model") + theme_mi2()

which.min(mp_rf$diff)

new_apartment <- apartmentsTest[which.min(mp_rf$diff), ]
new_apartment

new_apartment_rf <- single_prediction(explainer_rf, observation = new_apartment)
breakDown:::print.broken(new_apartment_rf)

plot(new_apartment_rf)

new_apartment_lm <- single_prediction(explainer_lm, observation = new_apartment)
plot(new_apartment_lm, new_apartment_rf)
