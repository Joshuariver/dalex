# Epilogue
# https://pbiecek.github.io/DALEX_docs/6-epilogue.html 

library("DALEX")

apartments_lm_model_improved <- lm(m2.price ~ I(construction.year < 1935 | construction.year > 1995) + surface + floor + 
                                     no.rooms + district, data = apartments)

explainer_lm_improved <- explain(apartments_lm_model_improved, 
                                 data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

mp_lm_improved <- model_performance(explainer_lm_improved)
plot(mp_lm_improved, geom = "boxplot")
