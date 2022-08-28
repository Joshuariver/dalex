# fairmodels: let’s fight with biased Machine Learning models (part 1 — detection)
# https://www.r-bloggers.com/fairmodels-lets-fight-with-biased-machine-learning-models-part-1%e2%80%8a-%e2%80%8adetection/

# 1. Create a model

devtools::install_github("ModelOriented/fairmodels")

library(fairmodels)
data("german")
y_numeric <- as.numeric(german$Risk) -1
lm_model <- glm(Risk~., data = german, family=binomial(link="logit"))

# 2. Create an explainer

library(DALEX)
explainer_lm <- explain(lm_model, data = german[,-1], y = y_numeric)

# 3. Use the fairness_check(). Here the epsilon value is set to default which is 0.1

fobject <- fairness_check(explainer_lm,
                          protected = german$Sex,
                          privileged = "male")

fobject

print(fobject)


#

library(ranger)

rf_model     <- ranger(Risk ~., data = german, probability = TRUE)
explainer_rf <- explain(rf_model, data = german[,-1], y = y_numeric)
fobject      <- fairness_check(explainer_rf, fobject)

