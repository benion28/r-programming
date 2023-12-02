# Basal Height
bh <- 1.3

# Starting Values
# ------ The available 2- parameter functions are:
start_values_naslund <- startHDnaslund(training$DIAMETER, training$HEIGHT, bh)
start_values_curtis <- startHDcurtis(training$DIAMETER, training$HEIGHT, bh)
start_values_michailoff <- startHDmichailoff(training$DIAMETER, training$HEIGHT, bh)
start_values_meyer <- startHDmeyer(training$DIAMETER, training$HEIGHT, bh)
start_values_power <- startHDpower(training$DIAMETER, training$HEIGHT, bh)
start_values_naslund2 <- startHDnaslund2(training$DIAMETER, training$HEIGHT, bh)
start_values_naslund3 <- startHDnaslund3(training$DIAMETER, training$HEIGHT, bh)
start_values_naslund4 <- startHDnaslund4(training$DIAMETER, training$HEIGHT, bh)
start_values_michaelis_menten <- startHDmicment(training$DIAMETER, training$HEIGHT, bh)
start_values_michaelis_menten2 <- startHDmicment2(training$DIAMETER, training$HEIGHT, bh)
start_values_wykoff <- startHDwykoff(training$DIAMETER, training$HEIGHT, bh)
# --------- The available 3- parameter functions are:
start_values_prodan <- startHDprodan(training$DIAMETER, training$HEIGHT, bh)
start_values_logistic <- startHDlogistic(training$DIAMETER, training$HEIGHT, bh)
start_values_chapman <- startHDrichards(training$DIAMETER, training$HEIGHT, bh)
start_values_weibull <- startHDweibull(training$DIAMETER, training$HEIGHT, bh)
start_values_gomperz <- startHDgomperz(training$DIAMETER, training$HEIGHT, bh)
start_values_sibbesen <- startHDsibbesen(training$DIAMETER, training$HEIGHT, bh)
start_values_korf <- startHDkorf(training$DIAMETER, training$HEIGHT, bh)
start_values_ratkowsky <- startHDratkowsky(training$DIAMETER, training$HEIGHT, bh)
start_values_hossfeldIV <- startHDhossfeldIV(training$DIAMETER, training$HEIGHT, bh)

# ------ The available 2- parameter functions are:
# ----------------------------- Model Naslund ----------------------------- #
# Model Naslund Fitting
model_naslund <- nls(formula = HEIGHT ~ (bh + DIAMETER^2/(a + b * DIAMETER)^2), 
                     data = training, start=list(
                       a=start_values_naslund["a"], 
                       b=start_values_naslund["b"]))
summary_model_naslund <- summary(model_naslund)
length_model_naslund <- length(summary(model_naslund)$coef[,1])
model_naslund


# Making Predictions
predictions_model_naslund_training = predict(model_naslund, training)
predictions_model_naslund_testing = predict(model_naslund, testing)


# ----------------------------- Model Curtis ----------------------------- #
# Model Curtis Fitting
model_curtis <- nls(formula = HEIGHT ~ (bh + a * (DIAMETER/(1 + DIAMETER))^b), 
                    data = training, start=list(
                      a=start_values_curtis["a"], 
                      b=start_values_curtis["b"]))
summary_model_curtis <- summary(model_curtis)
length_model_curtis <- length(summary(model_curtis)$coef[,1])
model_curtis


# Making Predictions
predictions_model_curtis_training = predict(model_curtis, training)
predictions_model_curtis_testing = predict(model_curtis, testing)


# ----------------------------- Model Michailoff ----------------------------- #
# Model Michailoff Fitting
model_michailoff <- nls(formula = HEIGHT ~ (bh + a * exp(-b * DIAMETER^(-1))), 
                    data = training, start=list(
                      a=start_values_michailoff["a"], 
                      b=start_values_michailoff["b"]))
summary_model_michailoff <- summary(model_michailoff)
length_model_michailoff <- length(summary(model_michailoff)$coef[,1])
model_michailoff


# Making Predictions
predictions_model_michailoff_training = predict(model_michailoff, training)
predictions_model_michailoff_testing = predict(model_michailoff, testing)


# ----------------------------- Model Meyer ----------------------------- #
# Model Meyer Fitting
model_meyer <- nls(formula = HEIGHT ~ (bh + a * (1-exp(-b * DIAMETER))), 
                        data = training, start=list(
                          a=start_values_meyer["a"], 
                          b=start_values_meyer["b"]))
summary_model_meyer <- summary(model_meyer)
length_model_meyer <- length(summary(model_meyer)$coef[,1])
model_meyer


# Making Predictions
predictions_model_meyer_training = predict(model_meyer, training)
predictions_model_meyer_testing = predict(model_meyer, testing)


# ----------------------------- Model Power ----------------------------- #
# Model Power Fitting
model_power <- nls(formula = HEIGHT ~ (bh + a * DIAMETER^b), 
                   data = training, start=list(
                     a=start_values_power["a"], 
                     b=start_values_power["b"]))
summary_model_power <- summary(model_power)
length_model_power <- length(summary(model_power)$coef[,1])
model_power


# Making Predictions
predictions_model_power_training = predict(model_power, training)
predictions_model_power_testing = predict(model_power, testing)


# ----------------------------- Model Naslund2 ----------------------------- #
# Model Naslund2 Fitting
model_naslund2 <- nls(formula = HEIGHT ~ (bh + DIAMETER^2/(a + exp(b) * DIAMETER)^2), 
                   data = training, start=list(
                     a=start_values_naslund2["a"], 
                     b=start_values_naslund2["b"]))
summary_model_naslund2 <- summary(model_naslund2)
length_model_naslund2 <- length(summary(model_naslund2)$coef[,1])
model_naslund2


# Making Predictions
predictions_model_naslund2_training = predict(model_naslund2, training)
predictions_model_naslund2_testing = predict(model_naslund2, testing)


# ----------------------------- Model Naslund3 ----------------------------- #
# Model Naslund3 Fitting
model_naslund3 <- nls(formula = HEIGHT ~ (bh + DIAMETER^2/(exp(a) + b * DIAMETER)^2), 
                      data = training, start=list(
                        a=start_values_naslund3["a"], 
                        b=start_values_naslund3["b"]))
summary_model_naslund3 <- summary(model_naslund3)
length_model_naslund3 <- length(summary(model_naslund3)$coef[,1])
model_naslund3


# Making Predictions
predictions_model_naslund3_training = predict(model_naslund3, training)
predictions_model_naslund3_testing = predict(model_naslund3, testing)


# ----------------------------- Model Naslund4 ----------------------------- #
# Model Naslund4 Fitting
model_naslund4 <- nls(formula = HEIGHT ~ (bh + DIAMETER^2/(exp(a) + exp(b) * DIAMETER)^2), 
                      data = training, start=list(
                        a=start_values_naslund4["a"], 
                        b=start_values_naslund4["b"]))
summary_model_naslund4 <- summary(model_naslund4)
length_model_naslund4 <- length(summary(model_naslund4)$coef[,1])
model_naslund4


# Making Predictions
predictions_model_naslund4_training = predict(model_naslund4, training)
predictions_model_naslund4_testing = predict(model_naslund4, testing)


# ----------------------------- Model Michaelis-Menten ----------------------------- #
# Model Michaelis-Menten Fitting
model_michaelismen <- nls(formula = HEIGHT ~ (bh + a * DIAMETER/(b + DIAMETER)), 
                      data = training, start=list(
                        a=start_values_michaelis_menten["a"], 
                        b=start_values_michaelis_menten["b"]))
summary_model_michaelismen <- summary(model_michaelismen)
length_model_michaelismen <- length(summary(model_michaelismen)$coef[,1])
model_michaelismen


# Making Predictions
predictions_model_michaelismen_training = predict(model_michaelismen, training)
predictions_model_michaelismen_testing = predict(model_michaelismen, testing)


# ----------------------------- Model Michaelis-Menten2 ----------------------------- #
# Model Michaelis-Menten Fitting
model_michaelismen2 <- nls(formula = HEIGHT ~ (bh + DIAMETER/(a + b * DIAMETER)), 
                          data = training, start=list(
                            a=start_values_michaelis_menten2["a"], 
                            b=start_values_michaelis_menten2["b"]))
summary_model_michaelismen2 <- summary(model_michaelismen2)
length_model_michaelismen2 <- length(summary(model_michaelismen2)$coef[,1])
model_michaelismen2


# Making Predictions
predictions_model_michaelismen2_training = predict(model_michaelismen2, training)
predictions_model_michaelismen2_testing = predict(model_michaelismen2, testing)


# ----------------------------- Model Wykoff ----------------------------- #
# Model Wykoff Fitting
model_wykoff <- nls(formula = HEIGHT ~ (bh + exp(a + b/(DIAMETER + 1))), 
                           data = training, start=list(
                             a=start_values_wykoff["a"], 
                             b=start_values_wykoff["b"]))
summary_model_wykoff <- summary(model_wykoff)
length_model_wykoff <- length(summary(model_wykoff)$coef[,1])
model_wykoff


# Making Predictions
predictions_model_wykoff_training = predict(model_wykoff, training)
predictions_model_wykoff_testing = predict(model_wykoff, testing)



# --------- The available 3- parameter functions are:
# ----------------------------- Model Prodan ----------------------------- #
# Model Prodan Fitting
model_prodan <- nls(formula = HEIGHT ~ (bh + DIAMETER^2/(a + b * DIAMETER + c * DIAMETER^2)), 
                    data = training, start=list(
                      a=start_values_prodan["a"], 
                      b=start_values_prodan["b"],
                      c=start_values_prodan["c"]))
summary_model_prodan <- summary(model_prodan)
length_model_prodan <- length(summary(model_prodan)$coef[,1])
model_prodan


# Making Predictions
predictions_model_prodan_training = predict(model_prodan, training)
predictions_model_prodan_testing = predict(model_prodan, testing)


# ----------------------------- Model Logistic ----------------------------- #
# Model Logistic Fitting
model_logistic <- nls(formula = HEIGHT ~ (bh + a/(1 + b * exp(-c * DIAMETER))), 
                      data = training, start=list(
                        a=start_values_logistic["a"], 
                        b=start_values_logistic["b"],
                        c=start_values_logistic["c"]))
summary_model_logistic <- summary(model_logistic)
length_model_logistic <- length(summary(model_logistic)$coef[,1])
model_logistic


# Making Predictions
predictions_model_logistic_training = predict(model_logistic, training)
predictions_model_logistic_testing = predict(model_logistic, testing)


# ----------------------------- Model Chapman ----------------------------- #
# Model Chapman Fitting
model_chapman <- nls(formula = HEIGHT ~ (bh + a *(1 - exp(-b*DIAMETER))^c), 
                     data = training, start=list(
                       a=start_values_chapman["a"], 
                       b=start_values_chapman["b"],
                       c=start_values_chapman["c"]))
summary_model_chapman <- summary(model_chapman)
length_model_chapman <- length(summary(model_chapman)$coef[,1])
model_chapman


# Making Predictions
predictions_model_chapman_training = predict(model_chapman, training)
predictions_model_chapman_testing = predict(model_chapman, testing)


# ----------------------------- Model Weibull ----------------------------- #
# Model Weibull Fitting
model_weibull <- nls(formula = HEIGHT ~ (bh + a * (1 - exp(-b * DIAMETER^c))), 
                     data = training, start=list(
                       a=start_values_weibull["a"], 
                       b=start_values_weibull["b"],
                       c=start_values_weibull["c"]))
summary_model_weibull <- summary(model_weibull)
length_model_weibull <- length(summary(model_weibull)$coef[,1])
model_weibull


# Making Predictions
predictions_model_weibull_training = predict(model_weibull, training)
predictions_model_weibull_testing = predict(model_weibull, testing)


# ----------------------------- Model Gomperz ----------------------------- #
# Model Gomperz Fitting
model_gomperz <- nls(formula = HEIGHT ~ (bh + a * (1 - exp(-b * DIAMETER^c))), 
                     data = training, start=list(
                       a=start_values_gomperz["a"], 
                       b=start_values_gomperz["b"],
                       c=start_values_gomperz["c"]))
summary_model_gomperz <- summary(model_gomperz)
length_model_gomperz <- length(summary(model_gomperz)$coef[,1])
model_gomperz


# Making Predictions
predictions_model_gomperz_training = predict(model_gomperz, training)
predictions_model_gomperz_testing = predict(model_gomperz, testing)


# ----------------------------- Model Sibbesen ----------------------------- #
# Model Sibbesen Fitting
model_sibbesen <- nls(formula = HEIGHT ~ (bh + a * DIAMETER^(b * DIAMETER^(-c))), 
                     data = training, start=list(
                       a=start_values_sibbesen["a"], 
                       b=start_values_sibbesen["b"],
                       c=start_values_sibbesen["c"]))
summary_model_sibbesen <- summary(model_sibbesen)
length_model_sibbesen <- length(summary(model_sibbesen)$coef[,1])
model_sibbesen


# Making Predictions
predictions_model_sibbesen_training = predict(model_sibbesen, training)
predictions_model_sibbesen_testing = predict(model_sibbesen, testing)


# ----------------------------- Model Korf ----------------------------- #
# Model Korf Fitting
model_korf <- nls(formula = HEIGHT ~ (bh + a * exp(-b * DIAMETER^(-c))), 
                      data = training, start=list(
                        a=start_values_korf["a"], 
                        b=start_values_korf["b"],
                        c=start_values_korf["c"]))
summary_model_korf <- summary(model_korf)
length_model_korf <- length(summary(model_korf)$coef[,1])
model_korf


# Making Predictions
predictions_model_korf_training = predict(model_korf, training)
predictions_model_korf_testing = predict(model_korf, testing)


# ----------------------------- Model Ratkowsky ----------------------------- #
# Model Ratkowsky Fitting
model_ratkowsky <- nls(formula = HEIGHT ~ (bh + a * exp(-b/(DIAMETER + c))), 
                  data = training, start=list(
                    a=start_values_ratkowsky["a"], 
                    b=start_values_ratkowsky["b"],
                    c=start_values_ratkowsky["c"]))
summary_model_ratkowsky <- summary(model_ratkowsky)
length_model_ratkowsky <- length(summary(model_ratkowsky)$coef[,1])
model_ratkowsky


# Making Predictions
predictions_model_ratkowsky_training = predict(model_ratkowsky, training)
predictions_model_ratkowsky_testing = predict(model_ratkowsky, testing)


# ----------------------------- Model Hossfeld IV ----------------------------- #
# Model Hossfeld IV Fitting
model_hossfeldIV <- nls(formula = HEIGHT ~ (bh + a/(1 + 1/(b*DIAMETER^c))),
                       data = training, start=list(
                         a=start_values_hossfeldIV["a"], 
                         b=start_values_hossfeldIV["b"],
                         c=start_values_hossfeldIV["c"]))
summary_model_hossfeldIV <- summary(model_hossfeldIV)
length_model_hossfeldIV <- length(summary(model_hossfeldIV)$coef[,1])
model_hossfeldIV


# Making Predictions
predictions_model_hossfeldIV_training = predict(model_hossfeldIV, training)
predictions_model_hossfeldIV_testing = predict(model_hossfeldIV, testing)

