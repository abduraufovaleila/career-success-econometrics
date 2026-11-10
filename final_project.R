# ============================================================
# CAREER SUCCESS ECONOMETRICS
# Microeconometric Modelling for Business Analytics (R)
# Group 4
# ============================================================
#
# HOW TO RUN
#   1. Download the dataset from Kaggle ("Education and Career Success",
#      Shamim 2024) and save it as  data/education_career_success.csv
#   2. Open this file in RStudio (set the working directory to the
#      repository folder) and run it top to bottom.
#   Figures are written to the  figures/  folder.
#
# REQUIRED PACKAGES (install once):
#   install.packages(c("car", "lmtest", "tseries", "glmnet", "pscl"))

# ============================================
# PROJECT OBJECTIVE
# ============================================

# This project analyzes factors affecting students' career outcomes.
# Specifically:
# 1. Identify factors influencing Starting_Salary (continuous outcome -> Linear Model)
# 2. Convert relevant outcomes into discrete/binary variables where needed
# 3. Explore relationships between academic performance, skills, and job success
# The dataset contains students from colleges in different States of the US.

# ============================================
# VARIABLE DESCRIPTIONS
# ============================================

# Student_ID            : Unique identifier (categorical)
# Age                   : Student age (continuous)
# Gender                : Male/Female (categorical)
# High_School_GPA       : GPA in high school (continuous)
# SAT_Score             : SAT score (continuous)
# University_GPA        : University GPA (continuous)
# Field_of_Study        : Major (categorical)
# Internships_Completed : Number of internships (discrete count)
# Projects_Completed    : Number of projects (discrete count)
# Certifications        : Number of certifications (discrete count)
# Soft_Skills_Score     : Soft skills rating (scale 1-10, ordinal)
# Networking_Score      : Networking ability score (scale 1-10, ordinal)
# Job_Offers            : Number of job offers (discrete count)
# Starting_Salary       : First salary (continuous -> LM response)
# Career_Satisfaction   : Satisfaction score (1-10, ordinal)
# Years_to_Promotion    : Years to promotion (discrete)
# Current_Job_Level     : Entry/Mid/Senior (categorical, ordinal)
# Work_Life_Balance     : Score 1-10 (ordinal)
# Entrepreneurship      : Yes/No (binary)
# Dichotomous_Gender    : 1 = Male, 0 = Female (binary, created below)

# Responses used in the three models
#   Y continuous (Linear Model)  -> Starting_Salary
#   Y count      (Poisson)       -> Job_Offers
#   Y binary     (Logit)         -> Dichotomous_Gender

# ============================================
# 0. SETUP
# ============================================

library(car)       # vif()
library(lmtest)    # bptest()
library(tseries)   # jarque.bera.test()
library(glmnet)    # Lasso
library(pscl)      # pR2() for McFadden pseudo R-squared

data_path <- "data/education_career_success.csv"
dir.create("figures", showWarnings = FALSE)

# ============================================
# 1. DATA PREPARATION
# ============================================

education_career_success <- read.csv(data_path, stringsAsFactors = FALSE)
education <- education_career_success

str(education)

# Missing values
cat("\nMissing values per column:\n")
print(colSums(is.na(education)))
education <- na.omit(education)

# Basic consistency filters (adjust if your report used different rules)
education <- subset(education,
                    Starting_Salary > 0 &
                    University_GPA  > 0 & University_GPA <= 4 &
                    Internships_Completed >= 0 &
                    Projects_Completed    >= 0 &
                    Certifications        >= 0 &
                    Job_Offers            >= 0)

# Type conversions
education$Gender             <- factor(education$Gender, levels = c("Female", "Male"))
education$Field_of_Study     <- factor(education$Field_of_Study)
education$Current_Job_Level  <- factor(education$Current_Job_Level)
education$Entrepreneurship   <- factor(education$Entrepreneurship)

# Binary version of gender for the Logit model (1 = Male, 0 = Female)
education$Dichotomous_Gender <- ifelse(education$Gender == "Male", 1, 0)

cat("\nObservations after cleaning:", nrow(education), "\n")

# ============================================
# 2. SUMMARY STATISTICS AND EDA
# ============================================

summary(education)

cat("\nGender balance:\n");        print(table(education$Gender))
cat("\nJob offers distribution:\n"); print(table(education$Job_Offers))

num_vars <- c("Age", "High_School_GPA", "SAT_Score", "University_GPA",
              "Internships_Completed", "Projects_Completed", "Certifications",
              "Soft_Skills_Score", "Networking_Score", "Job_Offers",
              "Starting_Salary", "Career_Satisfaction", "Years_to_Promotion",
              "Work_Life_Balance")
cat("\nCorrelation with Starting_Salary:\n")
print(round(sort(cor(education[, num_vars])[, "Starting_Salary"],
                 decreasing = TRUE), 3))

png("figures/01_eda_distributions.png", width = 1000, height = 400)
par(mfrow = c(1, 3))
hist(education$Starting_Salary, main = "Starting salary",
     xlab = "USD", col = "grey80")
barplot(table(education$Job_Offers), main = "Job offers",
        xlab = "Number of offers", col = "grey80")
boxplot(Starting_Salary ~ Gender, data = education,
        main = "Salary by gender", ylab = "USD", col = "grey80")
dev.off()

# ============================================
# 3. LINEAR REGRESSION (OLS) -> Starting_Salary
# ============================================

# Model 0: all candidate predictors
# (Student_ID is an identifier; Dichotomous_Gender duplicates Gender)
model_0 <- lm(Starting_Salary ~ . - Student_ID - Dichotomous_Gender,
              data = education)
summary(model_0)

# Model 5: only predictors that remain significant
model_5 <- lm(Starting_Salary ~ University_GPA + Internships_Completed +
                Projects_Completed + Certifications + Work_Life_Balance +
                Gender + Job_Offers + Years_to_Promotion,
              data = education)
summary(model_5)

# --- 3.1 Stepwise selection (AIC): backward, forward and both directions
model_null <- lm(Starting_Salary ~ 1, data = education)

step_backward <- step(model_0, direction = "backward", trace = 0)
step_forward  <- step(model_null,
                      scope = formula(model_0),
                      direction = "forward", trace = 0)
step_both     <- step(model_0, direction = "both", trace = 0)

cat("\nStepwise (backward):", deparse(formula(step_backward)), "\n")
cat("Stepwise (forward): ", deparse(formula(step_forward)),  "\n")
cat("Stepwise (both):    ", deparse(formula(step_both)),     "\n")
cat("\nAIC full model:   ", AIC(model_0), "\n")
cat("AIC final model:  ", AIC(model_5), "\n")

# --- 3.2 Diagnostics
png("figures/02_ols_diagnostics.png", width = 900, height = 900)
par(mfrow = c(2, 2))
plot(model_5)
dev.off()
par(mfrow = c(1, 1))

cat("\nVariance inflation factors:\n")
print(vif(model_5))

cat("\nBreusch-Pagan test (H0: homoskedasticity):\n")
print(bptest(model_5))

cat("\nJarque-Bera test (H0: normal residuals):\n")
print(jarque.bera.test(residuals(model_5)))

# Cook's distance: influential observations
cooks <- cooks.distance(model_5)
cat("\nObservations with Cook's distance > 4/n:",
    sum(cooks > 4 / nrow(education)), "\n")

# --- 3.3 Prediction error metrics
pred <- fitted(model_5)
obs  <- education$Starting_Salary
mape <- mean(abs((obs - pred) / obs)) * 100
mae  <- mean(abs(obs - pred))
rmse <- sqrt(mean((obs - pred)^2))
cat(sprintf("\nMAPE: %.2f%% | MAE: %.0f | RMSE: %.0f\n", mape, mae, rmse))

# --- 3.4 Regularization (Lasso) with leave-one-out cross-validation
x <- model.matrix(Starting_Salary ~ . - Student_ID - Dichotomous_Gender,
                  data = education)[, -1]
y <- education$Starting_Salary

set.seed(123)
lasso_cv <- cv.glmnet(x, y, alpha = 1,
                      nfolds = nrow(x), grouped = FALSE,   # leave-one-out
                      type.measure = "mae")

png("figures/03_lasso_cv.png", width = 800, height = 500)
plot(lasso_cv)
dev.off()

cat("\nLasso lambda (min MAE):", lasso_cv$lambda.min, "\n")
cat("Lasso lambda (1-SE):   ", lasso_cv$lambda.1se, "\n")
cat("\nCoefficients at lambda.min:\n")
print(coef(lasso_cv, s = "lambda.min"))
cat("\nCoefficients at lambda.1se:\n")
print(coef(lasso_cv, s = "lambda.1se"))

# ============================================
# 4. POISSON REGRESSION -> Job_Offers
# ============================================

modelo_poisson <- glm(Job_Offers ~ University_GPA + Internships_Completed +
                        Projects_Completed + Certifications +
                        Soft_Skills_Score + Networking_Score,
                      family = poisson(link = "log"),
                      data = education)
summary(modelo_poisson)

# Incidence rate ratios (IRR): multiplicative effect on expected offers
irr <- exp(coef(modelo_poisson))
cat("\nIncidence rate ratios:\n")
print(round(irr, 4))

# Goodness of fit: share of deviance explained
null_deviance  <- modelo_poisson$null.deviance
resid_deviance <- modelo_poisson$deviance
reduccion_error <- (null_deviance - resid_deviance) / null_deviance
cat("\nDeviance reduction (null vs residual):", round(reduccion_error, 4), "\n")

# Dispersion (< 1 under-dispersion, > 1 over-dispersion)
dispersion_deviance <- resid_deviance / modelo_poisson$df.residual
dispersion_pearson  <- sum(residuals(modelo_poisson, "pearson")^2) /
                       modelo_poisson$df.residual
cat("Dispersion (deviance/df):", round(dispersion_deviance, 4), "\n")
cat("Dispersion (Pearson/df): ", round(dispersion_pearson, 4), "\n")

png("figures/04_poisson_residuals.png", width = 800, height = 600)
plot(modelo_poisson, which = 1)
dev.off()

# Reduced model: only the significant predictor
modelo_poisson_reducido <- glm(Job_Offers ~ Internships_Completed,
                               family = poisson(link = "log"),
                               data = education)
summary(modelo_poisson_reducido)
cat("\nIRR (reduced model):\n")
print(round(exp(coef(modelo_poisson_reducido)), 4))

# --- Extension: test hypothesis H3 (do men receive more job offers?)
# Quasi-Poisson is used because the data is clearly under-dispersed, which
# makes the ordinary Poisson standard errors conservative.
modelo_poisson_h3 <- glm(Job_Offers ~ University_GPA + Internships_Completed +
                           Projects_Completed + Certifications +
                           Soft_Skills_Score + Networking_Score + Gender,
                         family = quasipoisson(link = "log"),
                         data = education)
cat("\nH3 check (quasi-Poisson with Gender):\n")
print(summary(modelo_poisson_h3)$coefficients["GenderMale", ])
cat("IRR for Gender (Male vs Female):",
    round(exp(coef(modelo_poisson_h3)["GenderMale"]), 4), "\n")

# ============================================
# 5. LOGISTIC REGRESSION -> Dichotomous_Gender (1 = Male)
# ============================================

modelo_logit <- glm(Dichotomous_Gender ~ University_GPA + Internships_Completed +
                      Projects_Completed + Certifications +
                      Soft_Skills_Score + Networking_Score,
                    family = binomial(link = "logit"),
                    data = education)
summary(modelo_logit)

# Readjusted model: significant predictors only
modelo_logit_2 <- glm(Dichotomous_Gender ~ University_GPA +
                        Projects_Completed + Certifications,
                      family = binomial(link = "logit"),
                      data = education)
summary(modelo_logit_2)

# Odds ratios and 95% confidence intervals
odds_ratios <- exp(coef(modelo_logit_2))
or_ci <- exp(confint.default(modelo_logit_2))
cat("\nOdds ratios (readjusted model):\n")
print(round(cbind(OR = odds_ratios, or_ci), 4))

# Model performance
cat("\nPseudo R-squared:\n")
print(round(pR2(modelo_logit_2), 4))

dispersion_logit <- sum(residuals(modelo_logit_2, "pearson")^2) /
                    modelo_logit_2$df.residual
cat("Dispersion (Pearson/df):", round(dispersion_logit, 3), "\n")

# Classification accuracy at a 0.5 cut-off
pred_prob  <- predict(modelo_logit_2, type = "response")
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
cat("\nConfusion matrix (rows = actual, columns = predicted):\n")
print(table(Actual = education$Dichotomous_Gender, Predicted = pred_class))
cat("Accuracy:", round(mean(pred_class == education$Dichotomous_Gender), 3), "\n")

# Odds ratio plot
png("figures/05_odds_ratios.png", width = 800, height = 500)
or_plot <- cbind(OR = odds_ratios, or_ci)[-1, ]
plot(or_plot[, "OR"], seq_len(nrow(or_plot)), log = "x", pch = 19,
     xlim = range(or_plot), yaxt = "n", ylab = "",
     xlab = "Odds ratio of being male (log scale)")
segments(or_plot[, "2.5 %"], seq_len(nrow(or_plot)),
         or_plot[, "97.5 %"], seq_len(nrow(or_plot)))
axis(2, at = seq_len(nrow(or_plot)), labels = rownames(or_plot), las = 1)
abline(v = 1, lty = 2, col = "red")
dev.off()

# ============================================
# 6. REPRODUCIBILITY
# ============================================
sessionInfo()
