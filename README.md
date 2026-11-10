# Career Success Econometrics: Salary, Job Offers and Gender Signals

An econometric study of what drives graduate career outcomes, built in R as the final project for *Microeconometric Modelling for Business Analytics*. Three models, each answering a different hiring question:

| Model | Outcome | Business question |
|---|---|---|
| **Linear (OLS)** | Starting salary | Which candidate attributes set a fair, competitive salary benchmark? |
| **Poisson (GLM)** | Number of job offers | Which attributes signal high market demand? |
| **Logit (GLM)** | Gender (1 = male) | Do men and women show different "signals", creating unintended bias in hiring? |

The full write-up, including diagnostics, plots and recommendations, is in [`Executive_Report_G4.pdf`](Executive_Report_G4.pdf).

## Data

[Education and Career Success](https://www.kaggle.com/datasets/adilshamim8/education-and-career-success) (Kaggle, Shamim 2024): 400 graduates from US universities, 20 variables covering grades, internships, projects, certifications, soft skills, job offers and salary. The data is not redistributed here; download it from Kaggle.

## Key findings

**Linear model (n = 400, adjusted R² = 0.9765).** Stepwise selection (AIC) in all three directions gives the same final model, with AIC 7877.98 against 7885.09 for the full model. Prediction error is low: MAPE 4.40%, MAE of about $3,387, RMSE of about $4,501.

| Predictor | Effect on starting salary (per unit) |
|---|---|
| University GPA | +$22,004 |
| Certifications | +$14,132 |
| Job offers | +$9,719 |
| Gender (male) | +$4,138 |
| Projects completed | +$2,002 |
| Years to promotion | -$2,778 |
| Work-life balance | -$1,645 |
| Internships completed | -$15,298 |

Lasso regularisation keeps the main predictors, and the negative internship coefficient persists. Diagnostics show high multicollinearity (VIF), heteroskedasticity (Breusch-Pagan) and non-normal residuals (Jarque-Bera); the report discusses why inference is still reasonable at n = 400.

**Poisson model.** Internships completed is the only significant predictor of job offers (IRR 1.39, p = 0.016): each extra internship multiplies expected offers by about 1.39. The model shows strong under-dispersion (dispersion statistic 0.055).

**Logit model (McFadden pseudo-R² = 0.138).** Three significant predictors of being male:

| Predictor | Odds ratio | Reading |
|---|---|---|
| Certifications | 6.35 | Odds of being male rise sharply with each certification |
| Projects completed | 0.33 | Odds of being male fall by about 67% per project |
| University GPA | 0.05 | Higher GPA is associated with lower odds of being male |

**Takeaway.** Skills-based signals (GPA, certifications, projects, offers) drive pay far more than background variables. Men and women appear to signal ability through different credentials, so recruiting on a single type of credential risks unintended gender bias.

## Methods

OLS with stepwise selection and Lasso, residual diagnostics (VIF, Breusch-Pagan, Jarque-Bera, Cook's distance), Poisson regression with incidence rate ratios and dispersion checks, and logistic regression with odds ratios and pseudo-R². The script also includes a quasi-Poisson extension that tests whether gender is associated with the number of job offers (hypothesis H3).

## Limitations

- **Observational data.** Coefficients are associations, not causal effects. The negative internship effect in particular is hard to interpret and may reflect multicollinearity with GPA, certifications and offers.
- **Gender as an outcome.** The logit model treats gender as the dependent variable to explore differences in signalling; it does not mean gender is caused by these attributes.
- **Very high explanatory power.** An R² near 0.98 is unusually high for salary data, so the results may partly reflect how this dataset was constructed. Validate on real hiring data before using the model to set pay.
- **Under-dispersion in the Poisson model** means standard errors may be conservative; a quasi-Poisson or generalised Poisson model is a natural extension.

## Repository contents

| File | Description |
|---|---|
| `Executive_Report_G4.pdf` | Full executive report |
| `final_project.R` | Complete, commented R script: data preparation, EDA, OLS, Lasso, Poisson and Logit |

Running the script also creates a `figures/` folder with the diagnostic plots.

## Run it yourself

1. Install the packages once:
   ```r
   install.packages(c("car", "lmtest", "tseries", "glmnet", "pscl"))
   ```
2. Download the dataset from [Kaggle](https://www.kaggle.com/datasets/adilshamim8/education-and-career-success) and save it as `data/education_career_success.csv`.
3. Open `final_project.R` in RStudio, set the working directory to the repository folder, and run the script from top to bottom.

## Authors

Group 4: Leila Abduraufova, Irene Peña, Katharina Rodríguez, Xinyi Zhu
