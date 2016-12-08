knee <- read.csv("knees.csv")

# dimensions of the dataset
dim(knee) 

# list the variables in dataset
names(knee)

# list the structure of dataset
str(knee) 

# list levels of factor Gender in dataset
levels(knee$Gender)

# print first 10 rows and 5 columns of dataset
head(knee[, 1: 5], n = 10)

model = lm(KneeScore_Surgeon ~. , data = knee)
model_back_aic = step(model, direction = "backward", trace = 0)
coef(model_back_aic)
model_for_aic = step(model, direction = "forward", trace = 0)
coef(model_for_aic)
model_both_aic = step(model, direction = "both", trace = 0)
coef(model_both_aic)

n = length(resid(model))
model_back_bic = step(model, direction = "backward", k = log(n), trace = 0)
coef(model_back_bic)
model_for_bic = step(model, direction = "forward", k = log(n), trace = 0)
coef(model_for_bic)
model_both_bic = step(model, direction = "both", k = log(n), trace = 0)
coef(model_both_bic)

calc_loocv_rmse = function(model1) {
  sqrt(mean((resid(model1) / (1 - hatvalues(model1))) ^ 2))
}

calc_loocv_rmse(model)
calc_loocv_rmse(model_back_aic)
calc_loocv_rmse(model_for_aic)
calc_loocv_rmse(model_both_aic)
calc_loocv_rmse(model_back_bic)
calc_loocv_rmse(model_for_bic)
calc_loocv_rmse(model_both_bic)
#calc_loocv_rmse(best_r2_model)
summary(model)$adj.r.squared
summary(model_back_aic)$adj.r.squared
summary(model_for_aic)$adj.r.squared
summary(model_both_aic)$adj.r.squared
summary(model_back_bic)$adj.r.squared
summary(model_for_bic)$adj.r.squared
summary(model_both_bic)$adj.r.squared




model_pat = lm(KneeScore_Patient ~. , data = knee)
model_pat_back_aic = step(model_pat, direction = "backward", trace = 0)
coef(model_pat_back_aic)
model_pat_for_aic = step(model_pat, direction = "forward", trace = 0)
coef(model_pat_for_aic)
model_pat_both_aic = step(model_pat, direction = "both", trace = 0)
coef(model_pat_both_aic)

n = length(resid(model_pat))
model_pat_back_bic = step(model_pat, direction = "backward", k = log(n), trace = 0)
coef(model_pat_back_bic)
model_pat_for_bic = step(model_pat, direction = "forward", k = log(n), trace = 0)
coef(model_pat_back_bic)
model_pat_both_bic = step(model_pat, direction = "both", k = log(n), trace = 0)
coef(model_pat_back_bic)
calc_loocv_rmse = function(model1) {
  sqrt(mean((resid(model1) / (1 - hatvalues(model1))) ^ 2))
}

calc_loocv_rmse(model_pat)
calc_loocv_rmse(model_pat_back_aic)
calc_loocv_rmse(model_pat_for_aic)
calc_loocv_rmse(model_pat_both_aic)
calc_loocv_rmse(model_pat_back_bic)
calc_loocv_rmse(model_pat_for_bic)
calc_loocv_rmse(model_pat_both_bic)
#calc_loocv_rmse(best_r2_model)
summary(model_pat)$adj.r.squared
summary(model_pat_back_aic)$adj.r.squared
summary(model_pat_for_aic)$adj.r.squared
summary(model_pat_both_aic)$adj.r.squared
summary(model_pat_back_bic)$adj.r.squared
summary(model_pat_for_bic)$adj.r.squared
summary(model_pat_both_bic)$adj.r.squared






library("leaps")
all_mod = summary(regsubsets(KneeScore_Surgeon ~ ., data = knee,really.big=T))
all_mod$adjr2
(best_r2_ind = which.max(all_mod$adjr2))
all_mod$which[best_r2_ind, ]
best_r2_model = lm(lpsa ~ .-gleason, data = prostate)