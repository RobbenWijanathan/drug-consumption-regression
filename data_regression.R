library(readr)
library(dplyr)

url_quantified <- "https://raw.githubusercontent.com/RobbenWijanathan/drug-consumption-regression/main/drug_consumption_quantified.csv"
df <- read_csv(url_quantified)

df

drug_cols <- c("Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis",
               "Choc","Coke","Crack","Ecstasy","Heroin","Ketamine",
               "Legalh","LSD","Meth","Mushrooms","Nicotine","VSA")

df[drug_cols] <- lapply(df[drug_cols], function(x) {
  as.numeric(gsub("CL", "", x))
})

df <- df[df$Semer == "CL0", ]
df

df$Age <- as.factor(df$Age)
df$Gender <- as.factor(df$Gender)
df$Education <- as.factor(df$Education)
df$Country <- as.factor(df$Country)
df$Ethnicity <- as.factor(df$Ethnicity)

df_cleaned <- na.omit(df)
df_cleaned <- df_cleaned %>% select(-Semer, -ID)

target_drugs <- c("Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke","Crack","Ecstasy","Heroin","Ketamine",
               "Legalh","LSD","Meth","Mushrooms","Nicotine","VSA")

predictors <- df_cleaned %>%
  select(-all_of(target_drugs))

X<- model.matrix(~ ., predictors)
X <- as.data.frame(X[, -1])
df_numeric <- cbind(X, df_cleaned[, target_drugs])

predictor_cols <- setdiff(names(df_numeric), target_drugs)

predictor_cols <- predictor_cols[complete.cases(predictor_cols)]
predictor_cols
corr <- cor(df_numeric[, target_drugs], df_numeric[, predictor_cols])

best_predictors_clean <- gsub("[0-9.-]+$", "", predictor_cols)
best_predictors_clean <- unique(best_predictors_clean)
best_predictors_clean

formula_multi <- as.formula(
  paste("cbind(", paste(target_drugs, collapse=","), ") ~ ",
        paste(best_predictors_clean, collapse = " + "))
)
formula_multi

set.seed(123)

n <- nrow(df_numeric)
train_index <- sample(1:n, size = 0.7*n)

# Split data
train_data <- df_cleaned[train_index, ]
test_data  <- df_cleaned[-train_index, ]
model_multi <- lm(formula_multi, data = train_data)

summary(model_multi)

predictions <- predict(model_multi, newdata = test_data)
head(predictions)

actual <- test_data[, drug_cols]

rmse <- sqrt(colMeans((predictions - actual)^2))
rmse

