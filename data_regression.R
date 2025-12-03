library(readr)
library(dplyr)

url_quantified <- "https://raw.githubusercontent.com/RobbenWijanathan/drug-consumption-regression/main/drug_consumption_quantified.csv"
df <- read_csv(url_quantified)

drug_cols <- c("Alcohol","Amphet","Amyl","Benzos","Caff","Cannabis",
               "Choc","Coke","Crack","Ecstasy","Heroin","Ketamine",
               "Legalh","LSD","Meth","Mushrooms","Nicotine","VSA")

df[drug_cols] <- lapply(df[drug_cols], function(x) {
  as.numeric(gsub("CL", "", x))
})

df$Age <- as.factor(df$Age)
df$Gender <- as.factor(df$Gender)
df$Education <- as.factor(df$Education)
df$Country <- as.factor(df$Country)
df$Ethnicity <- as.factor(df$Ethnicity)

df_cleaned <- na.omit(df)
df_cleaned

predictors <- df_cleaned %>%
  select(-ID, -all_of(drug_cols))

X<- model.matrix(~ ., predictors)
X <- as.data.frame(X[, -1])
df_numeric <- cbind(X, df_cleaned[, drug_cols])


# --- FIXED CORRELATION ---
predictor_cols <- setdiff(names(df_numeric), drug_cols)
corr <- cor(df_numeric[, drug_cols], df_numeric[, predictor_cols])

selected_predictors <- apply(corr, 2, function(col) any(abs(col) >= 0.25))
best_predictors <- names(selected_predictors)[selected_predictors]
# Check predictors
best_predictors
best_predictors_clean <- gsub("[0-9.-]+$", "", best_predictors)  # Remove trailing numbers
best_predictors_clean <- unique(best_predictors_clean)

# --- BUILD FORMULA ---
formula_multi <- as.formula(
  paste("cbind(", paste(drug_cols, collapse=","), ") ~ ",
        paste(best_predictors_clean, collapse = " + "))
)
formula_multi

set.seed(123)

# Create index
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
