data <- read.csv('Lekcja 04/crime.csv', sep = ",", dec = ",", header = T, stringsAsFactors = FALSE)
data[] <- lapply(data, as.numeric)

# Zad 1
pairs(data)

# Zad 2, 3
M <- cor(data, method = "pearson")
print(M["Rate",])
print(M)

# Zad 4
model <- lm(Rate ~ ., data = data)

r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared

cat("R^2: ", r_squared, "\n")
cat("Adjusted R^2: ", adj_r_squared, "\n")


coefficients <- summary(model)$coefficients
# Wyœwietlenie równania regresji
#cat("Równanie regresji:\n")
#cat("Rate = ", coefficients[1, 1], " + ", paste(coefficients[-1, 1], "*", rownames(coefficients)[-1], collapse = " + "), "\n")




new_data <- data.frame(Age = 150, S = 1, Ed = 90, Ex0 = 50, Ex1 = 60, LF = 500, M = 950, N = 30, NW = 300, U1 = 100, U2 = 40, W = 400, X = 200)

predicted_rate <- predict(model, newdata = new_data)
cat("Prognozowana wartosc Rate: ", predicted_rate, "\n")

# Zad 5
model_a <- lm(Rate ~ Ex1 + X + Ed + Age + U2, data = data)
model_b <- lm(Rate ~ Ex0 + LF + M + N + NW, data = data)

ra_squared <- summary(model_a)$r.squared
rb_squared <- summary(model_b)$r.squared

adj_ra_squared <- summary(model_a)$adj.r.squared
adj_rb_squared <- summary(model_b)$adj.r.squared

print(head(data, 1))

# Nie istotne
#coefficients_a <- summary(model_a)$coefficients
#coefficients_b <- summary(model_b)$coefficients
#cat("Model (b) - Równanie regresji:\n")
#cat("Rate = ", coefficients[1, 1], " + ", paste(coefficients[-1, 1], "*", rownames(coefficients)[-1], collapse = " + "), "\n")

# Zad 6
best_model <- which.max(c(adj_r_squared, adj_ra_squared, adj_rb_squared))
cat("Best model: Model", best_model, "\n")
coefficientsa <- summary(model_a)$coefficients
print(coefficientsa[-1, 1])

# Zad 7
predicted_rate <- predict(model_a, newdata = new_data)
cat("Prognozowana wartosc Rate: ", predicted_rate, "\n")

# Zad 8
residuals <- resid(model_a)
qqnorm(residuals)
qqline(residuals, col = "red")