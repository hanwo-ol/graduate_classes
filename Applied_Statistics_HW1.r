# if i use korean letters in long-Rscripts, My R always crashes, so i used english in this file...

#### 3 ####

# x: weight in 1000kg
# y: energy consumption in 1000 Btu/km
weight_x <- c(0.9, 1.3, 2.1, 2.5, 2.4, 1.7, 0.7, 1.2, 1.6)
energy_y <- c(2.0, 2.6, 4.3, 5.8, 5.1, 3.2, 1.8, 2.3, 3.0)

# Create a data frame
car_data <- data.frame(weight_x, energy_y)

# Basic Settings
n <- length(weight_x)
x_bar <- mean(weight_x)
y_bar <- mean(energy_y)
Sxx <- sum((weight_x - x_bar)^2)
Syy <- sum((energy_y - y_bar)^2)
Sxy <- sum((weight_x - x_bar) * (energy_y - y_bar))

cat("Basic Statistics:\n");cat("n =", n, "\n");cat("Mean x =", x_bar, "\n");cat("Mean y =", y_bar, "\n");cat("Sxx =", Sxx, "\n");cat("Syy =", Syy, "\n");cat("Sxy =", Sxy, "\n\n")

# --- (1) Scatter Plot ---
`plot(weight_x, energy_y,
     main = "Scatter Plot with Fitted Regression Line",
     xlab = "Weight (1000 kg)",
     ylab = "Energy Consumption (1000 Btu/km)",
     pch = 19,
     col = "blue")`
grid()

# --- (2) Define Simple Linear Regression Model ---
cat("Y_i = beta_0 + beta_1 * x_i + epsilon_i\n");cat("where Y_i is energy consumption, x_i is weight,\n")

# --- (3) Fit Regression Line using Least Squares ---
model <- lm(energy_y ~ weight_x, data = car_data)
summary(model)

# Extract coefficients
coefficients <- coef(model)
beta0_hat <- coefficients[1] # Intercept
beta1_hat <- coefficients[2] # Slope for weight_x

cat("Intercept (beta0_hat):", round(beta0_hat, 3), "\n"); cat("Slope     (beta1_hat):", round(beta1_hat, 3), "\n"); cat("Fitted Regression Line: y_hat =", round(beta0_hat, 3), "+", round(beta1_hat, 3), "* x\n\n")

# --- (4) Add Fitted Line to Scatter Plot ---
plot(weight_x, energy_y,
     main = "Scatter Plot with Fitted Regression Line",
     xlab = "Weight (1000 kg)",
     ylab = "Energy Consumption (1000 Btu/km)",
     pch = 19,
     col = "blue")
abline(model, col = "red", lwd = 2.5) 
legend("topleft", legend = paste("ŷ =", round(beta0_hat, 3), "+", round(beta1_hat, 3), "x"),
       col = "red", lty = 1, lwd = 2, bty = "n") 

# --- (5) ANOVA Table and Significance Test (alpha = 0.05) ---
anova_table <- anova(model)
(anova_table)
f_value <- anova_table$`F value`[1]
p_value_f <- anova_table$`Pr(>F)`[1]
alpha <- 0.05

cat("   H0: beta_1 = 0\n"); cat("   H1: beta_1 != 0\n"); cat("   Significance Level (alpha):", alpha, "\n"); cat("   F-statistic:", round(f_value, 2), "\n"); cat("   P-value:", p_value_f, "\n") # Format p-value

if (p_value_f < alpha) {
  cat("   Decision: Reject H0.\n");  cat("   Conclusion: At alpha = 0.05, there is a statistically significant linear relationship\n");cat("               between car weight and energy consumption.\n\n")
} else {
  cat("   Decision: Fail to reject H0.\n"); cat("   Conclusion: At alpha = 0.05, there is not enough evidence to conclude a significant\n");cat("               linear relationship between car weight and energy consumption.\n\n")
}
model_summary <- summary(model)

# --- (6) R-squared and Correlation Coefficient ---
r_squared <- model_summary$r.squared
correlation_r <- cor(weight_x, energy_y) # Pearson correlation
r_squared
correlation_r


# --- (7) 90% Confidence Interval for beta_0 ---
conf_int_beta0 <- confint(model, parm = "(Intercept)", level = 0.90)
conf_int_beta0


# --- (8) 95% Confidence Interval for beta_1 ---
conf_int_beta1 <- confint(model, parm = "weight_x", level = 0.95)
conf_int_beta1



# --- (9) Hypothesis Test  ---
beta1_test_results <- model_summary$coefficients["weight_x", ]
t_stat_beta1 <- beta1_test_results["t value"]
p_value_beta1 <- beta1_test_results["Pr(>|t|)"]

alpha
t_stat_beta1
p_value_beta1

if (p_value_beta1 < alpha) {
  cat("   Decision: Reject H0.\n");  cat("   Conclusion: At alpha = 0.05, the slope (beta_1) is significantly different from 0.\n\n")
} else {
  cat("   Decision: Fail to reject H0.\n");   cat("   Conclusion: At alpha = 0.05, there is not enough evidence to conclude the slope is\n");cat("               significantly different from 0.\n\n")
}



# --- (10) Calculate Residuals and Check Sum ---
residuals_e <- residuals(model)
sum_residuals <- sum(residuals_e)

print(residuals_e)
sum_residuals # nearly 0



# --- (11) Weighted Sum of Residuals by x_i ---
sum_x_residuals <- sum(weight_x * residuals_e)
sum_x_residuals


# --- (12) Weighted Sum of Residuals by y_hat_i ---
fitted_y <- fitted(model)
sum_yhat_residuals <- sum(fitted_y * residuals_e)
(fitted_y)
(sum_yhat_residuals)


# --- (13) Predict Mean Consumption and Calculate Ratio ---
new_data_3000 <- data.frame(weight_x = 3.0)
new_data_1000 <- data.frame(weight_x = 1.0)
pred_3000 <- predict(model, newdata = new_data_3000)
pred_1000 <- predict(model, newdata = new_data_1000)

pred_3000;pred_1000

# Calculate ratio
ratio_pred <- pred_3000 / pred_1000
ratio_pred

# --- (14) 90% Confidence and Prediction Intervals for x = 3.0 ---
conf_interval_mean <- predict(model, newdata = new_data_3000, interval = "confidence", level = 0.90)
pred_interval_individual <- predict(model, newdata = new_data_3000, interval = "prediction", level = 0.90)

conf_interval_mean[2]; conf_interval_mean[3]
pred_interval_individual[2]; pred_interval_individual[3]


# --- (15) Residual Plot and Analysis ---
# Plot 1: Residuals vs Fitted Values
plot(fitted(model), residuals(model),
     main = "Residuals vs. Fitted Values",
     xlab = "Fitted values (ŷ)",
     ylab = "Residuals (e)",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)

# Plot 2: Residuals vs Predictor (Weight)
plot(weight_x, residuals(model),
     main = "Residuals vs. Predictor (Weight)",
     xlab = "Weight (x)",
     ylab = "Residuals (e)",
     pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
# curved!!!!

#### 4 ####
n <- 50
set.seed(1015)

i <- 1:n
x <- i / n
z <- sin(i * pi / n)
epsilon <- rnorm(n, mean = 0, sd = 1)

# randomly selected values!
# Case 1: (y = beta0 + beta1*x + beta2*x^2 + epsilon)
beta0_1 <- 0.5
beta1_1 <- 1
beta2_1 <- 10 

y1_true <- beta0_1 + beta1_1 * x + beta2_1 * x^2 + epsilon
model1 <- lm(y1_true ~ x)
residuals1 <- residuals(model1)
summary(model1)


# Case 2: (y = beta0 + beta1*x + beta2*x^2 + beta3*x^3 + epsilon)
beta0_2 <- 0.2
beta1_2 <- -5
beta2_2 <- 30
beta3_2 <- -25

y2_true <- beta0_2 + beta1_2 * x + beta2_2 * x^2 + beta3_2 * x^3 + epsilon
model2 <- lm(y2_true ~ x) 
residuals2 <- residuals(model2)
model2

# Case 3: (y = beta0 + beta1*x + beta2*z + epsilon)
beta0_3 <- 1
beta1_3 <- 1.5
beta2_3 <- 8

y3_true <- beta0_3 + beta1_3 * x + beta2_3 * z + epsilon
model3 <- lm(y3_true ~ x)
residuals3 <- residuals(model3)


# --- Plotting ---
par(mfrow = c(3, 2), mar = c(4.5, 4.5, 2.5, 1), oma = c(0, 0, 2, 0))

# Case 1 Plots
plot(x, y1_true, main = "Case 1", xlab = "x", ylab = "y", pch = 16, cex = 0.8)
plot(x, residuals1, main = "Case 1 Residuals vs x",
     xlab = "x", ylab = "Residuals", pch = 16, cex = 0.8)
abline(h = 0, col = "red", lty = 2, lwd = 2)

# Case 2 Plots
plot(x, y2_true, main = "Case 2", xlab = "x", ylab = "y", pch = 16, cex = 0.8)
plot(x, residuals2, main = "Case 2 Residuals vs x",
     xlab = "x", ylab = "Residuals", pch = 16, cex = 0.8)
abline(h = 0, col = "red", lty = 2, lwd = 2)

# Case 3 Plots
plot(x, y3_true, main = "Case 3", xlab = "x", ylab = "y", pch = 16, cex = 0.8)
plot(x, residuals3, main = "Case 3 Residuals vs x",
     xlab = "x", ylab = "Residuals", pch = 16, cex = 0.8)
abline(h = 0, col = "red", lty = 2, lwd = 2)

mtext("Simulation with Enhanced Beta Coefficients", outer = TRUE, cex = 1.2)

par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

# if we use lm() function for these non-linear(?) equations, plot looks ridiculous.

#### 5 ####
x <- c(0, 0, 3, 3, 6, 6, 9, 9, 12, 12)
y <- c(8.5, 8.4, 7.9, 8.1, 7.8, 7.6, 7.3, 7.0, 6.8, 6.7)
alpha <- 0.05

fish_data <- data.frame(x, y)
# --- (1) Lack-of-Fit Test ---

# H0: The linear model (y = β₀ + β₁x + ε) is adequate.
# H1: The linear model is not adequate (there is lack of fit).

linear_model <- lm(y ~ x, data = fish_data)
summary(linear_model) 
plot(x,y)
abline(linear_model)

full_model <- lm(y ~ factor(x), data = fish_data)
summary(full_model)

plot(x,y)
abline(linear_model)
abline(full_model)

lack_of_fit_test <- anova(linear_model, full_model)
lack_of_fit_test

p_value_lof <- lack_of_fit_test$"Pr(>F)"[2]
p_value_lof

# Hypotheses
## H0 (Null Hypothesis): The linear model (`y ~ x`) is adequate. There is no significant lack of fit.
## H1 (Alternative Hypothesis): The linear model is not adequate. There is a significant lack of fit.

# Decision Rule Compare the p-value to the significance level.
## If p-value ≤ alpha (0.05), reject H0.
## If p-value > alpha (0.05), fail to reject H0.

# so... The p-value is 0.774, which is much greater than alpha (0.05).
## fail to reject the null hypothesis (H0)
## it means... theres is no statistically significant evidence of lack of fit.(at 5% sig level)
# Decision
if (p_value_lof < alpha) {
  cat("Conclusion: Reject H0. The p-value (", p_value_lof, ") is less than alpha (", alpha, ").\n")
  cat("There is significant evidence of lack of fit. The linear model is NOT adequate.\n")
  linear_model_adequate <- FALSE
} else {
  cat("Conclusion: Fail to reject H0. The p-value (", p_value_lof, ") is not less than alpha (", alpha, ").\n")
  cat("There is no significant evidence of lack of fit. The linear model is considered adequate.\n")
  linear_model_adequate <- TRUE
}


# --- (2) 95% Confidence Interval for β₁ ---

if (linear_model_adequate) {
  conf_intervals <- confint(linear_model, level = 0.95)
  print(conf_intervals)
  cat("the average freshness score decreases by an amount between","-0.162, -0.121")
} else {
  cat("The linear model was found to be inadequate")
}

#### 6 ####
x <- c(10, 20, 30, 40, 50, 60, 70)
y_a <- c(9.8, 12.5, 14.9, 16.5, 22.4, 24.1, 25.8)
y_b <- c(15.0, 14.5, 16.5, 19.1, 22.3, 20.8, 22.4)
alpha <- 0.05

speeds <- rep(x, 2) 
tread_life <- c(y_a, y_b) # tire
company <- factor(rep(c("A", "B"), each = length(x))) 

tire_data <- data.frame(speed = speeds, life = tread_life, company = company)

print(tire_data)
str(tire_data)

# --- (1) Scatter Plots ---
plot(life ~ speed, data = tire_data,
     col = ifelse(company == "A", "blue", "red"),
     pch = ifelse(company == "A", 1, 2),
     xlab = "Speed (x)",
     ylab = "Tread Life (y)")

model_a <- lm(life ~ speed, data = tire_data, subset = (company == "A"))
model_b <- lm(life ~ speed, data = tire_data, subset = (company == "B"))
abline(model_a, col = "blue", lty = 2)
abline(model_b, col = "red", lty = 2)

# --- (2) Test for Identical Lines ---
# H0: The regression lines are identical (β₀A = β₀B and β₁A = β₁B)
# H1: The regression lines are different (at least one intercept or slope differs)

model_reduced <- lm(life ~ speed, data = tire_data)
model_full <- lm(life ~ speed * company, data = tire_data)

# Compare the models using ANOVA (F-test): are they identifcal?
test_identical <- anova(model_reduced, model_full)
(test_identical)

p_value_identical <- test_identical$"Pr(>F)"[2]
p_value_identical

# Decision
if (p_value_identical < alpha) {
  cat("Conclusion: Reject H0. The p-value (", p_value_identical, ") is less than alpha (", alpha, ").\n")
  cat("There is significant evidence that the regression lines for Company A and B are different.")
} else {
  cat("Conclusion: Fail to reject H0. The p-value (", p_value_identical, ") is not less than alpha (", alpha, ").\n")
  cat("There is no significant evidence that the regression lines are different.")
}


# --- (3) Test for Equal Slopes ---
# H0: The slopes are equal (β₁A = β₁B) - allows intercepts to differ
# H1: The slopes are different (β₁A ≠ β₁B)

model_parallel <- lm(life ~ speed + company, data = tire_data) # dif intercept

# Full Model: I already made this model

# Compare the parallel lines model to the full model using ANOVA
test_slopes <- anova(model_parallel, model_full)
(test_slopes)

p_value_slopes <- test_slopes$"Pr(>F)"[2]
p_value_slopes

# Decision
if (p_value_slopes < alpha) {
  cat("Conclusion: Reject H0. The p-value (", p_value_slopes, ") is less than alpha (", alpha, ").\n")
  cat("There is significant evidence that the slopes of the regression lines are different for Company A and B.\n")
} else {
  cat("Conclusion: Fail to reject H0. The p-value (", p_value_slopes, ") is not less than alpha (", alpha, ").\n")
  cat("There is no significant evidence that the slopes are different.\n")
}


#### 7 ####
x <- c(6.4, 16.1, 42.1, 2.1, 30.7, 32.1, 7.2, 3.4, 20.8, 1.5) 
y <- c(1.7, 2.7, 4.9, 0.3, 3.9, 4.1, 1.2, 0.5, 3.3, 0.2) 
supermarket_data <- data.frame(x, y)

# --- (1) Scatter Plot ---
plot(x, y,
     xlab = "Purchase Amount (x, thousand Won)",
     ylab = "Checkout Time (y, minutes)",
     main = "Checkout Time vs. Purchase Amount",
     pch = 16, # Solid points
     col = "blue")

# --- (2) Simple Linear Regression & R-squared ---
model_linear <- lm(y ~ x, data = supermarket_data)
r_squared_linear_orig <- summary(model_linear)$r.squared
(r_squared_linear_orig)

# --- (3) comparison ---
comparable_r_squared <- list()
fitted_models <- list()
model_formulas_text <- list()

# --- Baseline: Simple Linear Model ---
fitted_models[["Linear: y ~ x"]] <- model_linear
model_formulas_text[["Linear: y ~ x"]] <- "y ~ x"
comparable_r_squared[["Linear: y ~ x"]] <- r_squared_linear_orig

# --- Model (a): y = exp(β₀ + β₁x + ε) => log(y) = β₀ + β₁x + ε' transformed ---
log_y <- log(supermarket_data$y)
model_a <- lm(log_y ~ x, data = supermarket_data)
fitted_y_a <- exp(fitted(model_a))
comparable_r_squared[["a) log(y) ~ x"]] <- cor(supermarket_data$y, fitted_y_a)^2
fitted_models[["a) log(y) ~ x"]] <- model_a
model_formulas_text[["a) log(y) ~ x"]] <- "log(y) ~ x"

# --- Model (b): y = β₀ + β₁√x + ε => y ~ sqrt(x) ---
# Need x >= 0
sqrt_x <- sqrt(supermarket_data$x)
model_b <- lm(y ~ sqrt_x, data = supermarket_data)
comparable_r_squared[["b) y ~ sqrt(x)"]] <- summary(model_b)$r.squared
fitted_models[["b) y ~ sqrt(x)"]] <- model_b
model_formulas_text[["b) y ~ sqrt(x)"]] <- "y ~ sqrt(x)"

# --- Model (c): y = β₀x^β₁ε => log(y) = log(β₀) + β₁log(x) + ε' => log(y) ~ log(x) ---
# Need x > 0 and y > 0
log_x <- log(supermarket_data$x)
model_c <- lm(log_y ~ log_x, data = supermarket_data)
fitted_y_c <- exp(fitted(model_c))
comparable_r_squared[["c) log(y) ~ log(x)"]] <- cor(supermarket_data$y, fitted_y_c)^2
fitted_models[["c) log(y) ~ log(x)"]] <- model_c
model_formulas_text[["c) log(y) ~ log(x)"]] <- "log(y) ~ log(x)"

# --- Model (d): y = β₀β₁^xε => log(y) = log(β₀) + x*log(β₁) + ε' => log(y) ~ x ---
comparable_r_squared[["d) log(y) ~ x (same as a)"]] <- comparable_r_squared[["a) log(y) ~ x"]]
fitted_models[["d) log(y) ~ x (same as a)"]] <- model_a
model_formulas_text[["d) log(y) ~ x (same as a)"]] <- "log(y) ~ x"

# --- Model (e): y = β₀ + β₁(1/x) + ε => y ~ 1/x ---
# Not x = 0
inv_x <- 1 / supermarket_data$x
model_e <- lm(y ~ inv_x, data = supermarket_data)
# R-squared is already on the original y scale
comparable_r_squared[["e) y ~ 1/x"]] <- summary(model_e)$r.squared
fitted_models[["e) y ~ 1/x"]] <- model_e
model_formulas_text[["e) y ~ 1/x"]] <- "y ~ 1/x"

# --- Find the Best Model based on Comparable R-squared ---
best_model_name <- names(which.max(comparable_r_squared))
best_comparable_r_squared <- max(unlist(comparable_r_squared))
best_model <- fitted_models[[best_model_name]]
best_formula_text <- model_formulas_text[[best_model_name]]

best_model_name; best_formula_text; best_comparable_r_squared
