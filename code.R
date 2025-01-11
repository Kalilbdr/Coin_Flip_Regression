## ---------------------------------------------------------------------------------------------------------------------------------------------
  
                                        ## Regression Methods Project ##

## ---------------------------------------------------------------------------------------------------------------------------------------------

# Read the data

library(ggplot2)
library(dplyr)
library(stringr)
library(SMPracticals)
library(lme4)
library(mgcv)
set.seed(12345)

#If these packages are not installed; you can install them using : install.packages("package_name")

# Set your working directory with the command setwd("Working_Directory")
# Set your path if necessary; using path = ""


df <- read.csv("data-agg.csv",header=T)
n <- nrow(df)
df1 <- df[,-c(2,4)] # heads to heads
df2 <- df[,-c(1,3)] # tails to heads
df2[,1] <- df2[,2]-df2[,1] # tails to tails 
names(df1) <- names(df2) <- c("y","m","person","coin") 
start <- rep(c("heads","tails"),c(n,n))
df <- rbind(df1,df2)
df$person <- factor(df$person) 
df$coin <- factor(df$coin)
df$start <- factor(start) 

head(df)
head(df1)
head(df2)


# ========================================================
# 1. Exploratory Data Analysis
# ========================================================

# --------------------------------------------------------
# 1.1 Empirical Distribution
# --------------------------------------------------------


# --------------------------------------------------------
# "Global analysis"


### Quick overview of the data
num_persons <- length(unique(df$person))
num_coins <- length(unique(df$coin))

cat("Number of persons:", num_persons, "\n")
cat("Number of coins:", num_coins, "\n")

### Histogram of proportions
df$success <- df$y / df$m
df_heads <- subset(df, start == "heads")
df_tails <- subset(df, start == "tails")

p_general <- ggplot(df, aes(x = success)) +
  geom_histogram(bins = 60, fill = "steelblue", color = "white", alpha = 0.8) +
  facet_wrap(~ start, ncol = 2) +
  labs(
    title = "Distribution of same-side proportions",
    x = "Same-side proportion",
    y = "Count"
  ) +
  theme_bw() + 
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title   = element_text(face = "bold", size = 12),
    axis.text    = element_text(size = 10, color = "black"), 
    plot.caption = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    legend.text  = element_text(hjust = 0.5)
  )

# Compute statistics
stats <- df %>%
  group_by(start) %>%
  summarize(
    mu = mean(success, na.rm = TRUE),
    sigma = sd(success, na.rm = TRUE),
    .groups = "drop"
  )

p_general <- p_general + 
  geom_text(
    data = stats,
    aes(
      x = 0.68, 
      y = Inf, 
      label = paste0(
        "atop(hat(mu) == ", round(mu, 3), 
        ", hat(sigma) == ", round(sigma, 3), ")"
      )
    ),
    parse = TRUE,
    hjust = 1.1, vjust = 1.1, 
    size = 3,
    color = "black",
    inherit.aes = FALSE
  )

print(p_general)

ggsave(
  filename = "distribution_success.pdf",  
  plot = p_general,  
  device = "pdf", 
  #path = #path, 
  width = 8,
  height = 5
)


# --------------------------------------------------------
# 1.2 Outliers Analysis
# --------------------------------------------------------



### (a) Summary of total flips and success by person
person_summary <- df %>%
  group_by(person) %>%
  summarize(
    total_flips = sum(m),
    mean_success = round(mean(success, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(mean_success)

print(person_summary)


### (b) Summary of total flips and success by coin
coin_summary <- df %>%
  group_by(coin) %>%
  summarize(
    total_flips = sum(m),
    mean_success = round(mean(success, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(mean_success)

print(coin_summary)


### (c) Summary of total flips and success by person-coin
person_coin_summary <- df %>%
  group_by(person, coin) %>%
  summarize(
    total_flips = sum(m),
    mean_success = round(mean(success, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(mean_success)  

print(person_coin_summary)


# Group the data
violin_data <- data.frame(
  category = factor(c(rep("by person", nrow(person_summary)),
                      rep("by coin", nrow(coin_summary)),
                      rep("by person x coin", nrow(person_coin_summary)))),
  mean_success = c(person_summary$mean_success,
                   coin_summary$mean_success,
                   person_coin_summary$mean_success)
)

# Plot
p_violin <- ggplot(violin_data, aes(x = "", y = mean_success)) +
  geom_violin(fill = "steelblue", color = "white", alpha = 0.8) +
  geom_boxplot(width = 0.1, fill = "lightgrey", color = "black", outlier.color = "red") +
  facet_wrap(~ category) +
  labs(
    title = "Distribution of Success Proportions",
    x = "",
    y = "Success Proportion"
  ) +
  theme_bw() + 
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title      = element_text(face = "bold", size = 12),
    axis.text       = element_text(size = 10, color = "black"),
    plot.caption    = element_text(hjust = 0.5),
    legend.title    = element_text(hjust = 0.5),
    legend.text     = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey90"),
    strip.text      = element_text(face = "bold")
  )

print(p_violin)

ggsave(
  filename = "violin_plot_success.pdf",  
  plot = p_violin,  
  device = "pdf", 
  #path = path, 
  width = 8,
  height = 5
)


# --------------------------------------------------------
# 1.3 Temporal Analysis
# --------------------------------------------------------

df_time <- read.csv("df-time-agg.csv", header = TRUE)
df_time <- df_time[, c("same_side", "agg")]

# Computation of the mean of success proportion for each aggregation
df_time <- df_time %>%
  group_by(agg) %>%
  summarize(mean_same_side = mean(same_side, na.rm = TRUE)) %>%
  ungroup()


# Transform the ACF results in DataFrame
acf_result <- acf(df_time$mean_same_side, plot = FALSE)
acf_df <- data.frame(
  lag = acf_result$lag,
  acf = acf_result$acf
)

# Create ACF plot
p_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white", alpha = 0.8) +  
  geom_hline(yintercept = 0, color = "black") +  
  geom_hline(yintercept = c(-1.96 / sqrt(nrow(df_time)), 1.96 / sqrt(nrow(df_time))), 
             linetype = "dashed", color = "red") +  
  labs(
    title = "Autocorrelation Function (ACF) of the success proportion",
    x = "Lag",
    y = "ACF"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title      = element_text(face = "bold", size = 12),
    axis.text       = element_text(size = 10, color = "black"),
    legend.title    = element_text(hjust = 0.5),
    legend.text     = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey90"),
    strip.text      = element_text(face = "bold")
  )


print(p_acf)

ggsave(
  filename = "acf_success.pdf",  
  plot = p_acf,  
  device = "pdf", 
  #path = #path, 
  width = 8,
  height = 5
)


# ========================================================
# 2. GLM Binomial
# ========================================================

# --------------------------------------------------------
# 2.1 Base Model
# --------------------------------------------------------


# GLM Binomial base


# Adjustment of the simple GLM binomial model
base_glm <- glm(cbind(y, m - y) ~ 1, family = binomial, data = df)

# Summary of the model to observe the results 
summary(base_glm)

(ci_logit_base <- confint(base_glm, level = 0.95))

# Transformation of logit in order to obtain a Confidence Interval
ic_p_base <- exp(ci_logit_base) / (1 + exp(ci_logit_base))
cat("\n95% CI for p (base_glm): [",
    round(ic_p_base[1], 4), ", ",
    round(ic_p_base[2], 4), "]\n\n")

base_coef <- coef(base_glm)
p_initial <- exp(base_coef) / (1 + exp(base_coef))
cat("Estimated same-side probability (initial model):", round(p_initial, 4), "\n")


# Computation of the dispersion ratio
dispersion_ratio <- summary(base_glm)$deviance / summary(base_glm)$df.residual
cat("Dispersion Ratio:", dispersion_ratio, "\n")

# Preparing the data for the residuals plot
df_resid <- data.frame(
  fitted = fitted(base_glm),
  pearson_res = residuals(base_glm, type = "pearson")
)


# Residuals plot
p_residuals <- ggplot(df_resid, aes(x = fitted, y = pearson_res)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals VS Fitted values",
    x = "Fitted values",
    y = "Pearson residuals"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title      = element_text(face = "bold", size = 12),
    axis.text       = element_text(size = 10, color = "black"),
    legend.title    = element_text(hjust = 0.5),
    legend.text     = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey90")
  )

print(p_residuals)

ggsave(
  filename = "p_residuals_baseline.pdf",  
  plot = p_residuals,  
  device = "pdf", 
  #path = #path, 
  width = 8,
  height = 5
)


df_resid$pearson_res <- residuals(base_glm, type = "pearson")

p_qq <- ggplot(df_resid, aes(sample = pearson_res)) +
  stat_qq(color = "steelblue", alpha = 0.7) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(
    title = "Q-Q Plot of Pearson Residuals",
    x = "Theoretical Residuals",
    y = "Residuals Quantiles"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text  = element_text(size = 10, color = "black")
  )

print(p_qq)

ggsave(
  filename = "qq_residuals.pdf",  
  plot = p_qq,  
  device = "pdf", 
  #path = #path, 
  width = 8,
  height = 5
)


# Compute the quantiles and IQR (Inter Quartile Range)
Q1 <- quantile(person_coin_summary$mean_success, 0.25)
Q3 <- quantile(person_coin_summary$mean_success, 0.75)
IQR_value <- Q3 - Q1

# Define the ouliers threshold
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter the observation detected as outliers
outliers_pc <- person_coin_summary %>%
  filter(mean_success < lower_bound | mean_success > upper_bound)

print(outliers_pc)

# Compute and add Cooks distance to the initial data 
cooks_d <- cooks.distance(base_glm)
df_with_cook <- df
df_with_cook$cooks_distance <- cooks_d


outlier_groups <- outliers_pc %>% select(person, coin)

df_outliers <- df_with_cook %>%
  inner_join(outlier_groups, by = c("person", "coin"))

df_outliers_selected <- df_outliers %>% select(person, coin, y, m, cooks_distance)

print(df_outliers_selected)

n <- nrow(df)

print(threshold <- 8/(n - 2*1))


# Identify and remove the observations with a Cook's distance > threshold
influential_indices <- which(cooks_d > threshold)
df_cleaned <- df[-influential_indices, ]

# Refit the model on the "new" dataset
refit_glm <- glm(cbind(y, m - y) ~ 1, family = binomial, data = df_cleaned)

# Summary of the model
summary(refit_glm)

(ci_logit_refit <- confint(refit_glm, level = 0.95))
ic_p_refit <- exp(ci_logit_refit) / (1 + exp(ci_logit_refit))
cat("\n95% CI for p (refit_glm): [",
    round(ic_p_refit[1], 4), ", ",
    round(ic_p_refit[2], 4), "]\n\n")

refit_coef <- coef(refit_glm)
p_refit <- exp(refit_coef) / (1 + exp(refit_coef))
cat("Estimated same-side probability (model without influential points):", round(p_refit, 4), "\n")


# --------------------------------------------------------
# 2.2 Normal Model
# --------------------------------------------------------


# Normal linear model base

df$prop <- df$y / df$m

# Fit a weighted least squares (WLS) model
wls_model <- lm(prop ~ 1, data = df, weights = df$m)

# Parameter extraction
beta0_hat <- coef(wls_model)[1]              
var_beta0_hat <- vcov(wls_model)[1, 1]       
se_beta0_hat <- sqrt(var_beta0_hat)          


# H0 Test : beta0 = 0.5

T_value <- (beta0_hat - 0.5) / se_beta0_hat
df_res   <- wls_model$df.residual

# Bilateral p-value (since the alternative hypothesis is two-sided : beta0 != 0.5)
p_value <- 2 * pt(abs(T_value), df_res, lower.tail = FALSE)

cat("\nTest H0: beta0 = 0.5 vs. H1: beta0 != 0.5 (WLS model)\n")
cat("---------------------------------------\n")
cat("Estimate beta0      =", round(beta0_hat, 4), "\n")
cat("Std Error           =", round(se_beta0_hat, 4), "\n")
cat("T-value             =", round(T_value, 4), "\n")
cat("Degrees of freedom  =", df_res, "\n")
cat("p-value (bilateral) =", format.pval(p_value, digits=4), "\n\n")


alpha <- 0.05
t_crit <- qt(1 - alpha/2, df_res)  

delta <- t_crit * se_beta0_hat
center <- beta0_hat
IC_left  <- center - delta
IC_right <- center + delta

cat("Manual 95% CI (around the estimate, testing 0.5) : [",
    round(IC_left, 4), ", ", round(IC_right, 4), "]\n\n")

# --------------------------------------------------------
# 2.3 Fixed effet GLM
# --------------------------------------------------------


# Define formulas for different candidate models
formulas <- list(
  "start + person + coin" = cbind(y, m - y) ~ start + person + coin,
  "start + person"       = cbind(y, m - y) ~ start + person,
  "start + coin"         = cbind(y, m - y) ~ start + coin,
  "person + coin"        = cbind(y, m - y) ~ person + coin,
  "start"                = cbind(y, m - y) ~ start,
  "person"               = cbind(y, m - y) ~ person,
  "coin"                 = cbind(y, m - y) ~ coin,
  "intercept"            = cbind(y, m - y) ~ 1
)


model_list <- list()
aic_values <- numeric(length(formulas))

# Fit each model and calculate its AIC value
for(i in seq_along(formulas)){
  formula_i <- formulas[[i]]
  model_list[[i]] <- glm(formula_i, family = binomial, data = df)
  aic_values[i] <- AIC(model_list[[i]])
}

# Summary Table
model_comparison <- data.frame(
  Model = names(formulas),
  AIC = aic_values
)

model_comparison <- model_comparison[order(model_comparison$AIC), ]
print(model_comparison)

# Identify the best model based on the minimum AIC value
best_model_index <- which.min(aic_values)
best_model_name <- model_comparison$Model[1]
best_model <- model_list[[best_model_index]]

cat("Best Model based on AIC :", best_model_name, "\n")
summary(best_model)

best_model <- glm(cbind(y, m - y) ~ person, family = binomial, data = df)
summary(best_model)

residuals_best <- residuals(best_model, type = "pearson")
fitted_best <- fitted(best_model)


df_resid_best <- data.frame(fitted = fitted_best, pearson_res = residuals_best)

# Plot of residuals vs fitted values
p_residuals_fixed_effect <- ggplot(df_resid_best, aes(x = fitted, y = pearson_res)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals VS Fitted values",
    x = "Fitted values",
    y = "Pearson residuals"
  ) +
  theme_bw() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title      = element_text(face = "bold", size = 12),
    axis.text       = element_text(size = 10, color = "black"),
    legend.title    = element_text(hjust = 0.5),
    legend.text     = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "grey90")
  )

print(p_residuals_fixed_effect)

ggsave(
  filename = "p_residuals_fixed_effect.pdf",  
  plot = p_residuals_fixed_effect,  
  device = "pdf", 
  #path = #path, 
  width = 8,
  height = 5
)

# Dispersion factor
dispersion_ratio <- summary(best_model)$deviance / summary(best_model)$df.residual
cat("Dispersion Ratio:", dispersion_ratio, "\n")


# Quasi binomial model (if wanted)
quasi_glm <- glm(cbind(y, m - y) ~ person, family = quasibinomial, data = df)
summary(quasi_glm)



# --------------------------------------------------------
# 2.4 Random Effet GLM
# --------------------------------------------------------

random_effect_person <- glmer(
  cbind(y, m - y) ~ 1 + (1|person), 
  family = binomial, 
  data = df
)

summary(random_effect_person)

dispersion_ratio <- summary(random_effect_person)$deviance / summary(random_effect_person)$df.residual
cat("Dispersion Ratio:", dispersion_ratio, "\n")


# Refit without outliers
random_effect_person_nout <- glmer(
  cbind(y, m - y) ~ 1 + (1 | person),
  family = binomial,
  data = df_cleaned
)




# --------------------------------------------------------
# 2.5 Nested Effect GLM
# --------------------------------------------------------


re_model_nested <- glmer(
  cbind(y, m - y) ~ 1 + (1|person) + (1|person:coin),
  family = binomial,
  data = df
)

summary(re_model_nested)

anova(random_effect_person, re_model_nested, test="LRT")
AIC(random_effect_person, re_model_nested)

dispersion_ratio <- summary(re_model_nested)$deviance / summary(re_model_nested)$df.residual
cat("Dispersion Ratio:", dispersion_ratio, "\n")

# Check impact of outliers on the previous result (AIC+ ANOVA) --> coin-factor not significant without outliers
re_model_nested_nout <- glmer(
  cbind(y, m - y) ~ 1 + (1 | person) + (1 | person : coin),
  family = binomial,
  data = df_cleaned
)
summary(re_model_nested_nout)

AIC(re_model_nested_nout)

anova(random_effect_person_nout, re_model_nested_nout, test="LRT")




# ========================================================
# 3. Learning effect
# ========================================================

# --------------------------------------------------------
# 3.1 Time Analysis
# --------------------------------------------------------


df_time_agg <- read.csv("df-time-agg.csv", header=T, stringsAsFactors = FALSE)

# Linear model
linear_learning <- glm(
  cbind(same_side, N - same_side) ~ mean_toss,
  family = binomial,
  data   = df_time_agg
)

summary(linear_learning)

# Spline model
spline_learning <- gam(
  cbind(same_side, N - same_side) ~ s(mean_toss),
  family = binomial,
  data   = df_time_agg
)

summary(spline_learning)

AIC(linear_learning, spline_learning)

# Plot of spline with CI 95%
new_data <- data.frame(mean_toss = seq(min(df_time_agg$mean_toss), 
                                       max(df_time_agg$mean_toss), 
                                       length.out = 200))

pred <- predict(spline_learning, newdata = new_data, type = "link", se.fit = TRUE)
linkinv <- spline_learning$family$linkinv

new_data$fit <- linkinv(pred$fit)
new_data$lwr <- linkinv(pred$fit - 2 * pred$se.fit)
new_data$upr <- linkinv(pred$fit + 2 * pred$se.fit)


p_spline <- ggplot(new_data, aes(x = mean_toss, y = fit)) +
  geom_line(color = "steelblue", size = 1) +                        
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "steelblue", alpha = 0.2) + 
  labs(
    title = "Estimated Learning Effect over Time",
    x = "Mean Toss",
    y = "Estimated Same-Side Probability"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text  = element_text(size = 10, color = "black")
  )

print(p_spline)

# Save the plot as a PDF
ggsave(
  filename = "learning_spline_plot.pdf",  
  plot = p_spline,  
  device = "pdf", 
  #path = #path,
  width = 8,
  height = 5
)



# --------------------------------------------------------
# 3.1 Previous Flip impact
# --------------------------------------------------------


# Treat the data
df_time_agg <- read.csv("df-time-agg.csv", header = TRUE, stringsAsFactors = FALSE)
df_time_agg <- df_time_agg %>%
  arrange(person, coin, agg)

df_time_agg <- df_time_agg %>%
  group_by(person, coin) %>%
  mutate(prev_prop_same_side = lag(same_side / N)) %>%
  ungroup()

df_time_agg_filtered <- df_time_agg %>% filter(!is.na(prev_prop_same_side))

# Fit the model
model_recent_block <- glm(
  cbind(same_side, N - same_side) ~ prev_prop_same_side,
  family = binomial,
  data = df_time_agg_filtered
)

summary(model_recent_block)



