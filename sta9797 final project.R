library(dplyr)
library(tidyr)
library(stringr)
library(glmnet)
library(ggplot2)
library(pROC)

###################################
########## Data Cleaning ##########
###################################

# function to convert multi-team game log to single team
make_team_opp_stats <- function(df, team_abbrev = "NYA") {
  
  # Only numeric/stat columns, exclude team name columns
  visiting_cols <- grep("^visiting_", names(df), value = TRUE)
  home_cols     <- grep("^home_", names(df), value = TRUE)
  
  # EXCLUDE these:
  visiting_cols <- setdiff(visiting_cols, "visiting_team")
  home_cols     <- setdiff(home_cols, "home_team")
  
  # Core stat names
  stat_names <- sub("^visiting_", "", visiting_cols)
  
  is_home <- df$home_team == team_abbrev
  out <- df
  
  for (stat in stat_names) {
    v_col <- paste0("visiting_", stat)
    h_col <- paste0("home_", stat)
    
    out[[paste0("team_", stat)]] <- ifelse(is_home, df[[h_col]], df[[v_col]])
    out[[paste0("opp_", stat)]]  <- ifelse(is_home, df[[v_col]], df[[h_col]])
  }
  
  out$win <- ifelse(
    (is_home & df$home_score > df$visiting_score) |
      (!is_home & df$visiting_score > df$home_score),
    1, 0
  )
  
  return(out)
}


# function to read in season game log
process_retro_mlb_season <- function(filepath, team_abbrev = "NYA") {
  
  # Read
  df <- read.csv(filepath)
  
  # Select relevant columns
  df <- df[c(1,4,7,10,11,22:77)]
  
  # Rename columns (FIXED TYPOS HERE)
  colnames(df) <- c(
    "date", "visiting_team", "home_team", "visiting_score", "home_score",
    "visiting_at_bat", "visiting_hits", "visiting_doubles", "visiting_triples",
    "visiting_HRs", "visiting_RBIs", "visiting_sac_hits", "visiting_sac_flies",
    "visiting_hit_by_pitch", "visiting_walks", "visiting_intentional_walks",
    "visiting_strikeouts", "visiting_stolen_bases", "visiting_caught_stealing",
    "visiting_grounded_DP", "visiting_awarded_first", "visiting_left_on_base",
    "visiting_pitchers_used", "visiting_individual_earned_runs",
    "visiting_team_earned_runs", "visiting_wild_pitches", "visiting_balks",
    "visiting_putouts", "visiting_assists", "visiting_errors",
    "visiting_passed_balls", "visiting_double_plays", "visiting_triple_plays",
    
    "home_at_bat", "home_hits", "home_doubles", "home_triples", "home_HRs",
    "home_RBIs", "home_sac_hits", "home_sac_flies", "home_hit_by_pitch",
    "home_walks", "home_intentional_walks", "home_strikeouts",
    "home_stolen_bases", "home_caught_stealing", "home_grounded_DP",
    "home_awarded_first", "home_left_on_base", "home_pitchers_used",
    "home_individual_earned_runs", "home_team_earned_runs",
    "home_wild_pitches", "home_balks", "home_putouts", "home_assists",
    "home_errors", "home_passed_balls", "home_double_plays",
    "home_triple_plays"
  )
  
  # Format date
  df$date <- as.Date(as.character(df$date), format = "%Y%m%d")
  
  # Filter for the team
  df_team <- df |>
    filter(home_team == team_abbrev | visiting_team == team_abbrev) |>
    mutate(home_away = ifelse(home_team == team_abbrev, "Home", "Away"))
  
  # Create team_/opp_ stats
  df_team <- make_team_opp_stats(df_team, team_abbrev)
  
  # Add opponent & home indicator
  df_team <- df_team |>
    mutate(
      opponent = ifelse(home_team == team_abbrev, visiting_team, home_team),
      home_game = ifelse(home_away == "Home", 1, 0)
    ) |>
    dplyr::select( # FIXED NAMESPACE HERE
      date, opponent, home_game, win,
      starts_with("team_"),
      starts_with("opp_")
    )
  
  return(df_team)
}

# combine gamelog data from retrosheet
seasons <- c("gl2021.txt", "gl2022.txt", "gl2023.txt", "gl2024.txt", "gl2025.txt")

yankees_seasons_combined <- lapply(seasons, process_retro_mlb_season) |>
  bind_rows()

str(yankees_seasons_combined) # NOTE THIS IS THE BASELINE DATAFRAME WE WORK WITH

readr::write_csv(yankees_seasons_combined, "yankees_gamelog.csv")
# uncomment to save csv file


########## outlier check ##########
# uncomment to use #
#library(MASS)
# Step 1: Select only numeric predictors (excluding non-numeric variables)
#numeric_vars <- sapply(yankees_seasons_combined, is.numeric)
#data_numeric <- yankees_seasons_combined[, numeric_vars]

# Step 2: Standardize the numeric data
#scaled_data <- scale(data_numeric)

# Step 3: Calculate the center and covariance matrix
#center <- colMeans(scaled_data)
#cov_matrix <- cov(scaled_data)

# Step 4: Compute Mahalanobis distances
#mahal_dist <- mahalanobis(scaled_data, center, cov_matrix)

# Step 5: Set threshold for outliers (e.g., 99th percentile of Chi-square)
#df <- ncol(scaled_data)
#threshold <- qchisq(0.99, df)

# Step 6: Identify outliers
#outliers <- which(mahal_dist > threshold)

# Step 7: View outlier rows
#print(paste("Number of outliers detected:", length(outliers)))
#print("Outlier observations:")

# no outliers detected to remove from dataset

###############################################################
######### end of retrosheet data cleaning #####################
###############################################################


#####################################################################
######### pre-process data for logistic regression purposes #########
#####################################################################

# now apply further pre-processing to the data for analysis purposes

# create "difference" predictors for analysis (team stats - opponent stats)
# this is better for interpretability of the model, especially using an elastic net & acts as normalization from game to game

# 1. Create a NEW dataframe (copy of the original)
model_data_diffs <- yankees_seasons_combined

# 2. Get all columns that start with "team_"
team_vars <- grep("^team_", names(model_data_diffs), value = TRUE)

# 3. Loop through them to create the difference on the NEW dataframe
for (var in team_vars) {
  
  # Identify the base name (e.g., change "team_hits" to "hits")
  base_stat <- sub("team_", "", var)
  
  # Construct the corresponding opponent variable name
  opp_var <- paste0("opp_", base_stat)
  
  # Check: Only subtract if the opponent variable actually exists
  if (opp_var %in% names(model_data_diffs)) {
    
    # Create the new variable name (e.g., "diff_hits")
    diff_name <- paste0("diff_", base_stat)
    
    # Perform the subtraction on the NEW dataframe
    model_data_diffs[[diff_name]] <- 
      model_data_diffs[[var]] - model_data_diffs[[opp_var]]
  }
}

# 4. Check the new dataframe
str(model_data_diffs)

# select only relevant columns for analysis
logistic_model_data <- model_data_diffs |>
  dplyr::select(-starts_with("team"),                # Remove all 'team' columns
         -starts_with("opp"),                 # Remove all 'opp' columns
         -diff_score,                         # Remove to prevent data leakage
         -diff_individual_earned_runs,        # Data leak for score
         -diff_team_earned_runs,              # Data leak for score 
         -diff_at_bat,                        # Data leak for winner, the losing Away team bats in 9 innings, the winning Home team only bats in 8 innings.
         -diff_putouts,                       # Data leak for how long a game lasted, can predict if bottom of the 9th happened                                                   
         -diff_left_on_base,                  # Data leak for score
         -diff_sac_flies,                     # Data leak for score                    
         -diff_pitchers_used,                 # Data leak for score
         -diff_triple_plays,                  # Extremely rare event
         -diff_awarded_first,                 # Extremely rare event
         -diff_HRs,                           # Data leak for score
         -diff_RBIs,                          # Data leak for score
         -date)


str(logistic_model_data) # should contain only relevant predictors to be used in analysis

########## Logistic Regression Model with Elastic-Net ########## 

# first start with a baseline for the logistic regression model for sanity check, will apply bootstrap later to support inference
# this was used to eliminate multiple data leakage predictors

# 1. Convert to Matrix format (Required for glmnet)
x_mat <- as.matrix(logistic_model_data |> dplyr::select(-win))
y_vec <- logistic_model_data$win

# 2. RUN CV ELASTIC NET (Just once)
set.seed(123) # For reproducibility
cv_fit <- cv.glmnet(x_mat, y_vec, 
                    alpha = 0.5,          # Elastic Net
                    family = "binomial",  # Logistic Regression
                    type.measure = "auc", # Measure performance by AUC
                    keep = TRUE)

# 3. CHECK PERFORMANCE
plot(cv_fit) # Visualizes how Lambda changes error
title("Elastic Net Tuning (Look for the Peak)", line = 2.5)

# 4. EXTRACT COEFFICIENTS
# We use "lambda.1se" (1 Standard Error Rule) for a simpler, more robust model
# If you want pure accuracy, swap this for "lambda.min"
final_coefs <- coef(cv_fit, s = "lambda.1se")

# Convert to a readable Data Frame
coef_df <- data.frame(
  Variable = rownames(final_coefs),
  Coefficient = as.vector(final_coefs)
) |>
  filter(Coefficient != 0) |>        # Remove variables that were dropped
  filter(Variable != "(Intercept)") |>
  arrange(desc(abs(Coefficient)))     # Sort by strength

# 5. VISUALIZE THE "ROUGH DRAFT" INFERENCE
print(coef_df)

ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Preliminary Variable Importance (Single Logisitc Regression)",
       subtitle = "Variables with Non-Zero Coefficients at Lambda.1se",
       x = "Variable", y = "Coefficient (Log Odds)") +
  theme_minimal()

# look at auc of model on the hold out kfold
lambda_index <- which(cv_fit$lambda == cv_fit$lambda.1se)

# Extract the saved predictions for that specific lambda
# 'fit.preval' contains the pre-validated predictions
cv_probs <- cv_fit$fit.preval[, lambda_index]

# GENERATE THE ROC CURVE
roc_obj <- roc(y_vec, cv_probs)

# This curve represents your 10-fold CV performance (The "Honest" Curve)
plot(roc_obj, 
     main = "Cross-Validated ROC Curve (Lambda.1se)",
     col = "#1c61b6", # Yankee Blue
     lwd = 3,
     print.auc = TRUE,
     print.auc.y = 0.3)

# Add a dashed line for random guessing
abline(a = 0, b = 1, lty = 2, col = "black")



###########################################
############ Bootstrapping ################
# perform bootstrapping to prove the selected predictors are stable across multiple bootstraps
# this confirms the inference part for us, these are the important variables to attribute to winning a baseball game
# "A stable model equates to better inference."

# ============================================
# BOOTSTRAP ANALYSIS FOR ELASTIC NET MODEL
# ============================================

# Setup: Using your existing data
x_mat <- as.matrix(logistic_model_data |> dplyr::select(-win))
y_vec <- logistic_model_data$win

# ============================================
# BOOTSTRAP CONFIGURATION
# ============================================
n_bootstrap <- 1000  # Number of bootstrap samples
n_obs <- nrow(x_mat)

# Storage for results
bootstrap_coefs <- list()
bootstrap_auc <- numeric(n_bootstrap)
bootstrap_lambda <- numeric(n_bootstrap)

# ============================================
# RUN BOOTSTRAP
# ============================================
set.seed(123)

cat("Running", n_bootstrap, "bootstrap iterations...\n")

for (i in 1:n_bootstrap) {
  # Progress indicator
  if (i %% 100 == 0) cat("Completed:", i, "/", n_bootstrap, "\n")
  
  # Sample with replacement
  boot_indices <- sample(1:n_obs, n_obs, replace = TRUE)
  x_boot <- x_mat[boot_indices, ]
  y_boot <- y_vec[boot_indices]
  
  # Fit elastic net with CV
  cv_boot <- cv.glmnet(x_boot, y_boot,
                       alpha = 0.5,
                       family = "binomial",
                       type.measure = "auc",
                       keep = TRUE)
  
  # Store lambda chosen
  bootstrap_lambda[i] <- cv_boot$lambda.1se
  
  # Extract coefficients
  boot_coefs <- coef(cv_boot, s = "lambda.1se")
  bootstrap_coefs[[i]] <- as.vector(boot_coefs)
  
  # Calculate AUC on bootstrap sample
  lambda_idx <- which(cv_boot$lambda == cv_boot$lambda.1se)
  boot_probs <- cv_boot$fit.preval[, lambda_idx]
  boot_roc <- roc(y_boot, boot_probs, quiet = TRUE)
  bootstrap_auc[i] <- auc(boot_roc)
}

cat("\nBootstrap complete!\n\n")

# ============================================
# PROCESS BOOTSTRAP RESULTS
# ============================================

# Convert to matrix (rows = bootstrap samples, cols = coefficients)
coef_matrix <- do.call(rbind, bootstrap_coefs)
var_names <- c("(Intercept)", colnames(x_mat))
colnames(coef_matrix) <- var_names

# Calculate statistics for each coefficient
coef_stats <- data.frame(
  Variable = var_names,
  Mean = apply(coef_matrix, 2, mean),
  SD = apply(coef_matrix, 2, sd),
  CI_Lower = apply(coef_matrix, 2, quantile, probs = 0.025),
  CI_Upper = apply(coef_matrix, 2, quantile, probs = 0.975),
  Pct_NonZero = apply(coef_matrix, 2, function(x) mean(x != 0) * 100)
) |>
  filter(Variable != "(Intercept)") |>
  arrange(desc(abs(Mean)))

# ============================================
# DISPLAY RESULTS
# ============================================

cat("=== BOOTSTRAP COEFFICIENT SUMMARY ===\n")
print(coef_stats, row.names = FALSE)

cat("\n=== AUC BOOTSTRAP SUMMARY ===\n")
cat("Mean AUC:", round(mean(bootstrap_auc), 4), "\n")
cat("95% CI:", round(quantile(bootstrap_auc, 0.025), 4), "-", 
    round(quantile(bootstrap_auc, 0.975), 4), "\n")
cat("SD:", round(sd(bootstrap_auc), 4), "\n")

cat("\n=== LAMBDA SELECTION SUMMARY ===\n")
cat("Mean Lambda.1se:", round(mean(bootstrap_lambda), 6), "\n")

# ============================================
# VISUALIZATIONS
# ============================================

# 1. Coefficient Stability Plot (with confidence intervals)
top_vars <- coef_stats |>
  filter(Pct_NonZero >= 50) |>  # Only show variables selected in >50% of bootstraps
  head(20)

p1 <- ggplot(top_vars, aes(x = reorder(Variable, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0.3, color = "darkred", linewidth = 0.8) +
  coord_flip() +
  labs(title = "Bootstrap Coefficient Estimates with 95% CI",
       subtitle = paste("Variables Selected in >50% of", n_bootstrap, "Bootstrap Samples"),
       x = "Variable", 
       y = "Coefficient (Log Odds)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p1)

# 2. AUC Distribution
p2 <- ggplot(data.frame(AUC = bootstrap_auc), aes(x = AUC)) +
  geom_histogram(bins = 50, fill = "#1c61b6", alpha = 0.7, color = "black") +
  geom_vline(xintercept = mean(bootstrap_auc), 
             linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = quantile(bootstrap_auc, c(0.025, 0.975)),
             linetype = "dotted", color = "darkred", linewidth = 0.8) +
  labs(title = "Bootstrap AUC Distribution",
       subtitle = paste(n_bootstrap, "Bootstrap Samples"),
       x = "AUC", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p2)

# 3. Selection Frequency Plot
freq_plot_data <- coef_stats |>
  filter(Pct_NonZero > 0) |>
  arrange(desc(Pct_NonZero)) |>
  head(20)

p3 <- ggplot(freq_plot_data, aes(x = reorder(Variable, Pct_NonZero), 
                                 y = Pct_NonZero)) +
  geom_bar(stat = "identity", fill = "forestgreen", alpha = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Variable Selection Frequency",
       subtitle = paste("How Often Each Variable Was Selected Across", 
                        n_bootstrap, "Bootstraps"),
       x = "Variable", 
       y = "Selection Frequency (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p3)

# 4. Coefficient Distribution for Top Variables (Violin Plots)
top_5_vars <- head(coef_stats$Variable, 5)
coef_long <- data.frame(coef_matrix[, top_5_vars]) |>
  tidyr::pivot_longer(cols = everything(), 
                      names_to = "Variable", 
                      values_to = "Coefficient") |>
  filter(Coefficient != 0)  # Remove zeros for cleaner visualization

p4 <- ggplot(coef_long, aes(x = reorder(Variable, Coefficient, mean), 
                            y = Coefficient)) +
  geom_violin(fill = "lightblue", alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.alpha = 0.3) +
  coord_flip() +
  labs(title = "Coefficient Distribution for Top 5 Variables",
       subtitle = "Distribution Across Bootstrap Samples (Zeros Excluded)",
       x = "Variable", y = "Coefficient Value") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p4)

# ============================================