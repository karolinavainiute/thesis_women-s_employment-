# Libraries
install.packages("tidyverse") # includes ggplot2, readr, dplyr
install.packages("margins") # marginal effects
install.packages("sandwich") # robust SE
install.packages("lmtest") # testing coefficients
install.packages("psych")
install.packages("knitr")
install.packages("stargazer")
install.packages("MissMech") # test Missing Completely At Random
install.packages("broom") # to summarize misingness model
install.packages("corrplot")
library(corrplot)
library(broom)
library(MissMech)
library(tidyverse)
library(margins)
library(sandwich)
library(lmtest)
library(knitr)
library(stargazer)
library(psych)

# Clear memory
rm(list = ls())

# Setting the working directory
setwd("<...>")

# ______________________________________________________________________________
# SECTION 1: DATA PREPARATION AND CLEANING
# This section loads raw data, filters, renames, recodes variables,
# handles missing values, merges datasets, and prepares the final analytical dataset.

# Load data
wvs_time_series <- read_csv("WVS_Time_Series.csv")

# View initial data (for inspection, can be commented out for final run)
# view(wvs_time_series)
# names(wvs_time_series)
# head(wvs_time_series)


### CLEAN AND FILTER WVS DATA ###

# Select variables
vars <- c("S002VS", "S003", "S006", "S020", "X001", "X003", "X007",
          "X011", "X025", "X025A_01", "X028", "C001", "D057", "D059",
          "D060", "D061", "D066_B", "D078", "MODE")
wvs_data <- wvs_time_series[, vars]
dim(wvs_data)


### CLEAN COUNTRIES ###
# Select only OECD countries
oecd_codes <- c(36, 40, 56, 124, 152, 170, 188, 203, 208, 233, 246, 250,
                276, 300, 348, 352, 372, 376, 380, 392, 410, 428, 440,
                442, 484, 528, 554, 578, 616, 620, 703, 705, 724, 752,
                756, 792, 826, 840)
wvs_data <- wvs_data %>% filter(S003 %in% oecd_codes)

# Replace the numeric country codes by the alpha3 code
country_code_lookup <- data.frame(
  numeric_code = c(36, 40, 56, 124, 152, 170, 188, 203, 208, 233, 246, 250,
                   276, 300, 348, 352, 372, 376, 380, 392, 410, 428, 440,
                   442, 484, 528, 554, 578, 616, 620, 703, 705, 724, 752,
                   756, 792, 826, 840),
  alpha3_code = c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", "FIN", "FRA",
                  "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU",
                  "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE",
                  "CHE", "TUR", "GBR", "USA")
)
wvs_data <- wvs_data %>%
  left_join(country_code_lookup, by = c("S003" = "numeric_code")) %>%
  mutate(S003 = alpha3_code) %>%
  select(-alpha3_code) # Remove extra column

head(wvs_data) #Check if went ok

# Select waves 6, 7
waves <- c(6, 7)
wvs_data <- wvs_data %>% filter(S002VS %in% waves)

# Rename variables for clarity
wvs_data <- wvs_data %>%
  rename(
    wave = S002VS,
    country_code = S003,
    respondent_number = S006,
    survey_year = S020,
    mode = MODE,
    sex = X001,
    age = X003,
    marital_status = X007,
    children = X011,
    education = X025,
    employment = X028,
    jobs_scarce = C001,
    housewife_fulfilling = D057,
    men_better_leaders = D059,
    uni_important_boys = D060,
    men_better_executives = D078,
    child_suffers = D061,
    problem_higher_income = D066_B
  )

# Keep only those countries that are common across the waves (after initial filtering and renaming)
# Get a list of countries per wave
countries_per_wave <- wvs_data %>%
  group_by(wave) %>%
  summarise(countries = list(unique(country_code)))

# 1. Find common countries across waves
common_countries <- Reduce(intersect, countries_per_wave$countries)

# 2. Keep only rows from common countries
wvs_data <- wvs_data %>%
  filter(country_code %in% common_countries)

# Select individuals age 18-64
wvs_data <- wvs_data %>%
  filter(age >= 18, age <= 64)

# Merge education variables from questions X025 and X025A_01
wvs_data$education <- ifelse(wvs_data$wave == 6, wvs_data$education, wvs_data$X025A_01)

# Delete column X025A_01
wvs_data <- wvs_data %>%
  select(-X025A_01)

# Remove students and retirees in employment(=4 & 6)
wvs_data <- wvs_data %>%
  filter(!(employment %in% c(4, 6)))

### HANDLE MISSING VALUES AND RECODE EDUCATION ###

# Check how many times specific value appears in each variable
# colSums(wvs_data == -1, na.rm = TRUE) 
# sum(wvs_data == -5, na.rm = TRUE) 
# sum(is.na(wvs_data)) 

wvs_data <- wvs_data %>%
  mutate(across(where(is.numeric), ~ replace(., . %in% c(-3, -4, -5), NA)))

#omit rows with missing values (-3, -4 and -5)
wvs_data <- na.omit(wvs_data)

#Check if the values are actually deleted
# sum(wvs_data == -5, na.rm = TRUE)

#recode -1 and -2 as NA
wvs_data <- wvs_data %>%
  mutate(across(where(is.numeric), ~ replace(., . %in% c(-1, -2), NA)))

#sum missing values
sum(is.na(wvs_data))

##LOGISTICS MODEL FOR MISSINGNESS for all gender attitude variables
#controlling for country effects 

# Variables to analyze
variables_to_analyze <- c("jobs_scarce", "housewife_fulfilling", 
                          "men_better_leaders", "uni_important_boys",
                          "child_suffers", "problem_higher_income", 
                          "men_better_executives")

# Initialize a list to store tidy summaries
tidy_models <- list()

# Loop through each variable
for (var in variables_to_analyze) {
  # 1: Create missingness indicator
  missing_var <- paste0("missing_", var)
  wvs_data[[missing_var]] <- ifelse(is.na(wvs_data[[var]]), 1, 0)
  
  # 2: Run logistic regression (with country_code as factor)
  model <- glm(as.formula(paste(missing_var, "~ wave + sex + age + education + employment + country_code")),
               data = wvs_data,
               family = binomial(link = "logit"))
  
  # 3: Tidy up results and remove country_code coefficients
  tidy_model <- tidy(model)
  tidy_model <- tidy_model %>%
    filter(!grepl("^country_code", term)) %>%   # Remove country coefficients
    mutate(variable = var)
  
  # 4: Store tidy result
  tidy_models[[var]] <- tidy_model
}

# Combine all tidy results into one table
combined_results <- bind_rows(tidy_models) %>%
  select(variable, term, estimate, std.error, statistic, p.value)

# View combined tidy table
# print(combined_results)

#omit rows with missing values (-1 and -2)
wvs_data <- na.omit(wvs_data)

# Education: Harmonize categories across waves
wvs_data <- wvs_data %>%
  mutate(
    education_harmonized = case_when(
      # No education
      (wave == 6 & education == 1) | (wave == 7 & education == 0) ~ 0,
      # Primary education
      (wave == 6 & education == 2) | (wave == 7 & education == 1) ~ 1,
      # Lower secondary education
      (wave == 6 & education %in% c(3, 5)) | (wave == 7 & education == 2) ~ 2,
      # Upper secondary education
      (wave == 6 & education %in% c(4, 6)) | (wave == 7 & education == 3) ~ 3,
      # Some university, no degree (short-cycle tertiary)
      (wave == 6 & education == 7) | (wave == 7 & education %in% c(4, 5)) ~ 4,
      # University with degree (Bachelor's, Master's, Doctoral)
      (wave == 6 & education == 8) | (wave == 7 & education %in% c(6, 7, 8)) ~ 5,
      TRUE ~ NA_real_
    )
  )

# Check the result of merging (for inspection)
wvs_data %>%
   group_by(wave, education_harmonized) %>%
   summarise(count = n()) %>%
   arrange(wave, education_harmonized)

# Replace variable education by variable education_harmonized
wvs_data <- wvs_data %>%
  mutate(education = education_harmonized) %>%
  select(-education_harmonized)


### MERGE WITH PARENTAL LEAVE POLICY DATA ###

# Read policy data treating all columns as character
policy_data <- read.csv("thesis - clean parental leave.csv", stringsAsFactors = FALSE)

# Convert numeric columns to numeric
numeric_cols <- c("Paid.maternity.leave", "Paid.parental.and.home.care.leave..mother.",
                  "Total.paid.leave.available.to.mothers", "Paid.paternity.leave",
                  "Paid.parental.and.home.care.leave.reserved.for.fathers",
                  "Total.paid.leave.reserved.for.fathers")
policy_data[numeric_cols] <- lapply(policy_data[numeric_cols], as.numeric)

# Rename variables in policy data
policy_data <- policy_data %>%
  rename(country_code = Reference.area,
         year = Time.period,
         maternity_leave = Paid.maternity.leave,
         parental_leave_mother = Paid.parental.and.home.care.leave..mother.,
         total_maternal_leave = Total.paid.leave.available.to.mothers,
         paternity_leave = Paid.paternity.leave,
         father_reserved_leave = Paid.parental.and.home.care.leave.reserved.for.fathers,
         total_paternal_leave = Total.paid.leave.reserved.for.fathers)

# Check numeric columns (should include zeros, no NAs except real missing values)
# print(policy_data) 

# Merge with WVS data
wvs_data <- wvs_data %>%
  mutate(policy_year = ifelse(wave == 6, 2015, 2023)) %>%
  left_join(policy_data, by = c("country_code", "policy_year" = "year"))

### RECODE ADDITIONAL VARIABLES FOR ANALYSIS ###

# Recode employment: 1 (full-time, part-time, self-employed) / 0 (others)
wvs_data <- wvs_data %>%
  mutate(employment_recoded = ifelse(employment %in% c(1, 2, 3), 1, 0))

# Recode part-time employment: 1 (part-time), 0 (others)
wvs_data <- wvs_data %>%
  mutate(parttime_recoded = ifelse(employment == 2, 1, 0))

# Recode marital status: 1 (married or living together), 0 (others)
wvs_data <- wvs_data %>%
  mutate(marital_status_recoded = ifelse(marital_status %in% c(1, 2), 1, 0))

# Recode gender role attitude questions to [-1, 0, +1]
wvs_data <- wvs_data %>%
  mutate(
    jobs_scarce = case_when(
      jobs_scarce == 1 ~ -1,
      jobs_scarce == 2 ~ +1,
      jobs_scarce == 3 ~ 0,
      TRUE ~ NA_real_
    ),
    housewife_fulfilling = case_when(
      housewife_fulfilling %in% c(1, 2) ~ -1,
      housewife_fulfilling %in% c(3, 4) ~ +1,
      TRUE ~ NA_real_
    ),
    men_better_leaders = case_when(
      men_better_leaders %in% c(1, 2) ~ -1,
      men_better_leaders %in% c(3, 4) ~ +1,
      TRUE ~ NA_real_
    ),
    uni_important_boys = case_when(
      uni_important_boys %in% c(1, 2) ~ -1,
      uni_important_boys %in% c(3, 4) ~ +1,
      TRUE ~ NA_real_
    ),
    men_better_executives = case_when(
      men_better_executives %in% c(1, 2) ~ -1,
      men_better_executives %in% c(3, 4) ~ +1,
      TRUE ~ NA_real_
    ),
    child_suffers = case_when(
      child_suffers %in% c(1, 2) ~ -1,
      child_suffers %in% c(3, 4) ~ +1,
      TRUE ~ NA_real_
    ),
    problem_higher_income = case_when(
      problem_higher_income == 1 ~ -1,
      problem_higher_income == 2 ~ 0,
      problem_higher_income == 3 ~ +1,
      TRUE ~ NA_real_
    )
  )
#______________________KMO to test suitability of data for PCA_________________

# Step 1: Subset the gender attitude variables
gender_attitude_vars <- c(
  "jobs_scarce",
  "housewife_fulfilling",
  "men_better_leaders",
  "uni_important_boys",
  "men_better_executives",
  "child_suffers",
  "problem_higher_income"
)

# Step 2: Run KMO for Wave 6
kmo_wave6 <- KMO(na.omit(wvs_data[wvs_data$wave == 6, gender_attitude_vars]))
print("KMO Test - Wave 6")
print(kmo_wave6)

# Step 3: Run KMO for Wave 7
kmo_wave7 <- KMO(na.omit(wvs_data[wvs_data$wave == 7, gender_attitude_vars]))
print("KMO Test - Wave 7")
print(kmo_wave7)

# ____________________________PCA_______________________________________________
# PCA for Gender Role Attitudes - ONLY FOR WAVE 6

# Filter data for Wave 6
wvs_data_wave6 <- wvs_data %>% filter(wave == 6)

# List of gender role attitude questions
gender_attitude_vars <- c(
  "jobs_scarce",
  "housewife_fulfilling",
  "men_better_leaders",
  "uni_important_boys",
  "men_better_executives",
  "child_suffers",
  "problem_higher_income"
)

# Subset to gender attitude questions for Wave 6
data_attitudes_wave6 <- wvs_data_wave6[, gender_attitude_vars]

# STEP 1: Standardize (z-scores)
# Exclude rows with NA values before scaling 
data_attitudes_wave6_cleaned <- na.omit(data_attitudes_wave6)
data_attitudes_wave6_scaled <- scale(data_attitudes_wave6_cleaned, center = TRUE, scale = TRUE)

#STEP 2: Covariance Matrix Computation
# Compute the covariance matrix of the standardized variables
cov_matrix_wave6 <- cov(data_attitudes_wave6_scaled)
# Print the covariance matrix
round(cov_matrix_wave6, 2)

# Plot the covariance (correlation) matrix
corrplot(cov_matrix_wave6, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8,
         title = "Correlation Matrix of Gender Attitudes (Wave 6)", mar = c(0,0,1,0))


#STEP 3: Compute the eigenvectors and eigenvalues of the covariance matrix
# to identify the principal components
# Compute eigenvalues and eigenvectors of the covariance matrix
eigen_decomp_wave6 <- eigen(cov_matrix_wave6)
# View eigenvalues (variance explained by each principal component)
eigen_decomp_wave6$values
# View eigenvectors (loadings of each original variable on each principal component)
eigen_decomp_wave6$vectors

# Scree plot 
plot(
  eigen_decomp_wave6$values,
  type = "b",
  xlab = "Principal Component",
  ylab = "Eigenvalue",
  pch = 19
)
abline(h = 1, col = "red", lty = 2) # Kaiser’s criterion (eigenvalues > 1),
abline(h = 0.7, col = "blue", lty = 2) # (eigenvalues > 0.7)


# Determine number of components to keep (Kaiser’s criterion (eigenvalues > 0.7)
num_components_wave6 <- sum(eigen_decomp_wave6$values > 0.7)
cat(paste0("Number of components with eigenvalues > 0.7 for Wave 6: ", num_components_wave6, "\n"))

# STEP 4: Create a Feature Vector
# Use the number of components determined in the previous step
feature_vector_wave6 <- eigen_decomp_wave6$vectors[, 1:num_components_wave6, drop = FALSE]
# Label the rows and columns for clarity
rownames(feature_vector_wave6) <- gender_attitude_vars
colnames(feature_vector_wave6) <- paste0("PC", 1:num_components_wave6)
print("Feature Vector (Eigenvectors for selected PCs - Wave 6):")
print(feature_vector_wave6)


# STEP 5: Recast the Data Along the Principal Component Axes
# Project standardized data onto the principal component space using the feature vector

# Multiply the standardized data by the feature vector (eigenvectors for selected PCs)
pca_scores_wave6 <- data_attitudes_wave6_scaled %*% feature_vector_wave6

# Convert to a data frame and name the new principal component columns
pca_scores_wave6_df <- as.data.frame(pca_scores_wave6)
colnames(pca_scores_wave6_df) <- paste0("gender_attitude_wave6_PC", 1:num_components_wave6)

# Add scores directly to wvs_data for respondents in wave 6
wvs_data[wvs_data$wave == 6, paste0("gender_attitude_wave6_PC", 1:num_components_wave6)] <- pca_scores_wave6_df

# Example: Print variance explained by PC1 for Wave 6
variance_pc1_wave6 <- round((eigen_decomp_wave6$values[1] / sum(eigen_decomp_wave6$values)) * 100, 2)
cat(paste0("Wave 6 PCA - Variance explained by PC1: ", variance_pc1_wave6, "%\n"))


# ______________________________________________________________________________
# PCA for Gender Role Attitudes - ONLY FOR WAVE 7

# Filter data for Wave 7
wvs_data_wave7 <- wvs_data %>% filter(wave == 7)

# List of gender role attitude questions
gender_attitude_vars <- c(
  "jobs_scarce",
  "housewife_fulfilling",
  "men_better_leaders",
  "uni_important_boys",
  "men_better_executives",
  "child_suffers",
  "problem_higher_income"
)

# Subset to gender attitude questions for Wave 7
data_attitudes_wave7 <- wvs_data_wave7[, gender_attitude_vars]

# STEP 1: Standardize (z-scores)
# Exclude rows with NA values before scaling to avoid issues
data_attitudes_wave7_cleaned <- na.omit(data_attitudes_wave7)
data_attitudes_wave7_scaled <- scale(data_attitudes_wave7_cleaned, center = TRUE, scale = TRUE)

#STEP 2: Covariance Matrix Computation
# Compute the covariance matrix of the standardized variables
cov_matrix_wave7 <- cov(data_attitudes_wave7_scaled)
# Print the covariance matrix
round(cov_matrix_wave7, 2)

# Plot the covariance (correlation) matrix
corrplot(cov_matrix_wave7, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8,
         title = "Correlation Matrix of Gender Attitudes (Wave 7)", mar = c(0,0,1,0))


#STEP 3: Compute the eigenvectors and eigenvalues of the covariance matrix
# to identify the principal components
# Compute eigenvalues and eigenvectors of the covariance matrix
eigen_decomp_wave7 <- eigen(cov_matrix_wave7)
# View eigenvalues (variance explained by each principal component)
eigen_decomp_wave7$values
# View eigenvectors (loadings of each original variable on each principal component)
eigen_decomp_wave7$vectors

# Scree plot 
plot(
  eigen_decomp_wave7$values,
  type = "b",
  xlab = "Principal Component",
  ylab = "Eigenvalue",
  pch = 19
)
abline(h = 1, col = "red", lty = 2) # Kaiser’s criterion (eigenvalues > 1),
abline(h = 0.7, col = "blue", lty = 2) # (eigenvalues > 0.7),

# Determine number of components to keep Kaiser’s criterion (eigenvalues > 0.7)
num_components_wave7 <- sum(eigen_decomp_wave7$values > 0.7)
cat(paste0("Number of components with eigenvalues > 0.7 for Wave 7: ", num_components_wave7, "\n"))

# STEP 4: Create a Feature Vector
# Use the number of components determined in the previous step
feature_vector_wave7 <- eigen_decomp_wave7$vectors[, 1:num_components_wave7, drop = FALSE]
# Label the rows and columns for clarity
rownames(feature_vector_wave7) <- gender_attitude_vars
colnames(feature_vector_wave7) <- paste0("PC", 1:num_components_wave7)
print("Feature Vector (Eigenvectors for selected PCs - Wave 7):")
print(feature_vector_wave7)

# STEP 5: Recast the Data Along the Principal Component Axes
# Project standardized data onto the principal component space using the feature vector

# Multiply the standardized data by the feature vector (eigenvectors for selected PCs)
pca_scores_wave7 <- data_attitudes_wave7_scaled %*% feature_vector_wave7

# Convert to a data frame and name the new principal component columns
pca_scores_wave7_df <- as.data.frame(pca_scores_wave7)
colnames(pca_scores_wave7_df) <- paste0("gender_attitude_wave7_PC", 1:num_components_wave7)

# Add scores directly to wvs_data for respondents in wave 7
wvs_data[wvs_data$wave == 7, paste0("gender_attitude_wave7_PC", 1:num_components_wave7)] <- pca_scores_wave7_df

# Example: Print variance explained by PC1 for Wave 7
variance_pc1_wave7 <- round((eigen_decomp_wave7$values[1]) / sum(eigen_decomp_wave7$values) * 100, 2)
cat(paste0("Wave 7 PCA - Variance explained by PC1: ", variance_pc1_wave7, "%\n"))


###Combine PC scores across waves into one variable PC1, PC2, PC3, PC4 
wvs_data <- wvs_data %>%
  mutate(
    PC1 = coalesce(gender_attitude_wave6_PC1, gender_attitude_wave7_PC1),
    PC2 = coalesce(gender_attitude_wave6_PC2, gender_attitude_wave7_PC2),
    PC3 = coalesce(gender_attitude_wave6_PC3, gender_attitude_wave7_PC3),
    PC4 = coalesce(gender_attitude_wave6_PC4, gender_attitude_wave7_PC4)
  )

## Flip the sign: now higher PC1 --> more egalitarian 
wvs_data$PC1 <- -1 * wvs_data$PC1

# ______________________________________________________________________________
##CONTROLS FOR COUNTRY-LEVEL MODEL

#proportion of female respondents in a given country-wave with some form 
#of tertiary education (short cycle/bachelors/masters/doctoral)

# 1. Create tertiary dummy based on harmonized variable
wvs_data <- wvs_data %>%
  mutate(
    tertiary_edu = ifelse(education %in% c(4, 5), 1, 0)
  )

# 2. Aggregate to country-wave level (only for females)
female_tertiary_rate <- wvs_data %>%
  filter(sex == 2) %>%
  group_by(country_code, wave) %>%
  summarise(
    female_tertiary_share = mean(tertiary_edu, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# 3. 
wvs_data <- wvs_data %>%
  left_join(female_tertiary_rate, by = c("country_code", "wave"))

###average number of children per female respondent in each country and wave.
# 1. Aggregate average number of children for females only, by country and wave
female_children_avg <- wvs_data %>%
  filter(sex == 2) %>%  # Keep only women
  group_by(country_code, wave) %>%
  summarise(
    avg_children = mean(children, na.rm = TRUE),
    n = n()  
  ) %>%
  ungroup()

# 2. Merge back into the main dataset
wvs_data <- wvs_data %>%
  left_join(female_children_avg, by = c("country_code", "wave"))

#### Aggregate female employment rate at country-wave level (for macro-level model)
emp_agg <- wvs_data %>%
  filter(sex == 2) %>%
  group_by(country_code, wave) %>%
  summarise(emp_rate_ct = mean(employment_recoded, na.rm = TRUE)) %>%
  ungroup()
# Step 2: Merge the aggregated variable back to the full dataset
wvs_data <- wvs_data %>%
  left_join(emp_agg, by = c("country_code", "wave"))


### Load other controls (GDP/capita and female unemployment rate) -->EVENTUALLY NOT NEEDED
# Step 1: Load country-level controls data
controls_data <- read_csv("country_level_controls.csv") 

# Step 2: Clean GDP column, convert unemployment to proportion, & ensure proper types
controls_data <- controls_data %>%
  mutate(
    GDP_capita = as.numeric(gsub(" ", "", GDP_capita)),
    female_unemployment = as.numeric(gsub(" ", "", female_unemployment)) / 100,  # Convert to proportion
    survey_year = as.numeric(survey_year)
  )

# Step 3: Map country names to 3-letter ISO codes (same codes used in wvs_data)
country_map <- data.frame(
  country = c("Australia", "Chile", "Colombia", "Germany", "Japan", "Mexico",
              "Netherlands", "New Zealand", "Korea", "Türkiye", "United States"),
  country_code = c("AUS", "CHL", "COL", "DEU", "JPN", "MEX",
                   "NLD", "NZL", "KOR", "TUR", "USA")
)

# Step 4: Add country codes
controls_data <- controls_data %>%
  left_join(country_map, by = "country")

# Step 5: Merge GDP and unemployment data into main dataset
wvs_data <- wvs_data %>%
  left_join(controls_data %>%
              select(country_code, survey_year, GDP_capita, female_unemployment),
            by = c("country_code", "survey_year"))

# ______________________________________________________________________________
###G_ct ###
### G_ct_1 to G_ct_4: Average PC scores by country and wave

# 1. Compute averages of PC1 to PC4 by country and wave
Gct_all <- wvs_data %>%
  group_by(country_code, wave) %>%
  summarise(
    Gct_1 = mean(PC1, na.rm = TRUE),
    Gct_2 = mean(PC2, na.rm = TRUE),
    Gct_3 = mean(PC3, na.rm = TRUE),
    Gct_4 = mean(PC4, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Merge macro-level PC scores back into wvs_data
wvs_data <- wvs_data %>%
  left_join(Gct_all, by = c("country_code", "wave"))

# ______________________________________________________________________________
###Maternal leave squared variable 
wvs_data <- wvs_data %>%
  mutate(total_maternal_leave_sq = total_maternal_leave^2)



# Save the final cleaned and prepared dataset
write.csv(wvs_data, "wvs_data_final_analytical_pca.csv", row.names = FALSE)


# ______________________________________________________________________________
# SECTION 2: ANALYSIS AND VISUALIZATION
# This section performs descriptive statistics, runs models, and generates plots
# using the 'wvs_data_final_analytical_pca.csv'

# If starting a new R session from here, load:
# wvs_data <- read.csv("wvs_data_final_analytical_pca.csv")


### DESCRIPTIVE STATISTICS ###

### SURVEY MODE ###
mode_summary <- wvs_data %>%
  group_by(wave, mode) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(wave, mode)

print(mode_summary)

### Total number of observations and Gender breakdown ###
# (male/female counts and shares) per wave.
gender_counts <- wvs_data %>%
  filter(sex %in% c(1, 2)) %>% # Keep only male (1) and female (2)
  group_by(wave, sex) %>%
  summarise(count = n(), .groups = "drop")
wave_totals <- wvs_data %>%
  group_by(wave) %>%
  summarise(total = n(), .groups = "drop")
gender_breakdown <- gender_counts %>%
  left_join(wave_totals, by = "wave") %>%
  mutate(share = round(count / total * 100, 2)) # share as percentage
print(gender_breakdown)


### DATA DESCRIPTIVES TABLE 1 ###

# Subset data per wave (for descriptives)
wave6_data <- wvs_data %>% filter(wave == 6)
wave7_data <- wvs_data %>% filter(wave == 7)

# List of variables to include in descriptives
vars_desc <- c("survey_year", "age", "children", "education",
               "sex", "employment_recoded", "parttime_recoded", "marital_status_recoded")

# Function to create descriptive summary
get_descriptive_table <- function(df) {
  describe(df[, vars_desc]) %>%
    as.data.frame() %>%
    select(n, mean, sd, min, max) %>%
    rownames_to_column(var = "variable")
}

# Generate descriptive tables
full_sample <- get_descriptive_table(wvs_data)
wave6 <- get_descriptive_table(wave6_data)
wave7 <- get_descriptive_table(wave7_data)

# Round columns to 2 decimal points
round_columns <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ round(., 2)))
}

full_sample <- round_columns(full_sample)
wave6 <- round_columns(wave6)
wave7 <- round_columns(wave7)

# Combine all tables
combined_table <- bind_rows(
  tibble(variable = "Full Sample", n = NA, mean = NA, sd = NA, min = NA, max = NA),
  full_sample,
  tibble(variable = "Wave 6", n = NA, mean = NA, sd = NA, min = NA, max = NA),
  wave6,
  tibble(variable = "Wave 7", n = NA, mean = NA, sd = NA, min = NA, max = NA),
  wave7
)

# Recode variable names for clarity
combined_table$variable <- recode(combined_table$variable,
                                  "survey_year" = "Survey Year",
                                  "age" = "Age",
                                  "children" = "Number of Children",
                                  "education" = "Education",
                                  "sex" = "Sex (1=Male, 2=Female)",
                                  "employment_recoded" = "Employed (1=Yes, 0=No)",
                                  "parttime_recoded" = "Part-time Employed (1=Yes, 0=No)",
                                  "marital_status_recoded" = "Married/Partnered (1=Yes, 0=No)")

# Replace NAs in summary rows with blank strings
combined_table <- combined_table %>%
  mutate(across(c(n, mean, sd, min, max), ~ ifelse(is.na(.), "", .)))

# Export to LaTeX
stargazer(combined_table,
          type = "latex",
          summary = FALSE,
          rownames = FALSE,
          title = "Descriptive Statistics of Sample Characteristics",
          label = "tab:desc_stats_all",
          out = "descriptive_statistics_full_sample_and_waves.tex")

# Preview in R
print(combined_table, n = 24)


### EDUCATION, EMPLOYMENT, AND PART-TIME EMPLOYMENT PER GENDER ###

# Helper function
get_gender_descriptives <- function(df, var) {
  male_stats <- df %>%
    filter(sex == 1) %>%
    summarise(
      n = n(),
      mean = round(mean(!!sym(var), na.rm = TRUE), 2),
      sd = round(sd(!!sym(var), na.rm = TRUE), 2),
      min = min(!!sym(var), na.rm = TRUE),
      max = max(!!sym(var), na.rm = TRUE)
    ) %>%
    mutate(variable = paste(var, "(Male)"))
  
  female_stats <- df %>%
    filter(sex == 2) %>%
    summarise(
      n = n(),
      mean = round(mean(!!sym(var), na.rm = TRUE), 2),
      sd = round(sd(!!sym(var), na.rm = TRUE), 2),
      min = min(!!sym(var), na.rm = TRUE),
      max = max(!!sym(var), na.rm = TRUE)
    ) %>%
    mutate(variable = paste(var, "(Female)"))
  
  bind_rows(male_stats, female_stats) %>%
    select(variable, n, mean, sd, min, max)
}

# Full sample
education_full <- get_gender_descriptives(wvs_data, "education")
employment_full <- get_gender_descriptives(wvs_data, "employment_recoded")
parttime_full <- get_gender_descriptives(wvs_data, "parttime_recoded")

# Wave 6
# ensure wave6_data is up-to-date with latest wvs_data
wave6_data <- wvs_data %>% filter(wave == 6)
education_wave6 <- get_gender_descriptives(wave6_data, "education")
employment_wave6 <- get_gender_descriptives(wave6_data, "employment_recoded")
parttime_wave6 <- get_gender_descriptives(wave6_data, "parttime_recoded")

# Wave 7
# ensure wave7_data is up-to-date with latest wvs_datawave7_data <- wvs_data %>% filter(wave == 7)
education_wave7 <- get_gender_descriptives(wave7_data, "education")
employment_wave7 <- get_gender_descriptives(wave7_data, "employment_recoded")
parttime_wave7 <- get_gender_descriptives(wave7_data, "parttime_recoded")

# Full sample
full_sample_gender <- bind_rows(education_full, employment_full, parttime_full)

# Wave 6
wave6_gender <- bind_rows(education_wave6, employment_wave6, parttime_wave6)

# Wave 7
wave7_gender <- bind_rows(education_wave7, employment_wave7, parttime_wave7)

# Print, copy-paste into latex editor to include in the table
print(full_sample_gender)
print(wave6_gender)
print(wave7_gender)


### PARENTAL LEAVE VISUALIZATIONS ###

# Summarise for plots (using the already merged policy data)
policy_data_summary <- policy_data %>%
  # Select the correct total leave variables
  select(country_code, year, total_maternal_leave, total_paternal_leave) %>%
  mutate(
    # Ensure both columns are numeric
    total_maternal_leave = as.numeric(total_maternal_leave),
    total_paternal_leave = as.numeric(total_paternal_leave)
  )

# Define the years for the panels
years_to_plot <- c(2015, 2023)

# Define the specific countries to include for both plots
countries_for_plots <- c("USA", "AUS", "NZL", "TUR", "MEX", "NLD", "COL", "CHL", "DEU", "KOR", "JPN")

# Create a complete grid of all desired countries and years
# This ensures that all countries in 'countries_for_plots' are present for both years,
complete_policy_grid <- expand_grid(
  country_code = countries_for_plots,
  year = years_to_plot
)

# Join the actual policy data with the complete grid
# This will fill in actual values where they exist and keep NAs for missing combinations
policy_data_complete <- left_join(complete_policy_grid, policy_data_summary,
                                  by = c("country_code", "year")) %>%
  # Replace NA leave values with 0 for the correct variables
  mutate(
    total_maternal_leave = replace_na(total_maternal_leave, 0),
    total_paternal_leave = replace_na(total_paternal_leave, 0)
  )

# Ensure country_code is a factor with the desired order for consistent plotting
policy_data_complete$country_code <- factor(policy_data_complete$country_code,
                                            levels = countries_for_plots)

# Create long format for plotting
wave6_data_long <- policy_data_complete %>%
  filter(year == 2015) %>%
  # Pivot the correct total leave columns
  pivot_longer(cols = c(total_maternal_leave, total_paternal_leave),
               names_to = "leave_type",
               values_to = "weeks")

wave7_data_long <- policy_data_complete %>%
  filter(year == 2023) %>%
  # Pivot the correct total leave columns
  pivot_longer(cols = c(total_maternal_leave, total_paternal_leave),
               names_to = "leave_type",
               values_to = "weeks")

# Panel A - 2015
panel_a <- ggplot(wave6_data_long, aes(x = country_code, y = weeks, fill = leave_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Weeks of paid leave (FRE)") +
  # Update the scale to match the new variable names and labels
  scale_fill_manual(
    values = c("total_maternal_leave" = "salmon", "total_paternal_leave" = "turquoise"),
    labels = c("total_maternal_leave" = "Total Maternal Leave", "total_paternal_leave" = "Total Paternal Leave")
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Panel B - 2023
panel_b <- ggplot(wave7_data_long, aes(x = country_code, y = weeks, fill = leave_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country", y = "Weeks of paid leave (FRE)") +
  # Update the scale to match the new variable names and labels
  scale_fill_manual(
    values = c("total_maternal_leave" = "salmon", "total_paternal_leave" = "turquoise"),
    labels = c("total_maternal_leave" = "Total Maternal Leave", "total_paternal_leave" = "Total Paternal Leave")
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Show plots
print(panel_a)
print(panel_b)

####_____________________FEMALE EDUCATION PER COUTNRY AND WAVE____________________

# Summarize women's education level by wave and country
women_education_by_wave_country <- wvs_data %>%
  # 1. Filter for female respondents only 
  filter(sex == 2) %>%
  
  # 2. Group by wave, country_code, and education level
  group_by(wave, country_code, education) %>%
  
  # 3. Count the number of women in each group
  summarise(count = n(), .groups = "drop_last") %>%
  
  # 4. Calculate the percentage for each education level within each wave-country group
  mutate(percentage = round(count / sum(count) * 100, 2)) %>%
  
  # 5. Ungroup to remove all grouping variables
  ungroup() %>%
  
  # 6. Arrange for better readability
  arrange(wave, country_code, education)

# Display the summary table
print(women_education_by_wave_country, n = Inf) # n = Inf to show all rows

# --- Optional: figure ---
# Define a color palette for education levels 
edu_colors <- c(
  "No education" = "#FCBBA1",           # Light red/orange
  "Primary education" = "#FC9272",      # Red-orange
  "Lower secondary" = "#FB6A4A",        # Medium red-orange
  "Upper secondary" = "#EF3B2C",        # Strong red-orange
  "Some university (no degree)" = "#CB181D", # Dark red
  "University with degree" = "#99000D"    # Very dark red (highest education)
)

# Create the stacked bar chart
ggplot(women_education_by_wave_country,
       aes(x = country_code, y = percentage, fill = education)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ wave, ncol = 1, scales = "free_x") + # Separate plots for each wave, with countries on X-axis
  scale_fill_manual(values = edu_colors, name = "Education Level") +
  labs(
    x = "Country",
    y = "Percentage of Women (%)"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # Angle X-axis labels for readability
    legend.position = "bottom", # Place legend at the bottom
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Center and bold title
    strip.text = element_text(size = 12, face = "bold"), # Make facet titles prominent
    panel.grid.major.x = element_blank(), # Remove vertical grid lines
    panel.grid.minor.x = element_blank()
  )


###________________Marital status by gender, country and wave____________________

# Summarize marital status by wave, country, and sex
marital_status_summary <- wvs_data %>%
  # Temporarily convert sex and marital_status_recoded to factors for better labels in output
  mutate(
    sex_label = factor(sex, levels = c(1, 2), labels = c("Men", "Women")),
    marital_status_label = factor(marital_status_recoded, levels = c(0, 1), labels = c("Not Married/Partnered", "Married/Partnered"))
  ) %>%
  # Group by all relevant categories
  group_by(wave, country_code, sex_label, marital_status_label) %>%
  # Count the number of individuals in each group
  summarise(count = n(), .groups = "drop_last") %>%
  # Calculate the percentage of each marital status within each wave-country-sex group
  mutate(percentage = round(count / sum(count) * 100, 2)) %>%
  # Ungroup for final table
  ungroup() %>%
  # Arrange for better readability
  arrange(wave, country_code, sex_label, marital_status_label)

# Display the summary table (shows a lot of rows)
print(marital_status_summary, n = Inf)

# --- Visualization for better insights ---
# A grouped bar chart comparing the percentage of Married/Partnered individuals
# between men and women within each country and wave.

# Filter for only the "Married/Partnered" percentage to simplify the plot
married_percentage_for_plot <- marital_status_summary %>%
  filter(marital_status_label == "Married/Partnered")

# Create the grouped bar chart
ggplot(married_percentage_for_plot,
       aes(x = country_code, y = percentage, fill = sex_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ wave, scales = "free_x", ncol = 1) + # Facet by wave, with countries on X-axis
  scale_fill_manual(values = c("Men" = "#42B3CC", "Women" = "#CB181D"), name = "Sex") + 
  labs(
    title = "Percentage of Married/Partnered Individuals by Country, Wave, and Sex",
    x = "Country",
    y = "Percentage Married/Partnered (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # Angle X-axis labels
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

###______________EMPLOYMENT PATTERNS BY GENDER, COUNTRY, WAVE_________________###

# --- 2. Analysis and Visualization for 'employment_recoded' ---

# Summarize employment status by wave, country, and sex
# All mutations to create labels and factors are done within this pipeline,
# not affecting the original 'wvs_data'.
employment_summary <- wvs_data %>%
  mutate(
    sex_label = factor(sex, levels = c(1, 2), labels = c("Men", "Women")),
    employment_status_label = factor(employment_recoded, levels = c(0, 1), labels = c("Not Employed", "Employed")),
    # Ensure country_code and wave are treated as factors for grouping in summary
    country_code_fct = factor(country_code),
    wave_fct = factor(wave)
  ) %>%
  group_by(wave_fct, country_code_fct, sex_label, employment_status_label) %>%
  summarise(count = n(), .groups = "drop_last") %>% # Count individuals
  mutate(percentage = round(count / sum(count) * 100, 2)) %>% # Calculate percentage within group
  ungroup() %>%
  # Arrange by the new factor variables for cleaner output
  arrange(wave_fct, country_code_fct, sex_label, employment_status_label)

# Display the summary table for employment status
cat("--- Summary for Employment Status (Recoded) ---\n")
print(employment_summary, n = Inf)


# Create a grouped bar chart for employment status (focus on 'Employed')
# Filter for only the "Employed" percentage to simplify the plot
employed_percentage_for_plot <- employment_summary %>%
  filter(employment_status_label == "Employed")

# Define custom colors for men and women
gender_colors <- c("Men" = "#42B3CC", "Women" = "#CB181D") 

# Generate the plot for Employment Status
plot_employment <- ggplot(employed_percentage_for_plot,
                          aes(x = country_code_fct, y = percentage, fill = sex_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ wave_fct, scales = "free_x", ncol = 1) + # Separate plots for each wave
  scale_fill_manual(values = gender_colors, name = "Sex") + # Apply custom colors
  labs(
    x = "Country",
    y = "Percentage Employed (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Display the plot
print(plot_employment)

# Save the plot
ggsave("employment_status_by_sex_country_wave_plot.png", plot = plot_employment, width = 12, height = 8, dpi = 300)
ggsave("employment_status_by_sex_country_wave_plot.pdf", plot = plot_employment, width = 12, height = 8)


# --- 3. Analysis and Visualization for 'parttime_recoded' ---

# Summarize part-time employment status by wave, country, and sex
# All mutations to create labels and factors are done within this pipeline,
# not affecting the original 'wvs_data'.
parttime_summary <- wvs_data %>%
  mutate(
    sex_label = factor(sex, levels = c(1, 2), labels = c("Men", "Women")),
    parttime_status_label = factor(parttime_recoded, levels = c(0, 1), labels = c("Not Part-time", "Part-time")),
    # Ensure country_code and wave are treated as factors for grouping in summary
    country_code_fct = factor(country_code),
    wave_fct = factor(wave)
  ) %>%
  group_by(wave_fct, country_code_fct, sex_label, parttime_status_label) %>%
  summarise(count = n(), .groups = "drop_last") %>% # Count individuals
  mutate(percentage = round(count / sum(count) * 100, 2)) %>% # Calculate percentage within group
  ungroup() %>%
  # Arrange by the new factor variables for cleaner output
  arrange(wave_fct, country_code_fct, sex_label, parttime_status_label)


# Display the summary table for part-time employment status
cat("\n--- Summary for Part-time Employment Status (Recoded) ---\n")
print(parttime_summary, n = Inf)


# Create a grouped bar chart for part-time employment status (focus on 'Part-time')
# Filter for only the "Part-time" percentage to simplify the plot
parttime_percentage_for_plot <- parttime_summary %>%
  filter(parttime_status_label == "Part-time")

# Generate the plot for Part-time Employment Status
plot_parttime <- ggplot(parttime_percentage_for_plot,
                        aes(x = country_code_fct, y = percentage, fill = sex_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ wave_fct, scales = "free_x", ncol = 1) + # Separate plots for each wave
  scale_fill_manual(values = gender_colors, name = "Sex") + # Apply custom colors
  labs(
    x = "Country",
    y = "Percentage Part-time Employed (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Display the plot
print(plot_parttime)


### ______________________GENDER ROLE ATTITUDES DESCRIPTIVES_________________ ###

# List of gender role attitude variables (re-declared for clarity in this section)
gender_attitude_vars_for_descriptives <- c(
  "jobs_scarce",
  "housewife_fulfilling",
  "men_better_leaders",
  "uni_important_boys",
  "men_better_executives",
  "child_suffers",
  "problem_higher_income"
)

# Helper function: descriptive stats for a set of variables
# This function is duplicated from before, but kept for independent execution if needed.
get_gender_descriptives_attitudes <- function(df, vars) {
  stats_list <- lapply(vars, function(var) {
    # Male stats
    male_stats <- df %>%
      filter(sex == 1) %>%
      summarise(
        n = sum(!is.na(.data[[var]])),
        mean = round(mean(.data[[var]], na.rm = TRUE), 2),
        sd = round(sd(.data[[var]], na.rm = TRUE), 2),
        min = min(.data[[var]], na.rm = TRUE),
        max = max(.data[[var]], na.rm = TRUE)
      ) %>%
      mutate(variable = paste(var, "(Male)"))
    
    # Female stats
    female_stats <- df %>%
      filter(sex == 2) %>%
      summarise(
        n = sum(!is.na(.data[[var]])),
        mean = round(mean(.data[[var]], na.rm = TRUE), 2),
        sd = round(sd(.data[[var]], na.rm = TRUE), 2),
        min = min(.data[[var]], na.rm = TRUE),
        max = max(.data[[var]], na.rm = TRUE)
      ) %>%
      mutate(variable = paste(var, "(Female)"))
    
    bind_rows(male_stats, female_stats)
  })
  bind_rows(stats_list) %>%
    select(variable, n, mean, sd, min, max)
}

# Full sample
attitudes_full <- get_gender_descriptives_attitudes(wvs_data, gender_attitude_vars_for_descriptives)

# Wave 6
wave6_data <- wvs_data %>% filter(wave == 6)
attitudes_wave6 <- get_gender_descriptives_attitudes(wave6_data, gender_attitude_vars_for_descriptives)

# Wave 7
wave7_data <- wvs_data %>% filter(wave == 7)
attitudes_wave7 <- get_gender_descriptives_attitudes(wave7_data, gender_attitude_vars_for_descriptives)

# Save for potential LaTeX tables
write.csv(attitudes_full, "gender_attitudes_descriptives_full.csv", row.names = FALSE)
write.csv(attitudes_wave6, "gender_attitudes_descriptives_wave6.csv", row.names = FALSE)
write.csv(attitudes_wave7, "gender_attitudes_descriptives_wave7.csv", row.names = FALSE)

### VISUALIZATION: BARS FOR WOMEN + DIAMOND POINTS FOR MEN (Gender Attitudes) ###

# Combine dataframes with a new column indicating wave
attitudes_full$wave <- "Panel A: Full Sample"
attitudes_wave6$wave <- "Panel B: Wave 6"
attitudes_wave7$wave <- "Panel C: Wave 7"
attitudes_combined <- bind_rows(attitudes_full, attitudes_wave6, attitudes_wave7)

# Separate variable name from gender for easier plotting
attitudes_combined <- attitudes_combined %>%
  separate(variable, into = c("question", "gender"), sep = " \\(", remove = FALSE) %>%
  mutate(gender = gsub("\\)", "", gender))

# Rename questions to Q1, Q2, etc. (uses the order of gender_attitude_vars_for_descriptives)
attitudes_combined$question <- factor(paste0("Q", match(attitudes_combined$question, gender_attitude_vars_for_descriptives)))

# Set gender as factor
attitudes_combined$gender <- factor(attitudes_combined$gender, levels = c("Female", "Male"))

# Plot
ggplot(attitudes_combined, aes(x = question, y = mean, group = gender)) +
  geom_col(data = attitudes_combined %>% filter(gender == "Female"),
           aes(fill = gender),
           position = position_dodge(width = 0.6), width = 0.6, alpha = 0.8) +
  geom_point(data = attitudes_combined %>% filter(gender == "Male"),
             aes(shape = gender),
             color = "#1F77B4", size = 3.5,
             position = position_dodge(width = 0.6)) +
  facet_wrap(~ wave, scales = "fixed", ncol = 3) +
  labs(
    x = "Question", y = "Mean Response",
    fill = "", shape = ""
  ) +
  scale_fill_manual(
    values = c("Female" = "#A6C6DE"),
    labels = c("Female (Bars)"),
    guide = guide_legend(order = 1)
  ) +
  scale_shape_manual(
    values = c("Male" = 18),
    labels = c("Male (Diamonds)"),
    guide = guide_legend(order = 2)
  ) +
  scale_x_discrete(labels = 1:7) + # Adjust x-axis labels to numbers
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )


### PCA - total and cummulative variance explained __________________________

###_______________________________________________________________________________
# Calculate total variance explained by the first 4 PCs
variance_explained_wave7 <- eigen_decomp_wave7$values[1:4] / sum(eigen_decomp_wave7$values)
total_variance_wave7_pc1_4 <- round(sum(variance_explained_wave7) * 100, 2)

# Print result
cat(paste0("Wave 7 PCA - Total variance explained by PC1 to PC4: ", total_variance_wave7_pc1_4, "%\n"))


# Calculate total variance explained by the first 4 PCs
variance_explained_wave6 <- eigen_decomp_wave6$values[1:4] / sum(eigen_decomp_wave6$values)
total_variance_wave6_pc1_4 <- round(sum(variance_explained_wave6) * 100, 2)

# Print result
cat(paste0("Wave 6 PCA - Total variance explained by PC1 to PC4: ", total_variance_wave6_pc1_4, "%\n"))

###_______________________________________________________________________________
###Cumulative variance explained 

eigen_decomp_wave6 <- eigen(cov_matrix_wave6)
eigenvalues <- eigen_decomp_wave6$values
# Calculate cumulative variance explained
cumulative_variance <- cumsum(eigenvalues) / sum(eigenvalues)

# Plot
plot(cumulative_variance,
     type = "b", pch = 19,
     xlab = "Number of Principal Components",
     ylab = "Cumulative Variance Explained",
     main = "Cumulative Variance Explained by PCs (Wave 6)")
abline(h = 0.7, col = "red", lty = 2)  #threshold line

# Wave 7
eigenvalues_wave7 <- eigen_decomp_wave7$values

# Calculate cumulative variance explained
cumulative_variance_wave7 <- cumsum(eigenvalues_wave7) / sum(eigenvalues_wave7)

# Plot
plot(cumulative_variance_wave7,
     type = "b", pch = 19,
     xlab = "Number of Principal Components",
     ylab = "Cumulative Variance Explained",
     main = "Cumulative Variance Explained by PCs (Wave 7)")
abline(h = 0.7, col = "red", lty = 2)  # Optional: Jolliffe's 0.7 threshold



### PCA INDEX DESCRIPTIVES AND VISUALIZATION ###

# Print variance explained by first PC from each wave (if not already printed)
cat(paste0("Wave 6 - Variance explained by PC1: ", variance_pc1_wave6, "%\n"))
cat(paste0("Wave 7 - Variance explained by PC1: ", variance_pc1_wave7, "%\n"))

# Summary statistics of PC1 by wave and sex
pca_summary <- wvs_data %>%
  group_by(wave, sex) %>%
  summarise(
    n = n(),
    mean = round(mean(PC1, na.rm = TRUE), 2),
    median = round(median(PC1, na.rm = TRUE), 2),
    sd = round(sd(PC1, na.rm = TRUE), 2),
    min = round(min(PC1, na.rm = TRUE), 2),
    max = round(max(PC1, na.rm = TRUE), 2),
    q1 = round(quantile(PC1, 0.25, na.rm = TRUE)[[1]], 2),
    q3 = round(quantile(PC1, 0.75, na.rm = TRUE)[[1]], 2)
  )

# Export LaTeX table
stargazer(
  pca_summary,
  summary = FALSE,
  rownames = FALSE,
  title = "Summary Statistics of Gender Norms Index (PC1) by Gender and Wave",
  label = "tab:pc1_summary",
  out = "pc1_summary_table.tex"
)

# Save to CSV
write.csv(pca_summary, "pc1_summary_by_gender_and_wave.csv", row.names = FALSE)

# Visualize PC1 by country and gender
pca_summary_by_country_gender <- wvs_data %>%
  group_by(country_code, wave, sex) %>%
  summarise(mean_pc1 = mean(PC1, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    gender = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  )

# 1. Desired order of countries
desired_country_order <- c("USA", "AUS", "NZL", "TUR", "MEX", "NLD", "COL", "CHL", "DEU", "KOR", "JPN")

# 2. Define global y-axis limits across both waves
combined_range <- range(
  c(pca_wave6_plot_data$mean_pc1, pca_wave7_plot_data$mean_pc1),
  na.rm = TRUE
)

# 3. Wave 6 plot
ggplot() +
  geom_col(data = filter(pca_wave6_plot_data, gender == "Female"),
           aes(x = country_code, y = mean_pc1, fill = gender),
           alpha = 0.8, width = 0.6) +
  geom_point(data = filter(pca_wave6_plot_data, gender == "Male"),
             aes(x = country_code, y = mean_pc1, shape = gender),
             color = "black", size = 3.5) +
  labs(x = "Country", y = "Mean Gender Norms Index (PC1)", fill = "", shape = "") +
  scale_fill_manual(values = c("Female" = "#A6C6DE")) +
  scale_shape_manual(values = c("Male" = 18)) +
  coord_cartesian(ylim = combined_range) +  # ensures both plots share same scale
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank())

# 4. Wave 7 plot
ggplot() +
  geom_col(data = filter(pca_wave7_plot_data, gender == "Female"),
           aes(x = country_code, y = mean_pc1, fill = gender),
           alpha = 0.8, width = 0.6) +
  geom_point(data = filter(pca_wave7_plot_data, gender == "Male"),
             aes(x = country_code, y = mean_pc1, shape = gender),
             color = "black", size = 3.5) +
  labs(x = "Country", y = "Mean Gender Norms Index (PC1)", fill = "", shape = "") +
  scale_fill_manual(values = c("Female" = "#A6C6DE")) +
  scale_shape_manual(values = c("Male" = 18)) +
  coord_cartesian(ylim = combined_range) +  # same y-axis limits here
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank())

# ______________________________________________________________________________
# SECTION 3: MICRO-LEVEL MODEL
# This section runs individual level logit model 
# Cluster st.errors at a country and wave level 

# Convert education to a factor with meaningful labels
wvs_data$education <- factor(wvs_data$education,
                             levels = c(0, 1, 2, 3, 4, 5),
                             labels = c("No education", "Primary education",
                                        "Lower secondary", "Upper secondary",
                                        "Some university (no degree)", "University with degree"))


#_______________LOGISTIC REGRESSION FOR EMPLOYMENT______________________________

# Create cluster id
wvs_data <- wvs_data %>%
  mutate(cluster_id = paste0(country_code, "_", wave))

# Null model (intercept only) for Chi-2 comparisons
null_model <- glm(
  employment_recoded ~ 1,
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)

# ---- Model 1: Demographics ----
emp_demographics_model <- glm(
  employment_recoded ~ education + age + marital_status_recoded + children +
    factor(country_code) + factor(wave),
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)
clustered_se_emp_demo <- vcovCL(emp_demographics_model, cluster = ~cluster_id)
coeftest(emp_demographics_model, vcov = clustered_se_emp_demo, df = Inf)

# Chi-2 for Model 1
chi2_demo <- 2 * (logLik(emp_demographics_model) - logLik(null_model))
print(paste("Model 1 Chi-squared:", round(chi2_demo, 2)))

# ---- Model 2: Gender Norms ----
emp_gendernorms_model <- glm(
  employment_recoded ~ education + age + marital_status_recoded + children +
    PC1 + Gct +
    factor(country_code) + factor(wave),
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)
clustered_se_emp_gender <- vcovCL(emp_gendernorms_model, cluster = ~cluster_id)
coeftest(emp_gendernorms_model, vcov = clustered_se_emp_gender, df = Inf)

chi2_gender <- 2 * (logLik(emp_gendernorms_model) - logLik(null_model))
print(paste("Model 2 Chi-squared:", round(chi2_gender, 2)))

# ---- Model 3: Leave Policies ----
emp_leavepolicies_model <- glm(
  employment_recoded ~ education + age + marital_status_recoded + children +
    PC1 + Gct +
    total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
    factor(country_code) + factor(wave),
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)
clustered_se_emp_leave <- vcovCL(emp_leavepolicies_model, cluster = ~cluster_id)
coeftest(emp_leavepolicies_model, vcov = clustered_se_emp_leave, df = Inf)

chi2_leave <- 2 * (logLik(emp_leavepolicies_model) - logLik(null_model))
print(paste("Model 3 Chi-squared:", round(chi2_leave, 2)))

# ---- Model 4: Full Interaction Model ----
emp_full_model <- glm(
  employment_recoded ~ education + age + marital_status_recoded + children +
    PC1 + Gct +
    total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
    PC1:total_maternal_leave + PC1:total_paternal_leave +
    factor(country_code) + factor(wave),
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)
clustered_se_emp_full <- vcovCL(emp_full_model, cluster = ~cluster_id)
coeftest(emp_full_model, vcov = clustered_se_emp_full, df = Inf)

chi2_full <- 2 * (logLik(emp_full_model) - logLik(null_model))
print(paste("Model 4 Chi-squared:", round(chi2_full, 2)))

#______________________ Report AMEs ______________________

# Model 1: Demographics
ame_emp_demo <- margins(emp_demographics_model, vcov = clustered_se_emp_demo)
summary(ame_emp_demo)

# Model 2: Gender Norms
ame_emp_gender <- margins(emp_gendernorms_model, vcov = clustered_se_emp_gender)
summary(ame_emp_gender)

# Model 3: Leave Policies
ame_emp_leave <- margins(emp_leavepolicies_model, vcov = clustered_se_emp_leave)
summary(ame_emp_leave)

# Model 4: Full Model
ame_emp_full <- margins(emp_full_model, vcov = clustered_se_emp_full)
summary(ame_emp_full)

#______________________ Report Number of Observations ______________________

# Number of observations for Model 1
nobs_m1 <- nobs(emp_demographics_model)
cat("No. observations (Model 1):", nobs_m1, "\n")

# Number of observations for Model 2
nobs_m2 <- nobs(emp_gendernorms_model)
cat("No. observations (Model 2):", nobs_m2, "\n")

# Number of observations for Model 3
nobs_m3 <- nobs(emp_leavepolicies_model)
cat("No. observations (Model 3):", nobs_m3, "\n")

# Number of observations for Model 4
nobs_m4 <- nobs(emp_full_model)
cat("No. observations (Model 4):", nobs_m4, "\n")

#___________________Visualize interaction terms________________________________
# --- Step 1: Choose meaningful values for PC1 to condition on ---
PC1_values_in_model <- model.frame(emp_full_model)$PC1
PC1_quantiles <- quantile(PC1_values_in_model, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
names(PC1_quantiles) <- c("10th Percentile (Traditional)", "Median (Neutral)", "90th Percentile (Egalitarian)")

cat("PC1 Values for Conditional Analysis:\n")
print(PC1_quantiles)
cat("\n")


# --- Analysis for Hypothesis 1: Effect of Maternal Leave by Gender Role Attitudes (PC1) ---
cat("--- AME of Total Maternal Leave conditional on PC1 (Gender Attitudes) ---\n")
mfx_maternal_leave_by_PC1 <- margins(
  emp_full_model,
  variables = "total_maternal_leave",
  at = list(PC1 = PC1_quantiles),
  vcov = clustered_se_emp_full
)
print(summary(mfx_maternal_leave_by_PC1))
cat("\n")

# --- Analysis for Hypothesis 2: Effect of Paternal Leave by Gender Role Attitudes (PC1) ---
cat("--- AME of Total Paternal Leave conditional on PC1 (Gender Attitudes) ---\n")
mfx_paternal_leave_by_PC1 <- margins(
  emp_full_model,
  variables = "total_paternal_leave",
  at = list(PC1 = PC1_quantiles),
  vcov = clustered_se_emp_full
)
print(summary(mfx_paternal_leave_by_PC1))
cat("\n")

# --- Plotting these Conditional AMEs ---

# Plot for Maternal Leave: AME of Maternal Leave across PC1 range
PC1_range <- seq(min(PC1_values_in_model, na.rm=TRUE), max(PC1_values_in_model, na.rm=TRUE), length.out = 100)

mfx_maternal_leave_plot_raw <- margins(
  emp_full_model,
  variables = "total_maternal_leave",
  at = list(PC1 = PC1_range),
  vcov = clustered_se_emp_full
)

# A Fix: Create a data frame from the summary output which has the expected column names
mfx_maternal_leave_plot_df <- as.data.frame(summary(mfx_maternal_leave_plot_raw))


ggplot(mfx_maternal_leave_plot_df, aes(x = PC1, y = AME)) + # Use PC1 from the dataframe
  geom_line(color = "darkblue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "lightblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", size = 0.8) +
  labs(
    x = "Gender Role Attitudes (PC1)",
    y = "AME of Total Maternal Leave (percentage points)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Plot for Paternal Leave: AME of Paternal Leave across PC1 range
mfx_paternal_leave_plot_raw <- margins(
  emp_full_model,
  variables = "total_paternal_leave",
  at = list(PC1 = PC1_range),
  vcov = clustered_se_emp_full
)

# A Fix: Create a data frame from the summary output
mfx_paternal_leave_plot_df <- as.data.frame(summary(mfx_paternal_leave_plot_raw))

ggplot(mfx_paternal_leave_plot_df, aes(x = PC1, y = AME)) + # Use PC1 from the dataframe
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "lightgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", size = 0.8) +
  labs(
    x = "Gender Role Attitudes (PC1)",
    y = "AME of Total Paternal Leave (percentage points)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))


#_______________ LOGISTIC REGRESSION FOR PART-TIME EMPLOYMENT___________________

# Create cluster id (already created above, no need to repeat unless in new session)

# Null model (intercept only) for Chi-2 comparisons
null_model_pt <- glm(
  parttime_recoded ~ 1,
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)

# ---- Model 1: Demographics ----
pt_demographics_model <- glm(
  parttime_recoded ~ education + age + marital_status_recoded + children +
    factor(country_code) + factor(wave),
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)
clustered_se_pt_demo <- vcovCL(pt_demographics_model, cluster = ~cluster_id)
coeftest(pt_demographics_model, vcov = clustered_se_pt_demo, df = Inf)

chi2_pt_demo <- 2 * (logLik(pt_demographics_model) - logLik(null_model_pt))
print(paste("Model 1 Chi-squared (part-time):", round(chi2_pt_demo, 2)))

# ---- Model 2: Gender Norms ----
pt_gendernorms_model <- glm(
  parttime_recoded ~ education + age + marital_status_recoded + children +
    PC1 + Gct +
    factor(country_code) + factor(wave),
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)
clustered_se_pt_gender <- vcovCL(pt_gendernorms_model, cluster = ~cluster_id)
coeftest(pt_gendernorms_model, vcov = clustered_se_pt_gender, df = Inf)

chi2_pt_gender <- 2 * (logLik(pt_gendernorms_model) - logLik(null_model_pt))
print(paste("Model 2 Chi-squared (part-time):", round(chi2_pt_gender, 2)))

# ---- Model 3: Leave Policies ----
pt_leavepolicies_model <- glm(
  parttime_recoded ~ education + age + marital_status_recoded + children +
    PC1 + Gct +
    total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
    factor(country_code) + factor(wave),
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)
clustered_se_pt_leave <- vcovCL(pt_leavepolicies_model, cluster = ~cluster_id)
coeftest(pt_leavepolicies_model, vcov = clustered_se_pt_leave, df = Inf)

chi2_pt_leave <- 2 * (logLik(pt_leavepolicies_model) - logLik(null_model_pt))
print(paste("Model 3 Chi-squared (part-time):", round(chi2_pt_leave, 2)))

# ---- Model 4: Full Interaction Model ----
pt_full_model <- glm(
  parttime_recoded ~ education + age + marital_status_recoded + children +
    PC1 + Gct +
    total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
    PC1:total_maternal_leave + PC1:total_paternal_leave +
    factor(country_code) + factor(wave),
  data = wvs_data %>% filter(sex == 2),
  family = binomial(link = "logit")
)
clustered_se_pt_full <- vcovCL(pt_full_model, cluster = ~cluster_id)
coeftest(pt_full_model, vcov = clustered_se_pt_full, df = Inf)

chi2_pt_full <- 2 * (logLik(pt_full_model) - logLik(null_model_pt))
print(paste("Model 4 Chi-squared (part-time):", round(chi2_pt_full, 2)))

#______________________ Report AMEs ______________________

# Model 1: Demographics
ame_pt_demo <- margins(pt_demographics_model, vcov = clustered_se_pt_demo)
summary(ame_pt_demo)

# Model 2: Gender Norms
ame_pt_gender <- margins(pt_gendernorms_model, vcov = clustered_se_pt_gender)
summary(ame_pt_gender)


# Model 3: Leave Policies
ame_pt_leave <- margins(pt_leavepolicies_model, vcov = clustered_se_pt_leave)
summary(ame_pt_leave)

# Model 4: Full Model
ame_pt_full <- margins(pt_full_model, vcov = clustered_se_pt_full)
summary(ame_pt_full)

#______________________ Report Number of Observations ______________________

cat("No. observations (Model 1, part-time):", nobs(pt_demographics_model), "\n")
cat("No. observations (Model 2, part-time):", nobs(pt_gendernorms_model), "\n")
cat("No. observations (Model 3, part-time):", nobs(pt_leavepolicies_model), "\n")
cat("No. observations (Model 4, part-time):", nobs(pt_full_model), "\n")

#___________________Visualize interaction terms________________________________

# --- Step 1: Choose meaningful values for PC1 to condition on ---
# Using PC1 from the full part-time model's data
PC1_values_in_pt_full_model <- model.frame(pt_full_model)$PC1
PC1_quantiles_pt <- quantile(PC1_values_in_pt_full_model, probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
names(PC1_quantiles_pt) <- c("10th Percentile (Traditional)", "Median (Neutral)", "90th Percentile (Egalitarian)")

cat("PC1 Values for Conditional Analysis (Part-Time Employment Model):\n")
print(PC1_quantiles_pt)
cat("\n")


# --- Analysis for Hypothesis 1: Effect of Maternal Leave by Gender Role Attitudes (PC1) on Part-Time Employment ---
cat("--- AME of Total Maternal Leave conditional on PC1 (Part-Time Employment Model) ---\n")
mfx_maternal_leave_by_PC1_pt <- margins(
  pt_full_model,
  variables = "total_maternal_leave",
  at = list(PC1 = PC1_quantiles_pt), # Use quantiles from the part-time model's PC1
  vcov = clustered_se_pt_full        # Use clustered SEs for the part-time model
)
print(summary(mfx_maternal_leave_by_PC1_pt))
cat("\n")

# --- Analysis for Hypothesis 2: Effect of Paternal Leave by Gender Role Attitudes (PC1) on Part-Time Employment ---
cat("--- AME of Total Paternal Leave conditional on PC1 (Part-Time Employment Model) ---\n")
mfx_paternal_leave_by_PC1_pt <- margins(
  pt_full_model,
  variables = "total_paternal_leave",
  at = list(PC1 = PC1_quantiles_pt), # Use quantiles from the part-time model's PC1
  vcov = clustered_se_pt_full        # Use clustered SEs for the part-time model
)
print(summary(mfx_paternal_leave_by_PC1_pt))
cat("\n")

# --- Plotting these Conditional AMEs for Part-Time Employment ---

# Plot for Maternal Leave: AME of Maternal Leave across PC1 range for Part-Time Employment
PC1_range_pt <- seq(min(PC1_values_in_pt_full_model, na.rm=TRUE), max(PC1_values_in_pt_full_model, na.rm=TRUE), length.out = 100)

mfx_maternal_leave_plot_raw_pt <- margins(
  pt_full_model,
  variables = "total_maternal_leave",
  at = list(PC1 = PC1_range_pt),
  vcov = clustered_se_pt_full
)

# Create a data frame from the summary output
mfx_maternal_leave_plot_df_pt <- as.data.frame(summary(mfx_maternal_leave_plot_raw_pt))

ggplot(mfx_maternal_leave_plot_df_pt, aes(x = PC1, y = AME)) +
  geom_line(color = "darkblue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "lightblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", size = 0.8) +
  labs(
    x = "Gender Role Attitudes (PC1)",
    y = "AME of Total Maternal Leave (percentage points)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Plot for Paternal Leave: AME of Paternal Leave across PC1 range for Part-Time Employment
mfx_paternal_leave_plot_raw_pt <- margins(
  pt_full_model,
  variables = "total_paternal_leave",
  at = list(PC1 = PC1_range_pt),
  vcov = clustered_se_pt_full
)

# Create a data frame from the summary output
mfx_paternal_leave_plot_df_pt <- as.data.frame(summary(mfx_paternal_leave_plot_raw_pt))

ggplot(mfx_paternal_leave_plot_df_pt, aes(x = PC1, y = AME)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "lightgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", size = 0.8) +
  labs(
    x = "Gender Role Attitudes (PC1)",
    y = "AME of Total Paternal Leave (percentage points)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# ______________________________________________________________________________
# SECTION 4: MACRO-LEVEL MODEL
# This section runs country-level model 
# Cluster st.errors by country


# Create a country-wave dataset 
country_level_data <- wvs_data %>%
  select(country_code, wave, emp_rate_ct, Gct_1, Gct_2, Gct_3, Gct_4, 
         total_maternal_leave, total_maternal_leave_sq,
         total_paternal_leave, female_tertiary_share, avg_children) %>%
  distinct()  # keep only one row per country-wave combo

write.csv()

# --- Define and Estimate Macro-Level Models (OLS) ---

# Model 1: Controls only
MR_C <- lm(emp_rate_ct ~ female_tertiary_share + avg_children + factor(wave),
           data = country_level_data)
clustered_se_mrc <- vcovCL(MR_C, cluster = ~country_code)
coeftest(MR_C, vcov = clustered_se_mrc)

# Model 2: + Gender Norms
MR_CN <- lm(emp_rate_ct ~ Gct +
              female_tertiary_share + avg_children + factor(wave),
            data = country_level_data)
clustered_se_mrcn <- vcovCL(MR_CN, cluster = ~country_code)
coeftest(MR_CN, vcov = clustered_se_mrcn)

# Model 3: + Parental Leave Policies
MR_CNP <- lm(emp_rate_ct ~ Gct +
               total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
               female_tertiary_share + avg_children + factor(wave),
             data = country_level_data)
clustered_se_mrcnp <- vcovCL(MR_CNP, cluster = ~country_code)
coeftest(MR_CNP, vcov = clustered_se_mrcnp)

# Model 4: Full Model with interactions
MR_FULL <- lm(emp_rate_ct ~ Gct +
                total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
                Gct:total_maternal_leave + Gct:total_paternal_leave +
                female_tertiary_share + avg_children + factor(wave),
              data = country_level_data)
clustered_se_full <- vcovCL(MR_FULL, cluster = ~country_code)
coeftest(MR_FULL, vcov = clustered_se_full)

### Extract model statistics

extract_model_stats <- function(model) {
  summary_model <- summary(model)
  
  adj_r2 <- summary_model$adj.r.squared
  f_stat <- summary_model$fstatistic[1]
  df1 <- summary_model$fstatistic[2]
  df2 <- summary_model$fstatistic[3]
  f_pval <- pf(f_stat, df1, df2, lower.tail = FALSE)
  
  return(data.frame(
    Adj_R2 = round(adj_r2, 3),
    F_stat = round(f_stat, 2),
    F_pval = signif(f_pval, 3)
  ))
}

# Apply to each model
model_stats <- rbind(
  C = extract_model_stats(MR_C),
  CN = extract_model_stats(MR_CN),
  CNP = extract_model_stats(MR_CNP),
  FULL = extract_model_stats(MR_FULL)
)

# Print results
print(model_stats)

#### Check for multicollinearity between policy and gender norms________________
cor(country_level_data[, c("Gct", "female_tertiary_share", "total_maternal_leave", "total_paternal_leave")], use = "complete.obs")


###____________________________________________________________________________
###____________SECTION 5: MODELS WITH ALL PCs____________________________________


###____________MACRO LEVEL MODELS___________

#Model 2: + Gender Norms ALL PCs (macro-level PCA)
MR_CN_GCTPCS  <- lm(emp_rate_ct ~ Gct_1 + Gct_2 + Gct_3 + Gct_4 +
                      female_tertiary_share + avg_children + factor(wave),
                    data = country_level_data)
clustered_se_cn_gctpcs <- vcovCL(MR_CN_GCTPCS, cluster = ~country_code)
coeftest(MR_CN_GCTPCS, vcov = clustered_se_cn_gctpcs)


#Model 3: + Leave Policies
MR_CNP_GCTPCS <- lm(emp_rate_ct ~ Gct_1 + Gct_2 + Gct_3 + Gct_4 +
                      total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
                      female_tertiary_share + avg_children + factor(wave),
                    data = country_level_data)
clustered_se_cnp_gctpcs <- vcovCL(MR_CNP_GCTPCS, cluster = ~country_code)
coeftest(MR_CNP_GCTPCS, vcov = clustered_se_cnp_gctpcs)

#Model 4: Full Model with Interactions (interact only Gct_1)
MR_FULL_GCTPCS <- lm(emp_rate_ct ~ Gct_1 + Gct_2 + Gct_3 + Gct_4 +
                       total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
                       Gct_1:total_maternal_leave + Gct_1:total_paternal_leave +
                       female_tertiary_share + avg_children + factor(wave),
                     data = country_level_data)
clustered_se_full_gctpcs <- vcovCL(MR_FULL_GCTPCS, cluster = ~country_code)
coeftest(MR_FULL_GCTPCS, vcov = clustered_se_full_gctpcs)

#Model 4B: Interaction with Gct_2
MR_FULL_GCTPCS_2 <- lm(emp_rate_ct ~ Gct_1 + Gct_2 + Gct_3 + Gct_4 +
                         total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
                         Gct_2:total_maternal_leave + Gct_2:total_paternal_leave +
                         female_tertiary_share + avg_children + factor(wave),
                       data = country_level_data)
clustered_se_full_gctpcs_2 <- vcovCL(MR_FULL_GCTPCS_2, cluster = ~country_code)
coeftest(MR_FULL_GCTPCS_2, vcov = clustered_se_full_gctpcs_2)

#Model 4C: Interaction with Gct_3
MR_FULL_GCTPCS_3 <- lm(emp_rate_ct ~ Gct_1 + Gct_2 + Gct_3 + Gct_4 +
                         total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
                         Gct_3:total_maternal_leave + Gct_3:total_paternal_leave +
                         female_tertiary_share + avg_children + factor(wave),
                       data = country_level_data)
clustered_se_full_gctpcs_3 <- vcovCL(MR_FULL_GCTPCS_3, cluster = ~country_code)
coeftest(MR_FULL_GCTPCS_3, vcov = clustered_se_full_gctpcs_3)

#Model 4D: Interaction with Gct_4
MR_FULL_GCTPCS_4 <- lm(emp_rate_ct ~ Gct_1 + Gct_2 + Gct_3 + Gct_4 +
                         total_maternal_leave + total_maternal_leave_sq + total_paternal_leave +
                         Gct_4:total_maternal_leave + Gct_4:total_paternal_leave +
                         female_tertiary_share + avg_children + factor(wave),
                       data = country_level_data)
clustered_se_full_gctpcs_4 <- vcovCL(MR_FULL_GCTPCS_4, cluster = ~country_code)
coeftest(MR_FULL_GCTPCS_4, vcov = clustered_se_full_gctpcs_4)

#Model statistics table
model_stats <- rbind(
  C = extract_model_stats(MR_C),
  CN_GCTPCS = extract_model_stats(MR_CN_GCTPCS),
  CNP_GCTPCS = extract_model_stats(MR_CNP_GCTPCS),
  FULL_GCTPCS = extract_model_stats(MR_FULL_GCTPCS),
  FULL_GCTPCS_2 = extract_model_stats(MR_FULL_GCTPCS_2),
  FULL_GCTPCS_3 = extract_model_stats(MR_FULL_GCTPCS_3),
  FULL_GCTPCS_4 = extract_model_stats(MR_FULL_GCTPCS_4)
)
print(model_stats)



