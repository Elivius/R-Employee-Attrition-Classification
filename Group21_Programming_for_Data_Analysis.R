# =============================================================================
# EMPLOYEE ATTRITION CLASSIFICATION
# Group Number : GROUP 21
# Members      : [Joshua Yeo Jing Hao, TP077315], [Chin Kai Jack, TP076605],
#                [Ee Jin Xing, TP076848], [Lee Hong Yi, TP076604]
# Date         : -
# =============================================================================
#
# SCRIPT OVERVIEW
# ---------------
# Section 1 : Libraries                     - load tools first
# Section 2 : Read Raw File                 - load CSV as-is
# Section 3 : Raw Data Exploration          - understand data BEFORE anything else
# Section 4 : Configuration                 - set values after seeing the data
# Section 5 : Cleaning                      - fix what Section 3 revealed
# Section 6 : Validation                    - confirm cleaning worked correctly
# Section 7 : Individual Objective Analysis - Analyse based on cleaned dataset 
# 
# =============================================================================


# =============================================================================
# SECTION 1: LIBRARIES (Use pacman - package manager)
# Handle the "check -> install -> load" workflow in one go
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, scales, gridExtra, janitor, arrow, caret, corrplot)

message("[OK] All libraries loaded — ready to proceed.")

# =============================================================================
# SECTION 2: DATA RETRIEVAL PIPELINE
# We implement a robust CSV retrieval pipeline utilizing 'na.strings' mapping
# to handle multiple null-value variations identified during exploration.
# This ensures a clean data entry point for the pre-processing engine.
# =============================================================================

# Before running — set your working directory to where your CSV is saved:
#   setwd("C:/Users/YourName/Documents/YourProjectFolder")
# OR in RStudio: Session -> Set Working Directory -> To Source File Location

raw_file <- "dataset_employee_attrition.csv"

# Check the file exists before trying to load it
if (!file.exists(raw_file)) {
  stop(
    "\n[ERROR] File not found: '", raw_file, "'\n\n",
    "To fix this:\n",
    "  1. Run getwd() to see which folder R is currently in\n",
    "  2. Run setwd('your/folder/path') to point to the right folder\n",
    "  3. Make sure your CSV file is saved in that same folder\n"
  )
} else if (file.size(raw_file) == 0) {
  stop("\n[ERROR] '", raw_file, "' is empty — nothing to load.\n")
} else {
  message("[OK] Raw File found and contains data.")
}

# Load the raw file
# stringsAsFactors = FALSE -> keeps text as plain text, not auto-converted
# na.strings -> tells R which values to treat as missing (NA)
df_raw <- read.csv(raw_file, stringsAsFactors = FALSE,
                   na.strings = c("", "NA", "N/A", "na", "n/a"))

message("[OK] File loaded — ", nrow(df_raw), " rows x ",
        ncol(df_raw), " columns")

# =============================================================================
# SECTION 3: RAW DATA EXPLORATION
# Answer 5 questions about the data before touching anything:
#   1. What is the size?
#   2. What is inside?
#   3. What does it look like?
#   4. How clean is it?
#   5. What are the actual values?
#   6. Is there Zero Variance feature?
# Every finding here justifies a cleaning decision in Section 5
# =============================================================================

# --- Question 1: What is the size? ---
message("\n--- Dataset Dimensions ---\n")
cat("Rows    :", nrow(df_raw), "\n")
cat("Columns :", ncol(df_raw), "\n")


# --- Question 2: What is inside? ---
# str() shows column names AND data types at the same time
# IMPORTANT -> Watch for: numeric columns showing as "chr" — means dirty values like "1423_"
message("\n--- Structure (column names + data types) ---\n")
str(df_raw)


# --- Question 3: What does it look like? ---
# See the actual raw data — most important visual check
# IMPORTANT -> This is where you spot: "sale", "1423_", "f", "YES" etc.
message("\n--- Sample Rows (first 10) ---\n")
print(head(df_raw, 10))


# --- Question 4: How clean is it? ---

# 4a. Missing Values
# Counts NAs per column — sorted from most missing to least
# Only shows columns that have missing values — skips clean ones
message("\n--- Missing Values Per Column ---\n")
missing_raw <- colSums(is.na(df_raw))
missing_raw <- sort(missing_raw[missing_raw > 0], decreasing = TRUE)
if (length(missing_raw) == 0) {
  message("No missing values found.\n")
} else {
  print(missing_raw)
  message("Total missing cells: ", sum(missing_raw), "\n")
}

# 4b. Duplicate Rows
# Finds rows that are exact copies of another row
message("\n--- Duplicate Rows ---\n")
dup_count <- sum(duplicated(df_raw))
if (dup_count > 0) {
  message("Duplicates found    : ", dup_count, "\n")
  message("Rows after removal  : ", nrow(df_raw) - dup_count, "\n")
  message(">>> ACTION: Remove duplicates in Section 5 using distinct()\n")
} else {
  message("Duplicates found    : 0 — no action needed.\n")
}

# 4c. Duplicate Employee IDs
# Check if the same employee appears more than once (not exact row duplicates)
message("\n--- Duplicate Employee IDs ---\n")

if ("EmployeeNumber" %in% names(df_raw)) {
  # Extract only non-NA IDs for the duplicate check
  valid_ids <- df_raw$EmployeeNumber[!is.na(df_raw$EmployeeNumber)]
  dup_ids <- sum(duplicated(valid_ids))
  
  if (dup_ids > 0) {
    message("WARNING: Found ", dup_ids, " duplicated Employee IDs!\n")
    message(">>> ACTION: Investigate if these are exact row duplicates or conflicting records.\n")
    
    # Extract the duplicated IDs (excluding NAs)
    repeated_ids <- valid_ids[duplicated(valid_ids)]
    
    # Get all rows that have these repeated IDs
    conflict_rows <- df_raw[df_raw$EmployeeNumber %in% repeated_ids, ]
    conflict_rows <- conflict_rows[order(conflict_rows$EmployeeNumber), ] # Sort so pairs are next to each other
    
    message("\n--- Rows with Repeated IDs ---\n")
    # Print the ID and a few key columns so it fits in the console
    cols_to_show <- intersect(c("EmployeeNumber", "Attrition", "Age", "Department", "JobRole"), names(df_raw))
    print(conflict_rows[, cols_to_show])
    message("\n")
    
  } else {
    message("Duplicate IDs found : 0 — all employees are unique.\n")
  }
} else {
  message("EmployeeNumber column not found.\n")
}

# --- Question 5: What are the actual values? ---

# 5a. Target Variable Distribution
# Count how many stayed vs left BEFORE cleaning
# useNA = "always" forces NA to show even if there are none
message("\n--- Attrition Distribution (raw) ---\n")
print(table(df_raw$Attrition, useNA = "always"))

# 5b. Unique Values in ALL Categorical Columns
# Auto-detects every text column — no need to hardcode names (As stated in Question 2)
# Reveals ALL inconsistent formats that need fixing in Section 5
# sort() puts similar values next to each other — makes duplicates obvious
message("\n--- Unique Values in Categorical Columns ---\n")

char_cols <- df_raw %>%
  select(where(is.character)) %>%   # find all text columns automatically
  names()                            # extract column names as a list

for (col in char_cols) {
  cat("\n", col, ":\n")
  print(sort(unique(df_raw[[col]])))
}

# 5c. Numeric Column Summary (Min / Average / Max)
# Auto-detects every numeric column — no hardcoding needed
# Note: dirty columns like "1423_" will NOT appear here (stored as text)
# Missing numeric columns = they have dirty values = need clean_numeric()
message("\n--- Numeric Column Summary ---\n")

num_cols <- df_raw %>%
  select(where(is.numeric)) %>%   # find all numeric columns automatically
  names()

# Print header row
cat(sprintf("  %-30s %-14s %-14s %s\n", "Column", "Min", "Average", "Max"))
cat(strrep("-", 72), "\n")

for (col in num_cols) {
  r   <- range(df_raw[[col]], na.rm = TRUE)
  avg <- mean(df_raw[[col]],  na.rm = TRUE)
  cat(sprintf("  %-30s min = %-8s avg = %-8.2f max = %s\n",
              col, r[1], avg, r[2]))
}

# 6. Zero Variance (Feature that have same value every single row) - Use caret package
# We saw that at 5b and 5c some feature that only have same value like Over18 only "Y"
# This is Zero Variance feature, it provides zero information to help a model to distinguish between different employees
near_zero_variance_feature <- nearZeroVar(df_raw, saveMetrics = TRUE) # saveMetrics to display details in 2D table
print(near_zero_variance_feature[near_zero_variance_feature$zeroVar == TRUE, ])


message("\n[OK] Exploration complete — review output above then proceed to Section 4.\n")


# =============================================================================
# SECTION 4: CONFIGURATION
# Now that you have seen the raw data in Section 3, set your values here
# Change anything here — it flows automatically through the rest of the script
# =============================================================================

# --- File Settings ---
DATA_FILE        <- raw_file
OUTPUT_CSV       <- "employee_attrition_cleaned.csv"
OUTPUT_PARQUET   <- "employee_attrition_cleaned.parquet"
OUTPUT_STATS_CSV <- "statistical_test_results.csv"

# --- Model Settings ---
RANDOM_SEED <- 42     # keeps results the same every run
TRAIN_SPLIT <- 0.80   # 80% trains the model, 20% tests it

# --- Plot Colour Palette ---
COLOR_NO     <- "#2196F3"   # blue   = stayed
COLOR_YES    <- "#F44336"   # red    = left
COLOR_BAR    <- "#9C27B0"   # purple = bar charts
COLOR_ORANGE <- "#FF9800"   # orange = training chart
COLOR_GREEN  <- "#4CAF50"   # green  = no overtime

# --- Factor Label Sets ---
# Defined once here — reused in Section 5
# You know these from your dataset description file
LBL_4POINT <- c("Low", "Medium", "High", "Very High")                       # satisfaction scales
LBL_WLB    <- c("Bad", "Good", "Better", "Best")                            # work life balance
LBL_PERF   <- c("Low", "Good", "Excellent", "Outstanding")                  # performance rating
LBL_EDU    <- c("Below College", "College", "Bachelor", "Master", "Doctor") # education level

message("[OK] Configuration set — proceeding to cleaning.")


# =============================================================================
# SECTION 5: DATA CLEANING & PRE-PROCESSING
# Fix everything found in Section 3
# Each step directly addresses a problem spotted during exploration
# =============================================================================

# -----------------------------------------------------------------------------
# 5.1 Standardise Column Names
# Problem (Q2): In case column names have mixed case and spaces
# Fix: clean_names() converts everything to lowercase snake_case
# MonthlyIncome -> monthly_income | DistanceFromHome -> distance_from_home
# -----------------------------------------------------------------------------
df <- df_raw %>% clean_names()

message("\n[OK] 5.1 Column names standardised.\n")
print(names(df))

# -----------------------------------------------------------------------------
# 5.2 Remove Zero Variance Features
# Problem: Zero-variance features skew/break models
# -----------------------------------------------------------------------------
nzv_metrics <- nearZeroVar(df, saveMetrics = TRUE)
zero_var_cols <- which(nzv_metrics$zeroVar == TRUE)

if (length(zero_var_cols) > 0) {
  df <- df[, -zero_var_cols]
  message("\n[OK] 5.2 Removed ", length(zero_var_cols), " zero-variance columns.\n")
} else {
  message("\n[OK] 5.2 All columns have variance. No removal needed.\n")
}


# -----------------------------------------------------------------------------
# 5.3 Employee ID Features
# Prevent Overfitting
# -----------------------------------------------------------------------------
if ("employee_number" %in% names(df)) {
  df <- df %>% select(-employee_number)
  message("[OK] 5.3 Removed ID column: employee_number\n")
} else {
  message("[OK] 5.3 No ID column found to remove.\n")
}

# -----------------------------------------------------------------------------
# 5.4 Universal Categorical Normalization
# Problem (Q5b): same values written in many inconsistent formats
# IMPROVEMENT: pre-clean each column ONCE with tolower(trimws()) first
# then case_when conditions are simple — no repeated wrapping needed
# -----------------------------------------------------------------------------
df <- df %>%
  
  # Step 1 — normalise all categorical columns to lowercase, no spaces
  # Done once here so case_when below is clean and easy to read
  mutate(across(where(is.character), ~tolower(trimws(.)))) %>%
  
  # Step 2 — standardise to final clean values
  # Conditions are now simple %in% checks — no tolower/trimws needed
  mutate(
    # Attrition — target variable
    # Found in Section 3: "yes" "YES" "Yes" "1" "no" "NO" "No" "0"
    attrition = case_when(
      attrition %in% c("yes", "1") ~ "Yes",
      attrition %in% c("no",  "0") ~ "No",
      TRUE ~ NA_character_
    ),
    
    # Business Travel
    # Found in Section 3: "rare" "TRAVEL_RARELY" "frequent" "nil" "non-travel"
    business_travel = case_when(
      business_travel %in%
        c("travel-rarely", "travel_rarely", "rare", "rarely", "travel rarely")                      ~ "Travel Rarely",
      business_travel %in%
        c("travel-frequently", "travel_frequently", "frequent", "frequently", "travel frequently")  ~ "Travel Frequently",
      business_travel %in%
        c("non-travel", "non_travel", "non", "nontravel", "nil", "no travel", "non travel", "none") ~ "No Travel",
      TRUE ~ NA_character_
    ),
    
    # Department
    # Found in Section 3: "sale" "r&d" "Research & Development" "hr"
    department = case_when(
      department %in% c("sales", "sale")                                                            ~ "Sales",
      department %in% c("r&d", "research & development", "research and development", "rd", "r & d") ~ "Research & Development",
      department %in% c("hr", "h&r", "human resources", "human resource")                           ~ "Human Resources",
      TRUE ~ NA_character_
    ),
    
    # Education Field
    # Found in Section 3: Synonyms like 'ls' for 'Life Sciences' and 'med' for 'Medical'
    education_field = case_when(
      education_field %in% c("life sciences", "ls")                            ~ "Life Sciences",
      education_field %in% c("medical", "med")                                 ~ "Medical Sciences",
      education_field %in% c("marketing", "mkt")                               ~ "Marketing",
      education_field %in% c("technical degree", "td")                         ~ "Technical",
      education_field %in% c("hr", "h&r", "human resources", "human resource") ~ "Human Resources",
      education_field %in% c("other", "others")                                ~ "Others",
      TRUE                                                                     ~ NA_character_
    ),
    
    # Gender
    # Found in Section 3: "f" "F" "female" "FEMALE" "m" "M" "male" "MALE"
    gender = case_when(
      gender %in% c("f", "female") ~ "Female",
      gender %in% c("m", "male")   ~ "Male",
      TRUE ~ NA_character_
    ),
    
    # Job Role
    # Found in Section 3: "rep" vs "representative", "exe" vs "executive"
    job_role = case_when(
      str_detect(job_role, "healthcare") ~ "Healthcare Representative",
      str_detect(job_role, "lab")        ~ "Laboratory Technician",
      str_detect(job_role, "manufac")    ~ "Manufacturing Director",
      str_detect(job_role, "research s") ~ "Research Scientist",
      str_detect(job_role, "research d") ~ "Research Director",
      str_detect(job_role, "sales exe")  ~ "Sales Executive",
      str_detect(job_role, "sales rep")  ~ "Sales Representative",
      str_detect(job_role, "manager")    ~ "Manager",
      str_detect(job_role, "human")      ~ "Human Resources",
      TRUE                               ~ "Other"
    ),
    
    # Marital Status
    # Found in Section 3: Casing (already fixed by tolower), but make it report-ready
    marital_status = case_when(
      marital_status == "single"   ~ "Single",
      marital_status == "married"  ~ "Married",
      marital_status == "divorced" ~ "Divorced",
      TRUE                         ~ NA_character_
    ),
    
    # OverTime
    # Found in Section 3: "yes" "YES" "1" "no" "NO" "0"
    over_time = case_when(
      over_time %in% c("yes", "1") ~ "Yes",
      over_time %in% c("no",  "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

cat("[OK] 5.4 Categorical columns standardised.\n")


# -----------------------------------------------------------------------------
# 5.5 Clean Dirty Numeric Columns
# Problem (Q2 + Q5c): numeric columns stored as text with junk characters
# Examples found: "1423_"  "329?"  "4_"  "670?"  "1?"  "2_"
# Fix: strip anything that is not a digit or decimal point
# IMPROVEMENT: simpler auto-detection using sapply instead of chained selects
# -----------------------------------------------------------------------------
clean_numeric <- function(x) {
  x <- gsub("[^0-9.]", "", as.character(x))  # strip non-numeric chars
  x[x == ""] <- NA                            # empty string = missing
  as.numeric(x)                               # convert to number
}

# Known categorical columns — excluded from numeric cleaning
known_categorical <- c(
  "attrition", "business_travel", "department", "education_field", "gender",
  "job_role", "marital_status", "over_time"
)

# Auto-detect columns that:
# (a) are not in known_categorical
# (b) are not already numeric
# (c) contain at least one dirty character like "_" or "?"
dirty_cols <- names(df)[
  !names(df) %in% known_categorical &
    sapply(df, function(x)
      !is.numeric(x) &&
        any(grepl("[^0-9.\\-]", na.omit(as.character(x))))
    )
]

df <- df %>% mutate(across(all_of(dirty_cols), clean_numeric))

message("\n[OK] 5.5 Cleaned ", length(dirty_cols), " dirty numeric columns.\n")
if (length(dirty_cols) > 0) {
  cat("Columns:", paste(dirty_cols, collapse = ", "), "\n")
}


# -----------------------------------------------------------------------------
# 5.6 Remove Duplicate Rows
# Problem (Q4b): duplicate rows inflate analysis results
# Fix: keep only the first occurrence of each duplicated row
# distinct() compares every column — only removes EXACT full-row duplicates
# -----------------------------------------------------------------------------
rows_before_dedup <- nrow(df)
df <- df %>% distinct()

dedup_removed <- rows_before_dedup - nrow(df)
message("\n[OK] 5.6 Removed ", dedup_removed, " duplicate rows.\n")


# -----------------------------------------------------------------------------
# 5.7 Remove Rows With Missing Target Variable (Attrition)
# Problem: 29 rows have no Attrition value — found in Section 3
# Why remove: Attrition is what we are PREDICTING
# Cannot guess whether someone left or stayed — no valid imputation exists
# Done BEFORE imputation so these rows don't affect median/mode calculations
# -----------------------------------------------------------------------------
rows_before_target <- nrow(df)
df <- df %>% filter(!is.na(attrition))
message("\n[OK] 5.7 Removed ", rows_before_target - nrow(df),
    " rows with missing Attrition (target variable).\n")
message("Rows remaining: ", nrow(df), "\n")


# -----------------------------------------------------------------------------
# 5.8 Impute Remaining Missing Values
# IMPROVEMENT: combined into ONE mutate() instead of two separate calls
# Numeric   -> median (robust to outliers, not skewed by extremes)
# Character -> mode   (most common value — only logical choice for text)
# -----------------------------------------------------------------------------
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])            # unique non-NA values
  ux[which.max(tabulate(match(x, ux)))] # return the most frequent one
}

na_before <- sum(is.na(df))

# Single mutate handles both numeric and categorical imputation together
df <- df %>%
  mutate(
    across(where(is.numeric),   ~ ifelse(is.na(.), median(., na.rm = TRUE), .)),
    across(where(is.character), ~ ifelse(is.na(.), get_mode(.), .))
  )

na_after <- sum(is.na(df))
message("\n[OK] 5.8 Imputation complete — filled ",
    na_before - na_after, " missing values.\n")


# -----------------------------------------------------------------------------
# 5.9 Convert to Labelled Factors
# Labels come from CONFIG (Section 4) — change them there, not here
# Must happen AFTER imputation — factors don't work well with imputation
# Without this R treats Education=4 as mathematically twice Education=2
# -----------------------------------------------------------------------------
df <- df %>%
  mutate(
    # Ordinal Variables (Ranked)
    education                 = factor(education, levels = 1:5, labels = LBL_EDU),
    environment_satisfaction  = factor(environment_satisfaction, levels = 1:4, labels = LBL_4POINT),
    job_satisfaction          = factor(job_satisfaction, levels = 1:4, labels = LBL_4POINT),
    job_involvement           = factor(job_involvement, levels = 1:4, labels = LBL_4POINT),
    relationship_satisfaction = factor(relationship_satisfaction, levels = 1:4, labels = LBL_4POINT),
    work_life_balance         = factor(work_life_balance, levels = 1:4, labels = LBL_WLB),
    performance_rating        = factor(performance_rating, levels = 1:4, labels = LBL_PERF),
    
    # Seniority & Financial Buckets
    job_level                 = factor(job_level),
    stock_option_level        = factor(stock_option_level),
    
    # Target Variable
    attrition                 = factor(attrition, levels = c("No", "Yes")),
    
    # Nominal Variables (Unordered Labels)
    gender                    = factor(gender),
    department                = factor(department),
    business_travel           = factor(business_travel),
    over_time                 = factor(over_time),
    marital_status            = factor(marital_status),
    
    # Categorical Labels
    education_field           = factor(education_field),
    job_role                  = factor(job_role)
  )

cat("[OK] 5.9 Columns converted to labelled factors.\n")

# -----------------------------------------------------------------------------
# 5.10 Impossible Logic Correction — Correct by Taking Logical Maximum/Minimum
# -----------------------------------------------------------------------------

message("\n--- 5.10 Impossible Logic Correction ---\n")
# Count before correction
flag1_count <- sum(df$total_working_years < df$years_at_company,
                   na.rm = TRUE)
flag2_count <- sum(df$age < (df$total_working_years + 14),
                   na.rm = TRUE)
flag3_count <- sum(df$years_in_current_role > df$years_at_company,
                   na.rm = TRUE)

total_initial_flags <- flag1_count + flag2_count + flag3_count

cat("Before correction:\n")
cat("  Flag 1 (TotalWorkingYears < YearsAtCompany)  :", flag1_count, "rows\n")
cat("  Flag 2 (Age < TotalWorkingYears + 14)        :", flag2_count, "rows\n")
cat("  Flag 3 (YearsInCurrentRole > YearsAtCompany) :", flag3_count, "rows\n")
cat("Total Impossible Logic:", total_initial_flags,"\n")

# Correct all three in one mutate
# NOTE: Fix 1 (push UP) and Fix 2 (push DOWN) can conflict when
# years_at_company > age - 14. Those rows become irreconcilable
# and are removed below after the heuristic pass.
df <- df %>%
  mutate(
    
    # Fix 1: total_working_years must be >= years_at_company
    # Take the higher value — you must have worked at least
    # as long as you have been at this company
    total_working_years = pmax(total_working_years, years_at_company),
    
    # Fix 2: total_working_years must be <= age - 14
    # Take the lower value — cannot have worked before age 14
    total_working_years = pmin(total_working_years, age - 14),
    
    # Fix 3: years_in_current_role must be <= years_at_company
    # Take the lower value — cannot be in role longer than at company
    years_in_current_role = pmin(years_in_current_role, years_at_company)
    
  )

# Verify all fixed
flag1_after <- sum(df$total_working_years < df$years_at_company,
                   na.rm = TRUE)
flag2_after <- sum(df$age < (df$total_working_years + 14),
                   na.rm = TRUE)
flag3_after <- sum(df$years_in_current_role > df$years_at_company,
                   na.rm = TRUE)

cat("\nAfter correction:\n")
cat("  Flag 1 remaining:", flag1_after, "\n")
cat("  Flag 2 remaining:", flag2_after, "\n")
cat("  Flag 3 remaining:", flag3_after, "\n")

# Remove the final 42 irreconcilable rows
rows_before_impossible_correction <- nrow(df)

df <- df %>%
  filter(total_working_years >= years_at_company)

message("\n[ACTION] Removed final ", rows_before_impossible_correction - nrow(df), 
    " irreconcilable rows that failed heuristic repair.\n")

# Verify all fixed - after removal
flag1_after <- sum(df$total_working_years < df$years_at_company,
                   na.rm = TRUE)
flag2_after <- sum(df$age < (df$total_working_years + 14),
                   na.rm = TRUE)
flag3_after <- sum(df$years_in_current_role > df$years_at_company,
                   na.rm = TRUE)

final_removed <- rows_before_impossible_correction - nrow(df)
total_corrected <- total_initial_flags - final_removed

cat("\nAfter removal:\n")
cat("  Flag 1 remaining:", flag1_after, "\n")
cat("  Flag 2 remaining:", flag2_after, "\n")
cat("  Flag 3 remaining:", flag3_after, "\n")

message("\n[OK] 5.10 All impossible logic corrected.
        \nTotal correction: ", total_corrected,
        "\nTotal removal: ", final_removed,
        "\nLeftover dataset: ", nrow(df), " x ", ncol(df), "\n")

# Rename as clean dataset
df_clean <- df
message("\n[OK] df_clean is ready — ", nrow(df_clean), " rows x ",
    ncol(df_clean), " columns.\n")


# =============================================================================
# SECTION 6: VALIDATION
# Confirm everything in Section 5 worked correctly
# Compare before/after for each cleaning step
# =============================================================================

# --- 6.1 Missing Values After Cleaning ---
# Should be 0 for all columns after imputation in 5.6
message("\n--- 6.1 Missing Values After Cleaning ---\n")
missing_clean <- colSums(is.na(df_clean))
missing_clean <- sort(missing_clean[missing_clean > 0], decreasing = TRUE)
if (length(missing_clean) == 0) {
  message("\n[OK] 6.1 No missing values remaining — imputation successful.\n")
} else {
  message("WARNING: Some NAs still remain:\n")
  print(missing_clean)
}


# --- 6.2 Confirm Categorical Cleaning Worked ---
# Compare with Section 3 — should now show only clean consistent values (Match with dataset_description.txt)
# Target and Key Demographics
message("\n--- 6.2 Cleaned Unique Value ---\n")

# Target & Basic Info
cat("Attrition         :"); print(levels(df_clean$attrition))
cat("Gender            :"); print(levels(df_clean$gender))
cat("Marital Status    :"); print(levels(df_clean$marital_status))
cat("OverTime          :"); print(levels(df_clean$over_time))

# Professional & Education
cat("Department        :"); print(levels(df_clean$department))
cat("Business Travel   :"); print(levels(df_clean$business_travel))
cat("Education Field   :"); print(levels(df_clean$education_field))
cat("Job Role          :"); print(levels(df_clean$job_role))
cat("Job Level         :"); print(levels(df_clean$job_level))
cat("Stock Option Level:"); print(levels(df_clean$stock_option_level))

# Ordinal Scales (Satisfaction & Performance)
cat("Education         :"); print(levels(df_clean$education))
cat("Job Satisfaction  :"); print(levels(df_clean$job_satisfaction))
cat("Env. Satisfaction :"); print(levels(df_clean$environment_satisfaction))
cat("Job Involvement   :"); print(levels(df_clean$job_involvement))
cat("Rel. Satisfaction :"); print(levels(df_clean$relationship_satisfaction))
cat("Work Life Balance :"); print(levels(df_clean$work_life_balance))
cat("Performance Rating:"); print(levels(df_clean$performance_rating))


# --- 6.3 Confirm Imputation Values Used ---
# Shows what median value was used to fill NAs per numeric column
# Useful for your report — state exactly what was imputed
message("\n--- 6.3 Median Values Used for Numeric Imputation ---\n")
df_clean %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ median(., na.rm = TRUE))) %>%
  pivot_longer(everything(),
               names_to  = "column",
               values_to = "median_used") %>%
  print(n = Inf)


# --- 6.4 Final Data Health Summary ---
cat("\n--- 6.4 Final Data Health Summary ---\n")
cat(sprintf("  %-40s %d\n",    "Raw rows loaded:",                         nrow(df_raw)))
cat(sprintf("  %-40s %d\n",    "Duplicates removed:",                      dedup_removed))
cat(sprintf("  %-40s %d\n",    "Missing target (Attrition) removed:",      rows_before_target - rows_before_impossible_correction))
cat(sprintf("  %-40s %d\n",    "NAs imputed:",                             na_before - na_after))
cat(sprintf("  %-40s %d\n",    "Zero variance features/columns removed:",  length(zero_var_cols)))
cat(sprintf("  %-40s %d\n",    "Impossible logic corrected:",              total_corrected))
cat(sprintf("  %-40s %d\n",    "Impossible logic removed:",                final_removed))
cat(sprintf("  %-40s %d\n",    "Final clean rows:",                        nrow(df_clean)))
cat(sprintf("  %-40s %d\n",    "Final clean cols:",                        ncol(df_clean)))
cat(sprintf("  %-40s %d\n",    "Remaining NAs:",                           sum(is.na(df_clean))))
cat(sprintf("  %-40s %d\n",    "Stayed (No):",                             sum(df_clean$attrition == "No")))
cat(sprintf("  %-40s %d\n",    "Left (Yes):",                              sum(df_clean$attrition == "Yes")))
cat(sprintf("  %-40s %.2f%%\n","Attrition Rate:",                          sum(df_clean$attrition == "Yes") / nrow(df_clean) * 100))

# Export clean dataset
write.csv(df_clean, OUTPUT_CSV, row.names = FALSE)
message("\n[OK] Clean dataset saved to:", OUTPUT_CSV, "\n")

# We convert the CSV to Parquet format. Unlike CSVs, Parquet is a binary 
# columnar format that allows for high-speed I/O and better compression.
write_parquet(df_clean, OUTPUT_PARQUET)
message("\n[OK] Clean dataset saved to:", OUTPUT_PARQUET, "\n")

message("\n[OK] Clean data saved as CSV and Optimized Parquet.")
message("\n>>> BASE SCRIPT COMPLETE — clean_parquet is ready for analysis.\n")

# =============================================================================
# SECTION 7 ONWARDS: YOUR GROUP'S ANALYSIS GOES HERE
# Each group member writes their assigned objective below this line
# =============================================================================