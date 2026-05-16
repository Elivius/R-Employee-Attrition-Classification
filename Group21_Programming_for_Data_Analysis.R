# =============================================================================
# EMPLOYEE ATTRITION CLASSIFICATION
# Group Number : GROUP 21
# Members      : [Joshua Yeo Jing Hao, TP077315], 
#                [Chin Kai Jack, TP076605],
#                [Ee Jin Xing, TP076848], 
#                [Lee Hong Yi, TP076604]
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

message("[OK] File loaded — ", nrow(df_raw), " rows x ", ncol(df_raw), " columns")

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
message("\n--- Question 1: Dataset Dimensions ---")
cat("Rows    :", nrow(df_raw), "\n")
cat("Columns :", ncol(df_raw), "\n")


# --- Question 2: What is inside? ---
# str() shows column names AND data types at the same time
# IMPORTANT -> Watch for: numeric columns showing as "chr" — means dirty values like "1423_"
message("\n--- Question 2: Structure (column names + data types) ---")
str(df_raw)


# --- Question 3: What does it look like? ---
# See the actual raw data — most important visual check
# IMPORTANT -> This is where you spot: "sale", "1423_", "f", "YES" etc.
message("\n--- Question 3: Sample Rows (first 10) ---")
print(head(df_raw, 10))


# --- Question 4: How clean is it? ---

# 4a. Missing Values
# Counts NAs per column — sorted from most missing to least
# Only shows columns that have missing values — skips clean ones
message("\n--- Question 4a: Missing Values Per Column ---")
missing_raw <- colSums(is.na(df_raw))
missing_raw <- sort(missing_raw[missing_raw > 0], decreasing = TRUE)
if (length(missing_raw) == 0) {
  cat("No missing values found.\n")
} else {
  print(missing_raw)
  cat("Total missing cells:", sum(missing_raw), "\n")
}

# 4b. Duplicate Rows
# Finds rows that are exact copies of another row
message("\n--- Question 4b: Duplicate Rows ---")
dup_count <- sum(duplicated(df_raw))
if (dup_count > 0) {
  cat("Duplicates found    :", dup_count, "\n")
  cat("Rows after removal  :", nrow(df_raw) - dup_count, "\n")
  cat(">>> ACTION: Remove duplicates in Section 5 using distinct()\n")
} else {
  cat("Duplicates found    : 0 — no action needed.\n")
}

# 4c. Duplicate Employee IDs
# Check if the same employee appears more than once (not exact row duplicates)
message("\n--- Question 4c: Duplicate Employee IDs ---")

if ("EmployeeNumber" %in% names(df_raw)) {
  # Extract only non-NA IDs for the duplicate check
  valid_ids <- df_raw$EmployeeNumber[!is.na(df_raw$EmployeeNumber)]
  dup_ids <- sum(duplicated(valid_ids))
  
  if (dup_ids > 0) {
    message("WARNING: Found ", dup_ids, " duplicated Employee IDs!")
    cat(">>> ACTION: Investigate if these are exact row duplicates or conflicting records.\n")
    
    # Extract the duplicated IDs (excluding NAs)
    repeated_ids <- valid_ids[duplicated(valid_ids)]
    
    # Get all rows that have these repeated IDs
    conflict_rows <- df_raw[df_raw$EmployeeNumber %in% repeated_ids, ]
    conflict_rows <- conflict_rows[order(conflict_rows$EmployeeNumber), ] # Sort so pairs are next to each other
    
    message("\n--- Rows with Repeated IDs ---")
    # Print the ID and a few key columns so it fits in the console
    cols_to_show <- intersect(c("EmployeeNumber", "Attrition", "Age", "Department", "JobRole"), names(df_raw))
    print(conflict_rows[, cols_to_show])
    cat("\n")
    
  } else {
    cat("Duplicate IDs found : 0 — all employees are unique.\n")
  }
} else {
  cat("EmployeeNumber column not found.\n")
}

# --- Question 5: What are the actual values? ---

# 5a. Target Variable Distribution
# Count how many stayed vs left BEFORE cleaning
# useNA = "always" forces NA to show even if there are none
message("\n--- Question 5a: Attrition Distribution (raw) ---")
print(table(df_raw$Attrition, useNA = "always"))

# 5b. Unique Values in ALL Categorical Columns
# Auto-detects every text column — no need to hardcode names (As stated in Question 2)
# Reveals ALL inconsistent formats that need fixing in Section 5
# sort() puts similar values next to each other — makes duplicates obvious
message("\n--- Question 5b: Unique Values in Categorical Columns ---")

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
message("\n--- Question 5c: Numeric Column Summary ---")

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
message("\n--- Question 6: Zero Variance ---")
near_zero_variance_feature <- nearZeroVar(df_raw, saveMetrics = TRUE) # saveMetrics to display details in 2D table
print(near_zero_variance_feature[near_zero_variance_feature$zeroVar == TRUE, ])


message("\n[OK] Exploration complete — review output above then proceed to Section 4.")


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
message("\n--- 5.1 Standardise Column Names ---")
df <- df_raw %>% clean_names()

print(names(df))
message("[OK] 5.1 Column names standardised.")

# -----------------------------------------------------------------------------
# 5.2 Remove Zero Variance Features
# Problem: Zero-variance features skew/break models
# -----------------------------------------------------------------------------
message("\n--- 5.2 Remove Zero Variance Features ---")
nzv_metrics <- nearZeroVar(df, saveMetrics = TRUE)
zero_var_cols <- which(nzv_metrics$zeroVar == TRUE)

if (length(zero_var_cols) > 0) {
  df <- df[, -zero_var_cols]
  message("[OK] 5.2 Removed ", length(zero_var_cols), " zero-variance columns.")
} else {
  message("[OK] 5.2 All columns have variance. No removal needed.")
}


# -----------------------------------------------------------------------------
# 5.3 Employee ID Features
# Prevent Overfitting
# -----------------------------------------------------------------------------
message("\n--- 5.3 Remove Employee ID Feature ---")
if ("employee_number" %in% names(df)) {
  df <- df %>% select(-employee_number)
  message("[OK] 5.3 Removed ID column: employee_number")
} else {
  message("[OK] 5.3 No ID column found to remove.")
}

# -----------------------------------------------------------------------------
# 5.4 Universal Categorical Normalization
# Problem (Q5b): same values written in many inconsistent formats
# IMPROVEMENT: pre-clean each column ONCE with tolower(trimws()) first
# then case_when conditions are simple — no repeated wrapping needed
# -----------------------------------------------------------------------------
message("\n--- 5.4 Universal Categorical Normalization ---")
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
      TRUE                                                                                          ~ NA_character_
    ),
    
    # Department
    # Found in Section 3: "sale" "r&d" "Research & Development" "hr"
    department = case_when(
      department %in% c("sales", "sale")                                                            ~ "Sales",
      department %in% c("r&d", "research & development", "research and development", "rd", "r & d") ~ "Research & Development",
      department %in% c("hr", "h&r", "human resources", "human resource")                           ~ "Human Resources",
      TRUE                                                                                          ~ NA_character_
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
      TRUE                          ~ NA_character_
    ),
    
    # Job Role
    # Found in Section 3: abbreviations ("hr", "sales rep", "sales exe"),
    # truncations ("manufacture director") and full names mixed together
    job_role = case_when(
      job_role %in% c("healthcare representative", "healthcare rep")    ~ "Healthcare Representative",
      job_role %in% c("laboratory technician", "lab technician")        ~ "Laboratory Technician",
      job_role %in% c("manufacturing director", "manufacture director") ~ "Manufacturing Director",
      job_role %in% c("research scientist")                             ~ "Research Scientist",
      job_role %in% c("research director")                              ~ "Research Director",
      job_role %in% c("sales executive", "sales exe")                   ~ "Sales Executive",
      job_role %in% c("sales representative", "sales rep")              ~ "Sales Representative",
      job_role %in% c("manager")                                        ~ "Manager",
      job_role %in% c("human resources", "hr")                          ~ "Human Resources",
      TRUE                                                              ~ NA_character_
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
      TRUE                          ~ NA_character_
    )
  )
message("[OK] 5.4 Categorical columns standardised.")


# -----------------------------------------------------------------------------
# 5.5 Clean Dirty Numeric Columns
# Problem (Q2 + Q5c): numeric columns stored as text with junk characters
# Examples found: "1423_"  "329?"  "4_"  "670?"  "1?"  "2_"
# Fix: strip anything that is not a digit or decimal point
# IMPROVEMENT: simpler auto-detection using sapply instead of chained selects
# -----------------------------------------------------------------------------
message("\n--- 5.5 Clean Dirty Numeric Columns ---")
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

message("[OK] 5.5 Cleaned ", length(dirty_cols), " dirty numeric columns.")
if (length(dirty_cols) > 0) {
  cat("  Columns:", paste(dirty_cols, collapse = ", "), "\n")
}


# -----------------------------------------------------------------------------
# 5.6 Remove Duplicate Rows
# Problem (Q4b): duplicate rows inflate analysis results
# Fix: keep only the first occurrence of each duplicated row
# distinct() compares every column — only removes EXACT full-row duplicates
# -----------------------------------------------------------------------------
message("\n--- 5.6 Remove Duplicate Rows ---")
rows_before_dedup <- nrow(df)
df <- df %>% distinct()

dedup_removed <- rows_before_dedup - nrow(df)
message("[OK] 5.6 Removed ", dedup_removed, " duplicate rows.")


# -----------------------------------------------------------------------------
# 5.7 Remove Rows With Missing Target Variable (Attrition)
# Problem: 29 rows have no Attrition value — found in Section 3
# Why remove: Attrition is what we are PREDICTING
# Cannot guess whether someone left or stayed — no valid imputation exists
# Done BEFORE imputation so these rows don't affect median/mode calculations
# -----------------------------------------------------------------------------
message("\n--- 5.7 Remove Rows With Missing Target ---")
rows_before_target <- nrow(df)
df <- df %>% filter(!is.na(attrition))
message("[OK] 5.7 Removed ", rows_before_target - nrow(df),
    " rows with missing Attrition (target variable).")
cat("  Rows remaining:", nrow(df), "\n")


# -----------------------------------------------------------------------------
# 5.8 Impute Remaining Missing Values
# IMPROVEMENT: combined into ONE mutate() instead of two separate calls
# Numeric   -> median (robust to outliers, not skewed by extremes)
# Character -> mode   (most common value — only logical choice for text)
# -----------------------------------------------------------------------------
message("\n--- 5.8 Impute Remaining Missing Values ---")
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])            # unique non-NA values
  ux[which.max(tabulate(match(x, ux)))] # return the most frequent one
}

na_before <- sum(is.na(df))

# Ordinal columns (1-4 or 1-5 scales) — use ROUNDED median so values
# stay as valid integers for factor conversion in Step 5.9
ordinal_cols <- c("education", "environment_satisfaction", "job_satisfaction",
                 "job_involvement", "relationship_satisfaction",
                 "work_life_balance", "performance_rating")

# Single mutate handles ordinal, continuous numeric, and categorical together
df <- df %>%
  mutate(
    across(all_of(ordinal_cols),
           ~ ifelse(is.na(.), round(median(., na.rm = TRUE)), .)),
    across(where(is.numeric) & !all_of(ordinal_cols),
           ~ ifelse(is.na(.), median(., na.rm = TRUE), .)),
    across(where(is.character), ~ ifelse(is.na(.), get_mode(.), .))
  )

na_after <- sum(is.na(df))
message("[OK] 5.8 Imputation complete — filled ",
    na_before - na_after, " missing values.")


# -----------------------------------------------------------------------------
# 5.9 Convert to Labelled Factors
# Labels come from CONFIG (Section 4) — change them there, not here
# Must happen AFTER imputation — factors don't work well with imputation
# Without this R treats Education=4 as mathematically twice Education=2
# -----------------------------------------------------------------------------
message("\n--- 5.9 Convert to Labelled Factors ---")
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

message("[OK] 5.9 Columns converted to labelled factors.")

# -----------------------------------------------------------------------------
# 5.10 Impossible Logic Correction — Correct by Taking Logical Maximum/Minimum
# -----------------------------------------------------------------------------
message("\n--- 5.10 Impossible Logic Correction ---")
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
    " irreconcilable rows that failed heuristic repair.")

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

message("\n[OK] 5.10 All impossible logic corrected.")
cat("  Total correction:", total_corrected, "\n")
cat("  Total removal:", final_removed, "\n")
cat("  Leftover dataset:", nrow(df), "x", ncol(df), "\n")

# Rename as clean dataset
df_clean <- df
message("\n[OK] df_clean is ready — ", nrow(df_clean), " rows x ",
    ncol(df_clean), " columns.")


# =============================================================================
# SECTION 6: VALIDATION
# Confirm everything in Section 5 worked correctly
# Compare before/after for each cleaning step
# =============================================================================

# --- 6.1 Missing Values After Cleaning ---
# Should be 0 for all columns after imputation in 5.6
message("\n--- 6.1 Missing Values After Cleaning ---")
missing_clean <- colSums(is.na(df_clean))
missing_clean <- sort(missing_clean[missing_clean > 0], decreasing = TRUE)
if (length(missing_clean) == 0) {
  message("[OK] 6.1 No missing values remaining — imputation successful.")
} else {
  message("WARNING: Some NAs still remain:")
  print(missing_clean)
}


# --- 6.2 Confirm Categorical Cleaning Worked ---
# Compare with Section 3 — should now show only clean consistent values (Match with dataset_description.txt)
# Target and Key Demographics
message("\n--- 6.2 Cleaned Unique Values ---")

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
message("\n--- 6.3 Median Values Used for Numeric Imputation ---")
df_clean %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ median(., na.rm = TRUE))) %>%
  pivot_longer(everything(),
               names_to  = "column",
               values_to = "median_used") %>%
  print(n = Inf)


# --- 6.4 Final Data Health Summary ---
message("\n--- 6.4 Final Data Health Summary ---")
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
message("\n[OK] Clean dataset saved to: ", OUTPUT_CSV)

# We convert the CSV to Parquet format. Unlike CSVs, Parquet is a binary 
# columnar format that allows for high-speed I/O and better compression.
write_parquet(df_clean, OUTPUT_PARQUET)
message("[OK] Clean dataset saved to: ", OUTPUT_PARQUET)

message("[OK] Clean data saved as CSV and Optimized Parquet.")
message("\n>>> BASE SCRIPT COMPLETE — df_clean is ready for analysis.")

# =============================================================================
# SECTION 7 ONWARDS: YOUR GROUP'S ANALYSIS GOES HERE
# Each group member writes their assigned objective below this line
# =============================================================================

#Section 7.1 Objective 1: Compensation
#Name: Joshua Yeo Jing Hao TP077315

#Theme Settings
theme_comp <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey50"),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin      = margin(10, 10, 10, 10)
  )

# Colour mapping — blue = stayed, red = left
comp_colors <- c("No" = "#2196F3", "Yes" = "#F44336")

cat("\n=== OBJECTIVE 1: COMPENSATION ANALYSIS ===\n")
cat("Variables: MonthlyIncome, PercentSalaryHike, StockOptionLevel\n")
cat("Hypothesis: Lower compensation is significantly associated with higher attrition\n\n")

#Section 7.2 Descriptive Analysis
# ====================================

cat("--- Compensation Summary by Attrition Group ---\n")

compensation_summary <- df_clean %>%
  group_by(attrition) %>%
  summarise(
    n                    = n(),
    avg_monthly_income   = round(mean(monthly_income,      na.rm = TRUE), 2),
    med_monthly_income   = round(median(monthly_income,    na.rm = TRUE), 2),
    avg_salary_hike      = round(mean(percent_salary_hike, na.rm = TRUE), 2),
    med_salary_hike      = round(median(percent_salary_hike, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(compensation_summary)

# Stock option distribution by attrition
cat("\n--- Stock Option Level Distribution by Attrition ---\n")
stock_table <- table(df_clean$stock_option_level, df_clean$attrition)
print(stock_table)
cat("\nRow percentages:\n")
print(round(prop.table(stock_table, margin = 1) * 100, 1))


#Section 7.2: Visualization

# --- Preparation: calculate summary stats for annotations ---
# Calculate summary stats for labels
income_summary <- df_clean %>%
  group_by(attrition) %>%
  summarise(
    n      = n(),
    mean   = round(mean(monthly_income, na.rm = TRUE), 0),
    median = round(median(monthly_income, na.rm = TRUE), 0),
    .groups = "drop"
  )

hike_summary <- df_clean %>%
  group_by(attrition) %>%
  summarise(
    mean_hike = round(mean(percent_salary_hike, na.rm = TRUE), 2),
    .groups   = "drop"
  )

#Plot 1: To investigate relationship of monthly income and attrition
plot1a <- ggplot(df_clean,
              aes(x = attrition, y = monthly_income, fill = attrition)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2,
               outlier.size = 1, width = 0.5) +
  geom_jitter(aes(color = attrition),
              width = 0.15, alpha = 0.15, size = 0.8) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  scale_fill_manual(values  = comp_colors) +
  scale_color_manual(values = comp_colors) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0.1, 0.05))) +
  labs(
    title   = "Monthly Income Distribution by Attrition",
    x       = "Attrition Status",
    y       = "Monthly Income (RM)",
    fill    = "Attrition",
    caption = "White diamond = Mean | Line = Median | Points = Individual employees"
  ) +
  theme_comp +
  theme(legend.position = "none")

print(plot1a)

#Conclusion:
#"The visual analysis of Monthly Income Distribution reveals a significant disparity between the two groups. 
#Employees who remained with the organization (Median $\approx$ RM5,000) earn more than those who left (Median $\approx$ RM4,000). 
#Furthermore, the density of individual data points (jitter) shows that attrition is most prevalent among those earning below RM7,500, with very few instances of attrition occurring in the high-income brackets (above RM15,000). 
#This suggests that financial compensation acts as a strong retention factor."

#Plot 2: To investigate relationship between salary hike% and attrition
# Reveals if employees who left received smaller raises
plot1b <- ggplot(df_clean %>% filter(!is.na(percent_salary_hike)),
              aes(x     = percent_salary_hike,
                  fill  = attrition,
                  color = attrition)) +
  geom_density(alpha = 0.4, size = 1) +
  geom_vline(data = hike_summary,
             aes(xintercept = mean_hike, color = attrition),
             linetype = "dashed", size = 1) +
  geom_text(data = hike_summary,
            aes(x     = mean_hike,
                y     = 0.15,
                label = paste0(attrition, "\nMean: ", mean_hike, "%"),
                color = attrition),
            nudge_x     = 0.8,
            size        = 3.2,
            fontface    = "bold",
            inherit.aes = FALSE) +
  scale_fill_manual(values  = comp_colors) +
  scale_color_manual(values = comp_colors) +
  labs(
    title    = "Salary Hike % Distribution by Attrition",
    subtitle = "Dashed lines show group means",
    x        = "Percent Salary Hike (%)",
    y        = "Density",
    fill     = "Attrition",
    color    = "Attrition"
  ) +
  theme_comp

print(plot1b)

#Conclusion:
#The average salary hike percentage is practically identical for employees who stay and employees who leave.
#Interestingly, a minor increase in attrition density is observed at the extreme high end of the scale (22%–24%).
#This suggests that absolute percentage raises do not act as a primary retention mechanism in isolation, and high raises alone are insufficient to mitigate other underlying push factors.

#Plot 3: To investigate relationship between stock option and attrition
plot1c <- df_clean %>%
  count(stock_option_level, attrition) %>%
  group_by(stock_option_level) %>%
  mutate(
    pct   = n / sum(n),
    label = paste0(round(pct * 100, 1), "%")
  ) %>%
  ggplot(aes(x    = factor(stock_option_level),
             y    = pct,
             fill = attrition)) +
  geom_col(position = "fill", alpha = 0.85, width = 0.6) +
  geom_text(aes(label = label),
            position = position_fill(vjust = 0.5),
            size     = 3.5,
            fontface = "bold",
            color    = "white") +
  scale_fill_manual(values = comp_colors) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Attrition Proportion by Stock Option Level",
    x        = "Stock Option Level (0 = None, 3 = High)",
    y        = "Proportion (%)",
    fill     = "Attrition"
  ) +
  theme_comp

print(plot1c)

#Conclusion:
#"The relationship between Stock Option Level and Attrition displays a distinct U-shaped curve, indicating a non-linear trend.
#Interestingly, attrition is highest at stock option level 3.
#This anomaly suggests that while mid-tier equity effectively secures core staff, top-tier stock option structures (typically held by senior executives) do not insulate high-ranking talent from market poaching or strategic exits following stock vesting cycles.


# --- Plot 4: Income by Job Level — Deep Dive ---
# Shows whether income gap exists consistently at EVERY seniority level
plot1d <- df_clean %>%
  group_by(job_level, attrition) %>%
  summarise(
    median_income = median(monthly_income, na.rm = TRUE),
    n             = n(),
    .groups       = "drop"
  ) %>%
  ggplot(aes(x    = factor(job_level),
             y    = median_income,
             fill = attrition)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = comma(round(median_income, 0))),
            position = position_dodge(width = 0.7),
            vjust    = -0.4,
            size     = 3,
            fontface = "bold") +
  scale_fill_manual(values = comp_colors) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Median Income by Job Level and Attrition",
    subtitle = "Does the income gap exist at every seniority level?",
    x        = "Job Level",
    y        = "Median Monthly Income (RM)",
    fill     = "Attrition"
  ) +
  theme_comp

print(plot1d)

#Conclusion:
#A critical internal pay gap is identified at Level 1 (Entry-level) and Level 4 (Senior Management). 
#At Level 4, a stark median income deficit of RM1,833 exists for employees who left compared to their retained peers, signaling severe internal salary compression or inequity that likely catalyzed their exit.
#Conversely, at Levels 2, 3, and 5, the median income between attrition groups is nearly identical.
#it indicates that while financial interventions are urgently required to retain talent at Levels 1 and 4, compensation adjustments will likely fail to reduce turnover among mid-level (Levels 2 and 3) and executive (Level 5) staff, where non-monetary retention drivers must be explored.

# --- Display all 4 plots in a 2x2 grid ---
grid.arrange(plot1a, plot1b, plot1c, plot1d,
             ncol = 2,
             top  = "OBJECTIVE 1: Compensation & Attrition Analysis")

#==============================
#Section 7.4 Statistical Tests
# To prove findings are statistically significant — not just coincidence

cat("\n--- Statistical Tests: Compensation vs Attrition ---\n")

# --- Test 1: T-Test — Monthly Income ---
# Use: To compare a numeric variable between two groups (stayed vs left)
# Null hypothesis: no difference in mean income between groups

ttest_income <- t.test(monthly_income ~ attrition, data = df_clean)

cat("\n1. T-Test: Monthly Income vs Attrition\n")
cat("   Mean income (Stayed) :", round(ttest_income$estimate[1], 2), "\n")
cat("   Mean income (Left)   :", round(ttest_income$estimate[2], 2), "\n")
cat("   Difference           :", round(diff(ttest_income$estimate), 2), "\n")
cat("   T-statistic          :", round(ttest_income$statistic, 4), "\n")
cat("   P-value              :", round(ttest_income$p.value, 6), "\n")
cat("   Result               :", ifelse(ttest_income$p.value < 0.05,
                                        "SIGNIFICANT — income differs significantly between groups",
                                        "NOT significant"), "\n")

# --- Test 2: T-Test — Salary Hike % ---
ttest_hike <- t.test(percent_salary_hike ~ attrition, data = df_clean)

cat("\n2. T-Test: Salary Hike % vs Attrition\n")
cat("   Mean hike (Stayed) :", round(ttest_hike$estimate[1], 2), "%\n")
cat("   Mean hike (Left)   :", round(ttest_hike$estimate[2], 2), "%\n")
cat("   T-statistic        :", round(ttest_hike$statistic, 4), "\n")
cat("   P-value            :", round(ttest_hike$p.value, 6), "\n")
cat("   Result             :", ifelse(ttest_hike$p.value < 0.05,
                                      "SIGNIFICANT — salary hike differs significantly between groups",
                                      "NOT significant"), "\n")

# --- Test 3: Chi-Square — Stock Option Level ---
# Use: testing association between two categorical variables
# Null hypothesis: stock option level and attrition are independent
chisq_stock <- chisq.test(
  table(df_clean$stock_option_level, df_clean$attrition)
)

cat("\n3. Chi-Square: Stock Option Level vs Attrition\n")
cat("   Chi-square statistic :", round(chisq_stock$statistic, 4), "\n")
cat("   Degrees of freedom   :", chisq_stock$parameter, "\n")
cat("   P-value              :", round(chisq_stock$p.value, 6), "\n")
cat("   Result               :", ifelse(chisq_stock$p.value < 0.05,
                                        "SIGNIFICANT — stock options are associated with attrition",
                                        "NOT significant"), "\n")

# --- Collect all test results into one clean table ---
comp_stats <- tibble(
  test       = c("T-Test", "T-Test", "Chi-Square"),
  variable   = c("Monthly Income", "Salary Hike %", "Stock Option Level"),
  statistic  = c(round(ttest_income$statistic, 4),
                 round(ttest_hike$statistic, 4),
                 round(chisq_stock$statistic, 4)),
  p_value    = c(round(ttest_income$p.value, 6),
                 round(ttest_hike$p.value, 6),
                 round(chisq_stock$p.value, 6)),
  significant = ifelse(
    c(ttest_income$p.value, ttest_hike$p.value, chisq_stock$p.value) < 0.05,
    "YES ***", "NO"
  ),
  conclusion = c(
    ifelse(ttest_income$p.value < 0.05,
           "Monthly income significantly lower for employees who left",
           "No significant income difference"),
    ifelse(ttest_hike$p.value < 0.05,
           "Salary hike % significantly differs by attrition",
           "No significant salary hike difference"),
    ifelse(chisq_stock$p.value < 0.05,
           "Stock option level significantly associated with attrition",
           "No significant association")
  )
)

cat("\n--- Compensation Statistical Results Summary ---\n")
print(comp_stats)

#Section 8.5: What if analysis
# Simulate the effect of compensation policy changes on attrition
# Uses the logistic regression model to predict new attrition probabilities