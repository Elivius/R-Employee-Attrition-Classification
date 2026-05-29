# =============================================================================
# 
# EMPLOYEE ATTRITION CLASSIFICATION
# Group Number : GROUP 21
# Members      :
# [Joshua Yeo Jing Hao, TP077315]
# [Chin Kai Jack, TP076605]
# [Ee Jin Xing, TP076848]
# [Lee Hong Yi, TP076604]
# 
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
# SECTION 7: ANALYSIS
# =============================================================================


# =============================================================================
# Analysis 1: OBJECTIVE 1 — To investigate the relationship between salary 
# compression including Monthly income, Percentage of salary hike and Stock 
# option level with attrition
# Member    : Joshua Yeo Jing Hao TP077315
# Variables : Attrition, MonthlyIncome, PercentSalaryHike, StockOptionLevel, Job Level, Age
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, scales, gridExtra, janitor, arrow, caret, corrplot)

message("[OK] All libraries loaded — ready to proceed.")


OUTPUT_PARQUET   <- "employee_attrition_cleaned.parquet"
RANDOM_SEED <- 42     # keeps results the same every run
TRAIN_SPLIT <- 0.80   # 80% trains the model, 20% tests it

# --- 7.1 Verify Parquet File Exists Before Reading ---
if (!file.exists(OUTPUT_PARQUET)) {
  stop(
    "\n[ERROR] Parquet file not found: '", OUTPUT_PARQUET, "'\n",
    "Make sure Section 6 ran successfully and write_parquet() completed.\n"
  )
}

cat("=== PARQUET DATA RETRIEVAL ===\n")
cat("Source file  :", OUTPUT_PARQUET, "\n")

# Objective 1 — Compensation
df_obj1 <- read_parquet(
  OUTPUT_PARQUET,
  col_select = c("attrition", "monthly_income",
                 "percent_salary_hike", "stock_option_level",
                 "job_level", "age")
)
cat("Obj 1 (Compensation)  :", ncol(df_obj1), "cols loaded\n")


# --- Re-apply factor levels after parquet read ---
# Parquet preserves values but R-specific factor attributes
# need to be reapplied for correct statistical modelling

df_obj1 <- df_obj1 %>%
  mutate(
    attrition          = factor(attrition,
                                levels = c("No", "Yes")),
    stock_option_level = factor(stock_option_level,
                                levels = c("0", "1", "2", "3")),
    job_level          = factor(job_level)
  )

cat("\n[OK] Objective 1 dataset loaded and factors restored.\n")
cat("     df_obj1 ready for analysis.\n")
cat("\n>>> Proceed to Section 7.1 — Objective 1: Compensation\n")


# --- Theme Settings ---
# Consistent theme applied to all Objective 1 plots
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
cat("Variables  : MonthlyIncome, PercentSalaryHike, StockOptionLevel\n")
cat("Dataset    : df_obj1 (", nrow(df_obj1), "rows x",
    ncol(df_obj1), "cols from Parquet)\n")
cat("Hypothesis : Lower compensation is significantly associated",
    "with higher attrition\n\n")


# =============================================================================
# Analysis 1.1: DESCRIPTIVE STATISTICS
# Understand compensation distribution before plotting
# =============================================================================

cat("--- Compensation Summary by Attrition Group ---\n")

compensation_summary <- df_obj1 %>%
  group_by(attrition) %>%
  summarise(
    n                = n(),
    avg_monthly_income = round(mean(monthly_income,        na.rm = TRUE), 2),
    med_monthly_income = round(median(monthly_income,      na.rm = TRUE), 2),
    avg_salary_hike    = round(mean(percent_salary_hike,   na.rm = TRUE), 2),
    med_salary_hike    = round(median(percent_salary_hike, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(compensation_summary)

# Stock option distribution by attrition
cat("\n--- Stock Option Level Distribution by Attrition ---\n")
stock_table <- table(df_obj1$stock_option_level, df_obj1$attrition)
print(stock_table)
cat("\nRow percentages:\n")
print(round(prop.table(stock_table, margin = 1) * 100, 1))


# =============================================================================
# Analysis 1.2: VISUALISATIONS
# 4 plots — one per compensation variable + one deep dive by job level
# =============================================================================

# --- Prepare summary stats used across multiple plots ---
income_summary <- df_obj1 %>%
  group_by(attrition) %>%
  summarise(
    n      = n(),
    mean   = round(mean(monthly_income,   na.rm = TRUE), 0),
    median = round(median(monthly_income, na.rm = TRUE), 0),
    .groups = "drop"
  )

hike_summary <- df_obj1 %>%
  group_by(attrition) %>%
  summarise(
    mean_hike = round(mean(percent_salary_hike, na.rm = TRUE), 2),
    .groups   = "drop"
  )


# --- Plot 1: Monthly Income vs Attrition ---
# Boxplot + jitter shows both distribution and individual data points
# Median labels show exact RM values — supports the "quantify" requirement
# White diamond = mean | horizontal line = median
plot1a <- ggplot(df_obj1,
                 aes(x = attrition, y = monthly_income, fill = attrition)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2,
               outlier.size = 1, width = 0.5) +
  geom_jitter(aes(color = attrition),
              width = 0.15, alpha = 0.15, size = 0.8) +
  stat_summary(fun  = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  geom_text(data = income_summary,
            aes(x     = attrition,
                y     = median,
                label = paste0("Median:\nRM", comma(median))),
            nudge_x     = 0.35,
            size        = 3.5,
            fontface    = "bold",
            inherit.aes = FALSE) +
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


# --- Plot 2: Salary Hike % vs Attrition ---
# Overlaid density plot — best for narrow range (11-25%)
# Smoother and clearer than histogram for comparing two groups
# Dashed lines show each group mean
plot1b <- ggplot(df_obj1 %>% filter(!is.na(percent_salary_hike)),
                 aes(x     = percent_salary_hike,
                     fill  = attrition,
                     color = attrition)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  geom_vline(data = hike_summary,
             aes(xintercept = mean_hike, color = attrition),
             linetype  = "dashed",
             linewidth = 1) +
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


# --- Plot 3: Stock Option Level vs Attrition ---
# 100% stacked bar — shows BOTH stayed and left proportions simultaneously
# More informative than showing only attrition rate
# White labels inside bars show exact percentages
plot1c <- df_obj1 %>%
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
    subtitle = "Level 2 shows lowest attrition — Level 3 shows unexpected spike",
    x        = "Stock Option Level (0 = None, 3 = High)",
    y        = "Proportion (%)",
    fill     = "Attrition"
  ) +
  theme_comp

print(plot1c)


# --- Plot 4: Income by Job Level ---
# Shows whether income gap exists consistently at EVERY seniority level
# Directly supports the "quantify" requirement in the objective
plot1d <- df_obj1 %>%
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


# --- Display all 4 plots in a 2x2 grid ---
grid.arrange(plot1a, plot1b, plot1c, plot1d,
             ncol = 2,
             top  = "OBJECTIVE 1: Compensation & Attrition Analysis")


# =============================================================================
# Analysis 1.3: STATISTICAL TESTS
# Prove findings are statistically significant, not just coincidence
# =============================================================================

cat("\n--- Statistical Tests: Compensation vs Attrition ---\n")

# --- Test 1: T-Test — Monthly Income ---
# Use: comparing a numeric variable between two groups (stayed vs left)
# Null hypothesis: no difference in mean income between groups
ttest_income <- t.test(monthly_income ~ attrition, data = df_obj1)

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
# Null hypothesis: no difference in mean salary hike between groups
ttest_hike <- t.test(percent_salary_hike ~ attrition, data = df_obj1)

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
  table(df_obj1$stock_option_level, df_obj1$attrition)
)

cat("\n3. Chi-Square: Stock Option Level vs Attrition\n")
cat("   Chi-square statistic :", round(chisq_stock$statistic, 4), "\n")
cat("   Degrees of freedom   :", chisq_stock$parameter, "\n")
cat("   P-value              :", round(chisq_stock$p.value, 6), "\n")
cat("   Result               :", ifelse(chisq_stock$p.value < 0.05,
                                        "SIGNIFICANT — stock options are associated with attrition",
                                        "NOT significant"), "\n")


# --- Collect all test results into one clean summary table ---
comp_stats <- tibble(
  test        = c("T-Test", "T-Test", "Chi-Square"),
  variable    = c("Monthly Income", "Salary Hike %", "Stock Option Level"),
  statistic   = c(round(ttest_income$statistic, 4),
                  round(ttest_hike$statistic, 4),
                  round(chisq_stock$statistic, 4)),
  p_value     = c(round(ttest_income$p.value, 6),
                  round(ttest_hike$p.value, 6),
                  round(chisq_stock$p.value, 6)),
  significant = ifelse(
    c(ttest_income$p.value,
      ttest_hike$p.value,
      chisq_stock$p.value) < 0.05,
    "YES ***", "NO"
  ),
  conclusion  = c(
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


# =============================================================================
# Analysis 1.4: WHAT-IF ANALYSIS
# Simulate the effect of compensation policy changes on predicted attrition
# Uses logistic regression to predict new attrition probabilities
# =============================================================================

cat("\n\n--- What-If Analysis: Compensation Policy Simulation ---\n")

# Build compensation-focused logistic regression
# Uses df_obj1 — loaded from parquet with compensation columns only
compensation_model_data <- df_obj1 %>%
  mutate(
    stock_option_level = factor(stock_option_level),
    job_level          = factor(job_level)
  ) %>%
  drop_na()

set.seed(RANDOM_SEED)
compensation_split <- initial_split(compensation_model_data,
                                    prop = TRAIN_SPLIT,
                                    strata = attrition)
compensation_train <- training(compensation_split)
compensation_test  <- testing(compensation_split)

compensation_recipe <- recipe(attrition ~ ., data = compensation_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

compensation_log_model <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

compensation_workflow <- workflow() %>%
  add_recipe(compensation_recipe) %>%
  add_model(compensation_log_model)

compensation_fit <- compensation_workflow %>%
  fit(data = compensation_train)

cat("[OK] Compensation model trained.\n\n")

# Baseline — current predicted attrition rate
pred_current <- predict(compensation_fit,
                        compensation_model_data,
                        type = "prob")$.pred_Yes
current_rate   <- mean(pred_current) * 100


# --- Scenario 1: 10% Salary Increase for ALL employees ---
scenario1 <- compensation_model_data %>%
  mutate(monthly_income = monthly_income * 1.10)

pred_scenario1 <- predict(compensation_fit, scenario1,
                          type = "prob")$.pred_Yes
scenario1_rate <- mean(pred_scenario1) * 100

cat("Scenario 1: 10% Salary Increase for All Employees\n")
cat("  Current predicted attrition rate  :", round(current_rate, 1),   "%\n")
cat("  Predicted rate after 10% increase :", round(scenario1_rate, 1), "%\n")
cat("  Predicted reduction               :", round(current_rate - scenario1_rate, 1), "%\n\n")


# --- Scenario 2: Minimum Salary Hike of 15% ---
scenario2 <- compensation_model_data %>%
  mutate(percent_salary_hike = pmax(percent_salary_hike, 15))

pred_scenario2 <- predict(compensation_fit, scenario2,
                          type = "prob")$.pred_Yes
scenario2_rate <- mean(pred_scenario2) * 100

cat("Scenario 2: Minimum Salary Hike of 15% for All Employees\n")
cat("  Current predicted attrition rate  :", round(current_rate, 1),   "%\n")
cat("  Predicted rate after policy change:", round(scenario2_rate, 1), "%\n")
cat("  Predicted reduction               :",
    round(current_rate - scenario2_rate, 1), "%\n\n")


# --- Scenario 3: Give Stock Options to Employees with Level 0 ---
scenario3 <- compensation_model_data %>%
  mutate(
    stock_option_level = factor(
      ifelse(as.numeric(as.character(stock_option_level)) == 0,
             1,
             as.numeric(as.character(stock_option_level)))
    )
  )

pred_scenario3 <- predict(compensation_fit, scenario3,
                          type = "prob")$.pred_Yes
scenario3_rate <- mean(pred_scenario3) * 100

cat("Scenario 3: Provide Minimum Stock Options to Employees with None\n")
cat("  Current predicted attrition rate  :", round(current_rate, 1),   "%\n")
cat("  Predicted rate after policy change:", round(scenario3_rate, 1), "%\n")
cat("  Predicted reduction               :",
    round(current_rate - scenario3_rate, 1), "%\n\n")


# --- What-If Summary Plot ---
whatif_data <- tibble(
  scenario = c(
    "Current",
    "10% Salary\nIncrease",
    "Min 15%\nSalary Hike",
    "Stock Options\nfor All"
  ),
  attrition_rate = c(
    current_rate,
    scenario1_rate,
    scenario2_rate,
    scenario3_rate
  )
)

whatif_plot <- ggplot(whatif_data,
                      aes(x    = fct_inorder(scenario),
                          y    = attrition_rate,
                          fill = scenario == "Current")) +
  geom_col(alpha = 0.85, width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(attrition_rate, 1), "%")),
            vjust    = -0.4,
            size     = 4,
            fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#F44336", "FALSE" = "#2196F3")) +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "What-If Analysis: Impact of Compensation Policy Changes",
    subtitle = "Predicted attrition rate under 3 different policy scenarios",
    x        = "Policy Scenario",
    y        = "Predicted Attrition Rate (%)",
    caption  = "Red = current baseline | Blue = policy scenarios"
  ) +
  theme_comp

print(whatif_plot)

message("\n[OK] Section 7 — Objective 1 (Compensation) complete.")


# =============================================================================
# OBJECTIVE 2: To investigate the impact of burnout and work-life pressure on employee attrition
# NAME  : Chin Kai Jack TP076605
# Variables: Attrition, OverTime, BusinessTravel, DistanceFromHome, MaritalStatus
# =============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, scales, gridExtra, janitor, arrow, caret, corrplot)

message("[OK] All libraries loaded — ready to proceed.")

OUTPUT_PARQUET <- "employee_attrition_cleaned.parquet"

df_clean <- read_parquet(
  OUTPUT_PARQUET,
  col_select = c("attrition", "over_time", "business_travel",
                 "distance_from_home", "marital_status")
)
cat("Obj 2 (Burnout) loaded:", nrow(df_clean), "rows x", ncol(df_clean), "cols\n")

# --- Objective 2 Theme -------------------------------------------
OBJ2_THEME <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(colour = "grey40", size = 11),
    axis.title    = element_text(face = "bold"),
    legend.position = "top"
  )
COLOR_NO  <- "#2196F3"   # blue = stayed
COLOR_YES <- "#F44336"   # red  = left
COLOR_BAR <- "#9C27B0"   # purple = bar charts


# =============================================================================
# ANALYSIS 2-1: Impact of Overtime on Attrition (Chi-Square + Cramér's V)
# =============================================================================
message("\n--- Analysis 2-1 Overtime x Attrition (Chi-Square) ---")

# A. Contingency Table
overtime_tbl <- table(df_clean$over_time, df_clean$attrition)
cat("Contingency Table:\n")
print(overtime_tbl)

# B. Chi-Square Test
overtime_chi <- chisq.test(overtime_tbl)
cat("\nChi-Square Results:\n")
print(overtime_chi)

# C. Effect Size — Cramér's V
# p-value = IF the association is real | Cramér's V = HOW STRONG it is
# Interpretation: 0.10–0.29 = medium, 0.30+ = strong
overtime_cramV <- sqrt(overtime_chi$statistic /
                         (nrow(df_clean) * (min(dim(overtime_tbl)) - 1)))
cat("Cramér's V =", round(overtime_cramV, 3), "\n")

# D. Visualization — Proportional Stacked Bar with Percentage Labels
p_obj2_1 <- df_clean %>%
  count(over_time, attrition) %>%
  group_by(over_time) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = over_time, y = pct, fill = attrition)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("No" = COLOR_NO, "Yes" = COLOR_YES)) +
  labs(
    title    = "Burnout / Workload Intensity: Overtime & Attrition",
    subtitle = paste("Chi-Square p =", format.pval(overtime_chi$p.value, digits = 3),
                      "  |  Cramér's V =", round(overtime_cramV, 3)),
    x = "Working Overtime", y = "Proportion (%)", fill = "Attrition"
  ) +
  OBJ2_THEME

print(p_obj2_1)


# =============================================================================
# ANALYSIS 2-2: Dose-Response Relationship of Business Travel and Attrition
# (Chi-Square + Cramér's V + Dose-Response)
# =============================================================================
message("\n--- Analysis 2-2 Business Travel x Attrition (Chi-Square) ---")

# A. Contingency Table (as row percentages)
travel_tbl <- table(df_clean$business_travel, df_clean$attrition)
cat("Row Percentages (%):\n")
print(round(prop.table(travel_tbl, margin = 1) * 100, 1))

# B. Chi-Square Test
travel_chi <- chisq.test(travel_tbl)
cat("\nChi-Square Results:\n")
print(travel_chi)

# C. Effect Size — Cramér's V
travel_cramV <- sqrt(travel_chi$statistic /
                       (nrow(df_clean) * (min(dim(travel_tbl)) - 1)))
cat("Cramér's V =", round(travel_cramV, 3), "\n")

# D. Visualization — Proportional Stacked Bar Chart
p_obj2_2 <- df_clean %>%
  # Reorder factor to show dose-response gradient
  mutate(business_travel = factor(business_travel, levels = c("No Travel", "Travel Rarely", "Travel Frequently"))) %>%
  count(business_travel, attrition) %>%
  group_by(business_travel) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = business_travel, y = pct, fill = attrition)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("No" = COLOR_NO, "Yes" = COLOR_YES)) +
  labs(
    title    = "Professional Strain: Travel Frequency & Attrition",
    subtitle = paste("Chi-Square p =", format.pval(travel_chi$p.value, digits = 3),
                      "  |  Cramér's V =", round(travel_cramV, 3),
                      "\nDose-Response: does attrition increase with each travel level? The more X given, the more Y happens"),
    x = "Travel Frequency (ordered)", y = "Proportion (%)", fill = "Attrition"
  ) +
  OBJ2_THEME

print(p_obj2_2)


# =============================================================================
# ANALYSIS 2-3: The Commute Penalty – Distance from Home vs. Attrition
# (Kruskal-Wallis + Violin)
# =============================================================================
message("\n--- Analysis 2-3 Distance from Home x Attrition (Kruskal-Wallis) ---")

# A. WHY Kruskal-Wallis? — Verify distance is NOT normally distributed
hist(df_clean$distance_from_home, breaks = 20, col = COLOR_BAR,
     main = "Distance from Home Distribution (Normality Check)",
     xlab = "Distance (km)")
# Observation: right-skewed distribution → t-test assumptions violated → use KW

# B. Non-Parametric Test
# distance_from_home split by (~) attrition
dist_kw <- kruskal.test(distance_from_home ~ attrition, data = df_clean)
cat("Kruskal-Wallis Results:\n")
print(dist_kw)

# C. Group Medians
dist_summary <- df_clean %>%
  group_by(attrition) %>%
  summarise(
    n           = n(),
    median_dist = median(distance_from_home),
    mean_dist   = round(mean(distance_from_home), 1),
    .groups     = "drop"
  )
cat("\nDistance Summary by Attrition:\n")
print(dist_summary)

# D. Visualization — Violin + Boxplot with Median Annotation
p_obj2_3 <- df_clean %>%
  ggplot(aes(x = attrition, y = distance_from_home, fill = attrition)) +
  geom_violin(width = 1, alpha = 0.5, colour = NA, show.legend = FALSE) +
  geom_boxplot(width = 0.2, colour = "black", outlier.shape = NA, alpha = 0.8, show.legend = FALSE) +
  # Annotate median values directly on the plot
  geom_text(data = dist_summary,
            aes(x = attrition, y = median_dist,
                label = paste("Mdn =", median_dist)),
            vjust = -1.2, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("No" = COLOR_NO, "Yes" = COLOR_YES)) +
  labs(
    title    = "The Commute Penalty: Distance & Attrition",
    subtitle = paste("Kruskal-Wallis p =", format.pval(dist_kw$p.value, digits = 3),
                      "  |  Non-parametric (distance is right-skewed)"),
    x = "Attrition", y = "Distance from Home (km)"
  ) +
  OBJ2_THEME

print(p_obj2_3)


# =============================================================================
# ANALYSIS 2-4: The Social Buffer – Martial Status and Attrition (Chi-Square)
# =============================================================================
message("\n--- Analysis 2-4 Marital Status x Attrition (Chi-Square) ---")

# A. Contingency Table
marital_tbl <- table(df_clean$marital_status, df_clean$attrition)
cat("Contingency Table:\n")
print(marital_tbl)

# B. Chi-Square Test for Marital Status
marital_chi <- chisq.test(marital_tbl)
cat("\nChi-Square Results:\n")
print(marital_chi)

# C. Effect Size — Cramér's V
marital_cramV <- sqrt(marital_chi$statistic /
                        (nrow(df_clean) * (min(dim(marital_tbl)) - 1)))
cat("Cramér's V =", round(marital_cramV, 3), "\n")

# D. Proportional Bar Chart
p_obj2_4 <- df_clean %>%
  count(marital_status, attrition) %>%
  group_by(marital_status) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = marital_status, y = pct, fill = attrition)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_stack(vjust = 0.5),
            colour = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("No" = COLOR_NO, "Yes" = COLOR_YES)) +
  labs(
    title    = "Social Buffer: Marital Status & Attrition",
    subtitle = paste("Chi-Square p =", format.pval(marital_chi$p.value, digits = 3),
                      "  |  Cramér's V =", round(marital_cramV, 3)),
    x = "Marital Status", y = "Proportion (%)", fill = "Attrition"
  ) +
  OBJ2_THEME

print(p_obj2_4)


# =============================================================================
# ANALYSIS 2-5: Multi-Dimensional Risk Profiling (Interaction Heatmap)
# (Combination of Marital Status × Overtime)
# =============================================================================
message("\n--- Analysis 2-5 Heatmap ---")

# A. Interaction Heatmap — Marital Status × Overtime
#    Which COMBINATION is most at risk?
heatmap_data <- df_clean %>%
  group_by(marital_status, over_time) %>%
  summarise(
    n        = n(),
    attr_pct = mean(attrition == "Yes") * 100,
    .groups  = "drop"
  )

cat("\nInteraction Table — Attrition Rate (%) by Marital Status x Overtime:\n")
print(heatmap_data)

p_obj2_5a <- heatmap_data %>%
  ggplot(aes(x = over_time, y = marital_status, fill = attr_pct)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = paste0(round(attr_pct, 1), " %\n(n = ", n, ")")),
            colour = "white", fontface = "bold", size = 4.5) +
  scale_fill_gradient(low = COLOR_NO, high = COLOR_YES,
                      name = "Attrition %") +
  labs(
    title    = "Risk Heatmap: Marital Status x Overtime Interaction",
    subtitle = "Single + Overtime = highest burnout vulnerability",
    x = "Overtime Status", y = "Marital Status"
  ) +
  OBJ2_THEME +
  theme(legend.position = "right")

print(p_obj2_5a)


# B. 3-way Interaction Heatmap — Marital Status × Overtime × Business Travel
#    Three-Way Interaction to isolate the ultimate compounded turnover risk
heatmap_data_3way <- df_clean %>%
  group_by(marital_status, over_time, business_travel) %>% # 1. Added travel to grouping
  summarise(
    n        = n(),
    attr_pct = mean(attrition == "Yes") * 100,
    .groups  = "drop"
  )

cat("\n3-Way Interaction Table — Attrition Rate (%) with Travel:\n")
print(heatmap_data_3way)

p_obj2_5b <- heatmap_data_3way %>%
  ggplot(aes(x = over_time, y = marital_status, fill = attr_pct)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = paste0(round(attr_pct, 1), " %\n(n = ", n, ")")),
            colour = "white", fontface = "bold", size = 3.5) + 
  scale_fill_gradient(low = COLOR_NO, high = COLOR_YES,
                      name = "Attrition %") +
  
  # Splits the heatmap into columns based on travel frequency
  facet_wrap(~business_travel) + 
  
  labs(
    title    = "Multi-Dimensional Risk Heatmap: Marital × Overtime × Travel",
    subtitle = "Identifying how operational travel compounding intensifies demographic burnout vulnerabilities",
    x = "Overtime Status", y = "Marital Status"
  ) +
  OBJ2_THEME +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 11) # Makes the facet headers stand out cleanly
  )

print(p_obj2_5b)


# =============================================================================
# ANALYSIS 2-6: Multivariate Diagnostic Model – Logistic Regression
# (All 4 Burnout Factors Combined)
# =============================================================================
message("\n--- Analysis 2-6 Logistic Regression — Burnout Model ---")

# A. Prepare binary response (glm requires numeric 0/1 for binomial family)
df_logit <- df_clean %>%
  mutate(attr_bin = ifelse(attrition == "Yes", 1, 0))

# B. Fit Model - glm() + binomial() = logistic regression
model_burnout <- glm(
  attr_bin ~ over_time + business_travel + distance_from_home + marital_status,
  data   = df_logit,
  family = binomial()
)

cat("Model Summary:\n")
print(summary(model_burnout))

# C. Odds Ratios with 95% Confidence Intervals
odds_df <- data.frame(
  term      = names(coef(model_burnout)),
  odds      = exp(coef(model_burnout)),
  ci_low    = exp(confint.default(model_burnout)[, 1]),
  ci_high   = exp(confint.default(model_burnout)[, 2])
)
odds_df <- odds_df[odds_df$term != "(Intercept)", ]   # drop intercept
rownames(odds_df) <- NULL

cat("\nOdds Ratios (95% CI):\n")
print(odds_df)

# D. Interpretation Guide
cat("\n--- How to read Odds Ratios ---\n")
cat("  OR > 1  → increases attrition risk   (e.g. 3.36 = 3.36× more likely)\n")
cat("  OR < 1  → decreases attrition risk   (e.g. 0.60 = 40% less likely)\n")
cat("  CI crossing 1.0 → NOT statistically significant\n")

# E. Forest Plot — Visual Summary of Logistic Regression
p_obj2_6 <- odds_df %>%
  mutate(
    # Clean labels for display
    label = case_when(
      term == "over_timeYes"                      ~ "Overtime (Yes)",
      term == "business_travelTravel Rarely"      ~ "Travel Rarely",
      term == "business_travelTravel Frequently"  ~ "Travel Frequently",
      term == "distance_from_home"                ~ "Distance from Home",
      term == "marital_statusMarried"             ~ "Married",
      term == "marital_statusSingle"              ~ "Single",
      TRUE                                        ~ term
    ),
    # Flag significance: CI does not cross 1.0
    significant = ifelse(ci_low > 1 | ci_high < 1, "Significant", "Not Significant")
  ) %>%
  ggplot(aes(x = odds, y = reorder(label, odds), colour = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3.5) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, linewidth = 0.8) +
  scale_colour_manual(values = c("Significant" = COLOR_YES,
                                  "Not Significant" = "grey60")) +
  labs(
    title    = "Burnout Model: What Predicts Attrition?",
    subtitle = "Logistic Regression Odds Ratios with 95% CI  |  Dashed line = no effect (OR = 1)",
    x = "Odds Ratio", y = NULL, colour = "Significance"
  ) +
  OBJ2_THEME

print(p_obj2_6)


# =============================================================================
# RESULTS SUMMARY TABLE — All Burnout Tests at a Glance
# =============================================================================
message("\n--- Burnout Analysis Summary ---")

burnout_results <- data.frame(
  Analysis  = c("Overtime x Attrition",
                "Travel x Attrition",
                "Distance x Attrition",
                "Marital Status x Attrition"),
  Test      = c("Chi-Square", "Chi-Square", "Kruskal-Wallis", "Chi-Square"),
  Statistic = c(round(overtime_chi$statistic, 2),
                round(travel_chi$statistic, 2),
                round(dist_kw$statistic, 2),
                round(marital_chi$statistic, 2)),
  p_value   = c(format.pval(overtime_chi$p.value, digits = 3),
                format.pval(travel_chi$p.value, digits = 3),
                format.pval(dist_kw$p.value, digits = 3),
                format.pval(marital_chi$p.value, digits = 3)),
  Effect    = c(paste("V =", round(overtime_cramV, 3)),
                paste("V =", round(travel_cramV, 3)),
                "—",
                paste("V =", round(marital_cramV, 3))),
  Verdict   = c(
    ifelse(overtime_chi$p.value < 0.05, "Reject H0", "Fail to Reject H0"),
    ifelse(travel_chi$p.value  < 0.05, "Reject H0", "Fail to Reject H0"),
    ifelse(dist_kw$p.value     < 0.05, "Reject H0", "Fail to Reject H0"),
    ifelse(marital_chi$p.value < 0.05, "Reject H0", "Fail to Reject H0")
  ),
  stringsAsFactors = FALSE
)

print(burnout_results)

message("\n[OK] Section 7 — Objective 2 (Burnout & Work-Life Pressure) complete.")



# =============================================================================
# Analysis 3 / OBJECTIVE 3: To investigate how Career Stagnation influences employee attrition
# Name: [EE JIN XING, TP076848]
# Variables : YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion,
#             JobLevel, TrainingTimesLastYear, NumCompaniesWorked
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, scales, gridExtra, janitor, arrow, caret, corrplot, broom)

message("[OK] All libraries loaded — ready to proceed.")

OUTPUT_PARQUET   <- "employee_attrition_cleaned.parquet"

# --- 7.1 Verify Parquet File Exists Before Reading ---
if (!file.exists(OUTPUT_PARQUET)) {
  stop(
    "\n[ERROR] Parquet file not found: '", OUTPUT_PARQUET, "'\n",
    "Make sure Section 6 ran successfully and write_parquet() completed.\n"
  )
}

cat("=== PARQUET DATA RETRIEVAL ===\n")
cat("Source file  :", OUTPUT_PARQUET, "\n")

# Objective 3 — Career Growth
# Includes all columns needed for Analysis 3:
#   years_in_current_role  — used in plot_3d (Role Stagnation Index)
#   num_companies_worked   — used in descriptive summary
df_obj3 <- read_parquet(
  OUTPUT_PARQUET,
  col_select = c("attrition", "years_since_last_promotion",
                 "training_times_last_year", "years_at_company",
                 "years_in_current_role", "num_companies_worked",
                 "job_level")
)
cat("Obj 3 (Career Growth) :", ncol(df_obj3), "cols loaded\n")


df_obj3 <- df_obj3 %>%
  mutate(
    attrition = factor(attrition, levels = c("No", "Yes")),
    job_level = factor(job_level)
  )

cat("\n[OK] All objective datasets loaded and factors restored.\n")
cat("     df_obj3 ready for analysis.\n")


# =============================================================================
# Config — colours and constants used across all objectives
# =============================================================================

RANDOM_SEED  <- 42
COLOR_NO     <- "#2196F3"   # blue   = stayed
COLOR_YES    <- "#F44336"   # red    = left
COLOR_BAR    <- "#9C27B0"   # purple = bar charts
COLOR_ORANGE <- "#FF9800"   # orange = training chart
COLOR_GREEN  <- "#4CAF50"   # green  = no overtime

LBL_4POINT <- c("Low", "Medium", "High", "Very High")
LBL_WLB    <- c("Bad", "Good", "Better", "Best")
LBL_PERF   <- c("Low", "Good", "Excellent", "Outstanding")
LBL_EDU    <- c("Below College", "College", "Bachelor", "Master", "Doctor")

# --- Shared theme (mirrors group style) ---
theme_career <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "grey50"),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin      = margin(10, 10, 10, 10)
  )

# Colour mapping — uses Section 7 config variables (not hardcoded)
career_colors <- c("No" = COLOR_NO, "Yes" = COLOR_YES)

cat("\n=== OBJECTIVE 3: CAREER GROWTH / STAGNATION ANALYSIS ===\n")
cat("Dataset  :", nrow(df_obj3), "rows x", ncol(df_obj3), "cols (from parquet)\n")
cat("Variables: years_at_company, years_in_current_role,\n")
cat("           years_since_last_promotion, job_level,\n")
cat("           training_times_last_year, num_companies_worked\n")
cat("Hypothesis: Stagnant career progression is significantly associated\n")
cat("            with higher attrition.\n\n")


# =============================================================================
# Analysis 3-1  DESCRIPTIVE ANALYSIS
# =============================================================================

cat("--- Career Growth Summary by Attrition Group ---\n")

career_summary <- df_obj3 %>%
  group_by(attrition) %>%
  summarise(
    n                         = n(),
    avg_years_at_company      = round(mean(years_at_company,             na.rm = TRUE), 2),
    med_years_at_company      = round(median(years_at_company,           na.rm = TRUE), 2),
    avg_years_current_role    = round(mean(years_in_current_role,        na.rm = TRUE), 2),
    med_years_current_role    = round(median(years_in_current_role,      na.rm = TRUE), 2),
    avg_years_since_promotion = round(mean(years_since_last_promotion,   na.rm = TRUE), 2),
    med_years_since_promotion = round(median(years_since_last_promotion, na.rm = TRUE), 2),
    avg_training_times        = round(mean(training_times_last_year,     na.rm = TRUE), 2),
    avg_num_companies_worked  = round(mean(num_companies_worked,         na.rm = TRUE), 2),
    .groups = "drop"
  )

print(career_summary)

# Job Level distribution by attrition
cat("\n--- Job Level Distribution by Attrition ---\n")
joblevel_table <- table(df_obj3$job_level, df_obj3$attrition)
print(joblevel_table)
cat("\nRow percentages (attrition rate per job level):\n")
print(round(prop.table(joblevel_table, margin = 1) * 100, 1))

# Training times distribution by attrition
cat("\n--- Training Times Last Year Distribution by Attrition ---\n")
training_table <- table(df_obj3$training_times_last_year, df_obj3$attrition)
print(training_table)
cat("\nRow percentages:\n")
print(round(prop.table(training_table, margin = 1) * 100, 1))

message("[OK] Analysis 3-1 Descriptive Analysis complete.")


# =============================================================================
# Analysis 3-2  VISUALISATIONS
# =============================================================================

# --- Pre-compute group means for plot annotations ---
promotion_summary <- df_obj3 %>%
  group_by(attrition) %>%
  summarise(mean_promo = round(mean(years_since_last_promotion, na.rm = TRUE), 2),
            .groups    = "drop")

training_summary <- df_obj3 %>%
  group_by(attrition) %>%
  summarise(mean_train = round(mean(training_times_last_year, na.rm = TRUE), 2),
            .groups    = "drop")

# -----------------------------------------------------------------------------
# plot_3a — Years Since Last Promotion vs Attrition (Boxplot + Jitter)
# Purpose : Show whether leavers experienced longer promotion droughts
# -----------------------------------------------------------------------------
plot_3a <- ggplot(df_obj3,
                  aes(x = attrition, y = years_since_last_promotion,
                      fill = attrition)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2,
               outlier.size = 1, width = 0.5) +
  geom_jitter(aes(color = attrition),
              width = 0.15, alpha = 0.15, size = 0.8) +
  stat_summary(fun = mean, geom = "point",
               shape = 18, size = 4, color = "white") +
  scale_fill_manual(values  = career_colors) +
  scale_color_manual(values = career_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title   = "Years Since Last Promotion by Attrition",
    x       = "Attrition Status",
    y       = "Years Since Last Promotion",
    fill    = "Attrition",
    caption = "White diamond = Mean | Line = Median | Points = Individual employees"
  ) +
  theme_career +
  theme(legend.position = "none")

print(plot_3a)

# Conclusion:
# Employees who left waited longer since their last promotion than those who
# stayed. The higher median and wider spread for the "Yes" group indicate that
# promotion drought is a meaningful attrition driver — employees who see no
# upward movement are more likely to seek opportunities elsewhere.


# -----------------------------------------------------------------------------
# plot_3b — Training Times Last Year (Density) by Attrition
# Purpose : Show whether fewer training opportunities are linked to leaving
# -----------------------------------------------------------------------------
plot_3b <- ggplot(df_obj3 %>% filter(!is.na(training_times_last_year)),
                  aes(x = training_times_last_year, fill = attrition,
                      color = attrition)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  geom_vline(data     = training_summary,
             aes(xintercept = mean_train, color = attrition),
             linetype = "dashed", linewidth = 1) +
  geom_text(data = training_summary,
            aes(x     = mean_train,
                y     = 0.25,
                label = paste0(attrition, "\nMean: ", mean_train),
                color = attrition),
            nudge_x     = 0.25,
            size        = 3.2,
            fontface    = "bold",
            inherit.aes = FALSE) +
  scale_fill_manual(values  = career_colors) +
  scale_color_manual(values = career_colors) +
  scale_x_continuous(breaks = 0:6) +
  labs(
    title    = "Training Sessions Last Year by Attrition",
    subtitle = "Dashed lines show group means",
    x        = "Number of Training Sessions (Last Year)",
    y        = "Density",
    fill     = "Attrition",
    color    = "Attrition"
  ) +
  theme_career

print(plot_3b)

# Conclusion:
# Leavers are slightly more concentrated at the lower end of training frequency
# (0-2 sessions). While training volume alone is not decisive, employees who
# feel under-invested in professionally are modestly more inclined to leave.


# -----------------------------------------------------------------------------
# plot_3c — Attrition Proportion by Job Level (Stacked Bar)
# Purpose : Show which seniority tiers face the greatest turnover risk
# -----------------------------------------------------------------------------
plot_3c <- df_obj3 %>%
  count(job_level, attrition) %>%
  group_by(job_level) %>%
  mutate(
    pct   = n / sum(n),
    label = paste0(round(pct * 100, 1), "%")
  ) %>%
  ggplot(aes(x = factor(job_level), y = pct, fill = attrition)) +
  geom_col(position = "fill", alpha = 0.85, width = 0.6) +
  geom_text(aes(label = label),
            position = position_fill(vjust = 0.5),
            size     = 3.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = career_colors) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Attrition Proportion by Job Level",
    subtitle = "Level 1 = Entry-level  |  Level 5 = Executive",
    x        = "Job Level",
    y        = "Proportion (%)",
    fill     = "Attrition"
  ) +
  theme_career

print(plot_3c)

# Conclusion:
# Attrition is highest at Job Level 1 (entry-level) and declines steadily as
# level increases. Career progression itself acts as an organic retention
# mechanism — once employees move beyond entry level, turnover risk drops
# significantly.


# -----------------------------------------------------------------------------
# plot_3d — Role Stagnation Index by Job Level (Grouped Bar)
# Purpose : Detect whether leavers spent a larger share of their tenure
#           stuck in the same role (years in role / years at company)
# -----------------------------------------------------------------------------
plot_3d <- df_obj3 %>%
  mutate(
    role_tenure_ratio = ifelse(
      years_at_company == 0, 0,
      years_in_current_role / years_at_company
    )
  ) %>%
  group_by(job_level, attrition) %>%
  summarise(
    avg_ratio = round(mean(role_tenure_ratio, na.rm = TRUE), 3),
    n         = n(),
    .groups   = "drop"
  ) %>%
  ggplot(aes(x = factor(job_level), y = avg_ratio, fill = attrition)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.65) +
  geom_text(aes(label = round(avg_ratio, 2)),
            position = position_dodge(width = 0.65),
            vjust = -0.4, size = 3, fontface = "bold") +
  scale_fill_manual(values = career_colors) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Role Stagnation Index by Job Level and Attrition",
    subtitle = "Index = Years in Current Role / Years at Company\nHigher value = more time spent in same role relative to tenure",
    x        = "Job Level",
    y        = "Role Stagnation Index (%)",
    fill     = "Attrition"
  ) +
  theme_career

print(plot_3d)

# Conclusion:
# At Job Levels 1 and 2, leavers show a higher stagnation index than stayers —
# they spent a disproportionate share of their tenure in the same role.
# Role immobility, not just tenure length, is what drives early attrition.
# The gap narrows at higher levels, confirming career velocity matters most
# for junior and mid-career employees.


# --- Render all 4 plots together in a 2x2 grid ---
grid.arrange(plot_3a, plot_3b, plot_3c, plot_3d,
             ncol = 2,
             top  = "OBJECTIVE 3: Career Growth / Stagnation & Attrition Analysis")

message("[OK] Analysis 3-2 Visualisations complete.")


# =============================================================================
# Analysis 3-3  STATISTICAL TESTS
# Purpose : Confirm that the visual patterns above are statistically
#           significant and not due to random sampling variation
# =============================================================================

cat("\n--- Statistical Tests: Career Growth vs Attrition ---\n")

# --- Test 1: Wilcoxon Rank-Sum — Years Since Last Promotion ---
# Why Wilcoxon (not t-test): distribution is right-skewed (many 0s, long tail)
# so a non-parametric rank-based test is more reliable here
wilcox_promo <- wilcox.test(years_since_last_promotion ~ attrition,
                            data = df_obj3, exact = FALSE)

promo_medians <- df_obj3 %>%
  group_by(attrition) %>%
  summarise(med = median(years_since_last_promotion, na.rm = TRUE),
            .groups = "drop")

cat("\n1. Wilcoxon Rank-Sum: Years Since Last Promotion vs Attrition\n")
cat("   Median (Stayed) :", promo_medians$med[promo_medians$attrition == "No"],  "years\n")
cat("   Median (Left)   :", promo_medians$med[promo_medians$attrition == "Yes"], "years\n")
cat("   W-statistic     :", round(wilcox_promo$statistic, 4), "\n")
cat("   P-value         :", round(wilcox_promo$p.value,   6), "\n")
cat("   Result          :", ifelse(wilcox_promo$p.value < 0.05,
                                   "SIGNIFICANT — promotion gap differs between groups",
                                   "NOT significant"), "\n")

# --- Test 2: Wilcoxon Rank-Sum — Training Times Last Year ---
# Why Wilcoxon: discrete integer counts (0-6), not normally distributed
wilcox_train <- wilcox.test(training_times_last_year ~ attrition,
                            data = df_obj3, exact = FALSE)

train_medians <- df_obj3 %>%
  group_by(attrition) %>%
  summarise(med = median(training_times_last_year, na.rm = TRUE),
            .groups = "drop")

cat("\n2. Wilcoxon Rank-Sum: Training Times Last Year vs Attrition\n")
cat("   Median (Stayed) :", train_medians$med[train_medians$attrition == "No"],  "sessions\n")
cat("   Median (Left)   :", train_medians$med[train_medians$attrition == "Yes"], "sessions\n")
cat("   W-statistic     :", round(wilcox_train$statistic, 4), "\n")
cat("   P-value         :", round(wilcox_train$p.value,   6), "\n")
cat("   Result          :", ifelse(wilcox_train$p.value < 0.05,
                                   "SIGNIFICANT — training frequency differs between groups",
                                   "NOT significant"), "\n")

# --- Test 3: Chi-Square — Job Level vs Attrition ---
# Why Chi-Square: job_level is categorical; attrition is binary
# tests whether the distribution of attrition differs across job levels
chisq_joblevel <- chisq.test(table(df_obj3$job_level, df_obj3$attrition))

cat("\n3. Chi-Square: Job Level vs Attrition\n")
cat("   Chi-square statistic :", round(chisq_joblevel$statistic, 4), "\n")
cat("   Degrees of freedom   :", chisq_joblevel$parameter, "\n")
cat("   P-value              :", round(chisq_joblevel$p.value, 6), "\n")
cat("   Result               :", ifelse(chisq_joblevel$p.value < 0.05,
                                        "SIGNIFICANT — job level is associated with attrition",
                                        "NOT significant"), "\n")

# --- Test 4: Wilcoxon Rank-Sum — Years in Current Role ---
# Secondary check: directly supports the Role Stagnation Index in plot_3d
wilcox_role <- wilcox.test(years_in_current_role ~ attrition,
                           data = df_obj3, exact = FALSE)

role_medians <- df_obj3 %>%
  group_by(attrition) %>%
  summarise(med = median(years_in_current_role, na.rm = TRUE),
            .groups = "drop")

cat("\n4. Wilcoxon Rank-Sum: Years in Current Role vs Attrition\n")
cat("   Median (Stayed) :", role_medians$med[role_medians$attrition == "No"],  "years\n")
cat("   Median (Left)   :", role_medians$med[role_medians$attrition == "Yes"], "years\n")
cat("   W-statistic     :", round(wilcox_role$statistic, 4), "\n")
cat("   P-value         :", round(wilcox_role$p.value,   6), "\n")
cat("   Result          :", ifelse(wilcox_role$p.value < 0.05,
                                   "SIGNIFICANT — role tenure differs between groups",
                                   "NOT significant"), "\n")

# --- Consolidated summary table ---
career_stats <- tibble(
  test      = c("Wilcoxon", "Wilcoxon", "Chi-Square", "Wilcoxon"),
  variable  = c("Years Since Last Promotion", "Training Times Last Year",
                "Job Level", "Years in Current Role"),
  statistic = c(round(wilcox_promo$statistic,   4),
                round(wilcox_train$statistic,   4),
                round(chisq_joblevel$statistic, 4),
                round(wilcox_role$statistic,    4)),
  p_value   = c(round(wilcox_promo$p.value,   6),
                round(wilcox_train$p.value,   6),
                round(chisq_joblevel$p.value, 6),
                round(wilcox_role$p.value,    6)),
  significant = ifelse(
    c(wilcox_promo$p.value, wilcox_train$p.value,
      chisq_joblevel$p.value, wilcox_role$p.value) < 0.05,
    "YES ***", "NO"
  ),
  conclusion = c(
    ifelse(wilcox_promo$p.value < 0.05,
           "Promotion drought significantly higher for leavers",
           "No significant difference in promotion timing"),
    ifelse(wilcox_train$p.value < 0.05,
           "Training frequency significantly differs by attrition",
           "No significant difference in training frequency"),
    ifelse(chisq_joblevel$p.value < 0.05,
           "Job level significantly associated with attrition",
           "No significant association between job level and attrition"),
    ifelse(wilcox_role$p.value < 0.05,
           "Years in current role significantly differs by attrition",
           "No significant difference in role tenure")
  )
)

cat("\n--- Career Growth Statistical Results Summary ---\n")
print(career_stats)

message("[OK] Analysis 3-3 Statistical Tests complete.")


# =============================================================================
# Analysis 3-4  WHAT IF ANALYSIS — Career Growth Intervention Scenarios
# Purpose : Simulate how targeted career growth changes shift the predicted
#           probability of an employee leaving the organisation
# Method  : Logistic regression trained on career growth variables only
#           Baseline = median employee profile
#           Each scenario changes ONE variable at a time (all else held fixed)
# =============================================================================

cat("\n=== WHAT IF: Career Growth Intervention Scenarios ===\n")

set.seed(RANDOM_SEED)  # Section 7 config — ensures reproducible results

# --- Step 1: Prepare modelling data ---
career_model_data <- df_obj3 %>%
  mutate(
    job_level_num    = as.numeric(as.character(job_level)),
    attrition_binary = ifelse(attrition == "Yes", 1, 0)
  )

# --- Step 2: Train logistic regression on career growth variables only ---
career_logit <- glm(
  attrition_binary ~
    years_since_last_promotion +
    training_times_last_year   +
    job_level_num              +
    years_in_current_role      +
    years_at_company,
  data   = career_model_data,
  family = binomial(link = "logit")
)

cat("\nLogistic Regression Summary (Career Growth Model):\n")
print(summary(career_logit))

# --- Step 3: Define baseline employee using median values ---
baseline <- data.frame(
  years_since_last_promotion = median(career_model_data$years_since_last_promotion, na.rm = TRUE),
  training_times_last_year   = median(career_model_data$training_times_last_year,   na.rm = TRUE),
  job_level_num              = median(career_model_data$job_level_num,              na.rm = TRUE),
  years_in_current_role      = median(career_model_data$years_in_current_role,      na.rm = TRUE),
  years_at_company           = median(career_model_data$years_at_company,           na.rm = TRUE)
)

baseline_prob <- predict(career_logit, newdata = baseline, type = "response")

cat("\n--- Baseline Employee Profile (Median Values) ---\n")
cat("  Years Since Last Promotion :", baseline$years_since_last_promotion, "\n")
cat("  Training Times Last Year   :", baseline$training_times_last_year,   "\n")
cat("  Job Level                  :", baseline$job_level_num,              "\n")
cat("  Years in Current Role      :", baseline$years_in_current_role,      "\n")
cat("  Years at Company           :", baseline$years_at_company,           "\n")
cat("  Baseline Attrition Risk    :", round(baseline_prob * 100, 2), "%\n")

# --- Step 4: Define three intervention scenarios ---

# Scenario 1: Employee received a promotion recently (0 yrs since last promo)
scenario1 <- baseline
scenario1$years_since_last_promotion <- 0
prob1 <- predict(career_logit, newdata = scenario1, type = "response")

# Scenario 2: Company doubled training sessions (capped at max of 6)
scenario2 <- baseline
scenario2$training_times_last_year <- min(baseline$training_times_last_year * 2, 6)
prob2 <- predict(career_logit, newdata = scenario2, type = "response")

# Scenario 3: Employee was promoted to the next job level (capped at 5)
scenario3 <- baseline
scenario3$job_level_num <- min(baseline$job_level_num + 1, 5)
prob3 <- predict(career_logit, newdata = scenario3, type = "response")

# --- Step 5: Print results table ---
cat("\n--- What If Scenario Results ---\n")
cat(sprintf("  %-50s %s\n", "Scenario", "Predicted Attrition Risk"))
cat(strrep("-", 75), "\n")
cat(sprintf("  %-50s %.2f%%\n",
            "Baseline (no change)", baseline_prob * 100))
cat(sprintf("  %-50s %.2f%%  (change: %+.2f%%)\n",
            "Scenario 1: Promoted recently (0 yrs since promo)",
            prob1 * 100, (prob1 - baseline_prob) * 100))
cat(sprintf("  %-50s %.2f%%  (change: %+.2f%%)\n",
            "Scenario 2: Double training sessions",
            prob2 * 100, (prob2 - baseline_prob) * 100))
cat(sprintf("  %-50s %.2f%%  (change: %+.2f%%)\n",
            "Scenario 3: Promoted to next job level",
            prob3 * 100, (prob3 - baseline_prob) * 100))

# --- Step 6: Visualise scenarios as bar chart ---
whatif_df <- data.frame(
  scenario = factor(
    c("Baseline\n(No Change)",
      "Scenario 1\nRecent Promotion\n(0 yrs since promo)",
      "Scenario 2\nDouble Training\nSessions",
      "Scenario 3\nPromoted to\nNext Job Level"),
    levels = c(
      "Baseline\n(No Change)",
      "Scenario 1\nRecent Promotion\n(0 yrs since promo)",
      "Scenario 2\nDouble Training\nSessions",
      "Scenario 3\nPromoted to\nNext Job Level"
    )
  ),
  probability = c(baseline_prob, prob1, prob2, prob3) * 100,
  type        = c("Baseline", "Intervention", "Intervention", "Intervention")
)

plot_3e <- ggplot(whatif_df, aes(x = scenario, y = probability, fill = type)) +
  geom_col(width = 0.55, alpha = 0.88) +
  geom_text(aes(label = paste0(round(probability, 1), "%")),
            vjust = -0.5, size = 4, fontface = "bold") +
  geom_hline(yintercept = baseline_prob * 100,
             linetype = "dashed", color = COLOR_YES, linewidth = 0.8) +
  annotate("text",
           x     = 3.7,
           y     = baseline_prob * 100 + 0.8,
           label = paste0("Baseline: ", round(baseline_prob * 100, 1), "%"),
           color = COLOR_YES, size = 3.5, fontface = "italic") +
  scale_fill_manual(values = c("Baseline"     = "#9E9E9E",
                               "Intervention" = COLOR_GREEN)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title    = "What If: Predicted Attrition Risk Under Career Growth Interventions",
    subtitle = "Green bars show predicted risk after each career growth improvement",
    x        = NULL,
    y        = "Predicted Attrition Probability (%)",
    fill     = NULL,
    caption  = "Based on logistic regression | All other variables held at median values"
  ) +
  theme_career +
  theme(legend.position = "bottom")

print(plot_3e)

# Conclusions:
# Scenario 1 (Recent Promotion)  : Largest single drop in risk. Timely career
#   recognition is the most powerful retention lever HR can act on.
# Scenario 2 (Double Training)   : Moderate risk reduction. Signals organisational
#   investment — an important psychological factor beyond skill-building alone.
# Scenario 3 (Next Job Level)    : Meaningful risk reduction from structural
#   advancement — title and responsibility gains compound the benefit beyond pay.
# Overall: All three interventions lower attrition risk, confirming that career
#   stagnation is a controllable, policy-addressable driver of employee turnover.

message("[OK] Analysis 3-4 What If Analysis complete.")
message("\n[OK] Section 7 — Objective 3 (Career Growth) complete.")


# =============================================================================
# SECTION 7 - OBJECTIVE 4: To evaluate how cultural and overall satisfaction impacts attrition
# Name : Lee Hong Yi (TP076604)
# Variables: environment_satisfaction, job_satisfaction,
#            relationship_satisfaction, work_life_balance, job_involvement
# =============================================================================
# GRAPHS:
#   Analysis 4-1  Grouped Bar  — Attrition rate by satisfaction level (all 5 variables)
#   Analysis 4-2  Stacked Bar  — Work-life balance: stayed vs left count
#   Analysis 4-3  Line Chart   — Composite culture score vs attrition rate
#   Analysis 4-4  Heatmap      — Attrition rate across all 5 culture variables
# =============================================================================

# =============================================================================
# CONFIGURATION
# =============================================================================

OUTPUT_PARQUET <- "employee_attrition_cleaned.parquet"

# --- Verify Parquet File Exists Before Reading ---
if (!file.exists(OUTPUT_PARQUET)) {
  stop(
    "\n[ERROR] Parquet file not found: '", OUTPUT_PARQUET, "'\n",
    "Make sure Section 6 ran successfully and write_parquet() completed.\n"
  )
}

cat("=== PARQUET DATA RETRIEVAL ===\n")
cat("Source file  :", OUTPUT_PARQUET, "\n")

# Objective 4 - Culture
df_clean <- read_parquet(
  OUTPUT_PARQUET,
  col_select = c(
    "attrition", "environment_satisfaction",
    "job_satisfaction", "work_life_balance",
    "relationship_satisfaction", "job_involvement"
  )
)
cat("Obj 4 (Culture)       :", ncol(df_clean), "cols loaded\n")


# Ordered level sets
LEVELS_4PT <- c("Low", "Medium", "High", "Very High")
LEVELS_WLB <- c("Bad", "Good", "Better", "Best")

# Colour palette
COL_STAYED <- "#3266AD" # blue  — stayed
COL_LEFT <- "#C0503A" # coral — left
COL_NEUTRAL <- "#7F77DD" # purple — composite chart line

# Variable display labels (for facet headings)
VAR_LABELS <- c(
  environment_satisfaction  = "Environment Satisfaction",
  job_satisfaction          = "Job Satisfaction",
  relationship_satisfaction = "Relationship Satisfaction",
  job_involvement           = "Job Involvement"
)

# Output folder (creates it if it doesn't exist)
OUTPUT_DIR <- "section7_plots"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# Shared ggplot2 theme — consistent look across all 4 charts
theme_obj4 <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, hjust = 0),
      plot.subtitle    = element_text(size = 10, color = "grey45", hjust = 0),
      plot.caption     = element_text(size = 9, color = "grey55", hjust = 1),
      axis.title       = element_text(size = 10),
      axis.text        = element_text(size = 9),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 9),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 10)
    )
}


# =============================================================================
# 4-1  GROUPED BAR — Attrition rate by satisfaction level (5 variables)
# Shows: which satisfaction level has the highest attrition rate per variable
# Excludes work_life_balance (covered separately in 4-2)
# =============================================================================

message("\n[7.1] Building grouped bar chart...")

# Build a long table of attrition rates for the 4 ordinal culture vars
plot71_data <- df_clean %>%
  select(
    attrition,
    environment_satisfaction,
    job_satisfaction,
    relationship_satisfaction,
    job_involvement
  ) %>%
  pivot_longer(
    cols      = -attrition,
    names_to  = "variable",
    values_to = "level"
  ) %>%
  mutate(
    level    = factor(level, levels = LEVELS_4PT),
    variable = recode(variable, !!!VAR_LABELS)
  ) %>%
  group_by(variable, level) %>%
  summarise(
    total = n(),
    left = sum(attrition == "Yes"),
    attrition_rate = left / total * 100,
    .groups = "drop"
  )

cat("\n=== 7.1 Summary: Attrition Rate by Satisfaction Level ===\n")
print(plot71_data, n = Inf)
cat("\n")

plot71 <- ggplot(
  plot71_data,
  aes(x = level, y = attrition_rate, fill = variable)
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65, colour = "white", linewidth = 0.3
  ) +
  geom_text(aes(label = sprintf("%.1f%%", attrition_rate)),
    position = position_dodge(width = 0.75),
    vjust = -0.5, size = 2.9, fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Environment Satisfaction"  = COL_LEFT,
    "Job Satisfaction"          = COL_STAYED,
    "Relationship Satisfaction" = "#1D7A5F",
    "Job Involvement"           = "#E5A336"
  )) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 45)) +
  labs(
    title    = "Attrition rate by satisfaction level",
    subtitle = "Low satisfaction consistently produces the highest attrition across all four dimensions",
    x        = "Satisfaction level",
    y        = "Attrition rate (%)",
    caption  = "Source: employee_attrition_cleaned.csv | Objective 4 — Culture Dissatisfaction"
  ) +
  theme_obj4()

print(plot71)
ggsave(file.path(OUTPUT_DIR, "7.1_grouped_bar_satisfaction.png"),
  plot71,
  width = 9, height = 5.5, dpi = 150
)
message("[OK] 7.1 saved.")


# =============================================================================
# 4-2  STACKED BAR — Work-life balance: stayed vs left
# Shows: volume of employees at each WLB level and proportion who left
# WLB uses a different 4-level scale (Bad/Good/Better/Best) — treated separately
# =============================================================================

message("\n[7.2] Building stacked bar chart...")

plot72_data <- df_clean %>%
  mutate(work_life_balance = factor(work_life_balance, levels = LEVELS_WLB)) %>%
  count(work_life_balance, attrition) %>%
  group_by(work_life_balance) %>%
  mutate(
    total          = sum(n),
    attrition_pct  = n / total * 100
  ) %>%
  ungroup()

cat("\n=== 7.2 Summary: Work-Life Balance Attrition Breakdown ===\n")
print(plot72_data, n = Inf)
cat("\n")

# Rate label — shown only on the "Yes" bar segment for readability
rate_labels <- plot72_data %>%
  filter(attrition == "Yes") %>%
  mutate(label = sprintf("%.1f%%\nleft", attrition_pct))

plot72 <- ggplot(
  plot72_data,
  aes(x = work_life_balance, y = n, fill = attrition)
) +
  geom_col(
    position = "stack", width = 0.6,
    colour = "white", linewidth = 0.4
  ) +

  # Attrition rate label on the "Left" segment
  geom_text(
    data = rate_labels,
    aes(x = work_life_balance, y = n / 2, label = label),
    colour = "white", size = 3, fontface = "bold",
    vjust = 0, inherit.aes = FALSE
  ) +

  # Total count on top of each bar
  geom_text(
    data = plot72_data %>% group_by(work_life_balance) %>%
      summarise(total = sum(n), .groups = "drop"),
    aes(
      x = work_life_balance, y = total + 20,
      label = paste0("n=", total)
    ),
    inherit.aes = FALSE,
    size = 2.9, colour = "grey40"
  ) +
  scale_fill_manual(
    values = c("No" = COL_STAYED, "Yes" = COL_LEFT),
    labels = c("No" = "Stayed", "Yes" = "Left")
  ) +
  scale_y_continuous(
    labels = label_comma(),
    breaks = seq(0, 1250, 250),
    expand = expansion(add = c(50, 0), mult = c(0, 0.08))
  ) +
  labs(
    title    = "Work-life balance: attrition count breakdown",
    subtitle = "\"Bad\" balance (29.2%) has the highest attrition rate — nearly double the \"Better\" group",
    x        = "Work-life balance rating",
    y        = "Number of employees",
    caption  = "Source: employee_attrition_cleaned.csv | Objective 4 — Culture Dissatisfaction"
  ) +
  theme_obj4()

print(plot72)
ggsave(file.path(OUTPUT_DIR, "7.2_stacked_bar_wlb.png"),
  plot72,
  width = 7, height = 5.5, dpi = 150
)
message("[OK] 7.2 saved.")


# =============================================================================
# 4-3  LINE CHART — Composite culture score vs attrition rate
# Composite = sum of all 5 culture scores (each mapped 1–4)
#   5  = lowest possible culture experience (all Low/Bad)
#   20 = highest possible culture experience (all Very High/Best)
# Point size encodes group size (n)
# =============================================================================

message("\n[7.3] Building composite score line chart...")

score_map <- c(
  "Low" = 1, "Medium" = 2, "High" = 3, "Very High" = 4,
  "Bad" = 1, "Good" = 2, "Better" = 3, "Best" = 4
)

plot73_data <- df_clean %>%
  mutate(
    env_score  = score_map[as.character(environment_satisfaction)],
    job_score  = score_map[as.character(job_satisfaction)],
    rel_score  = score_map[as.character(relationship_satisfaction)],
    wlb_score  = score_map[as.character(work_life_balance)],
    inv_score  = score_map[as.character(job_involvement)],
    composite  = env_score + job_score + rel_score + wlb_score + inv_score
  ) %>%
  group_by(composite) %>%
  summarise(
    total = n(),
    left = sum(attrition == "Yes"),
    attrition_rate = left / total * 100,
    .groups = "drop"
  ) %>%
  # Remove very sparse buckets (n < 5) — unreliable rates
  filter(total >= 5)

cat("\n=== 7.3 Summary: Composite Culture Score vs Attrition Rate ===\n")
print(plot73_data, n = Inf)
cat("\n")

plot73 <- ggplot(
  plot73_data,
  aes(x = composite, y = attrition_rate)
) +

  # Shaded ribbon for visual weight
  geom_area(fill = COL_NEUTRAL, alpha = 0.12) +
  geom_line(colour = COL_NEUTRAL, linewidth = 1.1) +

  # Bubble size = group size, colour = group size
  geom_point(aes(size = total, fill = total),
    colour = COL_NEUTRAL, shape = 21, stroke = 1.8
  ) +
  geom_text(aes(label = sprintf("%.1f%%", attrition_rate)),
    vjust = -1.1, size = 2.8, colour = "grey30"
  ) +
  scale_x_continuous(
    breaks = 5:20,
    labels = c("5\n(all low)", as.character(6:19), "20\n(all high)")
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 70),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_size_continuous(
    name   = "Group size (n)",
    range  = c(3, 10),
    breaks = c(50, 200, 370)
  ) +
  scale_fill_gradientn(
    name = "Group size (n)",
    colours = c(COL_LEFT, "#FAC775", COL_STAYED),
    breaks = c(50, 200, 370)
  ) +
  guides(size = guide_legend(), fill = guide_legend()) +

  # Annotation: high-risk zone
  annotate("rect",
    xmin = 4.5, xmax = 10.5,
    ymin = 0, ymax = 70,
    fill = COL_LEFT, alpha = 0.06
  ) +
  annotate("text",
    x = 7.5, y = 65,
    label = "High-risk zone\n(score 5–10)",
    size = 3, colour = COL_LEFT, fontface = "italic"
  ) +
  labs(
    title    = "Composite culture score vs attrition rate",
    subtitle = "Employees with all-low scores (5) leave at the highest rate; all-high (20) at ~0%",
    x        = "Composite culture score  (5 = all dissatisfied · 20 = all satisfied)",
    y        = "Attrition rate (%)",
    caption  = "Source: employee_attrition_cleaned.csv | Objective 4 — Culture Dissatisfaction"
  ) +
  theme_obj4() +
  theme(legend.position = "right")

print(plot73)
ggsave(file.path(OUTPUT_DIR, "7.3_line_composite_score.png"),
  plot73,
  width = 9, height = 5.5, dpi = 150
)
message("[OK] 7.3 saved.")


# =============================================================================
# 4-4  HEATMAP — Attrition rate across all 5 culture variables × all levels
# Rows = variables, Columns = satisfaction levels
# Cell colour = attrition rate (red = high, light yellow = low)
# =============================================================================

message("\n[7.4] Building heatmap...")

# Build a unified long table using a shared 4-level ordinal for display
plot74_data <- bind_rows(
  df_clean %>%
    mutate(level = factor(environment_satisfaction, levels = LEVELS_4PT)) %>%
    group_by(level) %>%
    summarise(total = n(), left = sum(attrition == "Yes"), .groups = "drop") %>%
    mutate(variable = "Environment\nSatisfaction"),
  df_clean %>%
    mutate(level = factor(job_satisfaction, levels = LEVELS_4PT)) %>%
    group_by(level) %>%
    summarise(total = n(), left = sum(attrition == "Yes"), .groups = "drop") %>%
    mutate(variable = "Job\nSatisfaction"),
  df_clean %>%
    mutate(level = factor(relationship_satisfaction, levels = LEVELS_4PT)) %>%
    group_by(level) %>%
    summarise(total = n(), left = sum(attrition == "Yes"), .groups = "drop") %>%
    mutate(variable = "Relationship\nSatisfaction"),
  df_clean %>%
    mutate(level = factor(job_involvement, levels = LEVELS_4PT)) %>%
    group_by(level) %>%
    summarise(total = n(), left = sum(attrition == "Yes"), .groups = "drop") %>%
    mutate(variable = "Job\nInvolvement"),
  df_clean %>%
    mutate(level = factor(work_life_balance,
      levels = LEVELS_WLB,
      labels = LEVELS_4PT
    )) %>% # remap labels to shared axis
    group_by(level) %>%
    summarise(total = n(), left = sum(attrition == "Yes"), .groups = "drop") %>%
    mutate(variable = "Work-Life\nBalance")
) %>%
  mutate(
    attrition_rate = left / total * 100,
    variable = factor(variable, levels = c(
      "Environment\nSatisfaction",
      "Job\nSatisfaction",
      "Relationship\nSatisfaction",
      "Job\nInvolvement",
      "Work-Life\nBalance"
    )),
    level = factor(level, levels = LEVELS_4PT)
  )

cat("\n=== 7.4 Summary: Attrition Rate Heatmap Data ===\n")
print(plot74_data, n = Inf)
cat("\n")

plot74 <- ggplot(
  plot74_data,
  aes(x = level, y = variable, fill = attrition_rate)
) +
  geom_tile(colour = "white", linewidth = 1.2) +

  # Rate label
  geom_text(
    aes(
      label  = sprintf("%.1f%%", attrition_rate),
      colour = attrition_rate > 20 # white text on dark tiles
    ),
    size = 4, fontface = "bold"
  ) +

  # n label (below rate)
  geom_text(aes(label = paste0("n=", total)),
    vjust = 2.2, size = 2.7, colour = "grey30"
  ) +
  scale_fill_gradient2(
    low      = "#9FE1CB", # teal — low attrition (good)
    mid      = "#FAC775", # amber — mid
    high     = "#C0503A", # coral — high attrition (bad)
    midpoint = 17,
    name     = "Attrition rate (%)",
    labels   = label_percent(scale = 1)
  ) +
  scale_colour_manual(
    values = c("FALSE" = "grey20", "TRUE" = "white"),
    guide = "none"
  ) +
  scale_x_discrete(
    labels = c(
      "Low"       = "Low\n(Bad)",
      "Medium"    = "Medium\n(Good)",
      "High"      = "High\n(Better)",
      "Very High" = "Very High\n(Best)"
    )
  ) +
  labs(
    title    = "Attrition rate heatmap — culture dissatisfaction variables",
    subtitle = "Darker red = higher attrition risk. Work-life balance (Bad) is the most critical cell at 29.2%",
    x        = "Satisfaction level",
    y        = NULL,
    caption  = "Source: employee_attrition_cleaned.csv | Objective 4 — Culture Dissatisfaction"
  ) +
  theme_obj4() +
  theme(
    legend.position  = "right",
    panel.grid       = element_blank(),
    axis.text.y      = element_text(size = 10, lineheight = 1.1)
  )

print(plot74)
ggsave(file.path(OUTPUT_DIR, "7.4_heatmap_culture.png"),
  plot74,
  width = 9, height = 5.5, dpi = 150
)
message("[OK] 7.4 saved.")


# =============================================================================
# SUMMARY PRINT — key findings for report write-up
# =============================================================================

message("\n========================================================")
message("  SECTION 7 KEY FINDINGS — Culture Dissatisfaction")
message("========================================================")

findings <- df_clean %>%
  mutate(
    across(c(
      environment_satisfaction, job_satisfaction,
      relationship_satisfaction, job_involvement
    ), ~ factor(., levels = LEVELS_4PT)),
    work_life_balance = factor(work_life_balance, levels = LEVELS_WLB)
  ) %>%
  summarise(
    env_low_rate  = mean(attrition[environment_satisfaction == "Low"] == "Yes") * 100,
    job_low_rate  = mean(attrition[job_satisfaction == "Low"] == "Yes") * 100,
    rel_low_rate  = mean(attrition[relationship_satisfaction == "Low"] == "Yes") * 100,
    inv_low_rate  = mean(attrition[job_involvement == "Low"] == "Yes") * 100,
    wlb_bad_rate  = mean(attrition[work_life_balance == "Bad"] == "Yes") * 100,
    env_high_rate = mean(attrition[environment_satisfaction == "Very High"] == "Yes") * 100,
    job_high_rate = mean(attrition[job_satisfaction == "Very High"] == "Yes") * 100
  )

cat(sprintf(
  "  Env. satisfaction  — Low: %.1f%%  vs Very High: %.1f%%\n",
  findings$env_low_rate, findings$env_high_rate
))
cat(sprintf(
  "  Job satisfaction   — Low: %.1f%%  vs Very High: %.1f%%\n",
  findings$job_low_rate, findings$job_high_rate
))
cat(sprintf("  Rel. satisfaction  — Low: %.1f%%\n", findings$rel_low_rate))
cat(sprintf("  Job involvement    — Low: %.1f%%\n", findings$inv_low_rate))
cat(sprintf(
  "  Work-life balance  — Bad: %.1f%%  (highest single risk factor)\n",
  findings$wlb_bad_rate
))
cat(sprintf("  Plots saved to   : %s/\n", OUTPUT_DIR))
message("========================================================\n")

message("\n[OK] Section 7 — Objective 4 (Culture Dissatisfaction) complete.")


# Extra feature 1
# Pacman
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tidyverse, tidymodels, scales, gridExtra, janitor, arrow, caret, corrplot)

# Instead of the standard manual install and load approach, pacman does the check,
# install and load workflow in one single line. This is more efficient and
# professional than writing separate install.library() and packages() calls.

# =============================================================================

# Extra feature 2
# Parquet
# OUTPUT_PARQUET <- "employee_attrition_cleaned.parquet"

# df_clean <- read_parquet(
#   OUTPUT_PARQUET,
#   col_select = c("attrition", "over_time", "business_travel",
#                  "distance_from_home", "marital_status")
# )
# cat("Obj 2 (Burnout) loaded:", nrow(df_clean), "rows x", ncol(df_clean), "cols\n")


# In addition to the standard CSV export, after all cleaning and validation steps,
# the cleaned data set was serialized into Apache Parquet format using write_parquet() from the arrow package.
# Parquet is a binary columnar storage file format, that organizes data by column rather than by row

# =============================================================================

# Extra feature 3
# =============================================================================
# LOGISTIC REGRESSION MODEL FOR ATTRITION
# =============================================================================

message("\n--- Logistic Regression ---")

# A. Read ALL columns from parquet for full-variable model
df_logit <- read_parquet(OUTPUT_PARQUET) %>%
  mutate(attr_bin = ifelse(attrition == "Yes", 1, 0)) %>%
  select(-attrition) %>%
  
  # Structural Hierarchy Anchors
  mutate(job_level = relevel(factor(job_level), ref = "5")) %>%
  mutate(stock_option_level = relevel(factor(stock_option_level), ref = "3")) %>%
  
  # Workplace & Employee Survey Anchors (Flipping to capture the downside risk)
  mutate(environment_satisfaction  = relevel(factor(environment_satisfaction), ref = "Very High")) %>%
  mutate(job_satisfaction          = relevel(factor(job_satisfaction), ref = "Very High")) %>%
  mutate(job_involvement           = relevel(factor(job_involvement), ref = "Very High")) %>%
  mutate(relationship_satisfaction = relevel(factor(relationship_satisfaction), ref = "Very High")) %>%
  mutate(work_life_balance         = relevel(factor(work_life_balance), ref = "Best"))

# B. Fit Model - glm() + binomial() = logistic regression
model_burnout <- glm(
  attr_bin ~ .,
  data   = df_logit,
  family = binomial()
)

cat("Model Summary:\n")
print(summary(model_burnout))

# C. Odds Ratios with 95% Confidence Intervals
odds_df <- data.frame(
  term      = names(coef(model_burnout)),
  odds      = exp(coef(model_burnout)),
  ci_low    = exp(confint.default(model_burnout)[, 1]),
  ci_high   = exp(confint.default(model_burnout)[, 2])
)
odds_df <- odds_df[odds_df$term != "(Intercept)", ]   # drop intercept
rownames(odds_df) <- NULL

cat("\nOdds Ratios (95% CI):\n")
print(odds_df)

# D. Interpretation Guide
cat("\n--- How to read Odds Ratios ---\n")
cat("  OR > 1  → increases attrition risk   (e.g. 3.36 = 3.36× more likely)\n")
cat("  OR < 1  → decreases attrition risk   (e.g. 0.60 = 40% less likely)\n")
cat("  CI crossing 1.0 → NOT statistically significant\n")

# E. Forest Plot — Visual Summary of Logistic Regression
p_logit <- odds_df %>%
  mutate(
    # Flag significance: CI does not cross 1.0
    significant = ifelse(ci_low > 1 | ci_high < 1, "Significant", "Not Significant")
  ) %>%
  ggplot(aes(x = odds, y = reorder(term, odds), colour = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3.5) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, linewidth = 0.8) +
  scale_colour_manual(values = c("Significant" = COLOR_YES,
                                 "Not Significant" = "grey60")) +
  labs(
    title    = "Burnout Model: What Predicts Attrition?",
    subtitle = "Logistic Regression Odds Ratios with 95% CI  |  Dashed line = no effect (OR = 1)",
    x = "Odds Ratio", y = NULL, colour = "Significance"
  ) +
  OBJ2_THEME

print(p_logit)