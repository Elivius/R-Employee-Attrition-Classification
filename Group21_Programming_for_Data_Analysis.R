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


#=============================================================================
# SECTION 7: ANALYTICS DATA RETRIEVAL (PARQUET PIPELINE)
# Purpose:
# Reload optimized parquet datasets for downstream analysis.
# Each objective loads only required columns to demonstrate
# Parquet columnar storage efficiency.
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, scales, gridExtra, janitor, arrow, caret, corrplot)

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

# --- Full dataset read (for anything needing all columns) ---
df_analysis <- read_parquet(OUTPUT_PARQUET)
cat("Full dataset :", nrow(df_analysis), "rows x",
    ncol(df_analysis), "cols\n\n")
# --- Objective-specific columnar reads ---
# Each objective only loads the columns it needs
# This is the core benefit of Parquet over CSV

# Objective 1 — Compensation
df_obj1 <- read_parquet(
  OUTPUT_PARQUET,
  col_select = c("attrition", "monthly_income",
                 "percent_salary_hike", "stock_option_level",
                 "job_level", "age")
)
cat("Obj 1 (Compensation)  :", ncol(df_obj1), "cols loaded\n")

# Objective 2 — Burnout
df_obj2 <- read_parquet(
  OUTPUT_PARQUET,
  col_select = c("attrition", "over_time",
                 "business_travel", "distance_from_home")
)
cat("Obj 2 (Burnout)       :", ncol(df_obj2), "cols loaded\n")

# Objective 3 — Career Growth
df_obj3 <- read_parquet(
  OUTPUT_PARQUET,
  col_select = c("attrition", "years_since_last_promotion",
                 "training_times_last_year", "years_at_company",
                 "job_level")
)
cat("Obj 3 (Career Growth) :", ncol(df_obj3), "cols loaded\n")

# Objective 4 — Culture
df_obj4 <- read_parquet(
  OUTPUT_PARQUET,
  col_select = c("attrition", "environment_satisfaction",
                 "job_satisfaction", "work_life_balance",
                 "relationship_satisfaction")
)
cat("Obj 4 (Culture)       :", ncol(df_obj4), "cols loaded\n")


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

df_obj2 <- df_obj2 %>%
  mutate(
    attrition       = factor(attrition,
                             levels = c("No", "Yes")),
    over_time       = factor(over_time,
                             levels = c("No", "Yes")),
    business_travel = factor(business_travel)
  )

df_obj3 <- df_obj3 %>%
  mutate(
    attrition = factor(attrition, levels = c("No", "Yes")),
    job_level = factor(job_level)
  )

df_obj4 <- df_obj4 %>%
  mutate(
    attrition                 = factor(attrition,
                                       levels = c("No", "Yes")),
    environment_satisfaction  = factor(environment_satisfaction,
                                       levels = c("Low", "Medium",
                                                  "High", "Very High")),
    job_satisfaction          = factor(job_satisfaction,
                                       levels = c("Low", "Medium",
                                                  "High", "Very High")),
    work_life_balance         = factor(work_life_balance,
                                       levels = c("Bad", "Good",
                                                  "Better", "Best")),
    relationship_satisfaction = factor(relationship_satisfaction,
                                       levels = c("Low", "Medium",
                                                  "High", "Very High"))
  )

cat("\n[OK] All objective datasets loaded and factors restored.\n")
cat("     df_obj1, df_obj2, df_obj3, df_obj4 ready for analysis.\n")
cat("\n>>> Proceed to Section 7.1 — Objective 1: Compensation\n")




# =============================================================================
# Analysis 3 / OBJECTIVE 3: CAREER GROWTH / STAGNATION ANALYSIS
# Name: [EE JIN XING, TP076848]
#
# Variables : YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion,
#             JobLevel, TrainingTimesLastYear, NumCompaniesWorked
# Hypothesis: Employees with stagnant career progression (slow promotions,
#             fewer training opportunities, low job level mobility) are
#             significantly more likely to leave the organisation.
# =============================================================================

# -----------------------------------------------------------------------------
# STANDALONE BLOCK
# Run this block if you are running analysis 3 ONLY
# (i.e. did NOT run Sections 1-6 and analysis 3 in this session)
# If you already ran the full script above, this block safely skips itself
# -----------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales, gridExtra, arrow, caret, broom)

OUTPUT_PARQUET <- "employee_attrition_cleaned.parquet"

# --- Config mirrors Section 4 exactly ---
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

if (!file.exists(OUTPUT_PARQUET)) {
  stop("[ERROR] Parquet file not found! Run Sections 1-6 first to generate it.")
}

# Load df_obj3 only if it does not exist yet OR is missing required columns
# This prevents overwriting the already-loaded version when running the full script
required_cols_obj3 <- c(
  "attrition", "years_since_last_promotion", "training_times_last_year",
  "years_at_company", "years_in_current_role", "num_companies_worked", "job_level"
)

if (!exists("df_obj3") || !all(required_cols_obj3 %in% names(df_obj3))) {
  message("[INFO] Loading df_obj3 from parquet...")
  df_obj3 <- read_parquet(
    OUTPUT_PARQUET,
    col_select = all_of(required_cols_obj3)
  ) %>%
    mutate(
      attrition = factor(attrition, levels = c("No", "Yes")),
      job_level = factor(job_level)
    )
  message("[OK] df_obj3 loaded — ", nrow(df_obj3), " rows x ", ncol(df_obj3), " cols")
} else {
  message("[OK] df_obj3 already loaded — skipping parquet read.")
}

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

# Colour mapping — uses Section 4 config variables (not hardcoded)
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

message("[OK] analysis 3-2 Visualisations complete.")


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

message("[OK] analysis 3-3 Statistical Tests complete.")


# =============================================================================
# Analysis 3-4  WHAT IF ANALYSIS — Career Growth Intervention Scenarios
# Purpose : Simulate how targeted career growth changes shift the predicted
#           probability of an employee leaving the organisation
# Method  : Logistic regression trained on career growth variables only
#           Baseline = median employee profile
#           Each scenario changes ONE variable at a time (all else held fixed)
# =============================================================================

cat("\n=== WHAT IF: Career Growth Intervention Scenarios ===\n")

set.seed(RANDOM_SEED)  # Section 4 config — ensures reproducible results

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


# =============================================================================
# Analysis 3-5  MODEL OVERVIEW — Full Logistic Regression (All Variables)
# Purpose : Show which predictors across the ENTIRE dataset are significant
#           in predicting attrition — places career growth in full context
# Style   : Dark background | Red = significant | Grey = not significant
#           Horizontal forest plot with 95% CI bars and OR = 1 reference line
# Note    : Reloads all columns from parquet — does not depend on df_obj3
# =============================================================================

# Reload full dataset with all columns and correct factor levels
df_full <- read_parquet(OUTPUT_PARQUET) %>%
  mutate(
    attrition                 = factor(attrition,                 levels = c("No", "Yes")),
    education                 = factor(education,                 levels = LBL_EDU),
    environment_satisfaction  = factor(environment_satisfaction,  levels = LBL_4POINT),
    job_satisfaction          = factor(job_satisfaction,          levels = LBL_4POINT),
    job_involvement           = factor(job_involvement,           levels = LBL_4POINT),
    relationship_satisfaction = factor(relationship_satisfaction, levels = LBL_4POINT),
    work_life_balance         = factor(work_life_balance,         levels = LBL_WLB),
    performance_rating        = factor(performance_rating,        levels = LBL_PERF),
    job_level                 = factor(job_level),
    stock_option_level        = factor(stock_option_level),
    gender                    = factor(gender),
    department                = factor(department),
    business_travel           = factor(business_travel),
    over_time                 = factor(over_time),
    marital_status            = factor(marital_status),
    education_field           = factor(education_field),
    job_role                  = factor(job_role)
  )

cat("Full dataset loaded for model overview:",
    nrow(df_full), "rows x", ncol(df_full), "cols\n")

# --- Step 1: Full logistic regression on ALL variables ---
full_attrition_model <- glm(
  attrition ~ .,
  data   = df_full,
  family = binomial(link = "logit")
)

# --- Step 2: Extract tidy odds ratios with 95% confidence intervals ---
or_df <- tidy(full_attrition_model,
              exponentiate = TRUE,
              conf.int     = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    significant = factor(
      ifelse(p.value < 0.05, "Significant", "Not Significant"),
      levels = c("Not Significant", "Significant")
    )
  )

# --- Step 3: Forest plot (dark background style) ---
plot_model_overview <- ggplot(
  or_df,
  aes(x = estimate, y = reorder(term, estimate), color = significant)
) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.4, linewidth = 0.55
  ) +
  geom_point(size = 2.2) +
  geom_vline(
    xintercept = 1,
    linetype   = "dashed",
    color      = "#AAAAAA",
    linewidth  = 0.7
  ) +
  scale_color_manual(
    values = c(
      "Not Significant" = "#888888",
      "Significant"     = COLOR_YES   # Section 4 config — red
    )
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = c(0, 2.5, 5.0, 7.5, 10.0)
  ) +
  labs(
    title    = "Career Growth Model: What Predicts Attrition?",
    subtitle = "Logistic Regression Odds Ratios with 95% CI  |  Dashed line = no effect (OR = 1)",
    x        = "Odds Ratio",
    y        = NULL,
    color    = "Significance"
  ) +
  theme_dark(base_size = 11) +
  theme(
    plot.background   = element_rect(fill = "#1A1A1A", color = NA),
    panel.background  = element_rect(fill = "#1A1A1A", color = NA),
    panel.grid.major  = element_line(color = "#2E2E2E", linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold", size = 14,
                                     hjust = 0.5, color = "white"),
    plot.subtitle     = element_text(size = 9.5, hjust = 0.5,
                                     color = "#CCCCCC"),
    axis.text.y       = element_text(size = 7,  color = "#CCCCCC"),
    axis.text.x       = element_text(size = 9,  color = "#CCCCCC"),
    axis.title.x      = element_text(face = "bold", size = 10, color = "white"),
    axis.ticks        = element_line(color = "#555555"),
    legend.position   = "top",
    legend.background = element_rect(fill = "#1A1A1A", color = NA),
    legend.text       = element_text(color = "white", size = 9),
    legend.title      = element_text(color = "white", size = 9, face = "bold"),
    legend.key        = element_rect(fill = "#1A1A1A", color = NA),
    plot.margin       = margin(15, 20, 10, 10)
  )

print(plot_model_overview)

ggsave(
  filename = "plot_analysis_3_5_model_overview.png",
  plot     = plot_model_overview,
  width    = 14,
  height   = 16,
  dpi      = 150,
  bg       = "#1A1A1A"
)

message("[OK] Analysis Model Overview complete — plot saved to plot_analysis_3_5_model_overview.png")
message("\n>>> OBJECTIVE 3 COMPLETE — Analysis 3-5 done.")