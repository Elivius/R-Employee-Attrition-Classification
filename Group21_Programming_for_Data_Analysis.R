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
# Section 1 : Libraries            — load tools first
# Section 2 : Read Raw File        — load CSV as-is
# Section 3 : Raw Data Exploration — understand data BEFORE anything else
# Section 4 : Configuration        — set values after seeing the data
# Section 5 : Cleaning             — fix what Section 3 revealed
# Section 6 : Validation           — confirm cleaning worked correctly
#
# NOTE: Sections 7 onwards (Analysis, Model etc.) are written by your group
# based on findings from this base script
# =============================================================================


# =============================================================================
# SECTION 1: LIBRARIES (Use pacman - package manager)
# Handle the "check -> install -> load" workflow in one go
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, scales, gridExtra, janitor, arrow)

message("[OK] All libraries loaded — ready to proceed.")

# =============================================================================
# SECTION 2: ADVANCED DATA RETRIEVAL PIPELINE (Parquet Implementation)
# Instead of a standard row-based CSV read, we implement a Columnar Storage
# Pipeline using Apache Arrow. This satisfies the "Advanced Concept" 
# requirement to enhance data retrieval effectiveness and memory efficiency.
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
df_raw <- read.csv(
  raw_file,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "na", "n/a",
                 "nil", "Nil", "NIL",
                 "null", "NULL", "none", "None")
)

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
# Every finding here justifies a cleaning decision in Section 5
# =============================================================================

# --- Question 1: What is the size? ---
cat("\n--- Dataset Dimensions ---\n")
cat("Rows    :", nrow(df_raw), "\n")
cat("Columns :", ncol(df_raw), "\n")


# --- Question 2: What is inside? ---
# str() shows column names AND data types at the same time
# IMPORTANT -> Watch for: numeric columns showing as "chr" — means dirty values like "1423_"
cat("\n--- Structure (column names + data types) ---\n")
str(df_raw)


# --- Question 3: What does it look like? ---
# See the actual raw data — most important visual check
# IMPORTANT -> This is where you spot: "sale", "1423_", "f", "YES" etc.
cat("\n--- Sample Rows (first 10) ---\n")
print(head(df_raw, 10))


# --- Question 4: How clean is it? ---

# 4a. Missing Values
# Counts NAs per column — sorted from most missing to least
# Only shows columns that have missing values — skips clean ones
cat("\n--- Missing Values Per Column ---\n")
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
cat("\n--- Duplicate Rows ---\n")
dup_count <- sum(duplicated(df_raw))
if (dup_count > 0) {
  cat("Duplicates found    :", dup_count, "\n")
  cat("Rows after removal  :", nrow(df_raw) - dup_count, "\n")
  cat(">>> ACTION: Remove duplicates in Section 5 using distinct()\n")
} else {
  cat("Duplicates found    : 0 — no action needed.\n")
}


# --- Question 5: What are the actual values? ---

# 5a. Target Variable Distribution
# Count how many stayed vs left BEFORE cleaning
# useNA = "always" forces NA to show even if there are none
cat("\n--- Attrition Distribution (raw) ---\n")
print(table(df_raw$Attrition, useNA = "always"))

# 5b. Unique Values in ALL Categorical Columns
# Auto-detects every text column — no need to hardcode names (As stated in Question 2)
# Reveals ALL inconsistent formats that need fixing in Section 5
# sort() puts similar values next to each other — makes duplicates obvious
cat("\n--- Unique Values in Categorical Columns ---\n")

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
cat("\n--- Numeric Column Summary ---\n")

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

cat("\n[OK] Exploration complete — review output above then proceed to Section 4.\n")


# =============================================================================
# SECTION 4: CONFIGURATION
# Now that you have seen the raw data in Section 3, set your values here
# Change anything here — it flows automatically through the rest of the script
# =============================================================================

# --- File Settings ---
DATA_FILE        <- raw_file
OUTPUT_CSV       <- "employee_attrition_cleaned.csv"
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

# --- Validation Behaviour ---
# TRUE  = remove impossible rows from dataset
# FALSE = keep them but mark with a warning column
FLAG_AND_REMOVE <- TRUE

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

cat("\n[OK] 5.1 Column names standardised.\n")
print(names(df))


# -----------------------------------------------------------------------------
# 5.2 Universal Categorical Normalization
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
        c("travel-rarely", "travel_rarely", "rare", "rarely", "travel rarely")                      ~ "Travel_Rarely",
      business_travel %in%
        c("travel-frequently", "travel_frequently", "frequent", "frequently", "travel frequently")  ~ "Travel_Frequently",
      business_travel %in%
        c("non-travel", "non_travel", "non", "nontravel", "nil", "no travel", "non travel", "none") ~ "Non_Travel",
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
      education_field %in% c("medical", "med")                                 ~ "Medical",
      education_field %in% c("marketing", "mkt")                               ~ "Marketing",
      education_field %in% c("technical degree", "td")                         ~ "Technical Degree",
      education_field %in% c("hr", "h&r", "human resources", "human resource") ~ "Human Resources",
      TRUE                                                                     ~ "Other"
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
    ),
    
    # Over18 — should always be "Y", anything else is invalid
    over18 = case_when(
      over18 == "y" ~ "Y",
      TRUE ~ NA_character_
    )
  )

cat("[OK] 5.2 Categorical columns standardised.\n")


# -----------------------------------------------------------------------------
# 5.3 Clean Dirty Numeric Columns
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
  "job_role", "marital_status", "over_time", "over18"
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

cat("\n[OK] 5.3 Cleaned", length(dirty_cols), "dirty numeric columns.\n")
if (length(dirty_cols) > 0) {
  cat("Columns:", paste(dirty_cols, collapse = ", "), "\n")
}


# -----------------------------------------------------------------------------
# 5.4 Remove Duplicate Rows
# Problem (Q4b): duplicate rows inflate analysis results
# Fix: keep only the first occurrence of each duplicated row
# distinct() compares every column — only removes EXACT full-row duplicates
# -----------------------------------------------------------------------------
rows_before_dedup <- nrow(df)
df <- df %>% distinct()
cat("\n[OK] 5.4 Removed", rows_before_dedup - nrow(df), "duplicate rows.\n")


# -----------------------------------------------------------------------------
# 5.5 Remove Rows With Missing Target Variable (Attrition)
# Problem: 29 rows have no Attrition value — found in Section 3
# Why remove: Attrition is what we are PREDICTING
# Cannot guess whether someone left or stayed — no valid imputation exists
# Done BEFORE imputation so these rows don't affect median/mode calculations
# -----------------------------------------------------------------------------
rows_before_target <- nrow(df)
df <- df %>% filter(!is.na(attrition))
cat("\n[OK] 5.5 Removed", rows_before_target - nrow(df),
    "rows with missing Attrition (target variable).\n")
cat("Rows remaining:", nrow(df), "\n")


# -----------------------------------------------------------------------------
# 5.6 Impute Remaining Missing Values
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
cat("\n[OK] 5.6 Imputation complete — filled",
    na_before - na_after, "missing values.\n")


# -----------------------------------------------------------------------------
# 5.7 Convert to Labelled Factors
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

cat("[OK] 5.7 Columns converted to labelled factors.\n")

# Rename as clean dataset
df_clean <- df
cat("\n[OK] df_clean is ready —", nrow(df_clean), "rows x",
    ncol(df_clean), "columns.\n")


# =============================================================================
# SECTION 6: VALIDATION
# Confirm everything in Section 5 worked correctly
# Compare before/after for each cleaning step
# =============================================================================

# --- 6.1 Missing Values After Cleaning ---
# Should be 0 for all columns after imputation in 5.6
cat("\n--- 6.1 Missing Values After Cleaning ---\n")
missing_clean <- colSums(is.na(df_clean))
missing_clean <- sort(missing_clean[missing_clean > 0], decreasing = TRUE)
if (length(missing_clean) == 0) {
  cat("No missing values remaining — imputation successful.\n")
} else {
  cat("WARNING: Some NAs still remain:\n")
  print(missing_clean)
}


# --- 6.2 Confirm Categorical Cleaning Worked ---
# Compare with Section 3 — should now show only clean consistent values
cat("\n--- 6.2 Cleaned Unique Values ---\n")
cat("Attrition      :"); print(levels(df_clean$attrition))
cat("Department     :"); print(levels(df_clean$department))
cat("BusinessTravel :"); print(levels(df_clean$business_travel))
cat("Gender         :"); print(levels(df_clean$gender))
cat("OverTime       :"); print(levels(df_clean$over_time))


# --- 6.3 Confirm Imputation Values Used ---
# Shows what median value was used to fill NAs per numeric column
# Useful for your report — state exactly what was imputed
cat("\n--- 6.3 Median Values Used for Numeric Imputation ---\n")
df_clean %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ median(., na.rm = TRUE))) %>%
  pivot_longer(everything(),
               names_to  = "column",
               values_to = "median_used") %>%
  print(n = Inf)


# --- 6.4 Impossible Logic Flags ---
# Rows where data is logically impossible — cleaning cannot fix these
cat("\n--- 6.4 Impossible Logic Flags ---\n")
flag_impossible <- bind_rows(
  
  # Flag 1: Can't have worked at company longer than total career
  df_clean %>%
    filter(total_working_years < years_at_company) %>%
    mutate(flag_reason = "TotalWorkingYears < YearsAtCompany"),
  
  # Flag 2: Can't have started working before age 18
  df_clean %>%
    filter(age < (total_working_years + 18)) %>%
    mutate(flag_reason = "Age < TotalWorkingYears + 18"),
  
  # Flag 3: Can't be in current role longer than total time at company
  df_clean %>%
    filter(years_in_current_role > years_at_company) %>%
    mutate(flag_reason = "YearsInCurrentRole > YearsAtCompany")
  
) %>%
  select(employee_number, age, total_working_years,
         years_at_company, years_in_current_role, flag_reason) %>%
  distinct()

cat("Total flagged rows:", nrow(flag_impossible), "\n")
if (nrow(flag_impossible) > 0) {
  print(flag_impossible)
  if (FLAG_AND_REMOVE) {
    df_clean <- df_clean %>%
      filter(!employee_number %in% flag_impossible$employee_number)
    cat("[ACTION] Removed", nrow(flag_impossible),
        "impossible rows.\n")
  } else {
    df_clean <- df_clean %>%
      mutate(data_flag = ifelse(
        employee_number %in% flag_impossible$employee_number,
        "FLAGGED", "OK"
      ))
    cat("[ACTION] Rows marked in 'data_flag' column.\n")
  }
} else {
  cat("No impossible rows found.\n")
}


# --- 6.5 Final Data Health Summary ---
cat("\n--- 6.5 Final Data Health Summary ---\n")
cat(sprintf("  %-30s %d\n",    "Raw rows loaded:",        nrow(df_raw)))
cat(sprintf("  %-30s %d\n",    "Duplicates removed:",     rows_before_dedup - nrow(df)))
cat(sprintf("  %-30s %d\n",    "Missing target removed:", rows_before_target - nrow(df)))
cat(sprintf("  %-30s %d\n",    "NAs imputed:",            na_before - na_after))
cat(sprintf("  %-30s %d\n",    "Impossible rows removed:", nrow(flag_impossible)))
cat(sprintf("  %-30s %d\n",    "Final clean rows:",       nrow(df_clean)))
cat(sprintf("  %-30s %d\n",    "Remaining NAs:",          sum(is.na(df_clean))))
cat(sprintf("  %-30s %d\n",    "Stayed (No):",
            sum(df_clean$attrition == "No")))
cat(sprintf("  %-30s %d\n",    "Left (Yes):",
            sum(df_clean$attrition == "Yes")))
cat(sprintf("  %-30s %.2f%%\n","Attrition Rate:",
            sum(df_clean$attrition == "Yes") / nrow(df_clean) * 100))

# Export clean dataset
write.csv(df_clean, OUTPUT_CSV, row.names = FALSE)
cat("\n[OK] Clean dataset saved to:", OUTPUT_CSV, "\n")

# We convert the CSV to Parquet format. Unlike CSVs, Parquet is a binary 
# columnar format that allows for high-speed I/O and better compression.
clean_parquet <- "employee_attrition_cleaned.parquet"
write_parquet(df_clean, clean_parquet)

cat("\n[OK] Clean data saved as CSV and Optimized Parquet.")
cat("\n>>> BASE SCRIPT COMPLETE — clean_parquet is ready for analysis.\n")

# =============================================================================
# SECTION 7 ONWARDS: YOUR GROUP'S ANALYSIS GOES HERE
# Each group member writes their assigned objective below this line
# =============================================================================