# Joshua Yeo Jing Hao, TP077315
# Chin Kai Jack, TP076605
# Ee Jin Xing, TP076848
# Lee Hong Yi, TP076604

#Section 1: Libraries
#Installation for packages
packages <- c("tidyverse", "tidymodels", "scales","gridExtra", "janitor")

#To install packages that are not on the machine
new_packages <- packages[!(packages %in% rownames (installed.packages()))]
if (length(new_packages) > 0) {
  message(">>> Installing missing packages: ",
          paste(new_packages, collapse = ", "))
  install.packages(new_packages, quiet = TRUE)
}

#Load Packages
invisible(lapply(packages, library, character.only = TRUE))
message("[OK] All libraries loaded, ready to proceed.")


#Section 2: Read Raw File
raw_file <- "4. dataset_employee_attrition.csv"

#Checking file exists before loading
if(!file.exists(raw_file)){
  stop(paste0("\n[ERROR] File not found: '", raw_file, "'\n\n"),
  "To fix this:\n",
  "1. Run getwd() to see which folder R is currently in\n",
  "2. Run setwd('your/folder/path') to point to the right folder\n",
  "3. Make sure your CSV file is saved in that same folder\n"
  )
}

if (file.size(raw_file) ==0) {
  stop("\n[ERROR] '", raw_file, "' is empty, nothing to load.\n")
}

#Load the raw file
df_raw <- read_csv(
  raw_file,
  na = c("", "NA", "N/A", "na", "n/a", "nil", "Nil", "NIL", "null", "NULL", "none", "NONE"),
  show_col_types = FALSE
)

message("[OK] File Loaded", nrow(df_raw), "rows x", ncol(df_raw), "columns")

#Section 3: Data Exploration
cat("\n---  DATASET Overview ---\n")
cat("Rows    :", nrow(df_raw), "\n")
cat("Columns :", nrow(df_raw), "\n")

cat("\n--- Names & Data Types ---\n")
str(df_raw)

cat("\n--- FIrst 10 Rows ---\n")
print(head(df_raw, 10))

#Finding Missing Values
cat("\n--- Missing Values Per Column ---\n")
missing_raw <- colSums(is.na(df_raw))
missing_raw <- sort(missing_raw[missing_raw > 0 ], decreasing = TRUE)
if (length(missing_raw) == 0) {
  cat("No missing values found.\n")
} else {
  print(missing_raw)
  cat("Total missing cells: ", sum(missing_raw), "\n")
}

#Duplicated Rows
cat("\n--- Duplicated Rows ---\n")
duplicated_count <- sum((duplicated(df_raw)))
cat("Duplicates Found:", duplicated_count, "\n")
if (duplicated_count > 0){
  cat("Duplicates Found  : ", duplicated_count, "\n")
  cat("Rows after removal: ", nrow(df_raw ) - duplicated_count, "\n")
  cat("\n---- Remove duplicates in Section 5 ---\n")
}else{
  cat("No Duplicates Found, No Action Needed")
}

#Variable Distribution
cat("\n--- Attrition Distribution ---\n")
print(table(df_raw$Attrition, useNA = "always"))

cat("\n--- Unique Values in Categorical Columns ---\n")
category_cols <- df_raw %>%
  select(where(is.character)) %>%
  names()

#Loops through every column to print its value
for (col in category_cols) {
  cat("\n", col, ":\n")
  print(sort(unique(df_raw[[col]])))
}

#Numeric Range Check
cat("\n--- Numeric Summary ---\n")

num_cols <- df_raw %>%
  select(where(is.numeric)) %>%
  names()

#Header
cat(sprintf(" % -30s %14s %-14s %s\n",  "column", "Min", "Average", "Max"))
cat(strrep("-", 72), "\n") #divider line of 72 dashes

#Loop through every column with numeric value
for (col in num_cols) {
  
  col_data <- df_raw[[col]]
  
  #Skip values that are NA
  if (all(is.na(df_raw[[col]]))){
    cat(sprintf(" %-30s %s\n", col, "ALL VALUES ARE MISSING - skipped"))
    next
  }
  
  min_value <- round(min(col_data, na.rm = TRUE), 2)
  max_value <- round(max(col_data, na.rm = TRUE), 2)
  avg_value <- round(mean(col_data, na.rm = TRUE), 2)
  
  cat(sprintf(" %-30s min = %-10s avg = %-10s max = %s\n", 
              col,
              as.character(min_value),
              as.character(avg_value),
              as.character(max_value)))
}
cat("\n[OK] Exploration complete")

#Section 4: Configuration
#File Settings
Data_File <- "4. dataset_employee_attrition.csv"
Output_CSV <- "employee_attrition_cleaned.csv"
Output_Stats_CSV <- "statistical_test_results.csv"

#Model Settings
RANDOM_SEED <- 42
TRAIN_SPLIT <- 0.80

#Plot Color Palette
Color_No <- "#2196F3"
Color_Yes <- "#F44336"
Color_Bar <- "#800080"
Color_Training <- "#FF9800"
Color_Green <- "#4CAF50" #No Overtime

#Factor Label Sets (defined once, reused everywhere)
Satisfaction_Scale <- c("Low", "Medium", "High", "Very High")
WLB_Scale          <- c("Bad", "Good", "Better", "Best")
Performance_Scale  <- c("Low", "Good", "Excellent", "Outstanding")
Education_Scale    <- c("Below College", "College", "Bachelor", "Master", "Doctor")

#Validation Behavior
FLAG_AND_REMOVE <- TRUE

#Section 5: Data Cleaning & Pre-processing

#5.1Standardize Column Names
df <- df_raw %>%
  clean_names()
cat("\n[OK] Names are standardized.\n")
print(names(df))

#5.2 Cleaning Categorical Data

df <- df %>%
  mutate(
    attrition       = tolower(trimws(attrition)),
    business_travel = tolower(trimws(business_travel)),
    department      = tolower(trimws(department)),
    gender          = tolower(trimws(gender)),
    over_time       = tolower(trimws(over_time)),
    over18          = tolower(trimws(over18))
    ) %>%
  
  #Standardize Values
  #attrition
  mutate(
    attrition = case_when(
      attrition %in% c("yes", "1") ~ "Yes",
      attrition %in% c("no", "0") ~ "No", 
      TRUE ~ NA_character_
    ),
    
    #Business Travel
    business_travel = case_when(
      business_travel %in%
        c("travel_rarely", "rare") ~ "Travel_Rarely",
      business_travel %in%
        c("travel_frequently", "frequent") ~ "Travel_Frequently",
      business_travel %in%
        c("non-travel") ~ "Non_Travel",
      TRUE ~ NA_character_
    ),
    
    #Department
    department = case_when(
      department %in%
        c("sales", sale) ~ "Sales",
      department %in%
        c("r&d", "research & development") ~ "Research & Development",
      department %in%
        department %in%
        c("hr", "human resources") ~ "Human Resource",
      TRUE ~ NA_character_
    ),
    
    #Gender
    gender = case_when(
      gender %in% c("f", "female") ~ "Female",
      gender %In% c("m", "male") ~ "Male",
      TRUE ~ NA_character_
    ),
    
    #Overtime
    over_time = case_when(
      overtime %in% c("yes", "1") ~ "Yes",
      overtime %in% c("no", "0") ~ "No",
      TRUE ~ NA_character_
    ),
    
    #Over18 
    over18 = case_when(
      overtime %in% c("y") ~ "Y",
      TRUE ~ NA_character_
    )
  )
cat("[OK] 5.2 Column Standardized. \n" )

#5.3 Clean Dirty Numeric Columns
clean_numeric <- function(x) {
  x <- gsub("[^0-9.", "", as.character(x))
  x[x ==""] <- NA
  as.numeric(x)
}