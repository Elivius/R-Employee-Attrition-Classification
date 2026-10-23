# Employee Attrition Classification (Group 21)

An R data-analysis pipeline that takes a messy HR dataset, cleans and validates it, and then studies which factors are linked to employees leaving the company (`attrition = Yes`).

This is the Group 21 assignment for **Programming for Data Analysis**. Everything lives in one script, [`Group21_Programming_for_Data_Analysis.R`](Group21_Programming_for_Data_Analysis.R). The shared part (Sections 1–6) does exploration, cleaning and validation. Each member then adds their own analysis objective in Section 7.

---

## Team

| Member | Student ID | Objective | Branch |
|---|---|---|---|
| Joshua Yeo Jing Hao | TP077315 | 1 — Compensation | `feat/joshua` |
| Chin Kai Jack | TP076605 | 2 — Burnout & Work-Life Pressure | `feat/elivius` |
| Ee Jin Xing | TP076848 | 3 — Career Growth / Stagnation | `feat/jinxing` |
| Lee Hong Yi | TP076604 | 4 — Workplace Culture & Satisfaction | `feat/hongyi` |

---

## Repository Structure

```
R-Employee-Attrition-Classification/
├── Group21_Programming_for_Data_Analysis.R   # The full pipeline (Sections 1–7)
├── dataset_employee_attrition.csv            # Raw input data (NOT tracked by git; see below)
├── R-Employee-Attrition-Classification.Rproj # RStudio project file (not tracked)
├── .gitignore
└── README.md
```

Files the script creates in the project folder when it runs:

| File | Created in | Description |
|---|---|---|
| `employee_attrition_cleaned.csv` | Section 6 | The cleaned dataset as CSV |
| `employee_attrition_cleaned.parquet` | Section 6 | The same cleaned dataset in Parquet format. Section 7 analyses read from this file |

> `.gitignore` excludes `*.csv`, `*.parquet`, `*.rds` and `*.Rproj`. **The raw dataset is not in the repository**, so each member needs their own copy in the project root.

---

## Dataset

`dataset_employee_attrition.csv` has **2,000 rows × 35 columns**. It looks like the IBM HR Analytics attrition dataset, but with dirty data added on purpose:

- **Inconsistent categories**: the same value is written many ways, e.g. `yes` / `YES` / `1`, `sale` / `Sales`, `R&D` / `Research & Development`, `f` / `FEMALE`, `ls` / `Life Sciences`, `sales exe` / `Sales Executive`
- **Numbers stored as text**: values with junk characters such as `1423_`, `329?`, `4_`
- **Missing values**: blank cells and `NA`, `N/A`, `na`, `n/a`
- **Duplicate rows** and **duplicate employee IDs**
- **Constant columns**: `EmployeeCount`, `Over18` and `StandardHours` have the same value in every row
- **Impossible values**, e.g. `TotalWorkingYears < YearsAtCompany`, or someone who would have started working before age 14

**Target variable:** `Attrition` (`No` = stayed, `Yes` = left)

**Feature groups**

| Group | Columns |
|---|---|
| Demographics | Age, Gender, MaritalStatus, DistanceFromHome, Education, EducationField |
| Job | Department, JobRole, JobLevel, JobInvolvement, BusinessTravel, OverTime |
| Compensation | MonthlyIncome, DailyRate, HourlyRate, MonthlyRate, PercentSalaryHike, StockOptionLevel |
| Satisfaction & Performance | EnvironmentSatisfaction, JobSatisfaction, RelationshipSatisfaction, WorkLifeBalance, PerformanceRating |
| Tenure & Career | TotalWorkingYears, YearsAtCompany, YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager, NumCompaniesWorked, TrainingTimesLastYear |
| Dropped during cleaning | EmployeeNumber (ID), EmployeeCount, Over18, StandardHours (constant) |

**Ordinal scale labels** (set in Section 4):

| Scale | 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|---|
| Education | Below College | College | Bachelor | Master | Doctor |
| Environment / Job / Relationship Satisfaction, Job Involvement | Low | Medium | High | Very High | — |
| Work-Life Balance | Bad | Good | Better | Best | — |
| Performance Rating | Low | Good | Excellent | Outstanding | — |

---

## Requirements

- **R** (recent 4.x release recommended)
- **RStudio** (recommended; the project ships an `.Rproj` file)
- R packages, which **load automatically** through [`pacman`](https://cran.r-project.org/package=pacman):

| Package | Used for |
|---|---|
| `tidyverse` | Data wrangling (`dplyr`, `tidyr`, …) and plotting (`ggplot2`) |
| `tidymodels` | Modelling framework |
| `janitor` | `clean_names()`, which turns column names into snake_case |
| `caret` | `nearZeroVar()`, which finds zero-variance columns |
| `arrow` | Reading and writing Parquet files |
| `scales`, `gridExtra`, `corrplot` | Plot formatting, multi-panel layouts, correlation plots |

If a package is missing, `pacman::p_load()` installs it the first time the script runs, so you need an internet connection for that first run.

---

## How to Run

1. **Clone the repository** and switch to the branch you want:
   ```bash
   git clone <repo-url>
   ```
2. **Put the dataset** `dataset_employee_attrition.csv` in the project root, next to the `.R` script.
3. **Open the project** in RStudio (`R-Employee-Attrition-Classification.Rproj`), or set the working directory to the project folder:
   - RStudio: *Session → Set Working Directory → To Source File Location*
   - or in the console: `setwd("path/to/R-Employee-Attrition-Classification")`
4. **Run the whole script** (`Ctrl + Shift + Enter` in RStudio), or from a terminal:
   ```bash
   Rscript Group21_Programming_for_Data_Analysis.R
   ```

Every step prints `[OK]`, `WARNING` or `[ERROR]` to the console. If the CSV can't be found, or is empty, the script stops with instructions for fixing it.

---

## Pipeline Overview

```
Raw CSV ─► 1. Libraries ─► 2. Load ─► 3. Explore ─► 4. Configure ─► 5. Clean ─► 6. Validate ─► Clean CSV + Parquet ─► 7. Objective analyses
```

### Section 1: Libraries
Loads all required packages in a single `pacman::p_load()` call (it checks, installs and loads each one).

### Section 2: Data Retrieval
Checks that the raw file exists and isn't empty, then reads it with `read.csv()`. `na.strings = c("", "NA", "N/A", "na", "n/a")` turns all the different spellings of "missing" into `NA` at load time.
*(`nil`, `null` and `none` are left out on purpose, because the data uses `nil` to mean "No Travel" in `BusinessTravel`.)*

### Section 3: Raw Data Exploration
Answers a fixed set of questions **before** changing anything. Each finding is the reason for a cleaning step in Section 5.

| # | Question | Method |
|---|---|---|
| 1 | What is the size? | `nrow()`, `ncol()` |
| 2 | What is inside? | `str()`: types and names, plus numeric columns wrongly read as `chr` |
| 3 | What does it look like? | `head(df_raw, 10)` |
| 4a | Missing values per column | `colSums(is.na())` |
| 4b | Exact duplicate rows | `duplicated()` |
| 4c | Duplicate employee IDs | Lists rows that share the same `EmployeeNumber` |
| 5a | Target distribution | `table(Attrition, useNA = "always")` |
| 5b | Unique values in every text column | Finds text columns automatically and prints sorted unique values |
| 5c | Min / mean / max of numeric columns | Finds numeric columns automatically |
| 6 | Zero-variance features | `caret::nearZeroVar(saveMetrics = TRUE)` |

### Section 4: Configuration
Holds all the values used later in the script. Change a value here and the change carries through the rest of the script.

| Setting | Value |
|---|---|
| `DATA_FILE` | `dataset_employee_attrition.csv` |
| `OUTPUT_CSV` / `OUTPUT_PARQUET` | `employee_attrition_cleaned.csv` / `.parquet` |
| `OUTPUT_STATS_CSV` | `statistical_test_results.csv` (reserved for statistical test output) |
| `RANDOM_SEED` | `42` |
| `TRAIN_SPLIT` | `0.80` (80 % train / 20 % test) |
| Colour palette | Blue `#2196F3` = stayed, Red `#F44336` = left, plus purple, orange and green for other charts |
| Factor label sets | `LBL_4POINT`, `LBL_WLB`, `LBL_PERF`, `LBL_EDU` |

### Section 5: Data Cleaning & Pre-processing

| Step | What it does | Why |
|---|---|---|
| 5.1 Standardise column names | `janitor::clean_names()`: `MonthlyIncome` → `monthly_income` | Consistent snake_case names |
| 5.2 Remove zero-variance features | Drops columns flagged `zeroVar` by `nearZeroVar()` | A column with one value gives the model nothing to learn from |
| 5.3 Remove employee ID | Drops `employee_number` | A unique ID can lead to overfitting |
| 5.4 Normalise categories | Lower-cases and trims every text column, then maps every variant to one clean label with `case_when()`. Columns: `attrition`, `business_travel`, `department`, `education_field`, `gender`, `job_role`, `marital_status`, `over_time` | Removes duplicate categories such as `sale` vs `Sales` |
| 5.5 Clean dirty numerics | `clean_numeric()` strips non-digit characters (`1423_` → `1423`). It only runs on non-categorical text columns, which it finds automatically | Turns text-typed numbers back into real numbers |
| 5.6 Remove duplicate rows | `distinct()` (exact full-row matches only) | Duplicates inflate the results |
| 5.7 Drop missing target | Removes rows where `attrition` is `NA` | You can't fill in the outcome you are trying to predict. This runs before imputation so these rows don't affect the medians and modes |
| 5.8 Impute missing values | **Ordinal** columns: median rounded to a whole number. **Other numeric** columns: median. **Text** columns: mode (`get_mode()`) | Median isn't thrown off by outliers. Rounding keeps ordinal values valid for the factor conversion in 5.9 |
| 5.9 Convert to labelled factors | Ordinal scales get labels from Section 4. `job_level` and `stock_option_level` become factors. Nominal columns become factors. `attrition` gets levels `No` then `Yes` | Stops R from treating `Education = 4` as "twice" `Education = 2` |
| 5.10 Fix impossible values | Checks 3 rules: `total_working_years ≥ years_at_company`, `age ≥ total_working_years + 14`, `years_in_current_role ≤ years_at_company`. Fixes them with `pmax()` / `pmin()`, then removes rows that can't satisfy all the rules at once | Makes the timeline data logically consistent |

The final result is **`df_clean`**.

### Section 6: Validation

| Step | Check |
|---|---|
| 6.1 | No `NA` values are left anywhere |
| 6.2 | Every factor shows only clean, expected levels |
| 6.3 | Median values of each numeric column, for reporting what was used to fill gaps |
| 6.4 | **Data Health Summary**: raw rows, duplicates removed, missing-target rows removed, NAs imputed, zero-variance columns removed, impossible values fixed or removed, final shape, stayed/left counts and **attrition rate** |

The script then saves `df_clean` as **CSV** and **Parquet**. Parquet is a compressed, column-based format, so a Section 7 analysis can load just the columns it needs.

### Section 7: Individual Objective Analysis
Each member adds their analysis after the base script, starting from `df_clean` or the Parquet file.

---

## Analysis Objectives

> On `main`, Section 7 is still an empty placeholder. Each objective is being built on its owner's branch, listed in the [Team](#team) table.

### Objective 1: Compensation (Joshua Yeo Jing Hao)
**Hypothesis:** Lower pay, or pay compression, is significantly linked to higher attrition.
- **Variables:** `monthly_income`, `percent_salary_hike`, `stock_option_level`, `job_level`, `age`
- **Methods:** descriptive statistics, visualisations, **Welch t-tests** (income and salary hike by attrition), **Chi-square** (stock option level × attrition)
- Also adds the per-objective Parquet loader: each objective reads only its own columns with `read_parquet(col_select = …)` and then re-applies its factor levels.

### Objective 2: Burnout & Work-Life Pressure (Chin Kai Jack)
**Hypotheses:**
- **H1:** Employees who work overtime leave significantly more often.
- **H2:** The more often someone travels for work, the more likely they are to leave.
- **H3:** A long commute adds to burnout risk.
- **H4:** Marital status changes how strongly burnout affects someone.

- **Variables:** `over_time`, `business_travel`, `distance_from_home`, `marital_status`
- **Methods:** three tiers
  - **Descriptive:** proportional bar charts, violin plots, heatmap
  - **Diagnostic:** Chi-square, Cramér's V, Kruskal–Wallis
  - **Predictive:** multivariate **logistic regression** (`glm`, `binomial`), with each baseline level set to the ideal state

### Objective 3: Career Growth / Stagnation (Ee Jin Xing)
**Hypothesis:** Employees whose careers have stalled (slow promotions, little training, no movement up job levels) are significantly more likely to leave.
- **Variables:** `years_at_company`, `years_in_current_role`, `years_since_last_promotion`, `job_level`, `training_times_last_year`, `num_companies_worked`, `total_working_years`
- **Methods:** descriptive analysis, visualisations, **Wilcoxon rank-sum** tests (chosen because the variables are right-skewed), **Chi-square** (job level × attrition)

### Objective 4: Workplace Culture & Satisfaction (Lee Hong Yi)
- **Variables:** `environment_satisfaction`, `job_satisfaction`, `work_life_balance`, `relationship_satisfaction`
- **Methods:** to be added on `feat/hongyi`

---

## Git Workflow

- **`main`** holds the shared base pipeline (Sections 1–6).
- **`feat/<member>`** branches hold each member's Section 7 objective. Bring them into `main` with pull requests.
- Commit messages start with a type prefix:

| Prefix | Use for |
|---|---|
| `feat:` | New functionality, such as a new cleaning step or analysis |
| `fix:` | Bug fixes or corrected logic |
| `refractor:` | Restructuring, renames, comments or indentation, with no change in behaviour |
| `chore:` | Housekeeping, such as `.gitignore` |

---

## Notes

- Run the script **top to bottom**. Each section relies on objects made in earlier sections, e.g. `df_raw` → `df` → `df_clean`.
- Analyses that read the Parquet file need Section 6 to have finished successfully first.
- Fixed random seed (`RANDOM_SEED = 42`) keeps modelling results reproducible.
- Keep the plot colours consistent across objectives: **blue = stayed**, **red = left**.
