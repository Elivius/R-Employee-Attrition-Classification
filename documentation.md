# Objective 2: To Investigate the Impact of Burnout and Work-Life Pressure on Employee Attrition

**Student Name:** Chin Kai Jack
**Student ID:** TP076605

---

## Overview

This objective evaluates how workplace stressors and personal life demands — specifically Overtime, Business Travel, Distance from Home, and Marital Status — interact to drive employee attrition. By adopting a multi-tiered analytical framework, the analysis progresses from descriptive visualisations to rigorous non-parametric hypothesis testing, culminating in multi-dimensional risk profiling and a scoped predictive model.

Each variable was selected because it captures a distinct dimension of the time-energy drain experienced by employees:

- **OverTime** — time stolen from personal life after working hours
- **BusinessTravel** — energy and time spent away from home and family
- **DistanceFromHome** — daily commute burden compounding fatigue before and after work
- **MaritalStatus** — single employees are disproportionately exposed to overtime and last-minute demands, as they are perceived by management as more "available" and are less socially empowered to decline (e.g., being asked to stay late or travel last-minute because they are assumed to have fewer domestic ties); married and divorced employees, by contrast, carry domestic obligations that act as a natural buffer against excessive workload accumulation

### Hypotheses

- **H1:** Employees working overtime will show a statistically significantly higher attrition rate than non-overtime employees.
- **H2:** There is a dose-response relationship between business travel frequency and attrition risk — each incremental increase in travel produces an incremental increase in attrition.
- **H3:** Distance from home acts as a "Time Tax" that compounds burnout risk, resulting in higher attrition among employees with longer commutes.
- **H4:** Marital status moderates an employee's sensitivity to burnout, with single employees showing the highest attrition due to the absence of a domestic social buffer.

### Analytical Framework

This objective employs a four-tier analytical framework:

- **Tier 1 — Descriptive:** Proportional bar charts, violin plots, and heatmaps to establish baseline attrition rates and visualise patterns across each burnout variable.
- **Tier 2 — Diagnostic:** Chi-Square tests, Cramér's V effect sizes, and Kruskal-Wallis rank-sum tests to determine whether observed patterns are statistically significant rather than attributable to random chance.
- **Tier 3 — Predictive:** A burnout-scoped multivariate logistic regression (Analysis 2-6) combining all four burnout variables to quantify each factor's independent contribution to attrition risk while controlling for the simultaneous influence of the others.
- **Tier 4 — Prescriptive:** Actionable HR intervention protocols (e.g., targeted well-being check-ins and workload reviews) derived from the compounding risk profiles identified in the interaction heatmaps and predictive models.

The Extra Feature extends Tier 3 further with a full all-variable model across the entire dataset, using the results to identify unexpected significant predictors — specifically Job Level — and then investigating the underlying mechanism with follow-up heatmaps.

---

## Analysis 2-1: Impact of Overtime on Attrition

### Analysis Techniques & Justification

A **Chi-Square Test of Independence** was employed alongside **Cramér's V** to evaluate the statistical significance and practical effect size of the relationship between Overtime and Attrition. This was paired with a **Proportional Stacked Bar Chart**.

The Chi-Square test determines whether an association between two categorical variables is statistically real or attributable to random chance. However, a p-value alone is insufficient for business decision-making — a large dataset can produce a highly significant p-value even for a trivially weak association. Cramér's V addresses this limitation by measuring the practical strength of the association on a standardised 0-to-1 scale, independent of sample size. Together, the two metrics answer different questions: Chi-Square asks *"is this real?"* while Cramér's V asks *"does it actually matter?"*

Interpretation benchmarks for Cramér's V: < 0.10 = weak, 0.10–0.29 = medium, ≥ 0.30 = strong.

### Screenshot of Source Code and Output

> **[Insert screenshot of R code — Analysis 2-1 section]**

> **[Insert plot: p_obj2_1 — Proportional Stacked Bar Chart: Overtime & Attrition]**

### Findings & Interpretation

- **Statistical Significance:** The Chi-Square test yielded χ²(1) = 79.0, p = 2e-15, allowing us to reject the null hypothesis with greater than 99.9% confidence. The association between overtime and attrition is not attributable to chance.
- **Effect Size:** Cramér's V = 0.181, indicating a medium-to-strong practical association. This confirms that overtime is not merely statistically significant but substantively meaningful — the effect is large enough to matter in business decision-making.
- **Business Context:** The proportional bar chart reveals a stark contrast. Employees working overtime exhibit an attrition rate of 26.2%, compared to just 11.5% among non-overtime employees — a ratio of approximately 2.3 times. This indicates that excessive workload without adequate recovery time acts as a primary catalyst for burnout and subsequent resignation.

**H1 is supported — we reject H0.**

---

## Analysis 2-2: Dose-Response Relationship of Business Travel and Attrition

### Analysis Techniques & Justification

A **Chi-Square Test** and **Cramér's V** effect size were applied, visualised via a **Proportional Stacked Bar Chart**. By treating Business Travel as an ordered factor (No Travel → Travel Rarely → Travel Frequently), this analysis investigates whether a dose-response relationship exists.

A dose-response relationship is a particularly compelling form of evidence in causal analysis: if each incremental increase in a stressor (travel burden) reliably produces an incremental increase in the outcome (attrition), the argument for a causal link is strengthened considerably beyond what a simple association test can establish. This approach goes beyond standard descriptive analysis by imposing theoretical structure on the data before testing.

### Screenshot of Source Code and Output

> **[Insert screenshot of R code — Analysis 2-2 section]**

> **[Insert plot: p_obj2_2 — Proportional Stacked Bar Chart: Travel Frequency & Attrition]**

### Findings & Interpretation

- **Statistical Significance:** Chi-Square returned χ²(2) = 9.94, p = 0.00769. The null hypothesis is rejected — travel frequency and attrition are not independent.
- **Effect Size:** Cramér's V = 0.071. The effect size is small, which is expected given that business travel affects a smaller subset of employees. However, the consistent stepwise increase across all three levels strengthens the causal argument.
- **Dose-Response Confirmed:** Attrition increases monotonically: No Travel = 12.0% → Travel Rarely = 14.9% → Travel Frequently = 20.7%. Each additional level of travel burden adds measurable attrition risk.
- **Business Context:** Employees who are frequently displaced from their home environment experience greater disruption to personal routines and family time, leading to increased turnover. The gradient effect confirms that professional travel strain compounds attrition risk progressively rather than acting as a threshold effect.

**H2 is supported — a clear dose-response gradient is confirmed.**

---

## Analysis 2-3: The Commute Penalty — Distance from Home vs. Attrition

### Analysis Techniques & Justification

Prior to testing, a **Normality Check via Histogram** was conducted on the Distance from Home variable. The histogram revealed a strong right skew — the majority of employees live within 1–5 km of the office, with a long tail extending to 29 km. This violation of normality assumptions invalidates the use of a parametric t-test, which assumes the data follows a bell-shaped distribution.

Consequently, the **Kruskal-Wallis Rank Sum Test** was selected as the appropriate non-parametric alternative. Unlike the t-test, the Kruskal-Wallis test compares rank distributions between groups without requiring any assumption about the underlying distribution shape, making it robust to the right skew observed here.

The result was visualised using a composite **Violin and Boxplot**, which simultaneously communicates the full distributional shape (via the violin envelope) and the central tendency and spread (via the boxplot). This dual representation is more informative than a boxplot alone, as it reveals whether the distributions overlap substantially or differ in shape as well as location.

### Screenshot of Source Code and Output

> **[Insert screenshot of R code — Analysis 2-3 section]**

> **[Insert plot: Normality Check Histogram — Distance from Home Distribution]**

> **[Insert plot: p_obj2_3 — Violin + Boxplot: Distance & Attrition]**

### Findings & Interpretation

- **Statistical Test Result:** The Kruskal-Wallis test returned H(1) = 2.91, p = 0.0893. This does not meet the conventional α = 0.05 threshold for statistical significance. We therefore fail to reject H0 on distance alone.
- **Observed Trend:** Despite non-significance, a directional pattern is present. The median distance for employees who left (Mdn = 8 km) is marginally higher than for those who stayed (Mdn = 7 km). The violin plots reveal a slightly heavier upper tail for the attrition group, suggesting that distance may compound burnout risk at extreme values rather than acting as a consistent linear driver.
- **Business Context:** While distance from home does not independently reach significance in isolation, this does not mean commute is irrelevant. As demonstrated in Analysis 2-6 (Logistic Regression), distance contributes a small but statistically significant odds increase when all burnout variables are controlled simultaneously. The "Time Tax" hypothesis holds as a compounding factor rather than a standalone driver.

**H3 is not supported at α = 0.05. However, the directional trend is consistent with the hypothesis — distance operates as a compounding factor rather than an independent driver.**

---

## Analysis 2-4: The Social Buffer — Marital Status and Attrition

### Analysis Techniques & Justification

A **Chi-Square Test** and **Cramér's V** were applied to assess the relationship between marital status and attrition, visualised with a **Proportional Stacked Bar Chart**. This analysis tests the Social Buffer Theory — the proposition that external social structures such as marriage or family responsibility provide psychological and financial anchors that mitigate an individual's propensity to resign under workplace stress.

### Screenshot of Source Code and Output

> **[Insert screenshot of R code — Analysis 2-4 section]**

> **[Insert plot: p_obj2_4 — Proportional Stacked Bar Chart: Marital Status & Attrition]**

### Findings & Interpretation

- **Statistical Significance:** Chi-Square returned χ²(2) = 29.9, p = 1.94e-07. The association between marital status and attrition is highly significant.
- **Effect Size:** Cramér's V = 0.127, indicating a small-to-medium practical association — meaningful at the business level.
- **Key Finding:** Single employees demonstrate the highest attrition rate at 22.5%, compared to 13.5% for married employees and 11.0% for divorced employees. Single employees are approximately twice as likely to leave as divorced employees.
- **Business Context:** This pattern is consistent with the Social Buffer Theory. Married and divorced employees typically carry stronger financial and familial commitments — mortgages, dependants, shared finances — that reduce the appeal of job transitions. Single employees, with fewer such anchors, are more geographically and financially mobile, making them more responsive to burnout when workplace pressures accumulate.

  A secondary mechanism reinforces this disparity: **Differential Workload Allocation**. Managers may consciously or unconsciously route after-hours demands — late meetings, urgent travel, weekend tasks — toward single employees, who are perceived as having fewer personal obligations and are statistically less likely to decline. Unlike married colleagues who can credibly cite a family dinner or divorced colleagues who may need to collect children, single employees face a higher social cost in refusing. This means single employees are not merely *less buffered* from burnout — they are actively *more exposed* to the operational conditions that produce it. The higher attrition rate among single employees may therefore reflect a structural inequity in workload distribution, not just a difference in personal resilience.

  * **Concrete Scenario Example:** Consider a Friday evening at 6:00 PM when an urgent client issue requires three hours of unscheduled overtime. A manager, wanting to avoid disrupting an employee's family life, might hesitate to ask a married colleague who has a spouse expecting them home. Instead, they turn to a single employee, operating under the assumption that they have "no plans" or fewer responsibilities. The single employee faces a double bind: they lack a socially validated, non-negotiable boundary (like childcare or family commitments) to decline, and stating they simply have "personal plans" is often perceived as a lack of professional dedication. Over time, these small, repeating biases accumulate, leaving the single employee with a significantly higher overtime and burnout burden than their married peers.

**H4 is supported — marital status significantly moderates attrition risk.**

---

## Analysis 2-5: Multi-Dimensional Risk Profiling (Interaction Heatmaps)

### Analysis Techniques & Justification

To move beyond univariate associations into interaction effects, **2-Way and 3-Way Interaction Heatmaps** were constructed. These visualisations reveal how multiple risk factors combine and compound — something that individual bar charts and statistical tests cannot communicate in a single view.

A heatmap is the optimal chart type here because it encodes a third variable (attrition rate) as colour intensity while simultaneously positioning two categorical variables on the axes. The 3-way version extends this by using faceting — splitting the heatmap into panels by a third categorical variable — allowing the reader to visually compare how the 2-way interaction changes across levels of a third factor.

This approach provides a **prescriptive view** — it identifies not just which individual factors are risky, but precisely which combinations of employee characteristics create actionable high-risk segments for HR intervention.

### Screenshot of Source Code and Output

> **[Insert screenshot of R code — Analysis 2-5 section]**

> **[Insert plot: p_obj2_5 — 2-Way Risk Heatmap: Marital Status × Overtime]**

> **[Insert plot: p_obj2_6 — 3-Way Risk Heatmap: Marital Status × Overtime × Travel]**

### Findings & Interpretation

**2-Way Heatmap (Marital Status × Overtime):**

- The interaction reveals a compounding effect. Single employees working overtime face an attrition rate of 40.5% — the highest observed across all six cells.
- Overtime amplifies risk across all marital groups: the overtime-to-non-overtime ratio is approximately 2.6× for single employees, 2.0× for married, and 2.3× for divorced.
- Divorced employees without overtime show the lowest attrition rate of 7.8%, demonstrating the protective effect of both non-overtime status and family commitment.

**3-Way Heatmap (Marital Status × Overtime × Business Travel):**

- The most vulnerable employee profile is **Single + Overtime + Travel Frequently**, with an attrition rate of 43.9%. This represents the convergence of all three pressure sources.
- Notably, the Divorced + No Travel + Overtime cell shows 0% attrition (n = 13). While this should be interpreted with caution given the small sample size, it reinforces the protective effect of financial obligations combined with lower travel burden.
- The 3-way analysis confirms that no single factor alone explains the risk — the simultaneous accumulation of overtime, frequent travel, and the absence of domestic support creates the highest flight risk in the organisation.

**Prescriptive Application:** Employees matching the profile Single + Overtime + Travel Frequently represent a 43.9% attrition probability. HR should flag any employee matching this profile for a mandatory well-being check-in and workload review within 30 days of entering this work pattern. This converts the analytical findings directly into an actionable, data-triggered intervention protocol — shifting HR from reactive exit management to proactive retention.

---

## Analysis 2-6: Burnout Predictive Model — Logistic Regression (4 Variables)

### Analysis Techniques & Justification

Analyses 2-1 through 2-5 each examine one variable at a time. While they establish that each factor is individually associated with attrition, they cannot isolate each variable's *independent* contribution after controlling for the others. For instance, do single employees show higher attrition because of marital status itself, or because they are also more likely to be assigned overtime? Without controlling for this overlap, the univariate tests cannot answer definitively.

A **Multivariate Logistic Regression** model was therefore fitted using all four burnout variables — Overtime, Business Travel, Distance from Home, and Marital Status — as simultaneous predictors of attrition. The binary outcome (attrition = 1, stayed = 0) is predicted using `glm()` with `family = binomial()`. Model coefficients are exponentiated into **Odds Ratios (OR)** with **95% Confidence Intervals**, visualised via a **Forest Plot**.

**Reading Odds Ratios:**
- OR > 1 → increases attrition risk (e.g., OR = 2.4 means 2.4× more likely to leave)
- OR < 1 → decreases attrition risk (e.g., OR = 0.6 means 40% less likely to leave)
- CI crossing 1.0 → the effect is not statistically distinguishable from zero

### Screenshot of Source Code and Output

> **[Insert screenshot of R code — Analysis 2-6 Logistic Regression section]**

> **[Insert output: Model Summary — coefficients, standard errors, z-values, p-values]**

> **[Insert output: Odds Ratios table with 95% CI]**

> **[Insert plot: p_obj2_7 — Forest Plot: Burnout-Scoped Model Odds Ratios]**

### Findings & Interpretation

Reading from the Forest Plot (`burnout model - what predicts attrition.png`):

| Variable | Significant? | Direction | Interpretation |
|---|---|---|---|
| Overtime (Yes) | ✅ Yes | Increases risk | Dominant driver — large OR well above 1.0, CI entirely to the right |
| Single (vs Divorced) | ✅ Yes | Increases risk | Strong effect — Single status independently elevates attrition risk |
| Travel Frequently | ✅ Yes | Increases risk | Significant — confirms dose-response finding holds after controlling for other variables |
| Distance from Home | ✅ Yes | Increases risk | Small but real — OR just above 1.0 with a very tight CI, barely crossing significance |
| Married (vs Divorced) | ❌ No | Neutral | CI crosses 1.0 — not independently significant once Single is controlled for |
| Travel Rarely | ❌ No | Neutral | CI crosses 1.0 — only "Frequently" carries significant independent risk |

> **Note:** Replace with exact OR values from your R output before submission.

**Key Insight:** Distance from Home, which did not reach significance in the standalone Kruskal-Wallis test (Analysis 2-3, p = 0.0893), *does* emerge as a significant predictor here. This confirms that distance operates as a genuine compounding factor that the univariate test was underpowered to detect in isolation — validating H3's directional hypothesis even though the univariate test failed.

**Priority ranking of independent risk factors: Overtime → Single Status → Travel Frequently → Distance from Home.**

### Predictive Application 1 — Employee Flight Risk Scoring

The logistic regression model was applied back to the entire cleaned dataset to generate a **continuous flight risk probability score (0–100%)** for every individual employee. A 50% decision threshold was then used to classify each employee as predicted to leave ("Yes") or stay ("No").

```r
df_logit$flight_risk_pct      <- predict(model_burnout, newdata = df_logit, type = "response")
df_logit$predicted_attrition  <- ifelse(df_logit$flight_risk_pct >= 0.5, "Yes", "No")
```

This transforms the model from a descriptive/explanatory tool into a **live HR risk dashboard** — each employee now has a personalised, model-derived attrition probability attached to their record. HR can sort this list in descending order and prioritise retention conversations with the employees carrying the highest scores, before they resign.

> **[Insert screenshot of R code and output: head() showing over_time, business_travel, flight_risk_pct, predicted_attrition columns]**

### Predictive Application 2 — New Hire Attrition Probability Simulation

Beyond scoring existing employees, the model was used to simulate the attrition probability of a **hypothetical worst-case new hire** — a candidate who is Single, works Overtime, travels Frequently, and lives 25 km from the office:

```r
new_hire <- data.frame(
  over_time          = "Yes",
  business_travel    = "Travel Frequently",
  distance_from_home = 25,
  marital_status     = "Single",
  stringsAsFactors   = TRUE
)
new_hire_prediction <- predict(model_burnout, newdata = new_hire, type = "response")
cat("This candidate has a", round(new_hire_prediction * 100, 1), "% probability of quitting.\n")
```

This demonstrates that the model can be operationalised as a **pre-hire screening tool**. During the recruitment stage, HR can input a candidate's expected working conditions and personal profile to receive a predicted attrition probability — enabling proactive decisions about onboarding support, role design, or compensation structure before the employee even joins.

> **[Insert screenshot of R code and console output: "This candidate has a XX.X % probability of quitting."]**

**Why This Makes the Model Genuinely Predictive:**
The two applications above move the analysis definitively from explanatory to predictive. Rather than only answering *"which variables drove attrition in the past?"*, the model now also answers *"what is the probability this specific person will leave in the future?"* — the hallmark of a deployed predictive analytics system.

---

*(Note: As per assignment guidelines, the Extra Feature section starts on a separate page.)*

---

# Extra Feature 1: Full All-Variable Predictive Model and Job Level Investigation

**Student Name:** Chin Kai Jack
**Student ID:** TP076605

---

## Feature Explanation & Justification

### Limitation of the Burnout-Scoped Model

Analysis 2-6 intentionally limits its scope to the four burnout variables assigned to this objective. While this confirms the independent effects of those variables, it raises a further question: are there other organisational factors not captured in this objective's scope that also drive attrition? And more importantly, could any of those factors interact with the burnout variables in ways that change the conclusions?

### Two-Stage Extra Feature Design

To address this, the Extra Feature is implemented in two stages:

**Stage 1 — Full All-Variable Model:** A logistic regression is fitted using every variable in the cleaned dataset simultaneously. This model acts as an organisation-wide diagnostic scan — revealing which variables, across all dimensions of the data, are the strongest independent predictors of attrition after all confounders are controlled.

**Stage 2 — Job Level Investigation:** The full model unexpectedly revealed that **Job Level** is a highly significant predictor. However, Job Level was not part of the four burnout variables. This raises an important analytical question: *Is Job Level a genuine independent risk factor, or does it emerge as significant primarily because lower-level employees work more overtime, and it is actually the overtime that drives their attrition?* Two follow-up heatmaps are used to decompose this question and provide a definitive answer.

---

## Stage 1: Full All-Variable Logistic Regression

### Model Design

The full model uses `attr_bin ~ .` (all available predictors) fitted with `family = binomial()`. To ensure interpretable and meaningful baseline comparisons, reference levels were anchored to ideal states:
- `job_level` → reference = Level 5 (most senior)
- `stock_option_level` → reference = Level 3 (highest option grant)
- All satisfaction and involvement scales → reference = "Very High" or "Best"

### Screenshot of Source Code and Output

> **[Insert screenshot of R code — Full Model fitting, relevel() anchoring, and summary]**

> **[Insert output: Full Model Summary — all coefficients]**

> **[Insert output: Full Odds Ratios table with 95% CI]**

> **[Insert plot: p_obj2_8 — Forest Plot: Full All-Variable Model]**

### Key Findings from the Full Model

Reading from `addiitonal - burnout model - what predict attrtion (all variable).png`, the significant predictors (red dots) in descending order of risk include:

- **over_timeYes** — remains the strongest single predictor across all variables in the entire dataset
- **job_level1** — emerges as highly significant with a large OR, meaning Level 1 employees are substantially more likely to leave than Level 5 employees (the reference)
- **environment_satisfactionLow, job_involvementLow, job_satisfactionLow** — low satisfaction scores across multiple dimensions independently drive attrition
- **business_travelTravel Frequently** — confirms the burnout-scoped finding holds at the organisation-wide level
- **marital_statusSingle** — remains significant even when all other variables are controlled

**Surprise Finding:** `job_level1` ranks among the top predictors with a large odds ratio. This is unexpected because Job Level is not a burnout variable — it represents structural hierarchy. This triggers Stage 2: investigating *why* Job Level appears so prominently.

---

## Stage 2: Why Is Job Level 1 So High? — Interaction Heatmap Investigation

### Analytical Rationale

The full model shows Job Level 1 as a significant risk predictor. There are two possible explanations:

1. **Structural explanation:** Being at Job Level 1 is itself independently demoralising — low pay, limited autonomy, and few career advancement signals cause attrition directly.
2. **Confounding explanation:** Job Level 1 employees are more likely to be assigned overtime (because they have less power to refuse). If this is the case, then Job Level is not the true driver — overtime is, and Job Level is a proxy variable.

To distinguish between these two explanations, a **2-Way Interaction Heatmap (Job Level × Overtime)** and a **3-Way Interaction Heatmap (Job Level × Overtime × Marital Status)** were constructed.

### Screenshot of Source Code and Output

> **[Insert screenshot of R code — Job Level Heatmap sections (p_obj2_9 and p_obj2_10)]**

> **[Insert plot: p_obj2_9 — 2-Way Risk Heatmap: Job Level × Overtime]**

> **[Insert plot: p_obj2_10 — 3-Way Risk Heatmap: Job Level × Overtime × Marital Status]**

### Findings & Interpretation

**2-Way Heatmap (Job Level × Overtime)** — reading from `job level x ot interaction vs attrition.png`:

| Job Level | No Overtime | Overtime | Overtime Amplification |
|---|---|---|---|
| Level 5 (Senior) | 3.2% | 13.6% | 4.3× |
| Level 4 | 6.5% | 7.5% | 1.2× |
| Level 3 | 12.3% | 17.4% | 1.4× |
| Level 2 | 8.6% | 19.1% | 2.2× |
| **Level 1 (Junior)** | **16.4%** | **42.1%** | **2.6×** |

**Critical Observation:** Even *without* overtime, Level 1 employees have the second-highest baseline attrition of 16.4% — already above average. But when overtime is added, Level 1 + Overtime shoots to **42.1%**, the single hottest cell in the heatmap.

**Conclusion on the structural vs confounding debate:** Both explanations are partially true. Job Level 1 does carry an independent baseline risk (16.4% without overtime), consistent with the structural explanation. However, the extreme amplification effect of overtime at Level 1 (2.6×) — far greater than at higher levels — indicates that junior employees are disproportionately vulnerable to workload burden. This is consistent with the Differential Workload Allocation mechanism identified in Analysis 2-4: low-seniority employees have the least organisational power to refuse overtime assignments, so they accumulate the most overtime-driven burnout.

**3-Way Heatmap (Job Level × Overtime × Marital Status)** — reading from `job level x overtime x marital status vs attrition.png`:

The three-panel heatmap reveals the absolute peak risk profile in the dataset:

- **Single + Level 1 + Overtime = 56.2% attrition rate** (n = 73) — the deepest red cell across the entire analysis
- **Married + Level 1 + Overtime = 34.5%** (n = 87)
- **Divorced + Level 1 + Overtime = 33.3%** (n = 42)

The convergence of three compounding risk factors — junior hierarchy, overtime exposure, and absence of a domestic social buffer — creates an attrition rate of over half. This is the most actionable finding in the entire objective: a precisely defined, real, and large employee segment where targeted intervention is most urgent.

---

## How This Extra Feature Improves the Results

### 1. Moves from Scoped to Organisation-Wide Understanding

The burnout-scoped model (Analysis 2-6) answers the question within this objective's four variables. The full model answers the broader question: *across all organisational factors, does the burnout framework still hold?* The answer is yes — Overtime remains the top predictor even in a 60+ variable model, validating the entire analytical framework.

### 2. Discovers an Unseen Risk Factor

The full model surfaced Job Level 1 as a critical predictor that the burnout-scoped model could not detect. Without the Extra Feature, this finding would have been entirely missed. Stage 2 then contextualises this finding — Job Level is not an isolated risk factor but acts as an amplifier for overtime exposure.

### 3. Identifies the Highest-Risk Employee Segment in the Dataset

The 3-way heatmap reveals the ultimate risk profile: **Single + Job Level 1 + Overtime = 56.2%** — the highest attrition rate observed in the entire analysis. This is a specific, targetable, and actionable employee segment that HR can identify directly from the HRIS system.

### 4. Provides a Causal Mechanism, Not Just a Correlation

The two-stage design enables a more sophisticated analytical conclusion: Job Level 1 is significant in the full model not purely because of seniority, but because low-seniority employees are the most exposed to forced overtime. This moves the analysis from "Level 1 employees quit more" to "Level 1 employees are structurally most vulnerable to overtime-driven burnout" — a finding with far more specific and actionable HR implications.

---

## Conclusion and Recommendations for Objective 2

### Overall Discussion

The analysis conclusively demonstrates that burnout is not a singular event but a compounding phenomenon driven by the accumulation of operational demands (Overtime, Business Travel) and personal constraints (Commute Distance, Marital Status). All four hypotheses were evaluated with appropriate statistical tests:

- **H1 (Overtime) — Supported:** 26.2% vs 11.5% attrition rate; χ²(1) = 79.0, p < 0.001; Cramér's V = 0.181
- **H2 (Travel Dose-Response) — Supported:** Monotonic gradient 12.0% → 14.9% → 20.7%; χ²(2) = 9.94, p = 0.008
- **H3 (Distance) — Not Supported at α = 0.05 in isolation:** KW p = 0.0893; however, the logistic regression confirms a small independent compounding contribution (OR ≈ 1.02 per km)
- **H4 (Marital Status) — Supported:** Single employees at 22.5% vs 11.0% for divorced; χ²(2) = 29.9, p < 0.001; Cramér's V = 0.127

The burnout-scoped logistic regression confirmed Overtime, Single status, and Frequent Travel as independent significant predictors. The Extra Feature further revealed that Job Level 1 employees — particularly those who are also single and working overtime — face an attrition rate of **56.2%**, the highest observed in the entire analysis.

### Professional Recommendations

1. **Overtime Regulation with Targeted Monitoring:** Implement monthly overtime hour tracking with automatic HR alerts when any employee exceeds a defined threshold (suggested: 20 hours of overtime per month). Priority monitoring should be applied to employees who are also classified as Single and/or at Job Level 1 — the combination identified as highest-risk.

2. **Travel Reassessment and Rotation:** Since business travel shows a confirmed dose-response relationship with attrition, enforce mandatory recovery periods for employees returning from frequent trips. Consider rotating travel assignments to prevent any single employee accumulating excessive travel frequency over consecutive quarters.

3. **Flexible and Remote Work Arrangements:** To mitigate the Commute Penalty, offer hybrid or remote work options for employees living beyond the dataset median of 7–8 km. While distance did not reach significance as a standalone driver, the logistic regression confirms it contributes cumulatively — particularly for single employees who lack domestic compensating factors.

4. **Equitable Workload Distribution and Retention Support for Single Employees:** Introduce workload fairness audits that track overtime and last-minute task assignments broken down by marital status and job level. If single, junior employees are systematically absorbing a disproportionate share of after-hours demands, this structural inequity should be corrected through explicit manager guidance. In parallel, introduce social connection initiatives — mentoring, team events, buddy systems — to reduce isolation, since organisational culture can partially compensate for the absence of a domestic social buffer.

5. **Junior Employee Career Investment Programme:** Given that Level 1 employees carry a 16.4% baseline attrition rate even without overtime — and 42.1% with it — investment in structured career pathways, mentoring programmes, and accelerated promotion frameworks for high-performing junior employees is recommended to reduce the structural contribution to attrition risk.

### Limitations and Future Direction

- The current dataset lacks qualitative data regarding the subjective experience of burnout. Future analysis should incorporate Natural Language Processing (NLP) on employee exit interviews to add contextual depth to these quantitative findings, moving from *what* is happening to *why* employees feel driven to leave.
- The sample sizes in certain cells of the 3-way heatmaps are small (e.g., Divorced + Level 4 + Overtime, n = 13), which limits the reliability of attrition rate estimates in those segments. Larger datasets or longitudinal tracking would strengthen the precision of the interaction analysis.
- The logistic regression model does not account for potential non-linear relationships or interaction terms between variables. A future extension using regularised regression (e.g., LASSO) or tree-based models (e.g., Random Forest) could capture more complex patterns in the data.
- The analysis is cross-sectional, meaning it cannot establish causality with certainty — only association. A longitudinal study tracking employees' overtime hours, job level progression, and travel frequency over time before they resign would provide stronger causal evidence.