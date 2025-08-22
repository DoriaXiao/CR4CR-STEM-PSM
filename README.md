# STEM Confidence, Context, and Problem-Solving  
Repository for analysis code and de-identified data accompanying the manuscript:  
**“STEM Confidence, Context, and Problem-Solving” (submitted to *Learning and Instruction*)**

---

## Overview  
This repository contains the cleaned, de-identified dataset and R analysis pipeline used in:  

Xiao, X., Wilson, M., Patz, R., & Cheng, Y. (2025). *STEM Confidence, Context, and Problem-Solving*. Manuscript submitted for review to *Learning and Instruction*.  

The study investigates how students’ motivational perceptions—confidence, interest, opportunity, and career aspirations—predict growth in mathematical problem-solving proficiency (PSM), and how these links vary by gender, ethnicity, and educational context (Middle School, Non-AP High School, AP High School).  

---

## Data Structure  
The analytic dataset includes:  
- **Outcome**: PSM proficiency (Weighted Likelihood Estimates, WLE) from IRT models.  
- **Repeated measures**: Pre- and post-administration assessments.  
- **Predictors**: Four STEM perception variables (confidence, interest, opportunity, career aspiration).  
- **Demographics**: Gender (Female, Male), Hispanic identity (Yes/No), English Language Learner (ELL/Non-ELL).  
- **Context**: Classroom type (Middle School, Non-AP High School, AP High School).  

The dataset is de-identified and excludes student IDs or other personally identifying information. As such, the public analyses reproduce **fixed-effect estimates** using a marginal specification. Random-effects (student-level intercepts, ICCs) require an anonymized linkage key that cannot be shared publicly.  

---

## Contents  
- **/code/**  
  - `analysis_pipeline.R`: Main R script for data cleaning, recoding, modeling, and figures.  
  - Includes fallback: runs either (a) mixed-effects models if a student key is available, or (b) marginal models with robust standard errors if not.  

- **/data/**  
  - `De-identified_dataset.xlsx`: Cleaned, de-identified dataset used for replication.  
  - `codebook.md`: Documentation of variables, coding schemes, and scales.  

- **/docs/**  
  - Output tables, model summaries, and plots generated during analysis.  

---

## Data Availability  
- The dataset in `/data/De-identified_dataset.xlsx` has been fully de-identified and contains no student linkage keys.  
- This version supports replication of all fixed-effect estimates.  
- Researchers requiring access to the original linked dataset (with anonymized within-student keys) may contact the corresponding author, subject to IRB and data use agreements.  

---

## Reproducibility  
To reproduce the analyses in the manuscript:  

1. Clone this repository:  
   ```bash
   git clone https://github.com/[your-username]/STEM-Confidence-PSM.git
