# Replication materials for Confirmatory Factor Analysis (CFA) and Latent Profile Analysis (LPA) presented in An Augmented Triangular Theory of the Expression of Love

## General Information
Authors: Arezoo Soleimani (Psychology, asc329@cornell.edu), Jiwoo Kim (Psychology, jk2759@cornell.edu), Robert Sternberg (Psychology, rjs487@cornell.edu)

Project Contacts (as of August 2025): Arezoo Soleimani, asc329@cornell.edu

## Content
This repo includes replication files for the CFA and LPA analysis in An Augmented Triangular Theory of the Expression of Love paper.

## Sharing / Access Information

### Files

- **CFA_Rcode.R**  
  R code for the Confirmatory Factor Analysis (CFA) conducted in this paper.

- **LPA_Rcode.R**  
  R code for the Latent Profile Analysis (LPA) conducted in this paper. This script uses the same dataset imputed in `CFA_Rcode.R`.

- **love_may.xls**  
  Final dataset used in this study.

---

### Dataset Overview
- **Source:** Directly collected by the authors.  
- **Size:**  
  - Final analytic sample: 253 participants (after excluding participants who answered less than 70% of the questions.)  
  - Original survey respondents: 279 participants  

---

### Variable Description

#### Meta variables
- `count`: Sequential numbering of responses  
- `id`: Unique participant identifier  

#### Scale variables

**Sternberg’s Love Theory items**  
- Variables starting with `a` or `b`  
  - `a`: Current relationship  
  - `b`: Ideal relationship  
- First digit after the prefix indicates the subscale:  
  - `1`: Intimacy (e.g., `a1-1`, `b1-3`)  
  - `2`: Commitment  
  - `3`: Passion  
- The second digit after the hyphen refers to the item number within the same subscale.
  
**ATTEL scale items**  
- Variables starting with `c` or `d`  
  - `c`: Current relationship  
  - `d`: Ideal relationship  
- First digit after the prefix indicates the subscale:  
  - `1`: Dependence  
  - `2`: Independence  
  - `3`: Constraints  
  - `4`: Freedom  
  - `5`: Lack of control  
  - `6`: Control  
- The second digit after the hyphen refers to the item number within the same subscale.
**Single-item relationship evaluations**  
- `Q276`: How satisfied are (or were) you with your relationship with your partner?  
- `Q277`: How satisfied are (or were) you with your partner in your relationship?  

**Response scale (7-point Likert):**  
1 = Extremely Dissatisfied  
2 = Very Dissatisfied  
3 = Somehow Dissatisfied  
4 = Mixed  
5 = Somewhat Satisfied  
6 = Very Satisfied  
7 = Extremely Satisfied  

---

### Demographic and relationship background variables
- `Q265`: Age (numeric)  
- `Q267`: Gender (Male / Female / Other / Prefer not to answer)  
- `Q268`: Currently in a relationship? (Yes/No) 
- `Q275`: Ethnicity  
  - Asian  
  - Asian American  
  - White or Caucasian  
  - Black or African American  
  - Hispanic or Latino  
  - American Indian or Alaska Native  
  - Two or more races  
  - Other  
  - Prefer not to answer  

---

### Missing Data Handling
- Missing values were imputed using the `missForest` package in R.

## Reproducibility
The analysis was conducted using R version 4.4.1. The file [systemsession.txt](systemsession.txt) contains the R session information, including the R version and all package versions used in the analysis, to ensure reproducibility.
