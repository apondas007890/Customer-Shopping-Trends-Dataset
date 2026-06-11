# 🛍️ ShopSmart: Consumer Behavior Data Preprocessing & Analysis

> A data science midterm project exploring real-world shopping behavior through systematic data cleaning, transformation, and preprocessing using R.

---

## 📌 Project Overview

This project applies core data preprocessing techniques to a consumer shopping dataset. From handling messy missing values to taming wild outliers, every step mirrors what a real data scientist faces before any model can be trained or insight drawn. The dataset captures customer demographics and purchasing patterns — and our job was to make it analysis-ready.

---

## 📂 Dataset

| Property | Details |
|---|---|
| **File** | `Midterm_Dataset_Section_A.xlsx` |
| **Key Features** | Age, Gender, Item Purchased, Frequency of Purchases, Purchase Amount (USD), Review Rating, Previous Purchases, Subscription Status |

---

## 🔧 What Was Done — Step by Step

### 1. 📥 Data Loading
Loaded the Excel dataset using the `readxl` package and performed an initial inspection to understand structure and dimensions.

---

### 2. 🔍 Missing Value Detection & Imputation
Identified missing values using `is.na()` and `colSums()`, then handled them in **two strategies**:

| Strategy | Numeric Columns | Categorical Columns |
|---|---|---|
| **Mean + Mode** | `Age` → filled with **mean** | `Gender`, `Item Purchased`, `Frequency of Purchases` → filled with **mode** |
| **Median + Mode** | `Age` → filled with **median** | Same categorical columns → filled with **mode** |

Visualized missingness patterns using the `naniar` package:
- `vis_miss()` — heatmap of missing data across the dataset
- `gg_miss_var()` — bar chart of missing values per variable

---

### 3. ⚖️ Class Imbalance Handling
The `Gender` column was imbalanced (more Females than Males). Two resampling techniques were applied:

- **Undersampling** — reduced the majority class (Female) to match minority class (Male) size
- **Oversampling** — duplicated the minority class (Male) with replacement to match majority class size

---

### 4. 🧹 Duplicate Removal
Used `dplyr::distinct()` to identify and remove duplicate rows, ensuring every observation is unique.

---

### 5. 🔄 Variable Transformation

**Numeric → Categorical (`numToCat`)**
Converted `Age` into meaningful life-stage groups:

| Age Range | Label |
|---|---|
| ≤ 30 | Young |
| 31–50 | Middle-aged |
| > 50 | Senior |

**Categorical → Numeric (`catToNum`)**
Encoded `Subscription Status`:
- `"Yes"` → `1`
- `"No"` → `0`

---

### 6. 📏 Normalization
Applied **Min-Max Normalization** to `Previous Purchases`, scaling values to the [0, 1] range:

```
normalized = (x - min(x)) / (max(x) - min(x))
```

---

### 7. 🎯 Outlier Detection & Removal
Used boxplots to visually detect outliers in `Age`. Filtered and confirmed extreme values (256, 235) — clearly data entry errors — then removed them. Post-removal boxplot confirmed a clean distribution.

---

### 8. 📊 Summary Statistics
Computed descriptive statistics (`summary()`) on the cleaned dataset for:
- `Age`
- `Purchase Amount (USD)`
- `Review Rating`
- `Previous Purchases`

---

## 📦 Libraries Used

| Package | Purpose |
|---|---|
| `readxl` | Reading Excel files |
| `naniar` | Missing data visualization |
| `dplyr` | Data manipulation & filtering |

---

## 🗂️ Datasets Produced

| Dataset Name | Description |
|---|---|
| `main_Dataset` | Original raw dataset |
| `omitInstances_Dataset` | NAs removed via listwise deletion |
| `meanMode_Dataset` | NAs imputed with mean/mode |
| `medianMode_Dataset` | NAs imputed with median/mode |
| `undersampled_Dataset` | Class-balanced via undersampling |
| `oversampled_Dataset` | Class-balanced via oversampling |
| `duplicateRemoved_Dataset` | Duplicates removed |
| `numToCat_Dataset` | Age converted to categories |
| `catToNum_Dataset` | Subscription Status encoded numerically |
| `normalized_Dataset` | Previous Purchases normalized |
| `outliersRemoved_Dataset` | Outliers removed from Age |

---

## 🚀 How to Run

1. Open R or RStudio
2. Install required packages (first run only):
   ```r
   install.packages(c("readxl", "naniar", "dplyr"))
   ```
3. Update the file path in `read_excel(...)` to match your local dataset location
4. Run the script top to bottom

---

## 👤 Author

**Course:** Introduction to Data Science  
**Section:** A  
**Exam:** Midterm Lab Project  
**Institution:** American International University-Bangladesh (AIUB)
