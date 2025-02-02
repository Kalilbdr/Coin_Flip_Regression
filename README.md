# Coin-Flip Experiment: A Regression Analysis  
*MATH 408 - Regression Methods, EPFL (2024/25)*  
**Authors:** Kalil Bouhadra, Gabriel Marival  

## Project Overview
This project analyzes coin-flip outcomes using **regression models** to detect potential biases.  
We leverage **GLM binomial models**, **fixed/random effects**, and **penalized splines** to assess statistical properties.  

**Data:** Extracted from **Bartoš et al. (2023)**, consisting of **350,757 flips** aggregated by participant and coin.

## Repository Structure

| File | Description |
|------|------------|
| `README.md` | Project documentation |
| `Bouhadra-Marival-RMProject-2025.pdf` | Final report |
| `Project_Regression_2024.pdf` | Original paper from Bartoš et al. (2023) |
| `code.R` | R script for regression analysis |
| `data-agg.csv` | Aggregated coin-flip results |
| `df-time-agg.csv` | Aggregated time-based results |
| `figures/` | Generated plots |

**Note:** Figures are generated during script execution.

## Installation & Dependencies

### **1️⃣ Prerequisites**
- **R 4.x+**
- **RStudio (optional)**  

### **2️⃣ Install Required Packages**
Run the following in R:  
```r
install.packages(c("ggplot2", "dplyr", "stringr", "SMPracticals", "lme4", "mgcv"))


