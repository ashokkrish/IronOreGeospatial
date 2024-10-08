# Geostatistical analysis of an Iron Ore Deposit

## Overview

This data frame consists of 1243 rows and 8 columns. The column names are

    |---East (X)
    |---North (Y)
    |---Elevation (Z)
    |---Fe (%)
    |---AL2O3 (%)
    |---Mn (%)
    |---P (%)
    |---Lithology

- `Fe (%)` is the concentration of Iron in the deposit.
- `AL2O3 (%)` is the concentration of Aluminium Oxide in the deposit.
- `Mn (%)` is the concentration of Manganese in the deposit.
- `P (%)` is the concentration of Phosphorous in the deposit.
- `Lithology` is a categorical variable (1 or 2).

There were several rows with partial or completely missing values (replaced by -99). We removed such rows and the cleaned data frame consists of 607 rows and 8 columns. We noticed there were repeated data collected at the same East (X) and North (Y) but at varying Elevations (Z). We decided to merge such rows into a single row by averaging the values for the numerical variable and replace the mode value for the categorical variable (Lithology). The merged data frame consists of 181 rows and 8 columns.

**Descriptive Statistics**

- Mean, five-number summary, standard deviation etc for Fe concentration. DONE.

**Correlation Matrix**

<img width="305" alt="image" src="https://github.com/user-attachments/assets/e20ef366-7a22-4e2f-8243-c2e226bab250">


**Frequency Distributions and Cross Tables**

- Frequency and relative frequency table for Fe Concentration. DONE

**Exploratory Data Analysis**

- Histogram for Fe Concentration. DONE

**Variogram**

![image](https://github.com/user-attachments/assets/e63d4fb9-8069-4bc5-bcf1-9c871251638e)

**Variogram Parameters**

| Parameter | Definition |
| --------- | ---------- |
| sill | Limit of the variogram tending to infinity lag distances. |
| range | The distance in which the difference of the variogram from the sill becomes negligible. |
| nugget | The height of the jump of the semivariogram at the discontinuity at the origin. |

**Variogram Models**

| Model | Definition |
| --------- | ---------- |
| Exp | Exponential. |
| Sph | Spherical. |
| Gau | Gaussian. |

**Kriging**

## Data Source

[Dr. Nasser Madani](https://research.nu.edu.kz/en/persons/nasser-madani).

## Collaborators

[Dr. Nasser Madani](https://research.nu.edu.kz/en/persons/nasser-madani), [Dr. Emmanouil Varouchakis](https://github.com/evarouchakis) and [Dr. Ashok Krishnamurthy](https://github.com/ashokkrish).

## Feedback

We welcome questions, insights, and feedback. You can also open an issue if you find a bug, or have a suggestion.
