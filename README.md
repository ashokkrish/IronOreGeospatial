# Geostatistical analysis of an Iron deposit dataset

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

- `Fe (%)` concentration of Iron in the deposit.
- `AL2O3 (%)` concentration of Aluminium Oxide in the deposit.
- `Mn (%)` concentration of Manganese in the deposit.
- `P (%)` concentration of Phosphorous in the deposit.

There were several rows with partial or completely missing values (replaced by -99). We removed such rows and the cleaned data frame consists of 607 rows and 8 columns. We noticed there were repeated data collected at the same East (X) and North (Y) but at varying Elevations (Z). We decided to merge such rows into a single row by averaging the values for the numerical variable and replace the mode value for the categorical variable (Lithology). The merged data frame consists of 181 rows and 8 columns.

## Key features

## Credits

## Feedback
