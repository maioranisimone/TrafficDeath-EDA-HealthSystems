# Exploratory Data Analysis of Estimated Traffic Fatality Rate on WHO Dataset
## Data Science for Health Systems Thesis
### Author: Simone Maiorani

Department of Engineering, University of Perugia, Perugia, Italy

## Abstract
This repository contains the code and analysis for the Exploratory Data Analysis (EDA) of the estimated road traffic fatality rates provided by the World Health Organisation (WHO). The analysis is part of the "Data Science for Health Systems" thesis conducted at the University of Perugia. The dataset covers the years 2000 to 2019 and includes information on gender, geographical areas, and other relevant factors.

## Introduction
Road traffic injuries are a significant public health concern, and this analysis aims to explore trends and differences in estimated road fatality rates. The dataset is obtained from the WHO, focusing on factors such as gender and geographical areas. The analysis utilizes the R programming language for data processing and visualization.

## Dataset
The dataset comes from the WHO web portal and includes estimated road fatality rates per 100,000 population. It employs various estimation methodologies based on the completeness of death registration data. The features include GeoArea, State, Year, Sex, Value, LowerConfidenceInterval, and UpperConfidenceInterval.

## Dataset Modelling
The data preprocessing stage involves cleaning, renaming features, and handling missing values. The final dataset includes features like GeoArea, State, Year, Sex, Value, LowerConfidenceInterval, and UpperConfidenceInterval.

## Exploratory Analysis
The EDA phase explores the distribution of values, gender differences, trends over the years, and geographical variations. Graphs and statistical tests are employed to highlight patterns and disparities. The analysis indicates variations in fatal accidents between genders, trends over the years, and significant differences among geographical areas.

## Key Findings
**Gender Disparity**: Fatal traffic accidents are more prevalent among males than females.
**Temporal Trends**: The analysis reveals an increase in values after 2016.
**Geographical Variations**: African areas exhibit higher values compared to European areas.

## Statistical Tests
Statistical tests, including Shapiro's test, Bartlett's test, and the Kruskal-Wallis test, confirm the observed patterns. Post-hoc analyses with Mann-Whitney tests identify specific pairs of geographic areas with significant differences.

## Key Tests:
**Mann-Whitney**: Confirms significant differences between male and female groups.
**Kruskal-Wallis**: Indicates significant differences among geographical areas.

## Conclusion
The exploratory analysis provides valuable insights into the factors influencing road fatality rates. The observed patterns warrant further investigation, and the results serve as a foundation for future studies in the field of road safety.
