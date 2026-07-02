# Modelling of HIV in South African female sex workers

## Code and data for modelling analysis

This repository contains the model code, processed model outputs, and R scripts used in the analysis for the manuscript: 

**“Do standard model assumptions realistically represent HIV dynamics in sex workers? A modelling analysis of South African data.”**

The analysis is based on version 4.7 of the Thembisa model. Six scenarios were implemented that combine different assumptions about female sex worker characteristics (age and duration of sex work) and client-to-FSW HIV transmission risk.

## Repository structure

- data/ – model output files used for the analysis
- Rscripts/ – R scripts used to process the outputs and generate the figures
- thembisa_v4.7/ – modified Thembisa model code and scenario-specific input files

Within thembisa_v4.7/:

- main_analysis/ contains the six main scenarios (1a–3b)
- sensitivity_analysis/ contains additional sensitivity analyses

## Analysis

The R scripts read the model outputs in data/ and reproduce the figures used in the manuscript.

## Acknowledgment

This work uses the Thembisa model developed by Leigh F. Johnson and colleagues. If you use or adapt this code, please cite:

    Johnson LF, Meyer-Rath G, Dorrington RE, Puren A, Seatlhodi T, Zuma K, Feizzadeh A. (2022). The effect of HIV programs in South Africa on national HIV incidence trends, 2000–2019. Journal of Acquired Immune Deficiency Syndromes, 90: 115–123.
