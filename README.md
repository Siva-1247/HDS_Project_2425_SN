# HDS_Project_2425_SN

# Geospatial Analysis of COVID-19 Vaccination, A Neighbourhood Level Analysis in Ireland

This repository presents the code, data processing scripts, and modeling outputs for a thesis project called **Geospatial Analysis of COVID-19 Vaccination, A Neighbourhood Level Analysis in Ireland**, conducted at the **Local Electoral Area (LEA)** level. While Ireland achieved commendably high overall vaccine coverage, this study reveals important **sub-national disparities** that have remained underexplored in finer spatial detail.

---

## Overview

This project investigates what can be possible **spatial and socio-demographic determinants** of COVID-19 vaccination uptake across Ireland’s 166 LEAs. Using a combination of **publicly available administrative data**, **Bayesian modeling**, and **spatial statistical methods**, we identify and quantify variables that are associated with vaccine coverage — with a particular focus on **education**, **age, health**, *spatial and temporal aspects* and *accessibility to vaccination services**. Our primary focus was on the primary vaccine dose rates.

---

## Research Questions

- What demographic variables are associated with vaccination uptake at the LEA level?
- Does geographic accessibility to vaccine services significantly influence uptake?
- Can spatial autocorrelation help identify hidden neighbourhood-level effects in vaccination disparities?
- How the vaccination trend has evolved over the vaccination roll out period across the LEAs in Ireland?

---

## Methods Summary

This study adopts an **ecological design** and integrates multiple data sources:

| Data Type                      | Source                                                            |
|-------------------------------|-------------------------------------------------------------------|
| Vaccination Rates             | Health Protection Surveillance Centre (HPSC)                     |
| Demographics 
        (Age, Education, Health)| Central Statistics Office (CSO), Census 2022                     |
| Health Service Locations      | HSE vaccine centre registry, GP and pharmacy listings            |
| LEA Boundaries                | Ordnance Survey Ireland (OSI)                                    |
| Accessibility Scores          | Engineered using drive-time isochrones with OpenRouteServices    | 
|Depriviation Index             | Area-level deprivation metrics derived from the Pobal Haase-Pratschke (HP) Deprivation Index |

### Data Engineering:
- Demographic variables were ALR-transformed to handle **compositional structure**.
- Accessibility scores were computed as **proportions of LEAs overlapped by travel isochrones** from vaccine service locations.
- Demographic and accessibility predictors were combine by common LEA keys
- Vaccination data was combined with LEA geometry data

### Modelling Techniques:
- **Beta regression**: For modeling bounded uptake rates of primary vaccine dose (0 < y < 1)
- **CAR (Conditional Autoregressive) models**: To account for **spatial autocorrelation**
- **Hierarchical nonlinear beta regression**: Using a 4-parameter **sigmoid growth curve** to model **temporal uptake trends**
- **Posterior predictions and marginal effect plots**: Used for interpreting and visualizing model results

---

## Key Findings

- **Age composition (55–70)** and **upper secondary education levels** were consistently strong **positive predictors** of vaccine uptake.
- **Health status** and **accessibility** showed **weak or statistically non-significant effects**.
- The **CAR model** revealed spatial clustering, indicating **unmeasured neighbourhood-level influences**.
- Marginal effects plots and predicted uptake trajectories highlighted clear **temporal and structural differences** across LEAs.

These results suggest that **demographic structure**, rather than pure geographic accessibility, plays a stronger role in shaping vaccine behavior in Ireland. Spatially targeted interventions may be required to close local equity gaps.Limitations of the current study have been outlined in detail in the thesis.

---

### `Code_Datasets/`
- `Accessibility_Codes/` — Scripts for building accessibility metrics  
- `Accessibility_Data/` — Shapefiles and processed accessibility datasets  
- `Dashboard/` — Code for interactive Shiny app  
- `Data_Scraping_Codes/` — Scripts for sourcing GP and pharmacy locations  
- `Dataset_Merging_Codes/` — Scripts for combining demographic and vaccination data  
- `Final_Merged_Dataset/` — Final analysis-ready merged dataset (CSV)  
- `Geocoding_Codes/` — Address and coordinates processing scripts  
- `Merging_ShapeFile_Codes/` — Scripts for handling LEA shapefile joins  
- `Misc_Files/` — Supplementary scripts and utility functions  
- `Model_Results/` — Model output files, summaries, and diagnostics  
- `Modelling/` — Core modeling scripts (beta, CAR, nonlinear sigmoid)  
- `Poster Codes/` — Code for generating poster graphics and visuals  
- `Vacc_Rates&Geocoded_Data/` — Cleaned vaccination rate data and geocoded inputs  
- `rsconnect/shinyapps.io/siva47/` — Deployment files for hosted Shiny app  
- `Thesis_V1.Rmd` — RMarkdown version of thesis  
- `Thesis_V1.pdf` — Final thesis PDF  
- `Draft_1.pdf`, `Draft_1.tex`, `Draft_1.log` — Early drafts and LaTeX versions 
- `arxiv.sty` — Style file for potential preprint submission

### Root Files
 
- `README.md` — Project overview (this file)  


