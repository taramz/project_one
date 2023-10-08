# Project One

This project aims to explore the effects of smoking during pregnancy (SDP) and environmental tobacco smoke (ETS) exposure on adolescent self-regulation, substance use, and externalizing. The data used from this project comes from two studies, <a href='https://pubmed.ncbi.nlm.nih.gov/27818283/'>one of which was published in 2016</a>. The data has not been made publicly available, but the information provided in this documentation will provide a thorough explanation of the data, while respecting data privacy. The code for this project will be provided, as well as summary tables that allow some insight into the data used.

Several packages were used in the this analysis. Their names and uses are outlined below:
- Data Manipulation: tidyverse
- Data Visualization: ggplot2, gridExtra, ghibli
- Displaying Tables: kableExtra, gtsummary

Table of Contents:
- project_script.R
- Project_One.Rmd
- Project_One.pdf

The project_script.R script contains the pre-processing that needed to be done prior to the analysis of the data, as well as the data manipulation and visualization performed. This pre-processing includes converting the race, sex, ethnicity, employment, and education columns from binary numbers to the actual values they represent (ex. turning 1 in the sex column to F). The code is extensively commented to enhance understanding.

This project has been completed with help from Dr. Lauren Micalizzi at Brown University.
