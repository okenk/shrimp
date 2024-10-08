<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13362802.svg)](https://doi.org/10.5281/zenodo.13362802)
<!-- badges: end -->

This respository contains non-confidential data, code, and manuscript text for:

Oken, K.L., S.D. Groth, D.S. Holland, A.E. Punt, E.J. Ward (*accepted*) Variability in somatic growth over time and space determines optimal season-opening date in the Oregon ocean shrimp (*Pandalus jordani*) fishery. *CJFAS*

## Files in Code folder

-   CMSI_symposium_plots.R: presentation style figures used for several talks

-   consolidate_length_data.R: consolidate length data from annual spreadsheets in Data/shrimp_length_files and Data/allshrimp.xlsx into a single csv file used for growth model. STEP 1 OF ANALYSIS

-   growth_model.rds: potentially delete

-   growth_model.stan: this is the main MARSS growth model

-   growth_model\_[description].stan: exploring variations on growth models. These did not pan out.

-   length_data_manip.R: preparing length data for growth model. STEP 2 OF ANALYSIS.

-   lengths_for_dan.R: reorganizing/summarizing data for Dan for 1) production function (abandoned) and 2) price - counter per pound relationship (used)

-   model_result_processing.R: potentially delete. I think anything important has been moved into other files.

-   run_covariate_models.R: run growth model with and without covariates. STEP 3 OF ANALYSIS.

-   YPR_modeling.R: run revenue-per-recruit model based on results of growth model. STEP 4 OF ANALYSIS.

## Files in the Data folder

-   shrimp_length_files: one spreadsheet for each year of mean length by area, month, age. Formatted generally consistently but very poorly.

-   Alldata.xlsx: "projected weight Bob" is based on sum of projwt from allshrimp file. bob-residual is projected weight Bob minus total weight. BCI is bob-residual divided by sample_ct. Ignored due to confidential information.

-   allshrimp.xlsx: rework tab has length, age, area, month, year, lots of other info, for each *individual* shrimp sampled (rather than average across each bulk port sample). projwt is the projected weight based on a length-weight relationship. Ignored due to large file size and some confidential columns. Available upon request.

-   BEUTI_monthly.csv, CUTI_monthly.csv: CUTI and BEUTI, downloaded from <https://oceanview.pfeg.noaa.gov/products/upwelling/cutibeuti> on 11/17/23.

-   Compiled_Lengths.csv: generated by Code/consolidate_length_data.R based on allshrimp.xlsx and annual spreadsheets in shrimp_length_files. Average length (mm) by age, area, month, year. Where individual data available, also contains sample size and sd.

-   Dan_aggregated_Lengths.csv: Generated by Code/lengths_for_Dan.R, average length across northern and southern areas by age, month, and year. Average is weighted by number of samples in each statistical area. Was generated for bioeconomic model that did not happen.

-   Dan_cpp.csv: Generated by Code/lengths_for_Dan.R, joins shrimp_tickets.csv (i.e., price data) with Alldata.xlsx (i.e., count per pound data). CONFIDENTIAL. (Ignored by git)

-   Kiva.xlsx: length-weight, BCI, CPP data. Used for length-weight relationship in yield-per-recruit model.

-   nceiErsstv5_LonPM180_24fe_b874_defd.csv: SST data, downloaded from <https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5_LonPM180.html> on 1/23/24.

-   shrimp age comp and count.xlsx: % composition, count per pound, and sample size by year, area, and month. One row for each port sample collected.

-   shrimp_tickets.csv: PacFIN pink shrimp landings tickets, 1981-2022. CONFIDENTIAL. (Ignored by git)

-   VPE-update-02022024.xlsx

    -   Larval release year: year that larvae were released in spring (age 0s in fall catch)

    -   Spawn year: first year spawning (age 1s, year recruit to fishery)

    -   North/South/OR Age X: Number of shrimp that survived up to, and then died, at a given age

    -   North/South/OR VPE: sum of regional age-specific columns for the row, estimate of total recruitment that year

    -   Spawners: Number of shrimp that spawned the row's recruitment. Sums shrimp that survived through to age 2 and
