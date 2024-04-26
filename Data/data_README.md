Files in the Data folder:

-   Alldata.xlsx: "projected weight Bob" is based on sum of projwt from allshrimp file. bob-residual is projected weight Bob minus total weight. BCI is bob-residual divided by sample_ct. There appear to be some vessel-catch date combinations with more than one bulk sample, and I cannot find a unique identifier for the bulk samples. Otherwise I was generally able to recreate the residuals.

-   allshrimp.xlsx: rework tab has length, age, area, month, year, lots of other info, for each *individual* shrimp sampled (rather than average across each bulk port sample). projwt is the projected weight based on a length-weight relationship (source/equation?).

-   shrimp_tickets.csv: Landings data. PRIVATE DO NOT SHARE.

-   Dan_aggregated_Lengths.csv: Generated by Code/lengths_for_Dan.R, average length across northern and southern areas by age, month, and year. Average is weighted by number of samples in each statistical area. Was generated for bioeconomic model that did not happen.

-   Dan_cpp.csv: shrimp_tickets.csv joined with BCI tab of alldata.xlsx by lengths_for_dan.R. PRIVATE DO NOTE SHARE.

-   Kiva.xlsx: length-weight, BCI, CPP data. Used for length-weight relationship in yield-per-recruit model.

-   VPE-update-02022024.xlsx: VPE

    -   Larval release year: year that larvae were released in spring (age 0s in fall catch)

    -   Spawn year: first year spawning (age 1s, year recruit to fishery)

    -   North/South/OR Age X: Number of shrimp that survived up to, and then died, at a given age

    -   North/South/OR VPE: sum of regional age-specific columns for the row, estimate of total recruitment that year

    -   Spawners: Number of shrimp that spawned the row's recruitment. Sums shrimp that survived through to age 2 and 3 from cohort two rows above, and shrimp that survived to age 3 three rows above. If shrimp only survived through to age 1, they died (were caught) before they had a chance to spawn in the fall. Assumes no survival \>3.

-   shrimp age comp and count.xlsx: % composition, count per pound, and sample size by year, area, and month. One row for each port sample collected.

-   Compiled_Lengths.csv: generated by Code/consolidate_length_data.R. Average length (mm) by age, area, month, year. Where individual data available, also contains sample size and sd.

-   shrimp_length_files: one spreadsheet for each year of mean length by area, month, age. Formatted generally consistently but very poorly.

-   BEUTI_monthly.csv, CUTI_monthly.csv: CUTI and BEUTI, downloaded from https://oceanview.pfeg.noaa.gov/products/upwelling/cutibeuti on 11/17/23.

-   nceiErsstv5_LonPM180_24fe_b874_defd.csv: SST data, downloaded from https://coastwatch.pfeg.noaa.gov/erddap/griddap/nceiErsstv5_LonPM180.html on 1/23/24.