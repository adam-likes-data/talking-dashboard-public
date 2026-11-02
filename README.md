# Seeing who we reach

Is NHS Talking Therapies reaching everyone in Norfolk and Waveney who could benefit?

A small R Shiny dashboard built from NHS England's published quarterly NHS Talking Therapies data. It compares Norfolk and Waveney with England, the East of England, similar areas, or areas you choose.

**Status:** work in progress. The data preparation is complete, overview, compare, who we reach, how they benefit, and about the data pages are built. Other pages and views tbc, potential next step is to build 'data-raw/area-profile'
| File | Source | Name must contain |
|---|---|---|
| English Indices of Deprivation 2025, File 7 (scores and population) | GOV.UK | `IoD2025` or `File_7` |
| Rural Urban Classification (2021) of LSOAs in England and Wales | ONS Open Geography Portal | `rural` or `RUC` |
| LSOA (2021) to Sub ICB Locations ... (April 2026) lookup | ONS Open Geography Portal | `SICBL` |