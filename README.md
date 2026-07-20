# NYC Housing on Public Vacant Lots

An analysis of publicly owned, residentially zoned, vacant lots in New York City to estimate the potential affordable housing capacity available to the Mamdani administration in pursuit of its 200,000-unit housing goal.

## Overview

Mayor Mamdani has pledged to build 200,000 new, publicly subsidized, affordable, rent-stabilized homes. This analysis asks a foundational question: how many units could realistically be built on land the city already owns?

Using NYC's PLUTO dataset, this project identifies every publicly owned, residentially zoned, vacant lot within a half mile of a subway station across all five boroughs. It then estimates the number of housing units that could be built on each lot using the lot's zoning district, floor area ratio (FAR), lot area, and an average HPD unit size of 900 square feet. The results are aggregated by borough and neighborhood to identify priority clusters for HPD site selection.

The full write-up and findings are published on [NYC Data Stories](https://sethgordon123.substack.com).

## Key Findings

- Nearly 6,000 publicly owned, residentially zoned, vacant lots exist within a half mile of a subway station across the five boroughs
- These lots could yield approximately 120,000 units under current zoning
- Despite having the fewest lots, Manhattan has the highest average units per lot (90+) due to dense residential zoning
- Priority clusters for HPD site selection include Inwood, Greenpoint, Tottenville, and the Rockaways
- Upzoning from the recent City of Yes rezoning could close much of the remaining gap to Mamdani's 200,000-unit goal

## Data Sources

- [NYC PLUTO (Primary Land Use Tax Lot Output)](https://www.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page) — MapPLUTO 25v3.1, NYC Department of City Planning. Provides lot-level land use, ownership, zoning, and geometry data for all parcels in New York City.
- [MTA Subway Stations](https://data.ny.gov/Transportation/MTA-Subway-Stations/39hk-dx4f) — MTA via NYC Open Data. Used to create a half-mile buffer around all active subway stations to filter lots by transit access.

## Methodology

1. **Filter PLUTO** for vacant lots (land use code 11) with residential zoning (R districts) that are publicly owned (owner type not "P")
2. **Remove lots with missing coordinates** to prevent spatial errors
3. **Transform to local CRS (EPSG:2263)** for accurate distance calculations in feet
4. **Create a half-mile (2,640 foot) buffer** around all subway stations and merge into a single shape
5. **Filter PLUTO lots** to only those intersecting the subway buffer
6. **Apply minimum lot size filters** by zoning district (R1: 5,700 sq ft; R2: 3,800 sq ft; R3+: 1,700 sq ft)
7. **Estimate potential units** by zoning type:
   - R1/R2: 1 unit (single family maximum)
   - R3-2: lot area / 625 sq ft per unit
   - R3 (other): lot area / 1,200 sq ft per unit
   - R4/R5: lot area / 900 sq ft per unit
   - R6-R10: (lot area x FAR) / 900 sq ft per unit
8. **Calculate net new units** by subtracting existing residential units from potential units
9. **Aggregate by borough** and export cleaned dataset for spatial visualization in QGIS

## Requirements

```r
install.packages(c("tidyverse", "sf", "janitor", "tidyr"))
```

Data files should be placed in the `Datasets/` folder before running the scripts. PLUTO can be downloaded directly from the NYC Department of City Planning link above. The MTA subway stations CSV is available via the NY Open Data portal.

## Author

Seth Gordon | Master of Urban Planning, NYU Wagner | [NYC Data Stories](https://sethgordon123.substack.com) | [LinkedIn](https://www.linkedin.com/in/seth-gordon-nyc)
