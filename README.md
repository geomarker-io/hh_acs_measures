# Harmonized Historical ACS Data

## About

This R code generates the Harmonized Historical American Community Survey (ACS) data, which are select ACS census tract-level characteristics generally available from 2013 onward. A grouped summary of the selected characteristics are:

- **Population**: number of kids, number of households with kids, single-parent households, racial and ethnic composition, limited English speaking households
- **Socioeconomic**: education, income, poverty, employment, health insurance, SNAP receipt
- **Housing**: vacancy, age, substandard conditions, monthly rent and housing costs

## Data

Data are available at `s3://codec-data/harmonized_historical_acs_data/`. See the `tabular-data-resource.yaml` file there for [CODEC metadata](https://geomarker.io/CODECtools/articles/codec-metadata.html).

After downloading this folder to your working directory, read this data into R with:

```
CODECtools::read_tdr_csv("harmonized_historical_acs_data")
```

## Notes

- Some data are unavailable in certain earlier years because the question was not included in the ACS that year.  Additionally, data summaries may be [suppressed ](https://www.census.gov/programs-surveys/acs/technical-documentation/data-suppression.html) by the census bureau.
