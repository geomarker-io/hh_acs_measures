# Connecticut tract identifiers

`ct_tract_crosswalk_2022_2020.csv` contains the distinct tract identifier pairs
from the CT Data Collaborative's [2022 tract crosswalk](https://github.com/CT-Data-Collaborative/2022-tract-crosswalk),
at commit `5dc032c0aadf1e79e834f97367d0c4e29604e6be`:

https://raw.githubusercontent.com/CT-Data-Collaborative/2022-tract-crosswalk/5dc032c0aadf1e79e834f97367d0c4e29604e6be/2022tractcrosswalk.csv

The source's `Tract_fips_2022` and `tract_fips_2020` columns are renamed
`geoid_2022` and `geoid_2020`, deduplicated, and sorted by `geoid_2022`.
No estimates are modified or interpolated. The source derives these tract
relationships from the Census Bureau's 2020 and 2022 TIGER/Line files.

The resulting 879 pairs are unique in both directions and cover all Connecticut
tracts in the 2020 and 2022 cartographic boundary files used for validation:

- https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_09_tract_500k.zip
- https://www2.census.gov/geo/tiger/GENZ2022/shp/cb_2022_09_tract_500k.zip

Five additional tracts in the full 2022 TIGER/Line file contain only water and
are absent from this project's cartographic scaffold: `09120990000`,
`09190990000`, `09170990000`, `09130990100`, and `09180990100`. They are not
mapped or aggregated. This preserves the existing exclusion of water-only
tracts, including the 2020 water tract that split into two in 2022.

The helper restores the 2020 GEOID for ACS years 2022 onward, before any measure
is calculated or joined. Earlier years and other states retain their source
GEOIDs. Unexpected unmapped Connecticut IDs raise an error. Corrected tract
results use a separate cache namespace so previous cached missing values
cannot be reused.
