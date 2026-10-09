# Offline calculation regression fixtures

`legacy-tract.rds` and `legacy-zcta.rds` were produced by the calculation and
assembly expressions in the two build scripts at commit
`38a3ffd`, immediately before the YAML migration. The source responses come
from `acs.R`, not from Census records. They exercise all implemented years and
measures, distinct component estimates, missing estimates and MOEs, a scaffold
row absent from ACS responses, ZCTA boundary transitions, Connecticut ID
mapping, inflation adjustment, and final rounding/types.

The migration tests compare the new catalog-driven output with these frozen
results. Zero-denominator behavior is checked separately because this migration
explicitly returns missing fractions instead of nonfinite values.
