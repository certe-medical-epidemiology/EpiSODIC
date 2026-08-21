# Geographic reference data for the choropleth panel

Geography here is operator-suppliable rather than tied to any single
mapping package or country: `pathogen`, institution types, and every
other domain concept in this codebase are already operator-defined,
unconstrained values, and geography follows the same principle. This is
the same shape of solution the package already uses elsewhere for
optional, instance-specific data: a shipped default (here, geometry for
the Netherlands' four-digit postcodes, `data-raw/geo_postcodes4_nl.R`
documents its provenance) that any operator can override by pointing
`EPISODIC_GEO_DATA` at their own file - one environment variable, the
same pattern `EPISODIC_CONFIG` and `EPISODIC_PALETTE_CONFIG` already
establish.

## Details

The contract is minimal and country-agnostic: an `sf` object with a `pc`
column (matching whatever an operator's own `episodic_case.pc` values
are - postcodes, zip codes, municipality codes, anything; this package
never validates or interprets that column beyond joining it) and a
`geometry` column. `sf`/GDAL/GEOS/PROJ are a real system-level
dependency beyond what CRAN alone can supply, so this whole feature is
guarded end to end: no `sf` installed means the geography panel falls
back to the existing PC bar breakdown, exactly as before this existed.

A second, entirely independent piece of geographic data is supported on
top of this: `EPISODIC_GEO_DATA_OVERLAY`
([`episodic_geo_overlay_resolve()`](https://certe-medical-epidemiology.github.io/EpiSODIC/reference/episodic_geo_overlay_resolve.md)),
an outline layer (region boundaries

- provinces, municipalities, whatever an operator wants for orientation)
  drawn with colour but no fill on top of the choropleth. It has no `pc`
  contract at all, since it carries no case counts to join.
