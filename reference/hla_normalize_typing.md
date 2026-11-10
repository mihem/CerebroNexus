# Normalize any accepted HLA input into the canonical long table

Accepts: \* a canonical/near-canonical long data.frame (has \`sample\` +
\`allele\`); \* a wide data.frame (\`sample\` + \`HLA-A_1\`,
\`HLA-A_2\`, ... columns); \* a named list (\`sample -\>
c("HLA-A\*02:01", ...)\`).

## Usage

``` r
hla_normalize_typing(
  x,
  source_type = "unknown",
  typing_method = NA_character_,
  source_reference = NA_character_
)
```

## Arguments

- x:

  One of the accepted inputs.

- source_type:

  One of \[HLA_SOURCE_TYPES\]; default "unknown".

- typing_method, source_reference:

  Optional provenance strings.

## Value

A canonical long data.frame with a "qc" attribute (warnings df).

## Details

Output columns are exactly \[HLA_TYPING_COLUMNS\]. \`copy\` (1/2) is
assigned per (sample, locus) in input order. Unrecognisable alleles are
dropped from the table but reported via attribute "qc" (a data.frame of
warnings). Provenance defaults to \`source_type = "unknown"\` with a
blocking warning when absent – allele format is never used to guess
\`genotyped\`.

## Examples

``` r
typing <- hla_normalize_typing(list(
  donor1 = c("HLA-A*02:01", "HLA-B*07:02"),
  donor2 = c("A*01:01", "B*08:01")
))
typing
#>   sample donor_id locus copy      allele resolution source_type typing_method
#> 1 donor1     <NA> HLA-A    1 HLA-A*02:01    2-field     unknown          <NA>
#> 2 donor1     <NA> HLA-B    1 HLA-B*07:02    2-field     unknown          <NA>
#> 3 donor2     <NA> HLA-A    1 HLA-A*01:01    2-field     unknown          <NA>
#> 4 donor2     <NA> HLA-B    1 HLA-B*08:01    2-field     unknown          <NA>
#>   source_reference confidence
#> 1             <NA>         NA
#> 2             <NA>         NA
#> 3             <NA>         NA
#> 4             <NA>         NA
```
