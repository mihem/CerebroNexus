# Choose a deterministic initial sample cohort for a large repertoire

Keeps all samples when the complete repertoire fits the graph guard. For
an atlas-scale repertoire, samples with the most unique CDR3s are added
until a conservative target is reached. Per-sample counts deliberately
overestimate the union, so shared clonotypes cannot push the result over
the budget.

## Usage

``` r
hla_choose_initial_samples(
  segments,
  samples,
  target = HLA_MOTIF_INITIAL_TARGET,
  max_total = HLA_MOTIF_MAX_TOTAL,
  bin_target = HLA_MOTIF_INITIAL_BIN_TARGET,
  max_bin = HLA_MOTIF_MAX_BIN
)
```

## Arguments

- segments:

  Parsed receptor segments with \`sample\` and \`cdr3\` columns.

- samples:

  Sample levels in display order.

- target:

  Conservative sum-of-per-sample unique-CDR3 budget.

- max_total:

  Hard all-cohort unique-CDR3 guard.

- bin_target:

  Conservative per-length-bin budget.

- max_bin:

  Hard per-length-bin guard.

## Value

A character vector of selected sample levels.
