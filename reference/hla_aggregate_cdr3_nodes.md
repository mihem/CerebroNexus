# Aggregate parsed segments into unique-CDR3 nodes carrying distributions

Node key = unique CDR3 amino-acid string, or \`(V gene, CDR3)\` when
\`by_v\` is TRUE. \`clone_count\` = number of cells carrying that node
key. Categorical metadata columns are summarised as their most-common
value (\`\_mode\`) plus a compact "N types: A (5), B (2)" distribution
string (\`\_dist\`) so the tooltip can show provenance without
collapsing it to a single label.

## Usage

``` r
hla_aggregate_cdr3_nodes(
  seg,
  meta_cols = character(0),
  context_col = NULL,
  by_v = FALSE,
  context_summary = hla_context_summary,
  .keys = NULL
)
```

## Arguments

- seg:

  Output of \[hla_parse_ir_segments()\].

- meta_cols:

  Character vector of metadata columns to summarise per node.

- context_col:

  Optional name of a per-cell context column. When given, the node gets
  a \`context_summary\` value instead of a plain mode, plus the usual
  \`\_dist\` string.

- by_v:

  When TRUE, aggregate with \`(v_gene, cdr3)\` as the node key.

- context_summary:

  How to collapse that column's per-cell values to one node value.
  Defaults to \[hla_context_summary()\] (Class I / Class II / "Mixed" /
  "Unknown"). The Class I x Class II pair scope passes
  \[hla_pair_class_summary()\] instead. It is a parameter and not a
  hardcoded call because these columns share one property that the plain
  mode destroys: a node spanning BOTH values is the finding, not a tie
  to be broken.

- .keys:

  Precomputed node keys for internal graph construction.

## Value

A per-node data.frame, or NULL when \`seg\` is empty.
