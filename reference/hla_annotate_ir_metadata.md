# Join cell metadata onto a named immune-repertoire list

The metadata index is resolved once across every sample. Existing
repertoire columns win, and the repertoire list name remains the
structural sample id.

## Usage

``` r
hla_annotate_ir_metadata(data, metadata)
```

## Arguments

- data:

  Named list of scRepertoire-style data.frames.

- metadata:

  Cell metadata containing \`cell_barcode\`.

## Value

\`data\` with missing metadata columns added by barcode.
