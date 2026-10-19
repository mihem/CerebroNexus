# Source the Trekker pure helpers (trekker_gene_suggest,
# trekker_numeric_meta_cols) from the same inst/ file the running app sources, so
# the unit tests exercise the real runtime code rather than a copy.
sys.source(viewer_test_path("trekker", "helpers.R"), envir = environment())
