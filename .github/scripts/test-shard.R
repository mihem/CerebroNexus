## Run one shard of the test suite: every SHARDS-th test file, starting at
## SHARD, in alphabetical order. R-tests.yaml runs the shards at once.
shard <- as.integer(Sys.getenv("SHARD", "1"))
shards <- as.integer(Sys.getenv("SHARDS", "1"))
stopifnot(!is.na(shard), !is.na(shards), shard >= 1L, shard <= shards)

files <- sort(list.files("tests/testthat", pattern = "^test.*\\.[rR]$"))
## testthat matches `filter` against the file path without ".R".
names <- sub("\\.[rR]$", "", sub("^test-?", "", files))
mine <- names[seq_along(names) %% shards == shard %% shards]
cat(sprintf(
  "Shard %d/%d: %d of %d test files\n",
  shard,
  shards,
  length(mine),
  length(names)
))

escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
testthat::test_local(
  ".",
  filter = paste0(
    "(^|/)(test[-_])?(",
    paste(escape(mine), collapse = "|"),
    ")$"
  ),
  reporter = testthat::default_reporter(),
  stop_on_failure = TRUE
)
