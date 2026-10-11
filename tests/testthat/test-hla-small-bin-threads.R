test_that("small HLA bins avoid thread startup without changing motif results", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("igraph")
  old <- options(sd_num_thread = 2L)
  on.exit(options(old), add = TRUE)
  distance <- stringdist::stringdistmatrix
  used <- integer()
  testthat::local_mocked_bindings(
    stringdistmatrix = function(..., nthread) {
      used <<- c(used, nthread)
      distance(..., nthread = nthread)
    },
    .package = "stringdist"
  )
  sequences <- apply(
    expand.grid(rep(list(c("A", "C", "G", "T")), 5L)),
    1L,
    paste0,
    collapse = ""
  )
  for (n in c(2L, 149L, 256L, 257L)) {
    df <- data.frame(cdr3 = sequences[seq_len(n)], stringsAsFactors = FALSE)
    actual <- hla_process_length_group(df)
    reference <- distance(df$cdr3, df$cdr3, method = "hamming", nthread = 2L)
    one <- distance(df$cdr3, df$cdr3, method = "hamming", nthread = 1L)
    expect_identical(one, reference)
    pairs <- which(reference == 1 & upper.tri(reference), arr.ind = TRUE)
    expect_identical(actual$edges$from, df$cdr3[pairs[, 1]])
    expect_identical(actual$edges$to, df$cdr3[pairs[, 2]])
    expect_identical(tail(used, 1L), if (n <= 256L) 1L else 2L)
    expect_identical(getOption("sd_num_thread"), 2L)
  }
})
