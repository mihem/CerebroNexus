# Tests for the Cerebro_v1.3 hla_typing slot + getter/setter round-trip and
# backward compatibility with objects that predate the field.

make_minimal_cerebro <- function() {
  # A minimal object is enough to exercise the HLA slot: the getter/setter do
  # not touch expression/metadata. initialize() takes no args (fields are set
  # post-hoc), so a bare $new() is sufficient.
  Cerebro_v1.3$new()
}

test_that("addHLATyping / getHLATyping round-trips a named list", {
  crb <- make_minimal_cerebro()
  crb$addHLATyping(
    list(
      sample_1 = c("HLA-A*02:01", "HLA-B*08:01"),
      sample_2 = c("HLA-A*01:01")
    ),
    source_type = "genotyped"
  )
  t <- crb$getHLATyping()
  expect_true(hla_is_typing_table(t))
  expect_equal(length(unique(t$sample)), 2L)
  expect_true(all(t$source_type == "genotyped"))
})

test_that("addHLATyping accepts a pre-normalized canonical table unchanged", {
  crb <- make_minimal_cerebro()
  canon <- hla_normalize_typing(
    list(s1 = "HLA-A*02:01"),
    source_type = "synthetic"
  )
  crb$addHLATyping(canon)
  t <- crb$getHLATyping()
  expect_equal(nrow(t), 1L)
  expect_equal(t$source_type, "synthetic")
})

test_that("getHLATyping returns an empty canonical table when none is set", {
  crb <- make_minimal_cerebro()
  t <- crb$getHLATyping()
  expect_true(hla_is_typing_table(t))
  expect_equal(nrow(t), 0L)
})

test_that("an object predating the field still returns an empty table", {
  crb <- make_minimal_cerebro()
  # Simulate an older object by removing the field from the instance.
  # R6 fields cannot be `rm`'d, so emulate the deserialization gap by forcing
  # NULL — getHLATyping() must still yield an empty canonical table, not error.
  crb$hla_typing <- NULL
  expect_silent(t <- crb$getHLATyping())
  expect_true(hla_is_typing_table(t))
  expect_equal(nrow(t), 0L)
})

test_that("stored typing is queryable by carrier index", {
  crb <- make_minimal_cerebro()
  crb$addHLATyping(
    list(s1 = "HLA-A*02:01", s2 = "HLA-A*02:01", s3 = "HLA-A*01:01"),
    source_type = "genotyped"
  )
  ci <- hla_carrier_index(crb$getHLATyping())
  expect_setequal(ci[["HLA-A*02:01"]], c("s1", "s2"))
})
