sys.source(
  builder_profile_inst_path("builder", "spatial.R"),
  envir = environment()
)
sys.source(
  builder_profile_inst_path("builder", "extras.R"),
  envir = environment()
)
sys.source(
  builder_profile_inst_path("builder", "build.R"),
  envir = environment()
)

test_that("Builder preserves uploaded PNG and JPEG bytes", {
  skip_if_not_installed("png")
  skip_if_not_installed("jpeg")
  pixels <- array(seq(0, 1, length.out = 4L * 6L * 3L), c(4L, 6L, 3L))
  files <- c(
    png = withr::local_tempfile(fileext = ".png"),
    jpeg = withr::local_tempfile(fileext = ".jpg")
  )
  png::writePNG(pixels, files[["png"]])
  jpeg::writeJPEG(pixels, files[["jpeg"]], quality = 0.91)

  for (format in names(files)) {
    path <- files[[format]]
    mime <- if (identical(format, "png")) "image/png" else "image/jpeg"
    image <- builder_read_image(path)

    expect_null(image$error, info = format)
    expect_identical(
      image$source_path,
      normalizePath(path, winslash = "/", mustWork = TRUE),
      info = format
    )
    expect_identical(image$mime, mime, info = format)
    expect_false(any(c("source_uri", "uri") %in% names(image)), info = format)
    expect_identical(
      image$source_content_md5,
      unname(as.character(tools::md5sum(path))),
      info = format
    )
    expect_identical(image$bytes, unname(file.size(path)), info = format)
    expect_identical(
      image[c("width", "height")],
      list(width = 6L, height = 4L),
      info = format
    )
  }
})

test_that("large images keep original bytes without an R raster copy", {
  skip_if_not_installed("png")
  path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(0, nrow = 1400L, ncol = 1500L), path)

  image <- builder_read_image(path)

  expect_null(image$error)
  expect_identical(
    image$source_dimensions,
    c(width = 1500L, height = 1400L)
  )
  expect_null(image$array)
  expect_true("max_pixels" %in% names(formals(builder_read_image)))
  expect_true(exists("BUILDER_IMAGE_MAX_PIXELS", inherits = TRUE))
  if (exists("BUILDER_IMAGE_MAX_PIXELS", inherits = TRUE)) {
    expect_identical(BUILDER_IMAGE_MAX_PIXELS, Inf)
  }
  expect_identical(BUILDER_IMAGE_MAX_BYTES, Inf)
})

test_that("Builder rejects malformed PNG and JPEG structure", {
  skip_if_not_installed("png")
  source <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 64L), nrow = 8L), source)
  original <- readBin(source, what = "raw", n = file.size(source))
  marker <- charToRaw("IDAT")
  start <- which(vapply(
    seq_len(length(original) - length(marker) + 1L),
    function(index) {
      identical(
        original[seq.int(index, length.out = length(marker))],
        marker
      )
    },
    logical(1)
  ))[[1L]]
  chunk_length <- .builder_image_uint32_be(
    original[(start - 4L):(start - 1L)]
  )
  payload_start <- start + length(marker)
  payload_end <- payload_start + chunk_length - 1L
  flip <- function(bytes, index) {
    bytes[[index]] <- as.raw(bitwXor(as.integer(bytes[[index]]), 0xffL))
    bytes
  }

  # A damaged IDAT payload fails its chunk checksum.
  corrupt_data <- flip(original, payload_start)
  # With a repaired checksum the same damage must fail decoding instead.
  undecodable <- flip(original, payload_start + 2L)
  undecodable[(payload_end + 1L):(payload_end + 4L)] <- .builder_png_crc32(c(
    marker,
    undecodable[payload_start:payload_end]
  ))
  # An IHDR bit depth outside the PNG specification, with a valid checksum.
  invalid_header <- original
  invalid_header[[25L]] <- as.raw(3L)
  invalid_header[30:33] <- .builder_png_crc32(c(
    charToRaw("IHDR"),
    invalid_header[17:29]
  ))
  # A JPEG frame whose quantization table holds a zero divisor.
  zero_quantization <- as.raw(c(
    0xff,
    0xd8,
    0xff,
    0xdb,
    0x00,
    0x43,
    0x00,
    0x00,
    rep(0x01, 63L),
    0xff,
    0xc0,
    0x00,
    0x0b,
    0x08,
    0x00,
    0x01,
    0x00,
    0x01,
    0x01,
    0x01,
    0x11,
    0x00,
    0xff,
    0xda,
    0x00,
    0x08,
    0x01,
    0x01,
    0x00,
    0x00,
    0x3f,
    0x00,
    0x01,
    0xff,
    0xd9
  ))
  invalid <- list(
    corrupt_png_data = list(corrupt_data, ".png"),
    undecodable_png_data = list(undecodable, ".png"),
    invalid_png_header = list(invalid_header, ".png"),
    zero_jpeg_quantization = list(zero_quantization, ".jpeg")
  )

  for (name in names(invalid)) {
    path <- withr::local_tempfile(fileext = invalid[[name]][[2L]])
    writeBin(invalid[[name]][[1L]], path)
    if (identical(name, "undecodable_png_data")) {
      expect_error(png::readPNG(path, native = TRUE))
    }
    expect_identical(
      builder_read_image(path)$error,
      "The image file has no valid encoded pixel data.",
      info = name
    )
  }
})

test_that("PNG zlib headers may span consecutive IDAT chunks", {
  skip_if_not_installed("png")
  path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), path)
  bytes <- readBin(path, what = "raw", n = file.size(path))
  marker <- charToRaw("IDAT")
  start <- which(vapply(
    seq_len(length(bytes) - length(marker) + 1L),
    function(index) {
      identical(
        bytes[seq.int(index, length.out = length(marker))],
        marker
      )
    },
    logical(1)
  ))[[1L]]
  chunk_length <- .builder_image_uint32_be(bytes[(start - 4L):(start - 1L)])
  payload <- bytes[(start + 4L):(start + 3L + chunk_length)]
  uint32 <- function(value) {
    as.raw(c(
      bitwAnd(bitwShiftR(value, 24L), 255L),
      bitwAnd(bitwShiftR(value, 16L), 255L),
      bitwAnd(bitwShiftR(value, 8L), 255L),
      bitwAnd(value, 255L)
    ))
  }
  split_chunks <- c(
    uint32(1L),
    marker,
    payload[[1L]],
    .builder_png_crc32(c(marker, payload[[1L]])),
    uint32(length(payload) - 1L),
    marker,
    payload[-1L],
    .builder_png_crc32(c(marker, payload[-1L]))
  )
  after <- start + 4L + chunk_length + 4L
  bytes <- c(
    bytes[seq_len(start - 5L)],
    split_chunks,
    bytes[after:length(bytes)]
  )
  writeBin(bytes, path)

  expect_null(builder_read_image(path)$error)
  expect_silent(png::readPNG(path, native = TRUE))
})

test_that("Builder has no default decoded-pixel budget", {
  skip_if_not_installed("png")
  path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), path)
  bytes <- readBin(path, what = "raw", n = file.size(path))
  bytes[17:20] <- as.raw(c(0x00, 0x00, 0x20, 0x00))
  bytes[21:24] <- as.raw(c(0x00, 0x00, 0x10, 0x01))
  bytes[30:33] <- .builder_png_crc32(c(charToRaw("IHDR"), bytes[17:29]))
  writeBin(bytes, path)

  expect_null(builder_read_image(path)$error)
  expect_identical(
    builder_read_image(path, max_pixels = 32 * 1024^2)$error,
    "This image exceeds the decoded-pixel limit."
  )
})

test_that("truncated JPEG uploads are rejected before they enter a project", {
  skip_if_not_installed("jpeg")
  complete <- withr::local_tempfile(fileext = ".jpg")
  truncated <- withr::local_tempfile(fileext = ".jpg")
  jpeg::writeJPEG(array(seq(0, 1, length.out = 90L), c(5L, 6L, 3L)), complete)
  bytes <- readBin(complete, what = "raw", n = file.size(complete))
  writeBin(bytes[-length(bytes)], truncated)

  expect_null(builder_read_image(complete)$error)
  expect_identical(
    builder_read_image(truncated)$error,
    "The image file is incomplete or truncated."
  )
  reader <- paste(deparse(body(builder_image_file_dimensions)), collapse = "\n")
  expect_false(grepl("n = file.size(path)", reader, fixed = TRUE))
})
