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

test_that("Builder rejects images without encoded pixel data", {
  directory <- withr::local_tempdir()
  png_path <- file.path(directory, "empty.png")
  jpeg_path <- file.path(directory, "empty.jpeg")
  writeBin(
    as.raw(c(
      0x89,
      0x50,
      0x4e,
      0x47,
      0x0d,
      0x0a,
      0x1a,
      0x0a,
      0x00,
      0x00,
      0x00,
      0x0d,
      0x49,
      0x48,
      0x44,
      0x52,
      0x00,
      0x00,
      0x00,
      0x01,
      0x00,
      0x00,
      0x00,
      0x01,
      0x08,
      0x02,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x49,
      0x45,
      0x4e,
      0x44,
      0x00,
      0x00,
      0x00,
      0x00
    )),
    png_path
  )
  writeBin(
    as.raw(c(
      0xff,
      0xd8,
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
    )),
    jpeg_path
  )

  for (path in c(png_path, jpeg_path)) {
    expect_identical(
      builder_read_image(path)$error,
      "The image file has no valid encoded pixel data.",
      info = basename(path)
    )
  }
})

test_that("Builder rejects PNG chunks with invalid checksums", {
  path <- withr::local_tempfile(fileext = ".png")
  writeBin(
    as.raw(c(
      0x89,
      0x50,
      0x4e,
      0x47,
      0x0d,
      0x0a,
      0x1a,
      0x0a,
      0x00,
      0x00,
      0x00,
      0x0d,
      0x49,
      0x48,
      0x44,
      0x52,
      0x00,
      0x00,
      0x00,
      0x01,
      0x00,
      0x00,
      0x00,
      0x01,
      0x08,
      0x02,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x01,
      0x49,
      0x44,
      0x41,
      0x54,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x49,
      0x45,
      0x4e,
      0x44,
      0x00,
      0x00,
      0x00,
      0x00
    )),
    path
  )

  expect_identical(
    builder_read_image(path)$error,
    "The image file has no valid encoded pixel data."
  )
})

test_that("Builder rejects corrupt PNG image data", {
  skip_if_not_installed("png")
  path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), path)
  bytes <- readBin(path, what = "raw", n = file.size(path))
  marker <- charToRaw("IDAT")
  starts <- which(vapply(
    seq_len(length(bytes) - length(marker) + 1L),
    function(index) {
      identical(
        bytes[seq.int(index, length.out = length(marker))],
        marker
      )
    },
    logical(1)
  ))
  expect_true(length(starts) >= 1L)
  payload <- starts[[1L]] + length(marker)
  bytes[[payload]] <- as.raw(bitwXor(as.integer(bytes[[payload]]), 0x01L))
  writeBin(bytes, path)

  expect_identical(
    builder_read_image(path)$error,
    "The image file has no valid encoded pixel data."
  )
})

test_that("Builder rejects PNG payloads that cannot be decoded", {
  skip_if_not_installed("png")
  path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 64L), nrow = 8L), path)
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
  payload_start <- start + length(marker)
  payload_end <- payload_start + chunk_length - 1L
  bytes[[payload_start + 2L]] <- as.raw(bitwXor(
    as.integer(bytes[[payload_start + 2L]]),
    0xffL
  ))
  bytes[(payload_end + 1L):(payload_end + 4L)] <- .builder_png_crc32(c(
    marker,
    bytes[payload_start:payload_end]
  ))
  writeBin(bytes, path)

  expect_error(png::readPNG(path, native = TRUE))
  expect_identical(
    builder_read_image(path)$error,
    "The image file has no valid encoded pixel data."
  )
})

test_that("Builder rejects invalid PNG header semantics", {
  skip_if_not_installed("png")
  source <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), source)
  original <- readBin(source, what = "raw", n = file.size(source))
  rewrite_ihdr <- function(offset, value) {
    bytes <- original
    bytes[[16L + offset]] <- as.raw(value)
    bytes[30:33] <- .builder_png_crc32(c(
      charToRaw("IHDR"),
      bytes[17:29]
    ))
    path <- tempfile(fileext = ".png")
    withr::defer(unlink(path), envir = parent.frame())
    writeBin(bytes, path)
    path
  }
  invalid <- list(
    bit_depth = rewrite_ihdr(9L, 3L),
    color_type = rewrite_ihdr(10L, 1L),
    compression = rewrite_ihdr(11L, 1L),
    filter = rewrite_ihdr(12L, 1L),
    interlace = rewrite_ihdr(13L, 2L),
    indexed_without_palette = rewrite_ihdr(10L, 3L)
  )

  for (name in names(invalid)) {
    expect_identical(
      builder_read_image(invalid[[name]])$error,
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

test_that("Builder rejects malformed JPEG quantization tables", {
  path <- withr::local_tempfile(fileext = ".jpeg")
  writeBin(
    as.raw(c(
      0xff,
      0xd8,
      0xff,
      0xdb,
      0x00,
      0x03,
      0x04,
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
    )),
    path
  )

  expect_identical(
    builder_read_image(path)$error,
    "The image file has no valid encoded pixel data."
  )
})

test_that("Builder rejects invalid JPEG tables and sampling factors", {
  jpeg_bytes <- function(quantization, sampling) {
    as.raw(c(
      0xff,
      0xd8,
      0xff,
      0xdb,
      0x00,
      0x43,
      0x00,
      quantization,
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
      sampling,
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
  }
  invalid <- list(
    zero_quantization = jpeg_bytes(c(0L, rep(1L, 63L)), 0x11),
    zero_sampling = jpeg_bytes(rep(1L, 64L), 0x00)
  )

  for (name in names(invalid)) {
    path <- withr::local_tempfile(fileext = ".jpeg")
    writeBin(invalid[[name]], path)
    expect_identical(
      builder_read_image(path)$error,
      "The image file has no valid encoded pixel data.",
      info = name
    )
  }
})

test_that("Builder rejects unsupported JPEG sample precision", {
  skip_if_not_installed("jpeg")
  path <- withr::local_tempfile(fileext = ".jpeg")
  jpeg::writeJPEG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), path)
  bytes <- readBin(path, what = "raw", n = file.size(path))
  frame <- which(
    as.integer(bytes[-length(bytes)]) == 0xffL &
      as.integer(bytes[-1L]) %in% c(0xc0L, 0xc1L, 0xc2L)
  )[[1L]]
  bytes[[frame + 4L]] <- as.raw(0L)
  writeBin(bytes, path)

  expect_identical(
    builder_read_image(path)$error,
    "The image file has no valid encoded pixel data."
  )
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

test_that("external image materialization is byte exact and keeps transforms", {
  skip_if_not_installed("png")
  source_path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 48L), nrow = 6L), source_path)
  inspected <- builder_read_image(source_path)
  record <- builder_alignment_record(
    source = list(name = "section.png", type = "image/png"),
    base_bounds = list(xmin = 0, xmax = 20, ymin = 10, ymax = 30),
    parameters = list(
      dx = 4,
      dy = -2,
      scale = 1.5,
      rotation = 37,
      flip_x = TRUE
    ),
    section = list(id = "fov-a", kind = "spatial"),
    source_path = inspected$source_path
  )
  record$source_content_md5 <- inspected$source_content_md5
  stage <- withr::local_tempdir()
  materialized <- .builder_build_materialize_spatial_images(
    list(
      id = "dataset-a",
      name = "Dataset A",
      images = list(`fov-a` = list(section = record))
    ),
    stage
  )
  descriptor <- materialized$images[["dataset-a"]][["fov-a"]]$section
  copied <- descriptor$path

  expect_true(file.exists(copied))
  expect_identical(unname(file.size(copied)), unname(file.size(source_path)))
  expect_identical(
    unname(as.character(tools::md5sum(copied))),
    unname(as.character(tools::md5sum(source_path)))
  )
  expect_identical(
    readBin(copied, what = "raw", n = file.size(copied)),
    readBin(source_path, what = "raw", n = file.size(source_path))
  )
  expect_identical(
    materialized$settings[["dataset-a"]][["fov-a"]]$section$rotation,
    -37
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

test_that("Builder image upload no longer re-encodes display pixels", {
  extras <- paste(
    readLines(
      builder_profile_inst_path("builder", "extras.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  server <- paste(
    readLines(
      builder_profile_inst_path("builder", "spatial_alignment_server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  client <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("builder_encode_image <-", extras, fixed = TRUE))
  expect_false(grepl("builder_encode_image(", server, fixed = TRUE))
  expect_false(grepl("max_px = 1400", server, fixed = TRUE))
  expect_false(grepl("resizeTissueImageBeforeUpload", client, fixed = TRUE))
  expect_false(grepl("TISSUE_IMAGE_MAX_EDGE", client, fixed = TRUE))
  expect_false(grepl("base64encode", extras, fixed = TRUE))
  expect_false(grepl("base64encode", server, fixed = TRUE))
  expect_false(grepl("builder_histology_image_payload", extras, fixed = TRUE))
  expect_false(grepl("builder_attach_spatial_image", extras, fixed = TRUE))
})
