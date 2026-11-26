## Slow Builder integration tests (app sourcing): CI only.
skip_on_cran()

builder_bytecode_package <- function(scope = parent.frame()) {
  package_dir <- withr::local_tempdir(.local_envir = scope)
  server <- file.path(package_dir, "builder", "server")
  dir.create(server, recursive = TRUE)
  file.copy(
    list.files(
      builder_profile_inst_path("builder", "server"),
      "[.]R$",
      full.names = TRUE
    ),
    server
  )
  .writeBuilderBytecode(package_dir)
  package_dir
}

## app.R parses with a srcfile, so its function definitions carry srcrefs
## (with a timestamp and working directory); install-time byte code has none,
## like the Viewer's. Drop them to compare what is executed.
builder_bytecode_strip_srcref <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  parts <- as.list(x)
  ## A parsed `function` call always has a fourth element, NULL without source.
  if (identical(parts[[1L]], as.name("function")) && length(parts) == 4L) {
    parts[4L] <- list(NULL)
  }
  ## Index access only: an empty argument (`x[i, ]`) cannot be bound to a name.
  for (i in seq_along(parts)) {
    if (is.call(parts[[i]])) {
      parts[[i]] <- builder_bytecode_strip_srcref(parts[[i]])
    }
  }
  as.call(parts)
}

builder_bytecode_app <- function(scope = parent.frame()) {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_options(
    cerebro.builder.precompile = FALSE,
    .local_envir = scope
  )
  withr::local_dir(builder_profile_inst_path("builder"), .local_envir = scope)
  sys.source("app.R", envir = app_env)
  app_env
}

test_that("install-time byte code matches the Builder's own compilation", {
  package_dir <- builder_bytecode_package()
  app_env <- builder_bytecode_app()
  files <- app_env$.builder_server_sources
  expect_length(files, 8L)
  for (file in files) {
    cached <- readRDS(file.path(
      package_dir,
      "builder-bytecode",
      paste0(file, ".rds")
    ))
    expect_identical(
      cached$md5,
      unname(tools::md5sum(builder_profile_inst_path("builder", file)))
    )
    ## Without precompilation app.R keeps each file as one `{` call.
    program <- app_env$.builder_server_programs[[file]]
    expect_true(is.call(program) && identical(program[[1L]], as.name("{")))
    ## app.R compiles in its own environment, where the Builder's helpers can
    ## shadow base functions the compiler would otherwise inline.
    runtime <- compiler::compile(
      builder_bytecode_strip_srcref(program),
      env = app_env,
      options = list(suppressAll = TRUE)
    )
    ## Nested byte code defeats waldo; compare the serialized instructions and
    ## constant pools instead.
    expect_true(
      identical(
        serialize(cached$code, NULL, version = 3L),
        serialize(runtime, NULL, version = 3L)
      ),
      info = file
    )
  }
})

test_that("stale or foreign Builder byte code is ignored", {
  package_dir <- builder_bytecode_package()
  app_env <- builder_bytecode_app()
  builder <- file.path(package_dir, "builder")
  file <- "server/foundation.R"
  path <- file.path(builder, file)
  withr::local_dir(builder)

  expect_false(is.null(app_env$.builder_load_cached_server_bytecode(
    file,
    path
  )))

  cache_path <- file.path(package_dir, "builder-bytecode", paste0(file, ".rds"))
  cached <- readRDS(cache_path)
  cached$r_version <- "R version 0.0.0"
  saveRDS(cached, cache_path)
  expect_null(app_env$.builder_load_cached_server_bytecode(file, path))

  .writeBuilderBytecode(package_dir)
  cat("\n", file = path, append = TRUE)
  expect_null(app_env$.builder_load_cached_server_bytecode(file, path))

  expect_null(app_env$.builder_load_cached_server_bytecode(
    "server/missing.R",
    path
  ))
})
