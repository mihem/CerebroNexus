test_that("headless Project open validates every external dataset source", {
  local({
    source(
      builder_profile_inst_path(
        "builder",
        "core",
        "bundle_path_contract.R"
      ),
      local = TRUE
    )
    source(builder_profile_inst_path("builder", "io.R"), local = TRUE)
    source(builder_profile_inst_path("builder", "project.R"), local = TRUE)

    root <- withr::local_tempdir()
    allowed <- file.path(root, "allowed")
    outside <- file.path(root, "outside")
    dir.create(allowed)
    dir.create(outside)
    inside_source <- file.path(allowed, "inside.rds")
    outside_source <- file.path(outside, "outside.rds")
    writeBin(charToRaw("inside"), inside_source)
    writeBin(charToRaw("outside"), outside_source)

    manifest <- list(
      datasets = list(list(
        id = "ds1",
        name = "Allowed dataset",
        source = list(kind = "external", path = inside_source)
      ))
    )
    checked <- builder_project_validate_external_sources(
      manifest,
      roots = allowed
    )
    expect_identical(
      checked$datasets[[1L]]$source$path,
      normalizePath(inside_source, winslash = "/", mustWork = TRUE)
    )

    manifest$datasets[[1L]]$name <- "Outside dataset"
    manifest$datasets[[1L]]$source$path <- outside_source
    expect_error(
      builder_project_validate_external_sources(manifest, roots = allowed),
      "Outside dataset",
      fixed = TRUE
    )
  })
})

test_that("server path fallback resolves symlinks inside explicit roots", {
  local({
    source(
      builder_profile_inst_path(
        "builder",
        "core",
        "bundle_path_contract.R"
      ),
      local = TRUE
    )
    source(
      builder_profile_inst_path("builder", "io.R"),
      local = TRUE
    )
    root <- withr::local_tempdir()
    allowed <- file.path(root, "allowed")
    outside <- file.path(root, "outside")
    inside <- file.path(allowed, "project")
    dir.create(inside, recursive = TRUE)
    dir.create(outside)

    expect_identical(
      builder_server_path_resolve(inside, "directory", roots = allowed),
      normalizePath(inside, winslash = "/", mustWork = TRUE)
    )
    expect_error(
      builder_server_path_resolve(outside, "directory", roots = allowed),
      "outside the allowed server folders",
      fixed = TRUE
    )

    linked <- file.path(allowed, "linked-outside")
    if (isTRUE(suppressWarnings(file.symlink(outside, linked)))) {
      expect_error(
        builder_server_path_resolve(linked, "directory", roots = allowed),
        "outside the allowed server folders",
        fixed = TRUE
      )
    }

    if (!identical(.Platform$OS.type, "windows")) {
      spaced <- file.path(allowed, "out ")
      dir.create(spaced)
      dir.create(file.path(allowed, "out"))
      expect_identical(
        builder_server_path_resolve(spaced, "directory", roots = allowed),
        normalizePath(spaced, winslash = "/", mustWork = TRUE)
      )
      expect_identical(
        builder_server_path_resolve(
          paste0(" ", file.path(allowed, "project"), "  "),
          "directory",
          roots = allowed
        ),
        normalizePath(inside, winslash = "/", mustWork = TRUE)
      )
    }
  })
})

test_that("the server path browser lists only inside the allowed folders", {
  local({
    source(
      builder_profile_inst_path(
        "builder",
        "core",
        "bundle_path_contract.R"
      ),
      local = TRUE
    )
    source(builder_profile_inst_path("builder", "io.R"), local = TRUE)
    source(
      builder_profile_inst_path("builder", "ui", "workflow.R"),
      local = TRUE
    )
    root <- withr::local_tempdir()
    allowed <- file.path(root, "allowed")
    outside <- file.path(root, "outside")
    dir.create(file.path(allowed, "b"), recursive = TRUE)
    dir.create(file.path(allowed, "a", "deeper"), recursive = TRUE)
    dir.create(outside)
    writeLines("{}", file.path(allowed, "builder-project.json"))
    writeLines("x", file.path(allowed, "notes.txt"))
    allowed_real <- normalizePath(allowed, winslash = "/", mustWork = TRUE)
    names_of <- function(listing) {
      vapply(listing$entries, `[[`, character(1), "name")
    }

    start <- builder_server_path_entries(NULL, roots = allowed)
    expect_null(start$path)
    expect_identical(start$entries[[1L]]$path, allowed_real)

    top <- builder_server_path_entries(allowed, "directory", roots = allowed)
    expect_identical(top$path, allowed_real)
    expect_null(top$parent)
    expect_identical(names_of(top), c("a", "b"))
    files <- builder_server_path_entries(
      allowed,
      "file",
      pattern = "[.]json$",
      roots = allowed
    )
    expect_identical(names_of(files), c("a", "b", "builder-project.json"))
    expect_identical(files$entries[[3L]]$type, "file")

    nested <- builder_server_path_entries(
      file.path(allowed, "a"),
      roots = allowed
    )
    expect_identical(nested$parent, allowed_real)
    expect_identical(names_of(nested), "deeper")

    expect_error(
      builder_server_path_entries(outside, roots = allowed),
      "outside the allowed server folders",
      fixed = TRUE
    )
    linked <- file.path(allowed, "linked-outside")
    if (isTRUE(suppressWarnings(file.symlink(outside, linked)))) {
      expect_error(
        builder_server_path_entries(linked, roots = allowed),
        "outside the allowed server folders",
        fixed = TRUE
      )
    }

    html <- as.character(builder_server_browser_ui(nested))
    expect_match(
      html,
      sprintf('data-server-browse="%s/a/deeper"', allowed_real),
      fixed = TRUE
    )
    expect_match(html, ">Up<", fixed = TRUE)
    expect_match(
      as.character(builder_server_browser_ui(simpleError("Not allowed."))),
      "role=\"alert\"",
      fixed = TRUE
    )

    ## Each allowed root is its own entry, named by itself.
    second <- file.path(root, "second")
    dir.create(second)
    roots <- builder_server_path_entries(NULL, roots = c(allowed, second))
    expect_identical(
      names_of(roots),
      c(allowed_real, normalizePath(second, winslash = "/"))
    )

    ## Many subfolders do not push the files out of a truncated list.
    crowded <- file.path(allowed, "crowded")
    dir.create(crowded)
    for (i in seq_len(6L)) {
      dir.create(file.path(crowded, sprintf("folder%02d", i)))
    }
    writeLines("{}", file.path(crowded, "zz-builder-project.json"))
    listing <- builder_server_path_entries(
      crowded,
      "file",
      pattern = "[.]json$",
      roots = allowed,
      limit = 4L
    )
    expect_true(listing$truncated)
    expect_identical(listing$total, 7L)
    expect_identical(
      names_of(listing),
      c("folder01", "folder02", "folder03", "zz-builder-project.json")
    )
    expect_match(
      as.character(builder_server_browser_ui(listing)),
      "Showing 4 of 7 entries. Type the path of one that is not listed.",
      fixed = TRUE
    )
  })
})
