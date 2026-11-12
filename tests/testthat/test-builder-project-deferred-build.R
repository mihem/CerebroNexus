## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

## A Build that runs before the Project exists leaves CRBs that the Save which
## creates the Project adopts, instead of building the same caches again.

deferred_build_app <- function(scope = parent.frame()) {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"), .local_envir = scope)
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this test.")
  }
  app_env
}

deferred_build_entry <- function() {
  entry <- builder_minimal_entry("ds1", "PBMC")
  entry$snapshot <- list(source_fingerprint = "source-fingerprint")
  entry
}

deferred_build_value <- function(built) {
  list(
    state = "success",
    published = TRUE,
    built = c(ds1 = built),
    verifications = list(ds1 = list(valid = TRUE)),
    release = list(
      target = dirname(built),
      identity = list(
        entries = list(list(
          path = basename(built),
          type = "file",
          md5 = "0123456789abcdef0123456789abcdef"
        ))
      )
    )
  )
}

test_that("a Build without a Project keeps its plan and what it was built from", {
  app_env <- deferred_build_app()
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(deferred_build_entry()))
    plan <- list(items = list(list(id = "ds1")))

    expect_false(builder_project_capture_build_plan(plan))
    expect_identical(isolate(builder_project_build_plan()), plan)
    expect_identical(
      isolate(builder_project_build_identities())$ds1,
      builder_project_entry_build_identity(isolate(sets())[[1L]])
    )

    ## A Build that starts with a Project open registers as it always did.
    builder_project(list(root = tempdir(), manifest = list()))
    expect_identical(builder_project_capture_build_plan(plan), plan)
    expect_null(isolate(builder_project_build_identities()))
    expect_null(isolate(builder_project_deferred_build()))
  })
})

test_that("a published Build without a Project is kept for the Save that creates one", {
  app_env <- deferred_build_app()
  built <- withr::local_tempfile(fileext = ".crb")
  writeLines("crb", built)
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(deferred_build_entry()))
    plan <- list(
      items = list(list(
        id = "ds1",
        reused_artifact = list(path = "an-earlier-release/ds1.crb")
      ))
    )
    ## One dataset was built; the plan is not all reused CRBs.
    plan$items[[2L]] <- list(id = "ds2")

    builder_project_capture_build_plan(plan)
    builder_project_handle_build_result(deferred_build_value(built))

    deferred <- isolate(builder_project_deferred_build())
    expect_identical(
      vapply(deferred$plan$items, `[[`, character(1), "id"),
      c("ds1", "ds2")
    )
    ## A CRB copied from an earlier release is registered like a built one.
    expect_null(deferred$plan$items[[1L]]$reused_artifact)
    expect_identical(deferred$value$built[["ds1"]], built)
    expect_identical(names(deferred$identities), c("ds1", "ds2"))
    expect_null(isolate(builder_project_build_plan()))
    expect_null(isolate(builder_project_build_identities()))

    ## A newer Build replaces what was kept.
    builder_project_capture_build_plan(plan)
    expect_null(isolate(builder_project_deferred_build()))
  })
})

test_that("a Build whose release identity is unknown is not kept", {
  app_env <- deferred_build_app()
  built <- withr::local_tempfile(fileext = ".crb")
  writeLines("crb", built)
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(deferred_build_entry()))
    builder_project_capture_build_plan(list(items = list(list(id = "ds1"))))
    value <- deferred_build_value(built)
    value$release <- NULL
    builder_project_handle_build_result(value)

    ## Still waiting for the result that carries the release: plan kept.
    expect_null(isolate(builder_project_deferred_build()))
    expect_false(is.null(isolate(builder_project_build_plan())))
    expect_false(is.null(isolate(builder_project_build_identities())))

    ## The published result then arrives and is kept.
    builder_project_handle_build_result(deferred_build_value(built))
    expect_false(is.null(isolate(builder_project_deferred_build())))
    expect_null(isolate(builder_project_build_plan()))
  })
})

test_that("a Save adopts the caches of datasets that are still what was built", {
  app_env <- deferred_build_app()
  built <- withr::local_tempfile(fileext = ".crb")
  writeLines("crb", built)
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(deferred_build_entry()))
    registered <- list()
    assign(
      "builder_project_register_build",
      function(plan, value, project, quiet = FALSE, on_done = NULL) {
        registered[[length(registered) + 1L]] <<- list(
          ids = vapply(plan$items, `[[`, character(1), "id"),
          quiet = quiet
        )
        on_done(TRUE)
      },
      envir = environment(builder_project_adopt_deferred_build)
    )
    adopt <- function(project = list(root = tempdir(), manifest = list())) {
      calls <- 0L
      builder_project(project)
      builder_project_adopt_deferred_build(then = function() {
        calls <<- calls + 1L
      })
      calls
    }
    capture <- function(entry = isolate(sets())[[1L]], file = built) {
      builder_project_deferred_build(NULL)
      builder_project_capture_build_plan(list(items = list(list(id = "ds1"))))
      builder_project_defer_build(
        list(items = list(list(id = "ds1"))),
        deferred_build_value(file),
        builder_project_build_identities_of(
          list(items = list(list(id = "ds1"))),
          list(entry)
        )
      )
    }

    ## Unchanged: registered quietly, then the CRB queue carries on once.
    capture()
    expect_identical(adopt(), 1L)
    expect_identical(registered, list(list(ids = "ds1", quiet = TRUE)))
    expect_null(isolate(builder_project_deferred_build()))

    ## Changed since the Build: left to the CRB queue.
    registered <- list()
    edited <- isolate(sets())[[1L]]
    capture(edited)
    edited$settings$groups <- c("cluster", "sample")
    use_state_only_fixture(list(edited))
    expect_identical(adopt(), 1L)
    expect_length(registered, 0L)

    ## The release was removed since the Build.
    use_state_only_fixture(list(deferred_build_entry()))
    gone <- withr::local_tempfile(fileext = ".crb")
    capture(file = gone)
    expect_identical(adopt(), 1L)
    expect_length(registered, 0L)

    ## No Project to adopt into, and nothing kept.
    capture()
    expect_identical(adopt(project = NULL), 1L)
    expect_length(registered, 0L)
    expect_null(isolate(builder_project_deferred_build()))
    expect_identical(adopt(), 1L)
  })
})

test_that("queuing the caches adopts a kept Build first", {
  app_env <- deferred_build_app()
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(deferred_build_entry()))
    order <- character()
    assign(
      "builder_project_adopt_deferred_build",
      function(ids = NULL, then = NULL) {
        order <<- c(order, "adopt")
        builder_project_deferred_build(NULL)
        then()
      },
      envir = environment(queue_builder_project_crbs)
    )
    builder_project_deferred_build(list(plan = list(items = list())))

    expect_true(queue_builder_project_crbs(character()))
    expect_identical(order, "adopt")
    expect_null(isolate(builder_project_deferred_build()))
    ## With nothing kept, queuing goes straight to the queue.
    expect_true(queue_builder_project_crbs(character()))
    expect_identical(order, "adopt")
  })
})

test_that("the observer reads the Build result itself, so it reruns when it changes", {
  ## The handler returns before it touches `value` while no plan is kept; a
  ## result passed on as a promise would then never be read, and the observer
  ## would not depend on it.
  expressions <- parse(
    builder_test_inst_path("builder", "server", "project.R"),
    keep.source = FALSE
  )
  observers <- Filter(
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], as.name("observe")) &&
        grepl(
          "builder_project_handle_build_result",
          paste(deparse(expression), collapse = " "),
          fixed = TRUE
        )
    },
    as.list(expressions)
  )
  expect_length(observers, 1L)
  body <- as.list(observers[[1L]][[2L]])[-1L]
  reads_result <- vapply(
    body,
    function(statement) {
      is.call(statement) &&
        identical(statement[[1L]], as.name("<-")) &&
        identical(statement[[3L]], quote(result()))
    },
    logical(1)
  )
  expect_true(any(reads_result))
})

test_that("adopting never throws and only takes the datasets asked for", {
  app_env <- deferred_build_app()
  built <- withr::local_tempfile(fileext = ".crb")
  writeLines("crb", built)
  shiny::testServer(app_env$server, {
    first <- deferred_build_entry()
    second <- builder_minimal_entry("ds2", "Second")
    second$snapshot <- list(source_fingerprint = "second-source")
    use_state_only_fixture(list(first, second))
    entries <- isolate(sets())
    plan <- list(items = list(list(id = "ds1"), list(id = "ds2")))
    registered <- list()
    assign(
      "builder_project_register_build",
      function(plan, value, project, quiet = FALSE, on_done = NULL) {
        registered[[length(registered) + 1L]] <<- vapply(
          plan$items,
          `[[`,
          character(1),
          "id"
        )
        on_done(TRUE)
      },
      envir = environment(builder_project_adopt_deferred_build)
    )
    keep <- function() {
      builder_project_capture_build_plan(plan)
      # `built` names only ds1: ds2 has no file to adopt.
      builder_project_defer_build(
        plan,
        deferred_build_value(built),
        builder_project_build_identities_of(plan, entries)
      )
      builder_project(list(root = tempdir(), manifest = list()))
    }

    keep()
    calls <- 0L
    expect_no_error(builder_project_adopt_deferred_build(
      then = function() calls <<- calls + 1L
    ))
    expect_identical(calls, 1L)
    expect_identical(registered, list("ds1"))

    ## Only the datasets that are asked for (the checked ones).
    registered <- list()
    keep()
    builder_project_adopt_deferred_build(ids = "ds2", then = function() NULL)
    expect_length(registered, 0L)
  })
})

test_that("a Build whose CRB the release does not list is not kept", {
  app_env <- deferred_build_app()
  built <- withr::local_tempfile(fileext = ".crb")
  writeLines("crb", built)
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(deferred_build_entry()))
    plan <- list(items = list(list(id = "ds1")))
    identities <- builder_project_build_identities_of(
      plan,
      isolate(sets())
    )
    value <- deferred_build_value(built)
    value$release$identity$entries[[1L]]$path <- "another-file.crb"
    expect_false(builder_project_defer_build(plan, value, identities))
    expect_null(isolate(builder_project_deferred_build()))
    expect_true(builder_project_defer_build(
      plan,
      deferred_build_value(built),
      identities
    ))
  })
})

test_that("a quiet registration that cannot start leaves the phase and the queue alone", {
  app_env <- deferred_build_app()
  built <- withr::local_tempfile(fileext = ".crb")
  writeLines("crb", built)
  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(deferred_build_entry()))
    notices <- 0L
    assign(
      "builder_notify",
      function(...) notices <<- notices + 1L,
      envir = environment(builder_project_register_build)
    )
    assign(
      "builder_async_submit",
      function(...) stop("no background process in this test"),
      envir = environment(builder_project_register_build)
    )
    project <- list(root = tempdir(), manifest = list())
    builder_project(project)
    plan <- list(items = list(list(id = "ds1")))
    value <- deferred_build_value(built)
    outcome <- NULL

    builder_project_operation_phase("idle")
    builder_project_register_build(
      plan,
      value,
      project,
      quiet = TRUE,
      on_done = function(ok) outcome <<- ok
    )
    expect_false(outcome)
    expect_identical(isolate(builder_project_operation_phase()), "idle")
    expect_identical(notices, 0L)

    ## Not quiet: the failure is reported and the Save is marked failed.
    builder_project_register_build(
      plan,
      value,
      project,
      on_done = function(ok) outcome <<- ok
    )
    expect_identical(isolate(builder_project_operation_phase()), "save_failed")
    expect_identical(notices, 1L)

    ## A phase set by someone else in between is not overwritten.
    builder_project_operation_phase("idle")
    assign(
      "builder_async_submit",
      function(...) {
        builder_project_operation_phase("saving")
        stop("no background process in this test")
      },
      envir = environment(builder_project_register_build)
    )
    builder_project_register_build(plan, value, project, quiet = TRUE)
    expect_identical(isolate(builder_project_operation_phase()), "saving")
  })
})
