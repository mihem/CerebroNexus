## Work that only keeps something for the next start (the finished page, the
## page as it is sent, the collection of what was read) runs when no session
## has been busy for a moment, one task per turn of the event loop. Run when it
## is made it sat between the first session and its first plot: 0.3 s to write
## the page in the middle of the start, 0.25 s to keep the document and to
## collect, a second R process to fetch it.
##
## Sessions report that they are busy with .viewerSessionActivity() (the
## Viewer does at every flush). A task waits until `quiet` seconds have passed
## since the last report, or since it was queued when no session reported.
.viewer_quiet <- new.env(parent = emptyenv())
.viewer_quiet$queue <- list()
.viewer_quiet$polling <- FALSE
.viewer_quiet$since <- NULL

.viewerSessionActivity <- function() {
  .viewer_quiet$since <- Sys.time()
  invisible(NULL)
}

.viewerWhenQuiet <- function(task, quiet = 2) {
  .viewer_quiet$queue[[length(.viewer_quiet$queue) + 1L]] <- task
  if (is.null(.viewer_quiet$since)) {
    .viewer_quiet$since <- Sys.time()
  }
  if (.viewer_quiet$polling) {
    return(invisible(NULL))
  }
  .viewer_quiet$polling <- TRUE
  tick <- function() {
    if (!length(.viewer_quiet$queue)) {
      .viewer_quiet$polling <- FALSE
      return(invisible(NULL))
    }
    waited <- as.numeric(difftime(
      Sys.time(),
      .viewer_quiet$since,
      units = "secs"
    ))
    if (waited >= quiet) {
      task <- .viewer_quiet$queue[[1L]]
      .viewer_quiet$queue[[1L]] <- NULL
      try(task(), silent = TRUE)
    }
    later::later(tick, 0.25)
  }
  later::later(tick, 0.25)
  invisible(NULL)
}
