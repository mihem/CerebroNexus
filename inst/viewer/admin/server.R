## Administrator-only navigation and share inventory.

admin_config <- viewer_admin_config(Cerebro.options)

viewer_admin_send <- function(ok, action, nonce = "", ...) {
  session$sendCustomMessage(
    "viewer_admin_result",
    c(
      list(ok = isTRUE(ok), action = action, nonce = nonce),
      list(...)
    )
  )
}

viewer_admin_inventory <- function(nonce = "") {
  if (!viewer_is_admin(session)) {
    viewer_admin_send(FALSE, "list", nonce, code = "forbidden")
    return(invisible(NULL))
  }
  if (is.null(coordviews_share_store)) {
    viewer_admin_send(
      FALSE,
      "list",
      nonce,
      code = "share_unavailable",
      message = "Share links are unavailable on this server."
    )
    return(invisible(NULL))
  }
  rows <- cv_share_store_list(coordviews_share_store)
  viewer_admin_send(
    TRUE,
    "list",
    nonce,
    records = viewer_admin_records(rows)
  )
  invisible(NULL)
}

admin_route <- viewer_admin_route(isolate(session$clientData$url_pathname))
admin_menu_inserted <- FALSE
viewer_admin_expose <- function(select = FALSE) {
  if (!admin_menu_inserted) {
    insertUI(
      selector = "#sidebar_item_admin_placeholder",
      where = "afterEnd",
      ui = tags$li(
        id = "sidebar_item_admin",
        menuItem(
          "Admin",
          tabName = "admin",
          icon = icon("shield-halved")
        )$children
      ),
      immediate = TRUE
    )
    admin_menu_inserted <<- TRUE
  }
  if (select) updateTabItems(session, "sidebar", selected = "admin")
}

session$onFlushed(
  function() {
    allowed <- viewer_is_admin(session)
    if ((!is.null(admin_config) && admin_route) || allowed) {
      viewer_admin_expose(select = admin_route)
    }
    session$sendCustomMessage(
      "viewer_admin_access",
      list(
        allowed = allowed,
        user = if (allowed) viewer_auth_context(session)$user else NULL
      )
    )
    if (allowed) viewer_admin_inventory()
  },
  once = TRUE
)

observeEvent(
  input[["viewer_admin_login"]],
  {
    request <- input[["viewer_admin_login"]]
    user <- if (is.list(request)) request$user else NULL
    password <- if (is.list(request)) request$password else NULL
    if (is.null(admin_config)) {
      session$sendCustomMessage(
        "viewer_admin_access",
        list(allowed = FALSE, unavailable = TRUE)
      )
      return(invisible(NULL))
    }
    login_key <- viewer_admin_login_key(session, admin_config$account)
    if (!viewer_admin_login_allowed(login_key)) {
      session$sendCustomMessage(
        "viewer_admin_access",
        list(allowed = FALSE, locked = TRUE)
      )
      return(invisible(NULL))
    }
    if (!viewer_admin_login(user, password, admin_config)) {
      viewer_admin_login_failed(login_key)
      session$sendCustomMessage(
        "viewer_admin_access",
        list(allowed = FALSE, invalid = TRUE)
      )
      return(invisible(NULL))
    }
    viewer_admin_login_succeeded(login_key)
    assign(
      "viewer_auth",
      list(authenticated = TRUE, user = user, is_admin = TRUE),
      envir = session$userData
    )
    viewer_admin_expose(select = TRUE)
    session$sendCustomMessage(
      "viewer_admin_access",
      list(allowed = TRUE, user = user)
    )
    viewer_admin_inventory()
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["viewer_admin_logout"]],
  {
    if (exists("viewer_auth", envir = session$userData, inherits = FALSE)) {
      rm("viewer_auth", envir = session$userData)
    }
    session$sendCustomMessage(
      "viewer_admin_access",
      list(allowed = FALSE, logged_out = TRUE)
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input[["viewer_admin_request"]],
  {
    request <- input[["viewer_admin_request"]]
    nonce <- if (
      is.list(request) &&
        is.character(request$nonce) &&
        length(request$nonce) == 1L &&
        !is.na(request$nonce)
    ) {
      substr(request$nonce, 1L, 128L)
    } else {
      ""
    }
    if (!viewer_is_admin(session)) {
      viewer_admin_send(FALSE, "forbidden", nonce, code = "forbidden")
      return(invisible(NULL))
    }
    action <- if (
      is.list(request) &&
        is.character(request$action) &&
        length(request$action) == 1L &&
        !is.na(request$action)
    ) {
      request$action
    } else {
      ""
    }
    tryCatch(
      {
        if (identical(action, "list")) {
          viewer_admin_inventory(nonce)
        } else if (identical(action, "revoke")) {
          if (is.null(coordviews_share_store)) {
            cv_share_abort(
              "share_unavailable",
              "Share links are unavailable on this server."
            )
          }
          token <- cv_share_token_input(request$token, "link")
          revoked_at <- cv_share_store_revoke_admin(
            coordviews_share_store,
            token
          )
          viewer_admin_send(
            TRUE,
            "revoke",
            nonce,
            token = token,
            revoked_at = revoked_at
          )
        } else {
          cv_share_abort("invalid_action", "The Admin request is invalid.")
        }
      },
      error = function(error) {
        viewer_admin_send(
          FALSE,
          action,
          nonce,
          code = error$code %||% "internal",
          message = if (inherits(error, "cv_share_error")) {
            conditionMessage(error)
          } else {
            "The Admin request could not be completed."
          }
        )
      }
    )
  },
  ignoreInit = TRUE
)
