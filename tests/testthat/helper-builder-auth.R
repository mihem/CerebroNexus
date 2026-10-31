builder_auth_test_accounts <- function() {
  list(
    list(
      id = "auth-account-1",
      username = " auth-user-a-7f31 ",
      password = "auth-password-a-7f31"
    ),
    list(
      id = "auth-account-2",
      username = "auth-user-b-8c42",
      password = "auth-password-b-8c42"
    )
  )
}

builder_auth_raw_contains <- function(raw_value, text) {
  stopifnot(
    is.raw(raw_value),
    is.character(text),
    length(text) == 1L,
    !is.na(text)
  )
  needle <- charToRaw(enc2utf8(text))
  if (!length(needle) || length(raw_value) < length(needle)) {
    return(FALSE)
  }
  starts <- seq_len(length(raw_value) - length(needle) + 1L)
  any(vapply(
    starts,
    function(index) {
      identical(
        raw_value[index + seq_along(needle) - 1L],
        needle
      )
    },
    logical(1)
  ))
}

builder_auth_value_contains <- function(value, text) {
  builder_auth_raw_contains(serialize(value, NULL, version = 3L), text)
}
