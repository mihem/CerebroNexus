.onLoad <- function(libname, pkgname) {
  namespace <- asNamespace(pkgname)
  marker <- ".cerebro_bundle_privacy_contract_version"
  assign(marker, 1L, envir = namespace)
  lockBinding(marker, namespace)
}
