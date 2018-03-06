.onAttach <- function(...) {
  packageStartupMessage("origami: Generalized Cross-Validation Framework")
  packageStartupMessage(
    "Version: ",
    utils::packageDescription("origami")$Version
  )
}
