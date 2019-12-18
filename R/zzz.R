.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "origami v", utils::packageDescription("origami")$Version,
    ": Generalized Cross-Validation Framework"
  ))
}
