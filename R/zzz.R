.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "origami v", utils::packageDescription("origami")$Version,
    ": ", utils::packageDescription("origami")$Title
  ))
}
