#' Configure Global R Environment for colleyRstats
#'
#' Sets ggplot2 themes and conflict preferences to match the
#' standards used in the colleyRstats workflow.
#'
#' @param set_options Logical. If \code{TRUE}, prints a notice that global
#'   options are no longer changed automatically. Default is \code{TRUE}.
#' @param set_theme Logical. If \code{TRUE}, sets the default \code{ggplot2} theme
#'   to \code{see::theme_lucid} with custom modifications. Default is \code{TRUE}.
#' @param set_conflicts Logical. If \code{TRUE}, sets \code{conflicted} preferences
#'   to favor \code{dplyr} and other tidyverse packages. Default is \code{TRUE}.
#' @param print_citation Logical. If \code{TRUE}, prints the citation information
#'   for this package. Default is \code{TRUE}.
#' @param verbose Logical. If \code{TRUE}, emit informational messages.
#'   Default is \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL}.
#' @export
#'
#' @examples
#' # Runs everywhere, no extra packages, no session side effects
#' colleyRstats::colleyRstats_setup(
#'   set_options = FALSE,
#'   set_theme = FALSE,
#'   set_conflicts = FALSE,
#'   print_citation = FALSE,
#'   verbose = FALSE
#' )
#'
#' \donttest{
#' # Full setup (requires suggested packages; changes session defaults)
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("see", quietly = TRUE)) {
#'   local({
#'     old_theme <- ggplot2::theme_get()
#'     on.exit(ggplot2::theme_set(old_theme), add = TRUE)
#'
#'     colleyRstats::colleyRstats_setup(
#'       set_options = FALSE,
#'       set_conflicts = FALSE,   # avoid persisting conflict prefs in checks
#'       print_citation = FALSE,
#'       verbose = TRUE
#'     )
#'
#'     ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
#'       ggplot2::geom_point()
#'   })
#' }
#' }
colleyRstats_setup <- function(set_options = TRUE,
                        set_theme = TRUE,
                        set_conflicts = TRUE,
                        print_citation = TRUE,
                        verbose = TRUE) {

  # 1. Global options: do not change them, just notify if requested
  if (isTRUE(set_options) && isTRUE(verbose)) {
    message(
      "Argument 'set_options' is deprecated and has no effect; ",
      "rCode no longer changes global options() for CRAN compliance.\n",
      "If you want these settings, call for example:\n",
      "  options(scipen = 999, digits = 10, digits.secs = 3)"
    )
  }

  # 2. Set conflict preferences
  if (isTRUE(set_conflicts)) {
    if (!requireNamespace("conflicted", quietly = TRUE)) {
      if (isTRUE(verbose)) {
        warning("Package 'conflicted' is not installed. Skipping conflict resolution.")
      }
    } else {
      preferences <- list(
        c("mutate", "dplyr"),
        c("filter", "dplyr"),
        c("select", "dplyr"),
        c("summarise", "dplyr"),
        c("summarize", "dplyr"),
        c("rename", "dplyr"),
        c("arrange", "dplyr"),
        c("first", "dplyr"),
        c("last", "dplyr"),
        c("lag", "dplyr"),
        c("recode", "dplyr"),
        c("src", "dplyr"),
        c("alpha", "scales"),
        c("col_factor", "scales"),
        c("annotate", "ggplot2"),
        c("%+%", "ggplot2"),
        c("ar", "brms"),
        c("cs", "brms"),
        c("bootCase", "car"),
        c("cache_info", "httr"),
        c("cohens_d", "effectsize"),
        c("eta_squared", "effectsize"),
        c("phi", "effectsize"),
        c("cor_test", "correlation"),
        c("describe", "psych"),
        c("headtail", "psych"),
        c("logit", "psych"),
        c("discard", "purrr"),
        c("some", "purrr"),
        c("display", "report"),
        c("expand", "tidyr"),
        c("extract", "tidyr"),
        c("pack", "tidyr"),
        c("unpack", "tidyr"),
        c("format_error", "insight"),
        c("format_message", "insight"),
        c("format_warning", "insight"),
        c("get_emmeans", "modelbased"),
        c("has_name", "tibble"),
        c("label", "xtable"),
        c("label<-", "Hmisc"),
        c("lmer", "lme4"),
        c("ngrps", "lme4"),
        c("rescale", "datawizard"),
        c("test", "devtools")
      )

      suppressMessages({
        for (p in preferences) {
          try(conflicted::conflict_prefer(p[1L], p[2L], quiet = TRUE), silent = TRUE)
        }
      })

      if (isTRUE(verbose)) {
        message("Conflict preferences set (favoring dplyr, ggplot2, etc.).")
      }
    }
  }

  # 3. Set ggplot2 theme
  if (isTRUE(set_theme)) {
    if (!requireNamespace("ggplot2", quietly = TRUE) ||
        !requireNamespace("see", quietly = TRUE)) {
      if (isTRUE(verbose)) {
        warning("Packages 'ggplot2' and/or 'see' not installed. Skipping theme setup.")
      }
    } else {
      custom_theme <- see::theme_lucid() +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          axis.title = ggplot2::element_text(size = 20),
          axis.text = ggplot2::element_text(size = 17),
          plot.title = ggplot2::element_text(size = 28),
          plot.subtitle = ggplot2::element_text(size = 17),
          legend.background = ggplot2::element_blank(),
          legend.position = "inside",
          legend.position.inside = c(0.85, 0.45),
          legend.text = ggplot2::element_text(size = 15),
          strip.text = ggplot2::element_text(size = 22)
        )

      ggplot2::theme_set(custom_theme)
      if (isTRUE(verbose)) {
        message("ggplot2 theme set to 'theme_lucid' with custom sizing.")
      }
    }
  }

  # 4. Citation
  if (isTRUE(print_citation)) {
    msg <- paste0(
      "\nIf you use these functions, please cite:\n\n",
      "Colley, M. (2024). rCode: Enhanced R Functions for Statistical Analysis and Reporting.\n",
      "Retrieved from https://github.com/M-Colley/rCode\n\n",
      "BibTeX:\n",
      "@misc{colley2024rcode,\n",
      "  author       = {Mark Colley},\n",
      "  title        = {rCode: Enhanced R Functions for Statistical Analysis and Reporting},\n",
      "  year         = {2024},\n",
      "  howpublished = {\\url{https://github.com/M-Colley/rCode}},\n",
      "  doi          = {10.5281/zenodo.16875755}\n",
      "}\n"
    )
    message(msg)
  }

  invisible(NULL)
}
