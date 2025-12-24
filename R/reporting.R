#' Generate the Latex-text based on the NPAV by Lüpsen (see \url{https://www.uni-koeln.de/~luepsen/R/}).
#' Only significant main and interaction effects are reported.
#' P-values are rounded for the third digit and partial eta squared values are provided when possible.
#' Attention: the independent variables of the formula and the term specifying the participant must be factors (i.e., use as.factor()).
#'
#' To easily copy and paste the results to your manuscript, the following commands must be defined in Latex:
#' \code{\\newcommand{\\F}[3]{$F({#1},{#2})={#3}$}}
#' \code{\\newcommand{\\p}{\\textit{p=}}}
#' \code{\\newcommand{\\pminor}{\\textit{p$<$}}}
#'
#' @param model the model of the np.anova
#' @param dv the name of the dependent variable that should be reported
#' @param write_to_clipboard whether to write to the clipboard
#'
#' @return A message describing the statistical results.
#' @export
#'
#' @examples
#' model <- data.frame(
#'   Df = c(1, 1, 10),
#'   `F value` = c(6.12, 5.01, NA),
#'   `Pr(>F)` = c(0.033, 0.045, NA),
#'   check.names = FALSE
#' )
#' rownames(model) <- c("Video", "gesture:eHMI", "Residuals")
#' reportNPAV(model, dv = "mental workload")

reportNPAV <- function(model, dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  .Deprecated("ARTool")
  not_empty(model)
  not_empty(dv)

  if ("Pr(>F)" %!in% colnames(model)) {
    message(paste0("No column ``Pr(>F)'' was found. Most likely, you want to use the command reportNPAVChi."))
  } else {
    if (!any(model$`Pr(>F)` < 0.05, na.rm = TRUE)) {
      if (write_to_clipboard) {
        message(paste0("The NPAV found no significant effects on ", dv, ". "))
        write_clip(paste0("The NPAV found no significant effects on ", dv, ". "))
      } else {
        message(paste0("The NPAV found no significant effects on ", dv, ". "))
      }
    } else {
      # there is a significant effect if any value is under 0.05
      # make the names accessible in a novel column
      model$descriptions <- rownames(model)

      # no empty space to allow backslash later
      model$descriptions <- gsub(":", " X", model$descriptions)


      for (i in 1:length(model$`Pr(>F)`)) {
        # Residuals have NA therefore, we need this double-check
        if (!is.na(model$`Pr(>F)`[i]) && model$`Pr(>F)`[i] < 0.05) {
          Fvalue <- round(model$`F value`[i], digits = 2) # round(model$`F value`[i], digits = 2)
          numeratordf <- model$Df[i]

          # denominator is next with an NA
          # potential for out-of-bounds
          for (k in i:length(model$`Pr(>F)`)) {
            if (is.na(model$`Pr(>F)`[k])) {
              denominatordf <- model$Df[k]
              break
            }
          }

          pValueNumeric <- model$`Pr(>F)`[i]
          if (pValueNumeric < 0.001) {
            pValue <- paste0("\\pminor{0.001}")
          } else {
            pValue <- paste0("\\p{", sprintf("%.3f", round(pValueNumeric, digits = 3)), "}")
          }


          if (str_detect(model$descriptions[i], "X")) {
            stringtowrite <- paste0("The NPAV found a significant interaction effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", numeratordf, "}{", denominatordf, "}{", sprintf("%.2f", Fvalue), "}, ", pValue, ")")
          } else {
            stringtowrite <- paste0("The NPAV found a significant main effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", numeratordf, "}{", denominatordf, "}{", sprintf("%.2f", Fvalue), "}, ", pValue, ")")
          }

          effect_size_text <- ""
          if (!is.na(denominatordf) && is.finite(denominatordf)) {
            effect_size <- tryCatch(
              effectsize::F_to_eta2(
                f = Fvalue,
                df = numeratordf,
                df_error = denominatordf,
                ci = 0.95
              ),
              error = function(e) NULL
            )

            if (!is.null(effect_size)) {
              effect_size <- as.data.frame(effect_size)
              eta_value <- effect_size$Eta2_partial
              ci_low <- effect_size$CI_low
              ci_high <- effect_size$CI_high
              if (!is.null(eta_value) && !is.na(eta_value)) {
                effect_size_text <- paste0(
                  ", $\\eta_{p}^{2}$=",
                  sprintf("%.2f", eta_value)
                )
                if (!is.null(ci_low) && !is.null(ci_high) && !any(is.na(c(ci_low, ci_high)))) {
                  effect_size_text <- paste0(
                    effect_size_text,
                    " [",
                    sprintf("%.2f", ci_low),
                    ", ",
                    sprintf("%.2f", ci_high),
                    "]"
                  )
                }
              }
            }
          }

          stringtowrite <- paste0(stringtowrite, effect_size_text, ". ")

          # gsub backslash needs four \: https://stackoverflow.com/questions/27491986/r-gsub-replacing-backslashes
          # nice format of X in Latex via \times
          # Replace "X" with LaTeX code if preceded by a space
          stringtowrite <- gsub("(?<=\\s)X", "$\\\\times$ \\\\", stringtowrite, perl = TRUE)

          if (write_to_clipboard) {
            message(stringtowrite)
            write_clip(stringtowrite)
          } else {
            message(stringtowrite)
          }
        }
      }
    }
  }
}


#' Generate the Latex-text based on the ARTool (see \url{https://github.com/mjskay/ARTool}). The ART result must be piped into an anova().
#' Only significant main and interaction effects are reported.
#' P-values are rounded for the third digit.
#' Attention: Effect sizes are not calculated!
#' Attention: the independent variables of the formula and the term specifying the participant must be factors (i.e., use as.factor()).
#'
#' To easily copy and paste the results to your manuscript, the following commands must be defined in Latex:
#' \code{\\newcommand{\\F}[3]{$F({#1},{#2})={#3}$}}
#' \code{\\newcommand{\\p}{\\textit{p=}}}
#' \code{\\newcommand{\\pminor}{\\textit{p$<$}}}
#'
#' @param model the model of the art
#' @param dv the name of the dependent variable that should be reported
#' @param write_to_clipboard whether to write to the clipboard
#'
#' @return A message describing the statistical results.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ARTool", quietly = TRUE)) {
#'   set.seed(123)
#'
#'   main_df <- data.frame(
#'     tlx_mental = stats::rnorm(80),
#'     Video      = factor(rep(c("A", "B"), each = 40)),
#'     gesture    = factor(rep(c("G1", "G2"), times = 40)),
#'     eHMI       = factor(rep(c("On", "Off"), times = 40)),
#'     UserID     = factor(rep(1:20, each = 4))
#'   )
#'
#'   art_model <- ARTool::art(
#'     tlx_mental ~ Video * gesture * eHMI +
#'       Error(UserID / (gesture * eHMI)),
#'     data = main_df
#'   )
#'
#'   model_anova <- stats::anova(art_model)
#'   reportART(model_anova, dv = "mental demand")
#' }
#' }
reportART <- function(model, dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  # Check that the model and dependent variable are not empty
  not_empty(model)
  not_empty(dv)

  # Check if the model has a "Pr(>F)" column
  if ("Pr(>F)" %!in% colnames(model)) {
    message(paste0("No column ``Pr(>F)'' was found."))
  } else {
    # Check if any p-values are significant
    if (!any(model$`Pr(>F)` < 0.05, na.rm = TRUE)) {
      # Output a message depending on the write_to_clipboard option
      message_to_write <- paste0("The ART found no significant effects on ", dv, ". ")
      if (write_to_clipboard) {
        message(message_to_write)
        write_clip(message_to_write)
      } else {
        message(message_to_write)
      }
    } else {
      # Process significant effects
      model$descriptions <- model[, 1] # Make the names accessible
      model$descriptions <- gsub(":", " X", model$descriptions) # Replace colon with "X"

      for (i in 1:length(model$`Pr(>F)`)) {
        if (!is.na(model$`Pr(>F)`[i]) && model$`Pr(>F)`[i] < 0.05) {
          # Extract and round values
          Fvalue <- round(model$`F value`[i], digits = 2)
          numeratordf <- model$Df[i]
          denominatordf <- model$Df.res[i]
          pValueNumeric <- model$`Pr(>F)`[i]
          pValue <- if (pValueNumeric < 0.001) paste0("\\pminor{0.001}") else paste0("\\p{", sprintf("%.3f", round(pValueNumeric, digits = 3)), "}")

          # Write interaction or main effect depending on the presence of "X"
          effect_type <- if (str_detect(model$descriptions[i], "X")) "interaction" else "main"
          stringtowrite <- paste0("The ART found a significant ", effect_type, " effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", numeratordf, "}{", denominatordf, "}{", sprintf("%.2f", Fvalue), "}, ", pValue, "). ")
          # Derive effect sizes via effectsize::F_to_eta2
          effect_size <- tryCatch(
            effectsize::F_to_eta2(
              f = Fvalue,
              df = numeratordf,
              df_error = denominatordf,
              ci = 0.95
            ),
            error = function(e) NULL
          )

          effect_size_text <- ""
          if (!is.null(effect_size)) {
            effect_size <- as.data.frame(effect_size)
            eta_value <- effect_size$Eta2_partial
            ci_low <- effect_size$CI_low
            ci_high <- effect_size$CI_high

            if (!is.null(eta_value) && !is.na(eta_value)) {
              effect_size_text <- paste0(
                ", $\\eta_{p}^{2}$ = ",
                sprintf("%.2f", eta_value)
              )

              if (!is.null(ci_low) && !is.null(ci_high) && !any(is.na(c(ci_low, ci_high)))) {
                effect_size_text <- paste0(
                  effect_size_text,
                  ", 95\\% CI: [",
                  sprintf("%.2f", ci_low),
                  ", ",
                  sprintf("%.2f", ci_high),
                  "]"
                )
              }
            }
          }

          # Write interaction or main effect depending on the presence of "X"
          effect_type <- if (str_detect(model$descriptions[i], "X")) "interaction" else "main"
          stringtowrite <- paste0(
            "The ART found a significant ",
            effect_type,
            " effect of \\",
            trimws(model$descriptions[i]),
            " on ",
            dv,
            " (\\F{",
            numeratordf,
            "}{",
            denominatordf,
            "}{",
            sprintf("%.2f", Fvalue),
            "}, ",
            pValue
          )

          if (nzchar(effect_size_text)) {
            stringtowrite <- paste0(stringtowrite, effect_size_text, ")")
          }

          stringtowrite <- paste0(stringtowrite, ". ")

          # Replace "X" with LaTeX code if preceded by a space
          stringtowrite <- gsub("(?<=\\s)X", "$\\\\times$ \\\\", stringtowrite, perl = TRUE)

          # Output the string depending on the write_to_clipboard option
          if (write_to_clipboard) {
            message(stringtowrite)
            write_clip(stringtowrite)
          } else {
            message(stringtowrite)
          }
        }
      }
    }
  }
}


#' Report the model produced by nparLD. The model provided must be the model generated by the command 'nparLD' \code{\link[nparLD]{nparLD}} (see \url{https://CRAN.R-project.org/package=nparLD}).
#'
#' #' Only significant main and interaction effects are reported.
#' P-values are rounded for the third digit and relative treatment effects (RTE) are included when available.
#' Attention: the independent variables of the formula and the term specifying the participant must be factors (i.e., use as.factor()).
#'
#' #' To easily copy and paste the results to your manuscript, the following commands must be defined in Latex:
#' \code{\\newcommand{\\F}{\\textit{F=}}}
#' \code{\\newcommand{\\p}{\\textit{p=}}}
#' \code{\\newcommand{\\pminor}{\\textit{p$<$}}}
#' @param model the model
#' @param dv the dependent variable
#' @param write_to_clipboard whether to write to the clipboard
#'
#' @return A message describing the statistical results.
#' @export
#'
#' @examples \donttest{
#' if (requireNamespace("nparLD", quietly = TRUE)) {
#'   # Small toy data set for nparLD
#'   set.seed(123)
#'   example_data <- data.frame(
#'     Subject = factor(rep(1:10, each = 3)),
#'     Time    = factor(rep(c("T1", "T2", "T3"), times = 10)),
#'     TLX1    = stats::rnorm(30, mean = 50, sd = 10)
#'   )
#'
#'   # Fit nparLD model
#'   model <- nparLD::nparLD(
#'     TLX1 ~ Time,
#'     data        = example_data,
#'     subject     = "Subject",
#'     description = FALSE
#'   )
#'
#'   # Report the nparLD result
#'   reportNparLD(model, dv = "TLX1")
#'   }
#' }
reportNparLD <- function(model, dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  not_empty(model)
  not_empty(dv)

  # first retrieve relevant subset
  model <- as.data.frame(model$ANOVA.test)

  if (!any(model$`p-value` < 0.05, na.rm = TRUE)) {
    message(paste0("The NPAV found no significant effects on ", dv, ". "))
  }

  # there is a significant effect if any value is under 0.05
  # make the names accessible in a novel column
  model$descriptions <- rownames(model)

  model$descriptions <- gsub(":", " X", model$descriptions)


  for (i in 1:length(model$`p-value`)) {
    # Residuals have NA therefore we need this double check
    if (!is.na(model$`p-value`[i]) && model$`p-value`[i] < 0.05) {
      Fvalue <- sprintf("%.2f", round(model$`Statistic`[i], digits = 2)) # round(model$`Statistic`[i], digits = 2)
      numeratordf <- round(model$df[i], digits = 0)

      pValueNumeric <- model$`p-value`[i]
      if (pValueNumeric < 0.001) {
        pValue <- paste0("\\pminor{0.001}")
      } else {
        pValue <- paste0("\\p{", sprintf("%.3f", round(pValueNumeric, digits = 3)), "}")
      }


      if (str_detect(model$descriptions[i], "X")) {
        stringtowrite <- paste0("The NPVA found a significant interaction effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", Fvalue, "}, \\df{", numeratordf, "}, ", pValue, ")")
      } else {
        stringtowrite <- paste0("The NPVA found a significant main effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", Fvalue, "}, \\df{", numeratordf, "}, ", pValue, ")")
      }

      effect_size_text <- ""
      if ("RTE" %in% names(model)) {
        rte_value <- model$RTE[i]
        if (!is.null(rte_value) && !is.na(rte_value)) {
          effect_size_text <- paste0(
            ", $RTE=",
            sprintf("%.2f", rte_value)
          )
        }
      }

      stringtowrite <- paste0(stringtowrite, effect_size_text, ". ")

      # gsub backslash needs four \: https://stackoverflow.com/questions/27491986/r-gsub-replacing-backslashes
      # nice format of X in Latex via \times
      # Replace "X" with LaTeX code if preceded by a space
      stringtowrite <- gsub("(?<=\\s)X", "$\\\\times$ \\\\", stringtowrite, perl = TRUE)

      if (write_to_clipboard) {
        message(stringtowrite)
        write_clip(stringtowrite)
      } else {
        message(stringtowrite)
      }
    }
  }
}


#' Transform text from `report::report()` into LaTeX-friendly output.
#'
#' This function transforms the text output from `report::report()` by performing several substitutions
#' to prepare the text for LaTeX typesetting. In particular, it replaces instances of `R2`, `%`, and `~` with
#' the corresponding LaTeX code. Additionally, it provides options to:
#' \itemize{
#'   \item Omit bullet items marked as "non-significant" (when `only_sig = TRUE`).
#'   \item Remove a concluding note about standardized parameters (when `remove_std = TRUE`).
#'   \item Wrap bullet items in a LaTeX `itemize` environment or leave them as plain text (controlled by `itemize`).
#' }
#'
#' @param x Character vector or a single string containing the report text.
#' @param print_result Logical. If `TRUE` (default), the formatted text is printed to the console.
#' @param only_sig Logical. If `TRUE`, bullet items containing "non-significant" are omitted. Default is `FALSE`.
#' @param remove_std Logical. If `TRUE`, the final standardized parameters note is removed. Default is `FALSE`.
#' @param itemize Logical. If `TRUE` (default), bullet items are wrapped in a LaTeX `itemize` environment;
#'   otherwise the bullet markers are simply removed.
#'
#' @return A single string with the LaTeX-friendly formatted report text.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("report", quietly = TRUE)) {
#'   # Simple linear model on the iris dataset
#'   model <- stats::lm(
#'     Sepal.Length ~ Sepal.Width + Petal.Length,
#'     data = datasets::iris
#'   )
#'
#'   # Format the report output, showing only significant items, removing the
#'   # standard note, and wrapping bullet items in an itemize environment.
#'   latexify_report(
#'     report::report(model),
#'     only_sig = TRUE,
#'     remove_std = TRUE,
#'     itemize = TRUE
#'   )
#' }
#' }
latexify_report <- function(x,
                            print_result = TRUE,
                            only_sig = FALSE,
                            remove_std = FALSE,
                            itemize = TRUE) {
  # If x is a character vector of lines, collapse to a single string
  if (length(x) > 1) {
    x <- paste(x, collapse = "\n")
  }

  # Perform substitutions:
  #   1. Replace "R2" with "$R^2$"
  #   2. Replace "%" with "\%"
  #   3. Replace "~" with "$\\sim$"
  out <- x |>
    gsub("R2", "$R^2$", x = _, fixed = TRUE) |>
    gsub("%", "\\%", x = _, fixed = TRUE) |>
    gsub("~", "$\\sim$", x = _, fixed = TRUE) |>
    gsub("Rhat", "$\\hat{R}$", x = _, fixed = TRUE)

  # Split into individual lines for processing
  lines <- strsplit(out, "\n")[[1]]

  # Prepare to reconstruct the report line-by-line.
  new_lines <- c()
  bullet_block <- c() # temporary holder for bullet items
  in_bullet_block <- FALSE # flag to denote if we are collecting bullet items

  # Define a pattern to identify the standard note line
  std_pattern <- "Standardized parameters were obtained by fitting the model"

  for (line in lines) {
    # Optionally remove the final standard note line
    if (remove_std && grepl(std_pattern, line, fixed = TRUE)) {
      next # Skip this line entirely
    }

    # Check if the line is a bullet candidate (i.e., starts with a dash)
    if (grepl("^\\s*-\\s+", line)) {
      # If only_sig==TRUE, skip bullet items that contain "non-significant"
      if (only_sig && grepl("non-significant", line, fixed = TRUE)) {
        next
      }

      if (itemize) {
        # Replace initial dash with LaTeX \item and add to bullet_block
        bullet_item <- sub("^\\s*-\\s+", "\\\\item ", line)
        bullet_block <- c(bullet_block, bullet_item)
        in_bullet_block <- TRUE
      } else {
        # If not itemizing, simply remove the dash and add the line directly
        new_lines <- c(new_lines, sub("^\\s*-\\s+", "", line))
      }
    } else {
      # If we reach a non-bullet line while inside a bullet block,
      # flush the bullet block into the new_lines (if itemize is TRUE)
      if (in_bullet_block && itemize) {
        new_lines <- c(new_lines, "\\begin{itemize}", bullet_block, "\\end{itemize}")
        # Reset bullet block and flag
        bullet_block <- c()
        in_bullet_block <- FALSE
      }
      # Add the non-bullet line
      new_lines <- c(new_lines, line)
    }
  }
  # At the end, if a bullet block is pending, flush it now
  if (in_bullet_block && itemize) {
    new_lines <- c(new_lines, "\\begin{itemize}", bullet_block, "\\end{itemize}")
  }

  # Re-combine the resulting lines into a single string.
  out <- paste(new_lines, collapse = "\n")

  # Optionally print to the console
  if (print_result) {
    message(out, "\n")
  }

  invisible(out)
}


#' Report the mean and standard deviation of a dependent variable for all levels of an independent variable rounded to the 2nd digit.
#'
#' #' To easily copy and paste the results to your manuscript, the following commands must be defined in Latex:
#' \code{\\newcommand{\\m}{\\textit{M=}}}
#' \code{\\newcommand{\\sd}{\\textit{SD=}}}
#' @param data the data frame
#' @param iv the independent variable
#' @param dv the dependent variable
#'
#' @return Mean and SD values
#' @export
#'
#' @examples \donttest{
#'
#' example_data <- data.frame(Condition = rep(c("A", "B", "C"),
#' each = 10), TLX1 = stats::rnorm(30))
#'
#' reportMeanAndSD(example_data, iv = "Condition", dv = "TLX1")
#' }
reportMeanAndSD <- function(data, iv = "testiv", dv = "testdv") {
  not_empty(data)
  not_empty(iv)
  not_empty(dv)

  test <- data |>
    drop_na(!!sym(iv)) |>
    drop_na(!!sym(dv)) |>
    group_by(!!sym(iv)) |>
    dplyr::summarise(across(!!sym(dv), list(mean = mean, sd = sd)))

  for (i in 1:nrow(test)) {
    row <- test[i, ]
    # do stuff with row
    message(paste0("%", row[[1]], ": \\m{", sprintf("%.2f", round(row[[2]], digits = 2)), "}, \\sd{", sprintf("%.2f", round(row[[3]], digits = 2)), "}\n"))
  }

  invisible(NULL)
}


#' Report statistical details for ggstatsplot.
#'
#' @param p the object returned by ggwithinstats or ggbetweenstats
#' @param iv the independent variable
#' @param dv the dependent variable
#' @param write_to_clipboard whether to write to the clipboard
#'
#' @return A message describing the statistical results.
#' @export
#'
#' @examples \donttest{
#' library(ggstatsplot)
#' library(dplyr)
#'
#' # Generate a plot
#' plt <- ggbetweenstats(mtcars, am, mpg)
#'
#' reportggstatsplot(plt, iv = "am", dv = "mpg")
#' }
reportggstatsplot <- function(p, iv = "independent", dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  not_empty(p)
  not_empty(dv)
  not_empty(iv)

  stats <- ggstatsplot::extract_stats(p)$subtitle_data
  resultString <- ""

  effectSize <- round(stats$estimate, digits = 2)
  pValueNumeric <- round(stats$p.value, digits = 3)
  if (pValueNumeric < 0.001) {
    pValue <- paste0("\\pminor{0.001}")
  } else {
    pValue <- paste0("\\p{", sprintf("%.3f", pValueNumeric), "}")
  }

  statistic <- round(stats$statistic, digits = 2)

  # Create String
  if (stats$method %in% c("Kruskal-Wallis rank sum test", "Friedman rank sum test")) {
    resultString <- paste0("(\\chisq(", stats$df.error, ")=", statistic, ", ", pValue, ", r=", effectSize, ")")
  } else if (stats$method %in% c("Paired t-test")) {
    resultString <- paste0("(t(", stats$df.error, ")=", statistic, ", ", pValue, ", r=", effectSize, ")")
  } else if (stats$method %in% c("Wilcoxon signed rank test")) {
    resultString <- paste0("(V=", statistic, ", ", pValue, ", r=", effectSize, ")")
  } else {
    # example: \F{7}{24.62}{1.01}, \p{0.45}
    resultString <- paste0("(\\F{", stats$df, "}{", stats$df.error, "}{", statistic, "}, ", pValue, ", r=", effectSize, ")")
  }


  if (!stats$p.value < 0.05) {
    msg <- paste0("A ", stats$method, " found no significant effects on ", dv, " ", resultString, ". ")
  } else {
    msg <- paste0("A ", stats$method, " found a significant effect of \\", iv, " on ", dv, " ", resultString, ". ")
  }

  if (write_to_clipboard) {
    message(msg)
    clipr::write_clip(msg)
  } else {
    message(msg)
  }

  invisible(NULL)
}


#' Report significant post-hoc pairwise comparisons
#'
#' This function extracts significant pairwise comparisons from a `ggstatsplot` object,
#' calculates the mean and standard deviation for the groups involved using the raw data,
#' and prints LaTeX-formatted sentences reporting the results.
#'
#' @section LaTeX Requirements:
#' To easily copy and paste the results to your manuscript, the following commands
#' (or similar) must be defined in your LaTeX preamble, as the function outputs
#' commands taking arguments (e.g., `\m{value}`):
#'
#' \preformatted{
#'   \newcommand{\m}[1]{\\textit{M}=#1}
#'   \newcommand{\sd}[1]{\\textit{SD}=#1}
#'   \newcommand{\padj}[1]{$p_{adj}=#1$}
#'   \newcommand{\padjminor}[1]{$p_{adj}<#1$}
#' }
#'
#' @param data A data frame containing the raw data used to generate the plot.
#' @param p A `ggstatsplot` object (e.g., returned by `ggbetweenstats`) containing the pairwise comparison statistics.
#' @param iv Character string. The column name of the independent variable (grouping variable).
#' @param dv Character string. The column name of the dependent variable.
#' @param label_mappings Optional named list or vector. Used to rename factor levels in the output text
#' (e.g., `list("old_name" = "New Label")`).
#'
#' @return No return value. The function prints LaTeX-formatted text to the console.
#' @export
#'
#' @examples
#' \donttest{
#' library(ggstatsplot)
#' library(dplyr)
#'
#' # Generate a plot
#' plt <- ggbetweenstats(mtcars, am, mpg)
#'
#' # Report stats
#' reportggstatsplotPostHoc(
#'   data = mtcars,
#'   p = plt,
#'   iv = "am",
#'   dv = "mpg",
#'   label_mappings = list("0" = "Automatic", "1" = "Manual")
#' )
#' }
reportggstatsplotPostHoc <- function(data, p, iv = "testiv", dv = "testdv", label_mappings = NULL) {
  # Asserts to ensure non-empty inputs
  not_empty(data)
  not_empty(p)
  not_empty(iv)
  not_empty(dv)

  # Extract stats from the ggstatsplot object
  stats <- ggstatsplot::extract_stats(p)$pairwise_comparisons_data

  if (!any(stats$p.value < 0.05, na.rm = TRUE)) {
    message(paste0("A post-hoc test found no significant differences for ", dv, ". "))
    return()
  }

  for (i in 1:length(stats$p.value)) {
    if (!is.na(stats$p.value[i]) && stats$p.value[i] < 0.05) {
      # Format p-value
      pValue <- if (stats$p.value[i] < 0.001) "\\padjminor{0.001}" else paste0("\\padj{", sprintf("%.3f", round(stats$p.value[i], 3)), "}")

      # Get conditions
      firstCondition <- stats$group1[i]
      secondCondition <- stats$group2[i]


      # Apply label mappings if available
      firstLabel <- ifelse(is.null(label_mappings), firstCondition, label_mappings[[firstCondition]])
      secondLabel <- ifelse(is.null(label_mappings), secondCondition, label_mappings[[secondCondition]])

      valueOne <- data |>
        filter(!!sym(iv) == firstCondition) |>
        dplyr::summarise(across(!!sym(dv), list(mean = mean, sd = sd)))

      valueTwo <- data |>
        filter(!!sym(iv) == secondCondition) |>
        dplyr::summarise(across(!!sym(dv), list(mean = mean, sd = sd)))

      # Format statistics
      firstStatsStr <- paste0(" (\\m{", sprintf("%.2f", as.numeric(round(valueOne[1, 1], 2))), "}, \\sd{", sprintf("%.2f", as.numeric(round(valueOne[1, 2], 2))), "})")
      secondStatsStr <- paste0(" (\\m{", sprintf("%.2f", as.numeric(round(valueTwo[1, 1], 2))), "}, \\sd{", sprintf("%.2f", as.numeric(round(valueTwo[1, 2], 2))), "})")

      # Construct and print output string
      if (as.numeric(round(valueOne[1, 1], 2)) > as.numeric(round(valueTwo[1, 1], 2))) {
        message(paste0("A post-hoc test found that ", firstLabel, " was significantly higher", firstStatsStr, " in terms of \\", dv, " compared to ", secondLabel, secondStatsStr, "; ", pValue, "). "))
      } else {
        message(paste0("A post-hoc test found that ", secondLabel, " was significantly higher", secondStatsStr, " in terms of \\", dv, " compared to ", firstLabel, firstStatsStr, "; ", pValue, "). "))
      }
    }
  }
  invisible(NULL)
}


#' Report dunnTest as text. Required commands in LaTeX:
#' \code{\\newcommand{\\padjminor}{\\textit{p$_{adj}<$}}}
#' \code{\\newcommand{\\padj}{\\textit{p$_{adj}$=}}}
#' \code{\\newcommand{\\rankbiserial}[1]{$r_{rb} = #1$}}
#'
#' @param d the dunn test object
#' @param data the data frame
#' @param iv independent variable
#' @param dv dependent variable
#'
#' @return A message describing the statistical results.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("FSA", quietly = TRUE)) {
#'   # Use built-in iris data
#'   data(iris)
#'
#'   # Dunn test on Sepal.Length by Species
#'   d <- FSA::dunnTest(Sepal.Length ~ Species,
#'     data   = iris,
#'     method = "holm"
#'   )
#'
#'   # Report the Dunn test
#'   reportDunnTest(d,
#'     data = iris,
#'     iv   = "Species",
#'     dv   = "Sepal.Length"
#'   )
#' }
#' }
reportDunnTest <- function(d, data, iv = "testiv", dv = "testdv") {
  not_empty(data)
  not_empty(d)
  not_empty(iv)
  not_empty(dv)

  # Check for significance globally first
  # Note: d$res$P.adj can contain NAs, so we remove them for the check
  if (!any(d$res$P.adj < 0.05, na.rm = TRUE)) {
    message(paste0("A post-hoc test found no significant differences for ", dv, ". "))
    return(invisible(NULL))
  }

  # 1. Collect all significant findings into a data frame/list
  findings <- list()

  for (i in 1:length(d$res$P.adj)) {
    if (!is.na(d$res$P.adj[i]) && d$res$P.adj[i] < 0.05) {
      # --- P-Value Formatting ---
      pValueNumeric <- d$res$P.adj[i]
      if (pValueNumeric < 0.001) {
        pValueStr <- "\\padjminor{0.001}"
      } else {
        pValueStr <- paste0("\\padj{", sprintf("%.3f", round(pValueNumeric, digits = 3)), "}")
      }

      # --- Split Conditions ---
      # Assuming standard Dunn output "A - B"
      parts <- strsplit(d$res$Comparison[i], " - ", fixed = TRUE)[[1]]
      condA <- parts[1]
      condB <- parts[2]

      # --- Calculate Effect Size ---
      data_subset <- data |>
        dplyr::filter(!!sym(iv) %in% c(condA, condB))

      esStr <- ""
      tryCatch(
        {
          es <- effectsize::rank_biserial(as.formula(paste(dv, "~", iv)), data = data_subset)
          esStr <- paste0(", \\rankbiserial{", sprintf("%.2f", abs(es$r_rank_biserial)), "}")
        },
        error = function(e) {}
      )

      # --- Calculate Means/SDs ---
      statsA <- data |>
        dplyr::filter(!!sym(iv) == condA) |>
        summarise(m = mean(!!sym(dv), na.rm = TRUE), sd = sd(!!sym(dv), na.rm = TRUE))

      statsB <- data |>
        dplyr::filter(!!sym(iv) == condB) |>
        summarise(m = mean(!!sym(dv), na.rm = TRUE), sd = sd(!!sym(dv), na.rm = TRUE))

      strStatsA <- paste0("(\\m{", sprintf("%.2f", statsA$m), "}, \\sd{", sprintf("%.2f", statsA$sd), "})")
      strStatsB <- paste0("(\\m{", sprintf("%.2f", statsB$m), "}, \\sd{", sprintf("%.2f", statsB$sd), "})")

      # --- Determine Direction (Winner vs Loser) ---
      if (statsA$m >= statsB$m) {
        winner <- trimws(condA)
        winnerStats <- strStatsA
        loser <- trimws(condB)
        # The stats/p/es string for the "loser" part of the sentence
        loserString <- paste0(
          trimws(condB), " (\\m{", sprintf("%.2f", statsB$m),
          "}, \\sd{", sprintf("%.2f", statsB$sd), "}; ", pValueStr, esStr, ")"
        )
      } else {
        winner <- trimws(condB)
        winnerStats <- strStatsB
        loser <- trimws(condA)
        loserString <- paste0(
          trimws(condA), " (\\m{", sprintf("%.2f", statsA$m),
          "}, \\sd{", sprintf("%.2f", statsA$sd), "}; ", pValueStr, esStr, ")"
        )
      }

      # Store finding
      findings[[length(findings) + 1]] <- list(
        winner = winner,
        winnerStats = winnerStats,
        loserString = loserString
      )
    }
  }

  # 2. Group findings by Winner and construct sentences
  if (length(findings) > 0) {
    # Convert list to dataframe for easier grouping
    df_res <- do.call(rbind, lapply(findings, as.data.frame, stringsAsFactors = FALSE))

    unique_winners <- unique(df_res$winner)

    for (w in unique_winners) {
      # Get all entries where this condition was the winner
      subset_res <- df_res[df_res$winner == w, ]

      # Helper for Oxford comma logic (A, B, and C)
      losers <- subset_res$loserString
      n <- length(losers)

      if (n == 1) {
        joined_losers <- losers[1]
      } else if (n == 2) {
        joined_losers <- paste(losers, collapse = " and ")
      } else {
        # Oxford comma: "A, B, and C"
        joined_losers <- paste0(paste(losers[1:(n - 1)], collapse = ", "), ", and ", losers[n])
      }

      # --- Construct the final sentence ---
      # Replace "scenario" with the LaTeX formatted IV name (e.g., \miou)
      iv_cmd <- paste0("\\", iv)

      final_str <- paste0(
        "A post-hoc test found that ", dv, " for the ", iv_cmd, " ", w,
        " was significantly higher ", subset_res$winnerStats[1],
        " than for ", joined_losers, ". "
      )

      message(final_str)
    }
  }
  invisible(NULL)
}


#' report Dunn test as a table. Customizable with sensible defaults. Required commands in LaTeX:
#' \code{\\newcommand{\\padjminor}{\\textit{p$_{adj}<$}}}
#' \code{\\newcommand{\\padj}{\\textit{p$_{adj}$=}}}
#' \code{\\newcommand{\\rankbiserial}[1]{$r_{rb} = #1$}}
#'
#' @param d the dunn test object
#' @param data the data frame
#' @param iv independent variable
#' @param dv dependent variable
#' @param orderByP whether to order by the p value
#' @param numberDigitsForPValue the number of digits to show
#' @param latexSize which size for the text
#' @param orderText whether to order the text
#'
#' @return A message describing the statistical results in a table.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("FSA", quietly = TRUE)) {
#'   # Use built-in iris data
#'   data(iris)
#'
#'   # Dunn test on Sepal.Length by Species
#'   d <- FSA::dunnTest(Sepal.Length ~ Species,
#'     data   = iris,
#'     method = "holm"
#'   )
#'
#'   # Report the Dunn test
#'   reportDunnTestTable(d,
#'     data = iris,
#'     iv   = "Species",
#'     dv   = "Sepal.Length"
#'   )
#' }
#' }
reportDunnTestTable <- function(d = NULL, data, iv = "testiv", dv = "testdv", orderByP = FALSE, numberDigitsForPValue = 4, latexSize = "small", orderText = TRUE) {
  not_empty(data)
  not_empty(iv)
  not_empty(dv)

  # If d is not provided, calculate it
  if (is.null(d)) {
    d <- FSA::dunnTest(as.formula(paste(dv, "~", iv)), data = data, method = "holm")
  }


  # Use the dunn test result that was passed in
  # dunnTest returns a list with $res component
  table <- data.frame(
    Comparison = d$res$Comparison,
    Z = d$res$Z,
    `p-adjusted` = d$res$P.adj,
    check.names = FALSE
  )

  # only show significant ones
  table <- subset(table, `p-adjusted` < 0.05)

  # Check if there are any significant results
  if (nrow(table) == 0) {
    message(paste0("A post-hoc test found no significant differences for ", dv, ". "))
    return(invisible(NULL))
  }

  # Calculate effect sizes for all comparisons (only for significant ones)
  effectSizes <- numeric(nrow(table))
  for (i in 1:nrow(table)) {
    comparison <- as.character(table[i, "Comparison"])
    firstCondition <- trimws(strsplit(comparison, " - ", fixed = TRUE)[[1]][1])
    secondCondition <- trimws(strsplit(comparison, " - ", fixed = TRUE)[[1]][2])

    data_subset <- data |>
      filter(!!sym(iv) %in% c(firstCondition, secondCondition))

    tryCatch(
      {
        es <- effectsize::rank_biserial(as.formula(paste(dv, "~", iv)),
          data = data_subset
        )
        effectSizes[i] <- abs(es$r_rank_biserial)
      },
      error = function(e) {
        effectSizes[i] <- NA
      }
    )
  }

  # Add effect size column
  table$r <- effectSizes

  if (orderByP) {
    table <- table[order(table$`p-adjusted`), ]
  }

  if (orderText) {
    table <- table[order(table$Comparison), ]
  }

  # Replace 0.000 with <0.001 automatically
  table$`p-adjusted` <- ifelse(table$`p-adjusted` < 0.001, "<0.001",
    formatC(table$`p-adjusted`, digits = numberDigitsForPValue, format = "f")
  )

  # Format effect size
  table$r <- formatC(table$r, digits = 2, format = "f")

  # Adjust the xtable call to handle the modified columns
  if (requireNamespace("xtable", quietly = TRUE)) {
    xtable_obj <- xtable::xtable(table,
      digits = c(0, 0, 4, 0, 0),
      caption = paste0(
        "Post-hoc comparisons for independent variable \\", iv,
        " and dependent variable \\", dv,
        ". Positive Z-values mean that the first-named level is sig. higher than the second-named. For negative Z-values, the opposite is true. Effect size reported as rank-biserial correlation (r)."
      ),
      label = paste0("tab:posthoc-", iv, "-", dv)
    )

    print(xtable_obj, type = "latex", size = latexSize, caption.placement = "top", include.rownames = FALSE)
  } else {
    message(paste0(
      "Post-hoc comparisons for independent variable \\", iv,
      " and dependent variable \\", dv,
      ". Positive Z-values mean that the first-named level is sig. higher than the second-named. For negative Z-values, the opposite is true. Effect size reported as rank-biserial correlation (r).\n"
    ))
    print(table)
  }

  invisible(NULL)
}
