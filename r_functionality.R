# enhance reproducibility
usethis::use_blank_slate()


# For documentation: To insert a documentation skeleton in RStudio use Ctrl + Alt + Shift + R



# install.packages("extrafont")
# library(extrafont)
# font_import()
# loadfonts()


library(pacman)

#read_xslx delivers a tibble
#read.xslx a data.frame, therefore, this is needed: main_df <- as.data.frame(main_df) after using read_xslx

pacman::p_load(devtools, clipr, readxl, tidyverse, Cairo, rstatix, nparLD, FSA, PMCMRplus, report, psych, RColorBrewer, pals, wesanderson, ggstatsplot, styler, pastecs)

# library(showtext)

source_url("http://www.uni-koeln.de/~luepsen/R/np.anova.R")

# don't use scientific notation
options(scipen = 999)
options(digits = 10)


# Box plot
# wes_palette("Royal1", n=5)
# wes_palette("Cavalcanti1", n=5)
# wes_palette("FantasticFox1", n=5)
# wes_palette("Darjeeling1", n=5)

# not in 
'%!in%' <- function(x,y)!('%in%'(x,y))

# replace NA with zero
# e.g. df$test <- na.zero(df$test)
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}


#' Check whether Internet connection is present.
#'
#' @return
#' @export
#'
#' @examples
havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}





# update all available packages


if (havingIP()) {
  
  # list all packages where an update is available
  numberOldPackages <- nrow(old.packages())
  
  if(!is.null(numberOldPackages) && numberOldPackages > 0){
    warning("YOU HAVE TO UPDATE PACKAGES as ", numberOldPackages, " are outdated!")
    warning(paste(old.packages()[, c("Package")], collapse=", "))
  }else{
    message("All packages are up-to-date.")
  }
  
  # update
  #update.packages(ask = TRUE)

  # https://fonts.google.com/
  # You will need to have internet connection
  # If you restart R you will need to execute this code again to use the font
  # font_add_google(name = "Pacifico",   # Name of the font on the Google Fonts site
  #                 family = "pacifico") # Name you want to use to call the font
  #
  # font_add_google(name = "Yellowtail", family = "Yellowtail")
  # font_add_google(name = "Source Serif Pro", family = "SSP")
  # showtext_auto()
  #
}

# pal <- wes_palette("FantasticFox1")
pal <- wes_palette("GrandBudapest1")
# pal <- wes_palette("GrandBudapest2")

pal <- brewer.pal(n = 6, name = "RdYlBu") # PRGn #BrBG #RdBu #RdYlBu

# Possibility to extend to any number of colors
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(16)









mywidth <- 0.4
pdfwidth <- 9
pdfheight <- 4.5
pdfsquare <- 6
myfontsize <- 30

# my_positiondodge <- position_dodge(0.5)

scale_fill_colors <- c("#E69F00", "#999999", "#56B4E9")
scale_fill_colors2 <- c("#7fcdbb", "#edf8b1", "#2c7fb8")
scale_fill_colors3 <- c("#a6bddb", "#ece2f0", "#1c9099")
scale_fill_colors4 <- c("#0EE87C", "#999999", "#148DFF")


scale_fill_colors_twelve <- c("#0EE87C", "#999999", "#148DFF", "#7fcdbb", "#edf8b1", "#2c7fb8", "#a6bddb", "#ece2f0", "#1c9099", "#CCCCCC", "#FFFFFF", "#148DFF")



#' Generating the sum and adding a crossbar.
#'
#' @param fun
#' @param geom
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
stat_sum_df <- function(fun, geom = "crossbar", ...) {
  stat_summary(fun.data = fun, colour = "red", geom = geom, width = 0.2, ...)
}




#' Checking the version of R (> 4.1.2) and effectsize as well as ggstatsplot. If not appropriate, a message for the user is generated.
#'
#' @return
#' @export
#'
#' @examples
checkPackageVersions <- function() {
  if (R.version.string >= "4.1.2") {
    print("R Version OK!")
  } else {
    print("updateR()!")
  }

  if (packageVersion("effectsize") >= "0.6.0") {
    print("effectsize OK!")
  } else {
    print("update effectsize!")
  }
  if (packageVersion("ggstatsplot") >= "0.9.1") {
    print("ggstatsplot OK!")
  } else {
    print("update ggstatsplot!")
  }
}



#' Check the data's distribution. If non-normal, take the non-parametric variant of *ggwithinstats*.
#' x and y have to be in parantheses, e.g., "ConditionID".
#'
#' @param data
#' @param x the independent variable, most likely "ConditionID"
#' @param y the dependent variable under investigation
#' @param ylab label to be shown for the dependent variable
#' @param xlabels labels to be used for the x-axis
#' @showPairwirseComp whether to show pairwise comparisons, TRUE as default
#'
#' @return
#' @export
#'
#' @examples ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "tlx_mental", ylab = "Mental Workload", xlabels = labels_xlab, showPairwirseComp = TRUE)
ggwithinstatsWithPriorNormalityCheck <- function(data, x, y, ylab, xlabels) {
  assertthat::not_empty(data)
  assertthat::not_empty(x)
  assertthat::not_empty(y)
  assertthat::not_empty(ylab)
  assertthat::not_empty(xlabels)


  normality_test <- with(data, tapply(data[[y]], data[[x]], shapiro.test))
  normallyDistributed <- TRUE
  for (i in normality_test) {
    if (!is.null(i)) {
      if (i$p.value < 0.05) {
        # print("You have to take the non-parametric test.")
        normallyDistributed <- FALSE
        break
      }
    }
  }

  type <- ifelse(normallyDistributed, "p", "np")

  ggstatsplot::ggwithinstats(
    data = data, x = !!x, y = !!y, type = type, centrality.type = "p", ylab = ylab, xlab = "", pairwise.comparisons = showPairwirseComp,
    centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue"), package = "pals", palette = "glasbey",
    p.adjust.method = "bonferroni", ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = 4, tip_length = 0.01)
  ) + scale_x_discrete(labels = xlabels)
}


#' Check the data's distribution. If non-normal, take the non-parametric variant of *ggbetweenstats*.
#' x and y have to be in parantheses, e.g., "ConditionID".
#'
#' @param data
#' @param x the independent variable, most likely "ConditionID"
#' @param y the dependent variable under investigation
#' @param ylab label to be shown for the dependent variable
#' @param xlabels labels to be used for the x-axis
#' @showPairwirseComp whether to show pairwise comparisons, TRUE as default
#'
#' @return
#' @export
#'
#' @examples ggbetweenstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "tlx_mental", ylab = "Mental Workload", xlabels = labels_xlab, showPairwirseComp = TRUE)
ggbetweenstatsWithPriorNormalityCheck <- function(data, x, y, ylab, xlabels) {
  assertthat::not_empty(data)
  assertthat::not_empty(x)
  assertthat::not_empty(y)
  assertthat::not_empty(ylab)
  assertthat::not_empty(xlabels)

  normality_test <- with(data, tapply(data[[y]], data[[x]], shapiro.test))
  normallyDistributed <- TRUE
  for (i in normality_test) {
    if (!is.null(i)) {
      if (i$p.value < 0.05) {
        # print("You have to take the non-parametric test.")
        normallyDistributed <- FALSE
        break
      }
    }
  }

  type <- ifelse(normallyDistributed, "p", "np")

  ggstatsplot::ggbetweenstats(
    data = data, x = !!x, y = !!y, type = type, centrality.type = "p", ylab = ylab, xlab = "", pairwise.comparisons = showPairwirseComp,
    centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue"), package = "pals", palette = "glasbey",
    p.adjust.method = "bonferroni", ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = 4, tip_length = 0.01)
  ) + scale_x_discrete(labels = xlabels)
}





#' Calculation based on Rosenthals formula (1994). N stands for the *number of measurements*.
#'
#' @param wilcoxModel
#' @param N
#'
#' @return
#' @export
#'
#' @examples
rFromWilcox <- function(wilcoxModel, N) {
  assertthat::not_empty(wilcoxModel)
  assertthat::not_empty(N)

  z <- qnorm(wilcoxModel$p.value / 2)
  r <- z / sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r= ", round(x = r, digits = 3), " z= ", z)
}

#' Title
#'
#' @param wilcoxModel
#' @param N
#' @param adjustFactor
#'
#' @return
#' @export
#'
#' @examples
rFromWilcoxAdjusted <- function(wilcoxModel, N, adjustFactor) {
  assertthat::not_empty(wilcoxModel)
  assertthat::not_empty(N)
  assertthat::not_empty(adjustFactor)

  z <- qnorm(wilcoxModel$p.value * adjustFactor / 2)
  r <- z / sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r= ", round(x = r, digits = 3), " z= ", z)
}


#' Calculation based on Rosenthals formula (1994). N stands for the *number of measurements*.
#'
#' @param pvalue
#' @param N
#'
#' @return
#' @export
#'
#' @examples
rFromNPAV <- function(pvalue, N) {
  assertthat::not_empty(pvalue)
  assertthat::not_empty(N)

  z <- qnorm(pvalue / 2)
  r <- z / sqrt(N)
  cat("Effect Size, r= ", round(x = r, digits = 3), " z= ", z)
}


#' Title
#'
#' @param dat
#' @param subset_vec
#'
#' @return
#' @export
#'
#' @examples
debug_contr_error <- function(dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      }
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}

#' Check the assumptions for an ANOVA with four factors: Normality and Homogeneity of variance assumption.
#'
#' @param data
#' @param y the dependent variable for which assumptions should be checked
#' @param factor_1
#' @param factor_2
#' @param factor_3
#' @param factor_4
#'
#' @return
#' @export
#'
#' @examples checkAssumptionsForAnovaFourFactors(data = main_df, y = "tlx_mental", factor_1 = "Video", factor_2 = "DriverPosition", factor_3 = "eHMI", factor_4 = "gesture")
checkAssumptionsForAnovaFourFactors <- function(data, y, factor_1, factor_2, factor_3, factor_4) {
  assertthat::not_empty(data)
  assertthat::not_empty(y)
  assertthat::not_empty(factor_1)
  assertthat::not_empty(factor_2)
  assertthat::not_empty(factor_3)
  assertthat::not_empty(factor_4)

  model <- lm(data[[y]] ~ data[[factor_1]] * data[[factor_2]] * data[[factor_3]] * data[[factor_4]], data = data)
  # Create a QQ plot of residuals
  ## ggqqplot(residuals(model))

  # Compute Shapiro-Wilk test of normality
  model_results <- shapiro_test(residuals(model))
  if (model_results$p.value < 0.05) {
    return("You must take the non-parametric ANOVA as model is non-normal.")
  }


  test <- data %>%
    group_by(.data[[factor_1]], .data[[factor_2]], .data[[factor_3]], .data[[factor_4]]) %>%
    shapiro_test(!!sym(y))

  # If data were normally distributed, all p are (p > 0.05).
  if (!(min(test$p) > 0.05)) {
    return("You must take the non-parametric ANOVA as normality assumption by groups is violated  (one or more p < 0.05).")
  }

  # ggqqplot(data, data[[y]], ggtheme = theme_bw()) +facet_grid(data[[factor_1]] ~ data[[factor_2]] ~ data[[factor_3]])

  # Homogneity of variance assumption
  levene_test_result <- levene_test(data, data[[y]] ~ data[[factor_1]] * data[[factor_2]] * data[[factor_3]] * data[[factor_4]])

  if (levene_test_result$p < 0.05) {
    return("You must take the non-parametric ANOVA as  Levene’s test is significant (p < 0.05).")
  }

  print("You may take parametric ANOVA (function anova_test). See https://www.datanovia.com/en/lessons/anova-in-r/#check-assumptions-1 for more information.")
}



#' Check the assumptions for an ANOVA with three factors: Normality and Homogneity of variance assumption.
#'
#' @param data
#' @param y the dependent variable for which assumptions should be checked
#' @param factor_1
#' @param factor_2
#' @param factor_3
#'
#' @return
#' @export
#'
#' @examples checkAssumptionsForAnovaThreeFactors(data = main_df, y = "tlx_mental", factor_1 = "Video", factor_2 = "DriverPosition", factor_3 = "eHMI")
checkAssumptionsForAnovaThreeFactors <- function(data, y, factor_1, factor_2, factor_3) {
  assertthat::not_empty(data)
  assertthat::not_empty(y)
  assertthat::not_empty(factor_1)
  assertthat::not_empty(factor_2)
  assertthat::not_empty(factor_3)

  model <- lm(data[[y]] ~ data[[factor_1]] * data[[factor_2]] * data[[factor_3]], data = data)
  # Create a QQ plot of residuals
  ## ggqqplot(residuals(model))

  # Compute Shapiro-Wilk test of normality
  model_results <- shapiro_test(residuals(model))
  if (model_results$p.value < 0.05) {
    return("You must take the non-parametric ANOVA as model is non-normal.")
  }


  test <- data %>%
    group_by(.data[[factor_1]], .data[[factor_2]], .data[[factor_3]]) %>%
    shapiro_test(!!sym(y))

  # If data were normally distributed, all p are (p > 0.05).
  if (!(min(test$p) > 0.05)) {
    return("You must take the non-parametric ANOVA as normality assumption by groups is violated  (one or more p < 0.05).")
  }

  # ggqqplot(data, data[[y]], ggtheme = theme_bw()) +facet_grid(data[[factor_1]] ~ data[[factor_2]] ~ data[[factor_3]])

  # Homogneity of variance assumption
  levene_test_result <- levene_test(data, data[[y]] ~ data[[factor_1]] * data[[factor_2]] * data[[factor_3]])

  if (levene_test_result$p < 0.05) {
    return("You must take the non-parametric ANOVA as  Levene’s test is significant (p < 0.05).")
  }

  print("You may take parametric ANOVA (function anova_test). See https://www.datanovia.com/en/lessons/anova-in-r/#check-assumptions-1 for more information.")
}


#' Check the assumptions for an ANOVA with two factors: Normality and Homogeneity of variance assumption.
#'
#' @param data
#' @param y the dependent variable for which assumptions should be checked
#' @param factor_1
#' @param factor_2
#'
#' @return
#' @export
#'
#' @examples checkAssumptionsForAnovaTwoFactors(data = main_df, y = "tlx_mental", factor_1 = "Video", factor_2 = "DriverPosition")
checkAssumptionsForAnovaTwoFactors <- function(data, y, factor_1, factor_2) {
  assertthat::not_empty(data)
  assertthat::not_empty(y)
  assertthat::not_empty(factor_1)
  assertthat::not_empty(factor_2)

  model <- lm(data[[y]] ~ data[[factor_1]] * data[[factor_2]], data = data)
  # Create a QQ plot of residuals
  ## ggqqplot(residuals(model))

  # Compute Shapiro-Wilk test of normality
  model_results <- shapiro_test(residuals(model))
  if (model_results$p.value < 0.05) {
    return("You must take the non-parametric ANOVA as model is non-normal.")
  }


  test <- data %>%
    group_by(.data[[factor_1]], .data[[factor_2]]) %>%
    shapiro_test(!!sym(y))

  # If data were normally distributed, all p are (p > 0.05).
  if (!(min(test$p) > 0.05)) {
    return("You must take the non-parametric ANOVA as normality assumption by groups is violated (one or more p < 0.05).")
  }

  # ggqqplot(data, data[[y]], ggtheme = theme_bw()) +facet_grid(data[[factor_1]] ~ data[[factor_2]])

  # Homogneity of variance assumption
  levene_test_result <- levene_test(data, data[[y]] ~ data[[factor_1]] * data[[factor_2]])

  if (levene_test_result$p < 0.05) {
    return("You must take the non-parametric ANOVA as  Levene’s test is significant (p < 0.05).")
  }

  print("You may take parametric ANOVA (function anova_test). See https://www.datanovia.com/en/lessons/anova-in-r/#check-assumptions-1 for more information.")
}





#' Check the assumptions for an ANOVA with one factor: Normality and Homogeneity of variance assumption.
#'
#' @param data
#' @param y the dependent variable for which assumptions should be checked
#' @param factor_1
#'
#' @return
#' @export
#'
#' @examples checkAssumptionsForAnovaTwoFactors(data = main_df, y = "tlx_mental", factor_1 = "Video")
checkAssumptionsForAnovaOneFactor <- function(data, y, factor_1) {
  assertthat::not_empty(data)
  assertthat::not_empty(y)
  assertthat::not_empty(factor_1)
  
  model <- lm(data[[y]] ~ data[[factor_1]], data = data)
  # Create a QQ plot of residuals
  ## ggqqplot(residuals(model))
  
  # Compute Shapiro-Wilk test of normality
  model_results <- shapiro_test(residuals(model))
  if (model_results$p.value < 0.05) {
    return("You must take the non-parametric ANOVA as model is non-normal.")
  }
  
  
  test <- data %>%
    group_by(.data[[factor_1]]) %>%
    shapiro_test(!!sym(y))
  
  # If data were normally distributed, all p are (p > 0.05).
  if (!(min(test$p) > 0.05)) {
    return("You must take the non-parametric ANOVA as normality assumption by groups is violated (one or more p < 0.05).")
  }
  
  # ggqqplot(data, data[[y]], ggtheme = theme_bw()) +facet_grid(data[[factor_1]] ~ data[[factor_2]])
  
  # Homogneity of variance assumption
  levene_test_result <- levene_test(data, data[[y]] ~ data[[factor_1]])
  
  if (levene_test_result$p < 0.05) {
    return("You must take the non-parametric ANOVA as  Levene’s test is significant (p < 0.05).")
  }
  
  print("You may take parametric ANOVA (function anova_test). See https://www.datanovia.com/en/lessons/anova-in-r/#check-assumptions-1 for more information.")
}













#' Generate the Latex-text based on the NPAV by Lüpsen (see \url{http://www.uni-koeln.de/~luepsen/R/}).
#' Only significant main and interaction effects are reported.
#' P-values are rounded for the third digit.
#' Attention: Effect sizes are not calculated!
#' Attention: the independent variables of the formula and the term specifying the participant must be factors (i.e., use as.factor()).
#'
#' To easily copy and paste the results to your manuscript, the following commands must be defined in Latex:
#' \newcommand{\F}[3]{$F({#1},{#2})={#3}$}
#' \newcommand{\p}{\textit{p=}}
#' \newcommand{\pminor}{\textit{p$<$}}
#'
#' @param model the model of the np.anova
#' @param dv the name of the dependent variable that should be reported
#'
#' @return
#' @export
#'
#' @examples model <- np.anova(formel = tlx_mental ~ Video * DriverPosition * gesture * eHMI + Error(UserID / (gesture * eHMI)), data = main_df, method = 0, compact = T)
#' reportNPAV(model, "mental workload")
reportNPAV <- function(model, dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  assertthat::not_empty(model)
  assertthat::not_empty(dv)

  if ("Pr(>F)" %!in% colnames(model)) {
    cat(paste0("No column ``Pr(>F)'' was found. Most likely, you want to use the command reportNPAVChi."))
  } else {
    if (!any(model$`Pr(>F)` < 0.05, na.rm = TRUE)) {
      if (write_to_clipboard) {
        write_clip(paste0("The NPAV found no significant effects on ", dv, ". "))
      } else {
        cat(paste0("The NPAV found no significant effects on ", dv, ". "))
      }
    } else {

      # there is a significant effect if any value is under 0.05
      # make the names accessible in a novel column
      model$descriptions <- rownames(model)

      # no empty space to allow backslash later
      model$descriptions <- gsub(":", " X", model$descriptions)


      for (i in 1:length(model$`Pr(>F)`)) {
        # Residuals have NA therefore we need this double check
        if (!is.na(model$`Pr(>F)`[i]) && model$`Pr(>F)`[i] < 0.05) {
          Fvalue <- round(model$`F value`[i], digits = 2) # round(model$`F value`[i], digits = 2)
          numeratordf <- model$Df[i]

          # denominator is next with an NA
          # potential for out of bounds
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
            stringtowrite <- paste0("The NPAV found a significant interaction effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", numeratordf, "}{", denominatordf, "}{", sprintf("%.2f", Fvalue), "}, ", pValue, "). ")
          } else {
            stringtowrite <- paste0("The NPAV found a significant main effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", numeratordf, "}{", denominatordf, "}{", sprintf("%.2f", Fvalue), "}, ", pValue, "). ")
          }

          # gsub backslash needs four \: https://stackoverflow.com/questions/27491986/r-gsub-replacing-backslashes
          # nice format of X in Latex via \times
          stringtowrite <- gsub("X", "$\\\\times$ \\\\", stringtowrite)

          if (write_to_clipboard) {
            write_clip(stringtowrite)
          } else {
            cat(stringtowrite)
          }
        }
      }
    }
  }
}




#' Generate the Latex-text based on the NPAV by Lüpsen (see \url{http://www.uni-koeln.de/~luepsen/R/}).
#' Only significant main and interaction effects are reported.
#' P-values are rounded for the third digit.
#' This is the version to report CHI Square values, which is necessary for between-subject studies!
#' Attention: Effect sizes are not calculated!
#' Attention: the independent variables of the formula and the term specifying the participant must be factors (i.e., use as.factor()).
#'
#'
#' To easily copy and paste the results to your manuscript, the following commands must be defined in Latex:
#' \newcommand{\F}[3]{$F({#1},{#2})={#3}$}
#' \newcommand{\p}{\textit{p=}}
#' \newcommand{\pminor}{\textit{p$<$}}
#'
#' @param model the model of the np.anova
#' @param dv the name of the dependent variable that should be reported
#'
#' @return
#' @export
reportNPAVChi <- function(model, dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  assertthat::not_empty(model)
  assertthat::not_empty(dv)
  
  # problem: when no value under 0.05 is found but a NA is present, throws error
  # here, it is okay as we don't use the residuals
  model <- na.omit(model)
  
  if (!any(model$` Pr(>Chi)` < 0.05, na.rm = TRUE)) {
    if(write_to_clipboard){
      write_clip(paste0("The NPAV found no significant effects on ", dv, ". "))
    }else{
      cat(paste0("The NPAV found no significant effects on ", dv, ". "))
    }

  }
  
  # there is a significant effect if any value is under 0.05
  # make the names accessible in a novel column
  model$descriptions <- rownames(model)
  
  # no empty space to allow backslash later
  model$descriptions <- gsub(":", " X", model$descriptions)
  
  
  for (i in 1:length(model$` Pr(>Chi)`)) {
    # Residuals have NA therefore we need this double check
    if (!is.na(model$` Pr(>Chi)`[i]) && model$` Pr(>Chi)`[i] < 0.05) {
      Chivalue <- round(model$` Chi Sq`[i], digits = 2)
      numeratordf <- model$Df[i]
      
      
      pValueNumeric <- model$` Pr(>Chi)`[i]
      if (pValueNumeric < 0.001) {
        pValue <- paste0("\\pminor{0.001}")
      } else {
        pValue <- paste0("\\p{", sprintf("%.3f", round(pValueNumeric, digits = 3)), "}")
      }
      
      
      if (str_detect(model$descriptions[i], "X")) {
        
        stringtowrite <- paste0("The NPAV found a significant interaction effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\chisq~(1)=", Chivalue, ", ", pValue, "). ")
      
        } else {
        stringtowrite <- paste0("The NPAV found a significant main effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\chisq~(1)=", Chivalue, ", ",  pValue, "). ")
      }
      
      # gsub backslash needs four \: https://stackoverflow.com/questions/27491986/r-gsub-replacing-backslashes
      # nice format of X in Latex via \times
      stringtowrite <- gsub("X", "$\\\\times$ \\\\", stringtowrite)
      
      if(write_to_clipboard){
        write_clip(stringtowrite)
      }else{
        cat(stringtowrite)
      }
      
    }
  }
}











#' Report the model produced by nparLD. The model provided must be the model generated by the command 'nparLD' \code{\link[nparLD]{nparLD}} (see \url{https://cran.r-project.org/web/packages/nparLD/nparLD.pdf}).
#'
#' #' Only significant main and interaction effects are reported.
#' P-values are rounded for the third digit.
#' Attention: Effect sizes are not calculated!
#' Attention: the independent variables of the formula and the term specifying the participant must be factors (i.e., use as.factor()).
#'
#' #' To easily copy and paste the results to your manuscript, the following commands must be defined in Latex:
#' \newcommand{\F}{\textit{F=}}
#' \newcommand{\p}{\textit{p=}}
#' \newcommand{\pminor}{\textit{p$<$}}
#' @param model
#' @param dv
#'
#' @return
#' @export
#'
#' @examples
reportNparLD <- function(model, dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  assertthat::not_empty(model)
  assertthat::not_empty(dv)

  # first retrieve relevant subset
  model <- as.data.frame(model$ANOVA.test)

  if (!any(model$`p-value` < 0.05, na.rm = TRUE)) {
    cat(paste0("The NPAV found no significant effects on ", dv, ". "))
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
        stringtowrite <- paste0("The NPVA found a significant interaction effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", Fvalue, "}, \\df{", numeratordf, "}, ", pValue, "). ")
      } else {
        stringtowrite <- paste0("The NPVA found a significant main effect of \\", trimws(model$descriptions[i]), " on ", dv, " (\\F{", Fvalue, "}, \\df{", numeratordf, "}, ", pValue, "). ")
      }
      
      # gsub backslash needs four \: https://stackoverflow.com/questions/27491986/r-gsub-replacing-backslashes
      # nice format of X in Latex via \times
      stringtowrite <- gsub("X", "$\\\\times$ \\\\", stringtowrite)
      
      if(write_to_clipboard){
        write_clip(stringtowrite)
      }else{
        cat(stringtowrite)
      }
    }
  }
}




#' Report the mean and standard deviation of a dependent variable for all levels of an independent variable rounded to the 2nd digit.
#'
#' #' To easily copy and paste the results to your manuscript, the following commands must be defined in Latex:
#' \newcommand{\m}{\textit{M=}}
#' \newcommand{\sd}{\textit{SD=}}
#' @param main_df
#' @param iv the independent variable
#' @param dv the dependent variable
#'
#' @return
#' @export
#'
#' @examples
reportMeanAndSD <- function(main_df, iv = "testiv", dv = "testdv") {
  assertthat::not_empty(main_df)
  assertthat::not_empty(iv)
  assertthat::not_empty(dv)
  

  test <- main_df  %>% drop_na(!! sym(iv)) %>% drop_na(!! sym(dv)) %>% group_by(!! sym(iv)) %>% summarise(across(!! sym(dv), list(mean = mean, sd = sd)))
  
  for(i in 1:nrow(test)) {
    row <- test[i,]
    # do stuff with row
    cat(paste0(row[[1]], ": \\m{",  sprintf("%.2f", round(row[[2]], digits = 2)), "}, \\sd{", sprintf("%.2f", round(row[[3]], digits = 2)), "}\n")) 
  }
  
  
}




#' Function to define a plot either showing the main or interaction effect in bold.
#'
#' @param df 
#' @param x factor shown on the x-axis
#' @param y dependent variable
#' @param fillColourGroup 
#' @param ytext label for y-axis
#' @param xtext label for x-axis
#' @param legendPos position for legend 
#' @param shownEffect either "main" or "interaction"
#' @param numberColors  
#'
#' @return a plot
#' @export
#'
#' @examples generateEffectPlot(df = main_df, x = "strategy", y = "trust_mean", fillColourGroup = "Emotion", ytext = "Trust", xtext = "Strategy", legendPos = c(0.1,0.23), shownEffect = "interaction")
generateEffectPlot <- function(df, x, y, fillColourGroup, ytext ="testylab", xtext="testxlab", legendPos = c(0.1,0.23), shownEffect ="main", numberColors = 6) {
  assertthat::not_empty(df)
  assertthat::not_empty(x)
  assertthat::not_empty(y)
  assertthat::not_empty(fillColourGroup)
  assertthat::not_empty(shownEffect)
  
  p <- df %>% ggplot() +
    theme_bw(base_size = myfontsize + 1) +
    aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fillColourGroup), colour = !!sym(fillColourGroup), group = !!sym(fillColourGroup)) +
    scale_colour_manual(values=wes_palette("Cavalcanti1", n=numberColors, type = "continuous")) + 
    ylab(ytext) +
    theme(legend.background = element_blank(), legend.position = legendPos, legend.text = element_text(size = myfontsize - 10)) +
    xlab(xtext) +
    stat_summary(fun = mean, geom = "point", size = 6.0) +
    stat_summary(fun = mean, geom = "point", size = 6.0,  aes(group = 1))
  
  if(shownEffect=="main"){
    p <- p + stat_summary(fun = mean, geom = "line",  size = 2, aes(group = 1)) + stat_summary(fun = mean, geom = "line", linetype = "dashed", size = 1)
  }else if(shownEffect=="interaction"){
    p <- p + stat_summary(fun = mean, geom = "line", linetype = "dashed",  size = 1, aes(group = 1)) + stat_summary(fun = mean, geom = "line", size = 2)
  }else{
    stop("ERROR: wrong effect defined for visualization.")
  }
  
  return(p)
  
}


