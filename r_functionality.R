# enhance reproducibility
usethis::use_blank_slate()

if (!require("easystats")) install.packages("easystats")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("Cairo")) install.packages("Cairo")
if (!require("rstatix")) install.packages("rstatix")
if (!require("afex")) install.packages("afex")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("FSA")) install.packages("FSA")
if (!require("PMCMRplus")) install.packages("PMCMRplus")
if (!require("psych")) install.packages("psych")
if (!require("pals")) install.packages("pals")
if (!require("wesanderson")) install.packages("wesanderson")
if (!require("ggstatsplot")) install.packages("ggstatsplot")
if (!require("ARTool")) install.packages("ARTool")
if (!require("pastecs")) install.packages("pastecs")
if (!require("rstantools")) install.packages("rstantools")
if (!require("styler")) install.packages("styler")
if (!require("assertthat")) install.packages("assertthat")
if (!require("reporttools")) install.packages("reporttools")
if (!require("stargazer")) install.packages("stargazer")
if (!require("writexl")) install.packages("writexl")
if (!require("cli")) install.packages("cli")
if (!require("DT")) install.packages("DT")
if (!require("flexdashboard")) install.packages("flexdashboard")

library(easystats)

# enforce everybody to use the latest R versions of easystats packages
easystats::easystats_update()

# For documentation: To insert a documentation skeleton in RStudio use Ctrl + Alt + Shift + R

#read_xslx delivers a tibble
#read.xslx a data.frame, therefore, this is needed: main_df <- as.data.frame(main_df) after using read_xslx

# afex: necessary for ggstatsplot
# Hmisc: necessary for mean_cl_normal --> 95% confidence intervals
library(clipr)
library(tidyverse)
library(Cairo)
library(rstatix)
#library(nparLD)
library(afex)
library(Hmisc)
library(FSA)
library(PMCMRplus)
library(psych)
library(RColorBrewer)
library(pals)
library(wesanderson)
library(ggstatsplot)
library(styler)
library(pastecs)
library(car)
library(dunn.test)
library(xtable)
library(rstantools)
library(ARTool)
#library(esquisse)
library(assertthat)
library(stargazer)
library(reporttools)
library(readxl)
library(BayesFactor)
library(bayestestR)
library(writexl)
library(foreign)


# library(showtext)
theme_set(theme_bw())

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


#  Converting a Windows path to the format that works in R
# No need for an argument. The path is printed to your console correctly and written to your clipboard for easy pasting to a script
# From: https://stackoverflow.com/questions/8425409/file-path-issues-in-r-using-windows-hex-digits-in-character-string-error
pathPrep <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}

# for label of number of data points
n_fun <- function(x){
  return(data.frame(y = median(x), label = paste0("n = ",length(x))))
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

#' This function normalizes the values in a vector to the range [new_min, new_max] 
#' based on their original range [old_min, old_max].
#'
#' @param x_vector A numeric vector that you want to normalize.
#' @param old_min The minimum value in the original scale of the data.
#' @param old_max The maximum value in the original scale of the data.
#' @param new_min The minimum value in the new scale to which you want to normalize the data.
#' @param new_max The maximum value in the new scale to which you want to normalize the data.
#' @return A numeric vector with the normalized values.
#' @export
#' @examples
#' normalize(c(1, 2, 3, 4, 5), 1, 5, 0, 1)
normalize <- function(x_vector, old_min, old_max, new_min, new_max) {
  return(new_min + ((x_vector - old_min) / (old_max - old_min)) * (new_max - new_min))
}


#' Checking the version of R (>= 4.3.2) and effectsize as well as ggstatsplot. If not appropriate, a message for the user is generated.
#'
#' @return
#' @export
#'
#' @examples
checkPackageVersions <- function() {
  if (R.version.string >= "4.3.2") {
    print("R Version OK!")
  } else {
    print("updateR()!")
    print("Attention: novel version of RTools is required!")
  }

  if (packageVersion("effectsize") >= "0.8.6") {
    print("effectsize OK!")
  } else {
    print("update effectsize!")
  }
  if (packageVersion("ggstatsplot") >= "0.12.2") {
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
#' @showPairwiseComp whether to show pairwise comparisons, TRUE as default
#' @plotType either "box", "violin", or "boxviolin" (default)
#'
#' @return
#' @export
#'
#' @examples ggwithinstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "tlx_mental", ylab = "Mental Workload", xlabels = labels_xlab, showPairwiseComp = TRUE, plotType = "boxviolin")
ggwithinstatsWithPriorNormalityCheck <- function(data, x, y, ylab, xlabels, showPairwiseComp = TRUE, plotType = "boxviolin") {
  assertthat::not_empty(data)
  assertthat::not_empty(x)
  assertthat::not_empty(y)
  assertthat::not_empty(ylab)
  assertthat::not_empty(xlabels)


  normality_test <- list()  # Initialize empty list to store test results
  normallyDistributed <- TRUE
  group_all_data_equal <- FALSE
  
  # Iterate over each group in data[[x]]
  for (group in unique(data[[x]])) {
    subset_data <- data[data[[x]] == group, y, drop = TRUE]

    # Check if subset_data is a data frame or list, and convert to numeric vector if needed
    if (is.data.frame(subset_data) || is.list(subset_data)) {
      subset_data <- as.numeric(subset_data[[1]])
    }
    
    # Remove NA values if any conversion failed
    subset_data <- subset_data[!is.na(subset_data)]
    
    
    # Check if all values in the subset are equal
    if (length(unique(subset_data)) > 1) {
      normality_test[[group]] <- shapiro.test(subset_data)
    } else {
      normality_test[[group]] <- NULL
      group_all_data_equal <- TRUE
    }
  }

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
    data = data, x = !!x, y = !!y, type = type, centrality.type = "p", ylab = ylab, xlab = "", pairwise.comparisons = showPairwiseComp, var.equal = group_all_data_equal,
    centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue"), package = "pals", palette = "glasbey",
    p.adjust.method = "holm", ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = 4, tip_length = 0.01)
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
#' @showPairwiseComp whether to show pairwise comparisons, TRUE as default
#' @plotType either "box", "violin", or "boxviolin" (default)
#'
#' @return
#' @export
#'
#' @examples ggbetweenstatsWithPriorNormalityCheck(data = main_df, x = "ConditionID", y = "tlx_mental", ylab = "Mental Workload", xlabels = labels_xlab, showPairwiseComp = TRUE, plotType = "boxviolin")
ggbetweenstatsWithPriorNormalityCheck <- function(data, x, y, ylab, xlabels, showPairwiseComp = TRUE, plotType = "boxviolin") {
  assertthat::not_empty(data)
  assertthat::not_empty(x)
  assertthat::not_empty(y)
  assertthat::not_empty(ylab)
  assertthat::not_empty(xlabels)
  
  normality_test <- list()  # Initialize empty list to store test results
  normallyDistributed <- TRUE
  group_all_data_equal <- FALSE
  
  # Iterate over each group in data[[x]]
  for (group in unique(data[[x]])) {
    subset_data <- data[data[[x]] == group, y, drop = TRUE]

    # Check if subset_data is a data frame or list, and convert to numeric vector if needed
    if (is.data.frame(subset_data) || is.list(subset_data)) {
      subset_data <- as.numeric(subset_data[[1]])
    }
    
    # Remove NA values if any conversion failed
    subset_data <- subset_data[!is.na(subset_data)]
    
    
    # Check if all values in the subset are equal
    if (length(unique(subset_data)) > 1) {
      normality_test[[group]] <- shapiro.test(subset_data)
    } else {
      normality_test[[group]] <- NULL
      group_all_data_equal <- TRUE
    }
  }
  
  # Check the p-value for each test result
  for (i in normality_test) {
    if (!is.null(i)) {
      if (i$p.value < 0.05) {
        normallyDistributed <- FALSE
        break
      }
    }
  }
  
  
  type <- ifelse(normallyDistributed, "p", "np")
  
  # if one group_all_data_equal then we use the var.equal = TRUE, see here: https://github.com/IndrajeetPatil/ggstatsplot/issues/880
  ggstatsplot::ggbetweenstats(
    data = data, x = !!x, y = !!y, type = type, centrality.type = "p", ylab = ylab, xlab = "", pairwise.comparisons = showPairwiseComp, var.equal = group_all_data_equal,
    centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue"), package = "pals", palette = "glasbey", plot.type = plotType,
    p.adjust.method = "holm", ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = 4, tip_length = 0.01)
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
#' @showPairwiseComp whether to show pairwise comparisons, TRUE as default
#' @plotType either "box", "violin", or "boxviolin" (default)
#'
#' @return
#' @export
#'
#' @examples ggbetweenstatsWithPriorNormalityCheckAsterisk(data = main_df, x = "ConditionID", y = "tlx_mental", ylab = "Mental Workload", xlabels = labels_xlab, showPairwiseComp = TRUE, plotType = "boxviolin")
ggbetweenstatsWithPriorNormalityCheckAsterisk <- function(data, x, y, ylab, xlabels, plotType = "boxviolin") {
  assertthat::not_empty(data)
  assertthat::not_empty(x)
  assertthat::not_empty(y)
  assertthat::not_empty(ylab)
  assertthat::not_empty(xlabels)
  
  normality_test <- list()  # Initialize empty list to store test results
  normallyDistributed <- TRUE
  group_all_data_equal <- FALSE
  
  # Iterate over each group in data[[x]]
  for (group in unique(data[[x]])) {
    subset_data <- data[data[[x]] == group, y, drop = TRUE]

    # Check if subset_data is a data frame or list, and convert to numeric vector if needed
    if (is.data.frame(subset_data) || is.list(subset_data)) {
      subset_data <- as.numeric(subset_data[[1]])
    }
    
    # Remove NA values if any conversion failed
    subset_data <- subset_data[!is.na(subset_data)]
    
    
    # Check if all values in the subset are equal
    if (length(unique(subset_data)) > 1) {
      normality_test[[group]] <- shapiro.test(subset_data)
    } else {
      normality_test[[group]] <- NULL
      group_all_data_equal <- TRUE
    }
  }

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
  
  
  (df <-
      pairwise_comparisons(data = data, x = !!x, y = !!y, type = type, p.adjust.method = "holm") %>%
      dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
      dplyr::arrange(group1) %>%
      dplyr::mutate(asterisk_label = ifelse(`p.value` < 0.05 & `p.value` > 0.01, "*", ifelse(`p.value` < 0.01 & `p.value` > 0.001, "**", ifelse(`p.value` < 0.001, "***", NA)))))

   df <- df %>% dplyr::filter(!is.na(asterisk_label))
  
  #y_positions_asterisks <- recode(df$asterisk_label, "NA=0.0; else=7.50") # 
  # adjust to maximum value in the dataset
  lowestNumberText <- paste0("NA=0.0; else=", toString(round((max(data[[y]]) + 0.5), digits = 2)))
  y_positions_asterisks <- recode(df$asterisk_label, recodes = lowestNumberText)
  
  
  count <- 0
  for (i in 1:length(y_positions_asterisks)) {
    if(y_positions_asterisks[i] != 0.0){
      y_positions_asterisks[i] <- y_positions_asterisks[i] + count * 0.25
      #print(y_positions_asterisks[i])
      count = count+1
    }
  }
  
  
  p <- ggstatsplot::ggbetweenstats(
    data = data, x = !!x, y = !!y, type = type, centrality.type = "p", ylab = ylab, xlab = "", pairwise.display = "none", var.equal = group_all_data_equal,
    centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue"), package = "pals", palette = "glasbey", plot.type = plotType,
    p.adjust.method = "holm", ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = 4, tip_length = 0.01)
  ) + scale_x_discrete(labels = xlabels)
  
  p + ggsignif::geom_signif(
      comparisons = df$groups,
      map_signif_level = TRUE,
      annotations = df$asterisk_label,
      y_position = y_positions_asterisks,
      size = 0.45, # 0.5 is default
      textsize = 3.90, # 3.88 is default
      fontface = "bold",
      test = NULL,
      na.rm = TRUE
    )
  
}


ggwithinstatsWithPriorNormalityCheckAsterisk <- function(data, x, y, ylab, xlabels, plotType = "boxviolin") {
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
  
  
  (df <-
      pairwise_comparisons(data = data, x = !!x, y = !!y, type = type, p.adjust.method = "holm") %>%
      dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
      dplyr::arrange(group1) %>%
      dplyr::mutate(asterisk_label = ifelse(`p.value` < 0.05 & `p.value` > 0.01, "*", ifelse(`p.value` < 0.01 & `p.value` > 0.001, "**", ifelse(`p.value` < 0.001, "***", NA)))))

  df <- df %>% dplyr::filter(!is.na(asterisk_label))
  
  #y_positions_asterisks <- recode(df$asterisk_label, "NA=0.0; else=7.50") # 
  # adjust to maximum value in the dataset
  lowestNumberText <- paste0("NA=0.0; else=", toString(round((max(data[[y]]) + 0.5), digits = 2)))
  y_positions_asterisks <- recode(df$asterisk_label, recodes = lowestNumberText)
  
  
  count <- 0
  for (i in 1:length(y_positions_asterisks)) {
    if(y_positions_asterisks[i] != 0.0){
      y_positions_asterisks[i] <- y_positions_asterisks[i] + count * 0.25
      #print(y_positions_asterisks[i])
      count = count+1
    }
  }
  
  
  p <- ggstatsplot::ggwithinstats(
    data = data, x = !!x, y = !!y, type = type, centrality.type = "p", ylab = ylab, xlab = "", pairwise.display = "none",
    centrality.point.args = list(size = 5, alpha = 0.5, color = "darkblue"), package = "pals", palette = "glasbey", plot.type = plotType,
    p.adjust.method = "holm", ggplot.component = list(theme(text = element_text(size = 16), plot.subtitle = element_text(size = 17, face = "bold"))), ggsignif.args = list(textsize = 4, tip_length = 0.01)
  ) + scale_x_discrete(labels = xlabels)
  
  p + ggsignif::geom_signif(
    comparisons = df$groups,
    map_signif_level = TRUE,
    annotations = df$asterisk_label,
    y_position = y_positions_asterisks,
    size = 0.45, # 0.5 is default
    textsize = 3.90, # 3.88 is default
    fontface = "bold",
    test = NULL,
    na.rm = TRUE
  )
  
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
#' Necessary command:
# \newcommand{\effectsize}{\textit{r=}}
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
  
  stringtowrite <- paste0("\\effectsize{", +round(x = r, digits = 3),"}, Z=",  round(x = z, digits = 2))
  stringtowrite <- trimws(stringtowrite)
  cat(stringtowrite)
  #cat("Effect Size, r= ", round(x = r, digits = 3), " z= ", z)
  
  #cat("Effect Size, r= ", round(x = r, digits = 3), " z= ", z)
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
          # Replace "X" with LaTeX code if preceded by a space
          stringtowrite <- gsub("(?<=\\s)X", "$\\\\times$ \\\\", stringtowrite, perl = TRUE)

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
      # Replace "X" with LaTeX code if preceded by a space
      stringtowrite <- gsub("(?<=\\s)X", "$\\\\times$ \\\\", stringtowrite, perl = TRUE)
      
      if(write_to_clipboard){
        write_clip(stringtowrite)
      }else{
        cat(stringtowrite)
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
#' \newcommand{\F}[3]{$F({#1},{#2})={#3}$}
#' \newcommand{\p}{\textit{p=}}
#' \newcommand{\pminor}{\textit{p$<$}}
#'
#' @param model the model of the art
#' @param dv the name of the dependent variable that should be reported
#'
#' @return
#' @export
#'
#' @examples model <- art(formula = tlx_mental ~ Video * DriverPosition * gesture * eHMI + Error(UserID / (gesture * eHMI)), data = main_df) |> anova()
#' reportART(model, "mental workload")
reportART <- function(model, dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  # Check that the model and dependent variable are not empty
  assertthat::not_empty(model)
  assertthat::not_empty(dv)
  
  # Check if the model has a "Pr(>F)" column
  if ("Pr(>F)" %!in% colnames(model)) {
    cat(paste0("No column ``Pr(>F)'' was found."))
  } else {
    # Check if any p-values are significant
    if (!any(model$`Pr(>F)` < 0.05, na.rm = TRUE)) {
      # Output a message depending on the write_to_clipboard option
      message_to_write <- paste0("The ART found no significant effects on ", dv, ". ")
      if (write_to_clipboard) {
        write_clip(message_to_write)
      } else {
        cat(message_to_write)
      }
    } else {
      # Process significant effects
      model$descriptions <- model[,1] # Make the names accessible
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
          
          # Replace "X" with LaTeX code if preceded by a space
          stringtowrite <- gsub("(?<=\\s)X", "$\\\\times$ \\\\", stringtowrite, perl = TRUE)
          
          # Output the string depending on the write_to_clipboard option
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
      # Replace "X" with LaTeX code if preceded by a space
      stringtowrite <- gsub("(?<=\\s)X", "$\\\\times$ \\\\", stringtowrite, perl = TRUE)
      
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
    cat(paste0("%", row[[1]], ": \\m{",  sprintf("%.2f", round(row[[2]], digits = 2)), "}, \\sd{", sprintf("%.2f", round(row[[3]], digits = 2)), "}\n")) 
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




#' Report dunnTest as text. Required commands in Latex:
# \newcommand{\padjminor}{\textit{p$_{adj}<$}}
# \newcommand{\padj}{\textit{p$_{adj}$=}}
#'
#' @param main_df 
#' @param d 
#' @param iv 
#' @param dv 
#'
#' @return
#' @export
#'
#' @examples reportDunnTest(main_df, d, iv = "scene", dv = "NASATLX")
#' # d <- dunnTest(NASATLX ~ scene, data = main_df, method = "holm")
reportDunnTest <- function(main_df, d, iv = "testiv", dv = "testdv") {
  assertthat::not_empty(main_df)
  assertthat::not_empty(d)
  assertthat::not_empty(iv)
  assertthat::not_empty(dv)

  if (!any(d$res$P.adj < 0.05, na.rm = TRUE)) {
    cat(paste0("A post-hoc test found no significant differences for ", dv, ". "))
  }


  for (i in 1:length(d$res$P.adj)) {
    # Residuals have NA therefore we need this double check
    if (!is.na(d$res$P.adj[i]) && d$res$P.adj[i] < 0.05) {

      # first get p-value
      pValueNumeric <- d$res$P.adj[i]

      if (pValueNumeric < 0.001) {
        pValue <- paste0("\\padjminor{0.001}")
      } else {
        pValue <- paste0("\\padj{", sprintf("%.3f", round(pValueNumeric, digits = 3)), "}")
      }

      # next, get comparison
      firstCondition <- strsplit(d$res$Comparison[i], " - ", fixed = T)[[1]][1]
      secondCondition <- strsplit(d$res$Comparison[i], " - ", fixed = T)[[1]][2]

      valueOne <- main_df %>%
        filter(!!sym(iv) == firstCondition) %>%
        summarise(across(!!sym(dv), list(mean = mean, sd = sd)))
      firstCondtionValues <- paste0(" (\\m{", sprintf("%.2f", round(valueOne[[1]], digits = 2)), "}, \\sd{", sprintf("%.2f", round(valueOne[[2]], digits = 2)), "}")

      valueTwo <- main_df %>%
        filter(!!sym(iv) == secondCondition) %>%
        summarise(across(!!sym(dv), list(mean = mean, sd = sd)))
      secondCondtionValues <- paste0(" (\\m{", sprintf("%.2f", round(valueTwo[[1]], digits = 2)), "}, \\sd{", sprintf("%.2f", round(valueTwo[[2]], digits = 2)), "}")

      # firstCondition bigger than second
      if (valueOne[[1]][1] > valueTwo[[1]][1]) {
        stringtowrite <- paste0("A post-hoc test found that ", trimws(firstCondition), " was significantly higher", firstCondtionValues, ") in terms of \\", dv, " compared to ", secondCondition, secondCondtionValues, "; ", pValue, "). ")
      } else {
        stringtowrite <- paste0("A post-hoc test found that ", trimws(secondCondition), " was significantly higher", secondCondtionValues, ") in terms of \\", dv, " compared to ", firstCondition, firstCondtionValues, "; ", pValue, "). ")
      }
      cat(stringtowrite)
    }
  }
}



#' report Dunn test as a table. Customizable with sensible defaults.
#'
#' @param main_df 
#' @param iv 
#' @param dv 
#' @param order 
#' @param numberDigitsForPValue 
#' @param latexSize 
#'
#' @return
#' @export
#'
#' @examples reportDunnTestTable(main_df, iv = "scene" , dv = "NASATLX")
reportDunnTestTable <- function(main_df, iv = "testiv", dv = "testdv", order = FALSE, numberDigitsForPValue = 4, latexSize = "small", orderText = TRUE){
  assertthat::not_empty(main_df)
  assertthat::not_empty(iv)
  assertthat::not_empty(dv)
  
  table <- dunn.test(main_df[[dv]], main_df[[iv]], method = "holm", list=TRUE)
  table <- cbind.data.frame(table$comparisons,table$Z,table$P.adjusted)
  
  # only show significant ones
  table <- subset(table, `table$P.adjusted` < 0.05)
  
  if(order){
    table <- table[order(table$`table$P.adjusted`),]
  }
  
  if(orderText){
    table <- table[order(table$`table$comparisons`),]
  }
  
  
  #rename
  names(table)[names(table) == 'table$P.adjusted'] <- 'p-adjusted'
  names(table)[names(table) == 'table$Z'] <- 'Z'
  names(table)[names(table) == 'table$comparisons'] <- 'Comparison'
  

  if (!any(table$`p-adjusted` < 0.05, na.rm = TRUE)) {
    cat(paste0("A post-hoc test found no significant differences for ", dv, ". "))
  }else{
    
    print(xtable(table, digits = c(4,4,4,numberDigitsForPValue), caption = paste0("Post-hoc comparisons for independent variable \\", iv, " and dependent variable \\", dv, ". Positive Z-values mean that the first-named level is sig. higher than the second-named. For negative Z-values, the opposite is true."), label = paste0("tab:posthoc-", iv), ), type = "latex", size = latexSize, caption.placement = "top", include.rownames=FALSE)
    message("Reminder: adjust all 0.000 to command $<$0.001.")
  }
}

#' Report statistical details for ggstatsplot.
#'
#' @param p: the object returned by ggwithinstats or ggbetweenstats
#' @param iv 
#' @param dv 
#' @param write_to_clipboard 
#'
#' @return
#' @export
#'
#' @examples p <- ggwithinstats(...) --> reportggstatsplot(p, iv = "Condition", dv="mental workload")
reportggstatsplot <- function(p, iv = "independent", dv = "Testdependentvariable", write_to_clipboard = FALSE) {
  assertthat::not_empty(p)
  assertthat::not_empty(dv)
  assertthat::not_empty(iv)

  stats <- extract_stats(p)$subtitle_data
  resultString <- ""

  effectSize <- round(stats$estimate, digits = 2)
  pValue <- round(stats$p.value, digits = 3)
  statistic <- round(stats$statistic, digits = 2)

  # Create String
  if (stats$method %in% c("Kruskal-Wallis rank sum test", "Friedman rank sum test")) {
    resultString <- paste0("(\\chisq(", stats$df.error, ")=", statistic, ", \\p{", pValue, "}, r=", effectSize, ")")
  } else if (stats$method %in% c("Paired t-test")) {
    resultString <- paste0("(t(", stats$df.error, ")=", statistic, ", \\p{", pValue, "}, r=", effectSize, ")")
  } else if (stats$method %in% c("Wilcoxon signed rank test")) {
    resultString <- paste0("(V=", statistic, ", \\p{", pValue, "}, r=", effectSize, ")")
  } else {
    # example: \F{7}{24.62}{1.01}, \p{0.45}
    resultString <- paste0("(\\F{", stats$df, "}{", stats$df.error, "}{", statistic, "}, \\p{", pValue, "}, r=", effectSize, ")")
  }


  if (!stats$p.value < 0.05) {
    if (write_to_clipboard) {
      write_clip(paste0("A ", stats$method, " found no significant effects on ", dv, " ", resultString, ". "))
    } else {
      cat(paste0("A ", stats$method, " found no significant effects on ", dv, " ", resultString, ". "))
    }
  } else {
    if (write_to_clipboard) {
      write_clip(paste0("A ", stats$method, " found a significant effect of \\", iv, " on ", dv, " ", resultString, ". "))
    } else {
      cat(paste0("A ", stats$method, " found a significant effect of \\", iv, " on ", dv, " ", resultString, ". "))
    }
  }
}


#' replace_values Description
#'
#' @name data
#' @type Data Frame
#' @description The `data` data frame contains a collection of records, with attributes organized in columns. It may include various types of values, such as numerical, categorical, or textual data.
#' @param data The input data frame to be modified.
#' @param to_replace A vector of values to be replaced within the data frame. This must be the same length as `replace_with`.
#' @param replace_with A vector of corresponding replacement values. This must be the same length as `to_replace`.
#' @example data <- replace_values(data, c("neg2", "neg1"), c("-2", "-1"))
#' @note The `replace_values` function ensures that the lengths of `to_replace` and `replace_with` are the same and will generate an error if they are not.
#' @return Modified data frame with specified values replaced.
replace_values <- function(data, to_replace, replace_with) {
  if (length(to_replace) != length(replace_with)) {
    stop("Length of 'to_replace' and 'replace_with' must be the same.")
  }
  
  for (i in 1:ncol(data)) {
    for (k in 1:nrow(data)) {
      if (is.na(data[k, i])) {
        next  # Skip NA values
      }
      for (j in 1:length(to_replace)) {
        if (data[k, i] == to_replace[j]) {
          data[k, i] <- replace_with[j]
          break
        }
      }
    }
  }
  
  return(data)
}



#' Reshape Excel Data Based on Custom Markers and Include Custom ID Column
#'
#' This function takes an Excel file with data in a wide format and transforms it to a long format.
#' It includes a customizable "ID" column in the first position and repeats it for each slice.
#' The function identifies sections of columns between markers that start with a user-defined string (default is "videoinfo")
#' and appends those sections under the first section, aligning by column index.
#' 
#' Relevant if you receive data in wide-format but cannot use built-in functionality due to naming (e.g., in LimeSurvey)
#' 
#' Attention, known bug: the ID column will first have only the IDs, this has to be fixed manually.
#'
#' @param input_filepath String, the file path of the input Excel file.
#' @param sheetName String, the name of the sheet to read from the Excel file. Default is "Results".
#' @param marker String, the string that identifies the start of a new section of columns. Default is "videoinfo".
#' @param id_col String, the name of the column to use as the ID column. Default is "ID".
#' @param output_filepath String, the file path for the output Excel file.
#' @return None, writes the reshaped data to an Excel file specified by output_filepath.
#' @examples
#' \dontrun{
#' reshape_data("results-survey626861.xlsx", marker = "videoinfo", id_col = "ID", output_filepath= "sample_output.xlsx")
#' }
#' @importFrom tidyverse select bind_rows bind_cols
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
reshape_data <- function(input_filepath, sheetName = "Results", marker = "videoinfo", id_col = "ID", output_filepath) {
  # Read the Excel file into a data frame
  df <- readxl::read_excel(input_filepath, sheet = sheetName)
  
  # Initialize an empty data frame to store the final long-form data
  long_df <- data.frame()
  
  # Initialize an empty vector to store the current columns for each marker section
  current_columns <- c()
  
  # Extract the custom "ID" column
  id_column <- df %>% select(all_of(id_col))
  
  # Loop through each column to identify given markers and reshape data accordingly
  for (col in names(df)) {
    if (startsWith(col, marker)) {
      if (length(current_columns) > 0) {
        print(length(current_columns))
        sliced_df <- df %>% select(all_of(current_columns))
        
        if (nrow(long_df) > 0) {
          # Add the ID column to the front of sliced_df
          sliced_df <- bind_cols(id_column, sliced_df)
          # Remove column names for alignment by index
          colnames(sliced_df) <- colnames(long_df)
          
        }
        
        long_df <- bind_rows(long_df, sliced_df, .id = NULL)  # Added .id = NULL to handle data types
      }
      current_columns <- c()
    } else {
      current_columns <- c(current_columns, col)
    }
  }
  
  if (length(current_columns) > 0) {
    sliced_df <- df %>% select(all_of(current_columns))
    sliced_df <- bind_cols(id_column, sliced_df)
    colnames(sliced_df) <- colnames(long_df)
    long_df <- bind_rows(long_df, sliced_df)
  }
  
  # Check if file exists and modify output_filepath to avoid overwriting
  counter <- 1
  new_output_filepath <- output_filepath
  while (file.exists(new_output_filepath)) {
    new_output_filepath <- paste0(gsub("\\.xlsx$", "", output_filepath), "_", counter, ".xlsx")
    counter <- counter + 1
  }
  
  # Write the long-form data frame to a new Excel file
  writexl::write_xlsx(long_df, new_output_filepath)
}










#' Remove outliers and calculate REI
#'
#' This function takes a data frame, optional header information, variables to consider,
#' and a range for a Likert scale. It then calculates the Response Entropy Index (REI)
#' and flags suspicious entries based on percentiles.
#'
#' For more information on the REI method, refer to:
#' [Response Entropy Index Method](https://ojs.ub.uni-konstanz.de/srm/article/view/7832)
#'
#' @param df Data frame containing the data.
#' @param header Logical indicating if the data frame has a header. Defaults to FALSE.
#' @param variables Character string specifying which variables to consider, separated by commas.
#' @param range Numeric vector specifying the range of the Likert scale. Defaults to c(1, 5).
#'
#' @return A data frame with calculated REI, percentile, and a 'Suspicious' flag.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(var1 = c(1, 2, 3), var2 = c(2, 3, 4))
#' result <- remove_outliers_REI(df, TRUE, "var1,var2", c(1, 5))
#' }
remove_outliers_REI <- function(df, header = FALSE, variables = "", range = c(1, 5)) {
  # Load required packages
  library(stringr)
  
  # Validate and parse variables
  if (variables == "" && header == TRUE) {
    stop("Please input variables to consider!")
  }
  iniVariables <- str_split(variables, ",")
  variableNames <- unique(trimws(iniVariables[[1]]))
  
  # Initialize data frame for REI calculation
  testDF <- data.frame(REI = numeric(nrow(df)))
  
  # Extract specified columns
  if (header == FALSE) {
    testDF <- cbind(testDF, df)
  } else { 
    for (i in variableNames) {
      columnMatches <- grep(paste("^", i, "$", sep=""), colnames(df))
      if (length(columnMatches) > 0) {
        testDF <- cbind(testDF, df[, columnMatches])
      }
    }
  }
  
  # Check column count for validity
  if (NCOL(testDF) <= 2) {
    stop("Not enough columns found with the given phrase.")
  }
  
  # Calculate REI and related metrics
  numLevels <- range[2] - range[1] + 1
  numQuestions <- ncol(testDF) - 1
  getResponses <- function(df) {
    recordedResponses <- unique(as.vector(as.matrix(df)))
    tallies <- sapply(recordedResponses, function(x) rowSums(df == x))
    return(tallies)
  }
  
  tallies <- getResponses(testDF[,-1])
  proportions <- tallies / numQuestions
  logs <- proportions * log10(proportions)
  logs[is.na(logs)] <- 0
  testDF[, "REI"] <- rowSums(logs, na.rm = TRUE) * -1
  
  # Calculate percentile and flag suspicious entries
  testDF$Percentile <- round(pnorm(testDF$REI, mean = mean(testDF$REI, na.rm = TRUE), sd = sd(testDF$REI, na.rm = TRUE)), digits = 2) * 100
  testDF$Suspicious <- "No"
  testDF$Suspicious[testDF$Percentile <= 10 | testDF$Percentile >= 90] <- "Maybe"
  testDF$Suspicious[testDF$Percentile <= 5 | testDF$Percentile >= 95] <- "Yes"
  
  return(testDF)
}


reportggstatsplotPostHoc <- function(main_df, p, iv = "testiv", dv = "testdv", label_mappings = NULL) {
  # Asserts to ensure non-empty inputs
  assertthat::not_empty(main_df)
  assertthat::not_empty(p)
  assertthat::not_empty(iv)
  assertthat::not_empty(dv)
  
  # Extract stats from the ggstatsplot object
  stats <- extract_stats(p)$pairwise_comparisons_data
  
  if (!any(stats$p.value < 0.05, na.rm = TRUE)) {
    cat(paste0("A post-hoc test found no significant differences for ", dv, ". "))
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
      
      valueOne <- main_df %>%
        filter(!!sym(iv) == firstCondition) %>%
        summarise(across(!!sym(dv), list(mean = mean, sd = sd)))
      
      valueTwo <- main_df %>%
        filter(!!sym(iv) == secondCondition) %>%
        summarise(across(!!sym(dv), list(mean = mean, sd = sd)))
      
      # Format statistics
      firstStatsStr <- paste0(" (\\m{", sprintf("%.2f", as.numeric(round(valueOne[1,1], 2))), "}, \\sd{", sprintf("%.2f", as.numeric(round(valueOne[1,2], 2))), "})")
      secondStatsStr <- paste0(" (\\m{", sprintf("%.2f", as.numeric(round(valueTwo[1,1], 2))), "}, \\sd{", sprintf("%.2f", as.numeric(round(valueTwo[1,2], 2))), "})")
      
      # Construct and print output string
      if (as.numeric(round(valueOne[1,1], 2)) > as.numeric(round(valueTwo[1,1], 2))) {
        cat(paste0("A post-hoc test found that ", firstLabel, " was significantly higher", firstStatsStr, " in terms of \\", dv, " compared to ", secondLabel, secondStatsStr, "; ", pValue, "). "))
      } else {
        cat(paste0("A post-hoc test found that ", secondLabel, " was significantly higher", secondStatsStr, " in terms of \\", dv, " compared to ", firstLabel, firstStatsStr, "; ", pValue, "). "))
      }
    }
  }
}
