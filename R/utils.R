#' Ensure input is not empty
#'
#' Stops execution if x is NULL, empty, or contains only NAs.
#'
#' @param x The object to check
#' @param msg The error message to display
#' @return Invisible TRUE if valid.
#' @export
not_empty <- function(x, msg = "Input must not be empty.") {
  if (is.null(x) || length(x) == 0) {
    stop(msg, call. = FALSE)
  }

  if (is.atomic(x) && all(is.na(x))) {
    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}

# not in
"%!in%" <- function(x, y) !("%in%"(x, y))

# replace NA with zero
# e.g. df$test <- na.zero(df$test)
na.zero <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}


#  Converting a Windows path to the format that works in R
# No need for an argument. The path is printed to your console correctly and written to your clipboard for easy pasting to a script
# From: https://stackoverflow.com/questions/8425409/file-path-issues-in-r-using-windows-hex-digits-in-character-string-error
pathPrep <- function(path = "clipboard", read_fn = NULL, write_fn = NULL) {
  get_clip_reader <- function() {
    if (!is.null(read_fn)) {
      return(read_fn)
    }
    if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
      return(clipr::read_clip)
    }
    if (exists("readClipboard", mode = "function")) {
      return(get("readClipboard", mode = "function"))
    }
    stop("Clipboard is not available. Provide a custom `read_fn` or a direct path.")
  }

  get_clip_writer <- function() {
    if (!is.null(write_fn)) {
      return(write_fn)
    }
    if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
      return(clipr::write_clip)
    }
    if (exists("writeClipboard", mode = "function")) {
      return(get("writeClipboard", mode = "function"))
    }
    return(function(...) invisible(NULL))
  }

  writer <- get_clip_writer()

  y <- if (identical(path, "clipboard")) {
    reader <- get_clip_reader()
    reader()
  } else {
    path
  }

  x <- chartr("\\", "/", y)
  writer(x)
  return(x)
}

# for label of number of data points
n_fun <- function(x) {
  return(data.frame(y = median(x), label = paste0("n = ", length(x))))
}


#' Generating the sum and adding a crossbar.
#'
#' @param fun function
#' @param geom geom to be shown
#' @param ... Additional arguments passed to stat_summary
#'
#' @return A \code{ggplot2} layer that can be added to a ggplot object.
#' @export
#'
#' @examples \donttest{
#'   # Simple summary function: use the mean as y, ymin, and ymax
#'   mean_fun <- function(x) {
#'     m <- mean(x, na.rm = TRUE)
#'     data.frame(y = m, ymin = m, ymax = m)
#'   }
#'
#'   ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) +
#'     stat_sum_df(mean_fun)
#' }
stat_sum_df <- function(fun, geom = "crossbar", ...) {
  stat_summary(fun.data = fun, colour = "red", geom = geom, width = 0.2, ...)
}

#' This function normalizes the values in a vector to the range \[new_min, new_max\]
#' based on their original range \[old_min, old_max\].
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


#' Check normality for groups
#'
#' @param data the data frame
#' @param x the x column
#' @param y the y column
#'
#' @return TRUE if all groups are normal, FALSE otherwise
check_normality_by_group <- function(data, x, y) {
  # Input validation
  if (missing(data) || missing(x) || missing(y)) stop("Missing arguments")

  # Ensure numeric
  if (!is.numeric(data[[y]])) {
    val <- as.numeric(data[[y]])
    if (all(is.na(val))) {
      return(FALSE)
    } # Non-numeric data
    data[[y]] <- val
  }

  results <- data |>
    dplyr::group_by(!!dplyr::sym(x)) |>
    dplyr::summarise(
      p_value = if (dplyr::n() >= 3 && stats::var(!!dplyr::sym(y), na.rm = TRUE) > 0) {
        stats::shapiro.test(!!dplyr::sym(y))$p.value
      } else {
        NA_real_ # Cannot test
      },
      .groups = "drop"
    )

  # If any group is significant (p < 0.05), data is NOT normal
  all_normal <- !any(results$p_value < 0.05, na.rm = TRUE)

  return(all_normal)
}


#' Check homogeneity of variances across groups
#'
#' @param data the data frame
#' @param x the grouping variable (column name as string)
#' @param y the dependent variable (column name as string)
#'
#' @return TRUE if Levene's test is non-significant (p >= .05), FALSE otherwise
check_homogeneity_by_group <- function(data, x, y) {
  not_empty(data)
  not_empty(x)
  not_empty(y)

  if (!requireNamespace("rstatix", quietly = TRUE)) {
    warning("Package 'rstatix' not installed. Assuming unequal variances (var.equal = FALSE).")
    return(FALSE)
  }

  formula_string <- paste(y, "~", x)
  levene_res <- rstatix::levene_test(
    data    = data,
    formula = stats::as.formula(formula_string)
  )

  # rstatix::levene_test returns a tibble with column 'p'
  p_val <- levene_res$p[1L]

  if (is.na(p_val)) {
    return(FALSE)
  }

  return(p_val >= 0.05)
}


#' Calculation based on Rosenthal's formula (1994). N stands for the *number of measurements*.
#'
#' @param wilcoxModel the Wilcox model
#' @param N number of measurements in the experiment
#'
#' @return Invisibly returns a list with components:
#'   \itemize{
#'     \item \code{r}: effect size as a numeric scalar.
#'     \item \code{z}: corresponding z-statistic.
#'     \item \code{text}: character string that is also sent to the console.
#'   }
#' @export
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   group = rep(c("A", "B"), each = 10),
#'   value = rnorm(20)
#' )
#' w <- stats::wilcox.test(value ~ group, data = d, exact = FALSE)
#' rFromWilcox(w, N = nrow(d))
rFromWilcox <- function(wilcoxModel, N) {
  not_empty(wilcoxModel)
  not_empty(N)

  z <- stats::qnorm(wilcoxModel$p.value / 2)
  r <- z / sqrt(N)

  msg <- sprintf(
    "%s Effect Size, r = %.3f, z = %.3f",
    wilcoxModel$data.name, r, z
  )
  message(msg)

  invisible(list(r = r, z = z, text = msg))
}

#' rFromWilcoxAdjusted
#'
#' @param wilcoxModel the Wilcox model
#' @param N number of measurements in the experiment
#' @param adjustFactor ad adjustment factor
#'
#' @return Invisibly returns a list with components:
#'   \itemize{
#'     \item \code{r}: adjusted effect size as a numeric scalar.
#'     \item \code{z}: adjusted z-statistic.
#'     \item \code{text}: character string that is also sent to the console.
#'   }
#' @export
#'
#' @examples \donttest{
#' set.seed(1)
#' d <- data.frame(
#'   group = rep(c("A", "B"), each = 10),
#'   value = rnorm(20)
#' )
#' w <- stats::wilcox.test(value ~ group, data = d, exact = FALSE)
#' rFromWilcoxAdjusted(w, N = nrow(d), adjustFactor = 2)
#' }
rFromWilcoxAdjusted <- function(wilcoxModel, N, adjustFactor) {
  not_empty(wilcoxModel)
  not_empty(N)
  not_empty(N)

  z <- stats::qnorm(wilcoxModel$p.value * adjustFactor / 2)
  r <- z / sqrt(N)

  msg <- sprintf(
    "%s Effect Size, r = %.3f, z = %.3f",
    wilcoxModel$data.name, r, z
  )
  message(msg)
  invisible(list(r = r, z = z, text = msg))
}


#' Calculation based on Rosenthal's formula (1994). N stands for the *number of measurements*.
#' Necessary command:
# \newcommand{\effectsize}{\textit{r=}}
#' @param pvalue p value
#' @param N number of measurements in the experiment
#'
#' @return Invisibly returns a list with components:
#'   \itemize{
#'     \item \code{r}: effect size as a numeric scalar.
#'     \item \code{z}: corresponding z-statistic.
#'     \item \code{text}: LaTeX-formatted character string that is also sent
#'       to the console.
#'   }
#' @export
#'
#' @examples rFromNPAV(0.02, N = 180)
rFromNPAV <- function(pvalue, N) {
  not_empty(pvalue)
  not_empty(N)

  z <- qnorm(pvalue / 2)
  r <- z / sqrt(N)

  stringtowrite <- sprintf(
    "\\effectsize{%s}, Z=%s",
    format(round(r, 3), trim = TRUE, nsmall = 3),
    format(round(z, 2), trim = TRUE, nsmall = 2)
  )
  message(stringtowrite)
  invisible(list(r = r, z = z, text = stringtowrite))
}


#' Debug contrast errors in ANOVA-like models
#'
#' @param dat A data frame of predictors.
#' @param subset_vec Optional logical or numeric index vector used to subset rows before checks.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{nlevels}{Integer vector giving the number of levels for each factor
#'   variable in \code{dat}.}
#'   \item{levels}{List of factor level labels for each factor variable in
#'   \code{dat}.}
#' }

#' @export
#'
#' @examples
#' \donttest{
#' dat <- data.frame(
#'   group = factor(rep(letters[1:3], each = 3)),
#'   score = rnorm(9)
#' )
#'
#' debug_contr_error(dat = dat)
#' }
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


#' Check the assumptions for an ANOVA with a variable number of factors: Normality and Homogeneity of variance assumption.
#'
#' @param data the data frame
#' @param y The dependent variable for which assumptions should be checked
#' @param factors A character vector of factor names
#'
#' @return A message indicating whether to use parametric or non-parametric ANOVA
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' main_df <- data.frame(
#'   tlx_mental      = rnorm(40),
#'   Video           = factor(rep(c("A", "B"), each = 20)),
#'   DriverPosition  = factor(rep(c("Left", "Right"), times = 20))
#' )
#'
#' checkAssumptionsForAnova(
#'   data    = main_df,
#'   y       = "tlx_mental",
#'   factors = c("Video", "DriverPosition")
#' )
#' }
checkAssumptionsForAnova <- function(data, y, factors) {
  # Ensure data and variables are not empty
  not_empty(data)
  not_empty(y)
  not_empty(factors)

  # Dynamically construct the formula based on the number of factors
  formula_string <- paste(y, "~", paste(factors, collapse = " * "))
  model <- lm(as.formula(formula_string), data = data)

  # Shapiro-Wilk test of normality on model residuals
  model_results <- shapiro_test(residuals(model))
  if (model_results$p.value < 0.05) {
    return("You must take the non-parametric ANOVA as model is non-normal.")
  }

  # Check normality for each group
  test <- data |>
    group_by(across(dplyr::all_of(factors))) |>
    shapiro_test(!!sym(y))

  # Check if the normality assumption holds (p > 0.05 for all groups)
  if (!(min(test$p) > 0.05)) {
    return("You must take the non-parametric ANOVA as normality assumption by groups is violated (one or more p < 0.05).")
  }

  # Homogeneity of variance assumption using Levene's Test
  levene_formula <- as.formula(paste(y, "~", paste(factors, collapse = " * ")))
  levene_test_result <- levene_test(data, levene_formula)

  if (levene_test_result$p < 0.05) {
    return("You must take the non-parametric ANOVA as Levene's test is significant (p < 0.05).")
  }

  message("You may take parametric ANOVA (function anova_test). See https://www.datanovia.com/en/lessons/anova-in-r/#check-assumptions-1 for more information.")

  invisible(NULL)
}


#' Replace values across a data frame
#'
#' @description
#' Replace all occurrences of given values in all columns of a data frame.
#'
#' @name data the data frame
#' @description The `data` data frame contains a collection of records, with attributes organized in columns. It may include various types of values, such as numerical, categorical, or textual data.
#' @param data The input data frame to be modified.
#' @param to_replace A vector of values to be replaced within the data frame. This must be the same length as `replace_with`.
#' @param replace_with A vector of corresponding replacement values. This must be the same length as `to_replace`.
#'
#' @return Modified data frame with specified values replaced.
#' @export
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   q1 = c("neg2", "neg1", "0"),
#'   q2 = c("1", "neg2", "neg1")
#' )
#'
#' replace_values(
#'   data,
#'   to_replace = c("neg2", "neg1"),
#'   replace_with = c("-2", "-1")
#' )
#' }
replace_values <- function(data, to_replace, replace_with) {
  if (length(to_replace) != length(replace_with)) {
    stop("Length of 'to_replace' and 'replace_with' must be the same.")
  }

  # Create a named vector for replacements
  replace_map <- setNames(replace_with, to_replace)

  # Apply replacements column-wise
  data[] <- lapply(data, function(column) {
    # Convert factors to characters
    if (is.factor(column)) column <- as.character(column)

    # Replace values using replace_map
    column <- ifelse(!is.na(column) & column %in% names(replace_map),
      replace_map[column],
      column
    )
    return(column)
  })

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
#'
#' @return None, writes the reshaped data to an Excel file specified by output_filepath.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace(c("write_xlsx", "readxl"), quietly = TRUE)) {
#'   tmp_in  <- tempfile(fileext = ".xlsx")
#'   tmp_out <- tempfile(fileext = ".xlsx")
#'
#'   # Minimal toy input that includes your required pieces:
#'   # an ID column and something that contains the marker value.
#'   toy <- data.frame(
#'     ID = c(1, 1, 2, 2),
#'     section = c("videoinfo", "videoinfo", "videoinfo", "videoinfo"),
#'     key = c("fps", "duration_s", "fps", "duration_s"),
#'     value = c(30, 12.3, 25, 9.8),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   writexl::write_xlsx(toy, tmp_in)
#'
#'   reshape_data(
#'     input_filepath = tmp_in,
#'     marker = "videoinfo",
#'     id_col = "ID",
#'     output_filepath = tmp_out
#'   )
#'
#'   out <- readxl::read_excel(tmp_out)
#'   print(out)
#' }
#' }
#'
#' @importFrom dplyr select bind_rows bind_cols
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
reshape_data <- function(input_filepath, sheetName = "Results", marker = "videoinfo", id_col = "ID", output_filepath) {
  # Read the Excel file into a data frame. If the requested sheet is missing,
  # fall back to the first available sheet to keep the helper robust for
  # single-sheet workbooks created on the fly (e.g., in tests).
  available_sheets <- readxl::excel_sheets(input_filepath)
  sheet_to_read <- if (sheetName %in% available_sheets) sheetName else available_sheets[[1]]
  df <- readxl::read_excel(input_filepath, sheet = sheet_to_read)

  # Initialize an empty data frame to store the final long-form data
  long_df <- data.frame()

  # Initialize an empty vector to store the current columns for each marker section
  current_columns <- c()

  # Extract the custom "ID" column
  id_column <- df |> select(dplyr::all_of(id_col))

  # Loop through each column to identify given markers and reshape data accordingly
  for (col in names(df)) {
    if (startsWith(col, marker)) {
      if (length(current_columns) > 0) {
        # print(length(current_columns))
        sliced_df <- df |> select(dplyr::all_of(current_columns))

        if (nrow(long_df) > 0) {
          # Add the ID column to the front of sliced_df
          sliced_df <- bind_cols(id_column, sliced_df)
          # Remove column names for alignment by index
          colnames(sliced_df) <- colnames(long_df)
        }

        long_df <- bind_rows(long_df, sliced_df, .id = NULL) # Added .id = NULL to handle data types
      }
      current_columns <- c()
    } else {
      current_columns <- c(current_columns, col)
    }
  }

  if (length(current_columns) > 0) {
    sliced_df <- df |> select(dplyr::all_of(current_columns))
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


#' Add Pareto EMOA Column to a Data Frame
#'
#' This function calculates the Pareto front for a given set of objectives in a data frame and adds a new column, `PARETO_EMOA`, which indicates whether each row in the data frame belongs to the Pareto front.
#'
#' @param data A data frame containing the data, including the objective columns.
#' @param objectives A character vector specifying the names of the objective columns in `data`. These columns should be numeric and will be used to calculate the Pareto front.
#'
#' @return A data frame with the same columns as `data`, along with an additional column, `PARETO_EMOA`, which is `TRUE` for rows that are on the Pareto front and `FALSE` otherwise.
#' @export
#'
#' @examples
#' # Define objective columns
#' objectives <- c("trust", "predictability", "perceivedSafety", "Comfort")
#'
#' # Example data frame
#' main_df <- data.frame(
#'   trust = runif(10),
#'   predictability = runif(10),
#'   perceivedSafety = runif(10),
#'   Comfort = runif(10)
#' )
#'
#' # Add the Pareto front column
#' main_df <- add_pareto_emoa_column(data = main_df, objectives)
#' head(main_df)
add_pareto_emoa_column <- function(data, objectives) {
  if (!requireNamespace("emoa", quietly = TRUE)) {
    stop("Package 'emoa' is required for add_pareto_emoa_column(). Please install it.")
  }

  # Input checks
  not_empty(data)
  not_empty(objectives)

  # Select only the objective columns
  objective_data <- data |> select(dplyr::all_of(objectives))

  # If there's only one row, mark it as PARETO_EMOA directly
  if (nrow(objective_data) == 1) {
    data$PARETO_EMOA <- TRUE
    return(data)
  }

  # Transpose and convert to matrix as required by the nondominated_points function
  pareto_points <- emoa::nondominated_points(t(as.matrix(objective_data)))

  # Convert the Pareto points matrix back to a data frame for comparison
  pareto_df <- as.data.frame(t(pareto_points))

  # Initialize the PARETO_EMOA column as FALSE
  data$PARETO_EMOA <- FALSE

  # Mark TRUE for rows in the original data that match any row in the Pareto front
  for (i in 1:nrow(pareto_df)) {
    matching_row <- which(
      apply(objective_data, 1, function(x) all(x == pareto_df[i, ]))
    )
    if (length(matching_row) > 0) {
      data$PARETO_EMOA[matching_row] <- TRUE
    }
  }

  # Return the updated data frame
  return(data)
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
#' @export
#'
#' @examples
#' \donttest{
#' df <- data.frame(var1 = c(1, 2, 3), var2 = c(2, 3, 4))
#' result <- remove_outliers_REI(df, TRUE, "var1,var2", c(1, 5))
#' }
remove_outliers_REI <- function(df, header = FALSE, variables = "", range = c(1, 5)) {
  # Validate and parse variables
  if (variables == "" && header == TRUE) {
    stop("Please input variables to consider!")
  }
  iniVariables <- stringr::str_split(variables, ",")
  variableNames <- unique(trimws(iniVariables[[1]]))

  # Initialize data frame for REI calculation
  testDF <- data.frame(REI = numeric(nrow(df)))

  # Extract specified columns
  if (header == FALSE) {
    testDF <- cbind(testDF, df)
  } else {
    for (i in variableNames) {
      columnMatches <- grep(paste("^", i, "$", sep = ""), colnames(df))
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

  tallies <- getResponses(testDF[, -1])
  proportions <- tallies / numQuestions
  logs <- proportions * log10(proportions)
  logs[is.na(logs)] <- 0
  testDF[, "REI"] <- rowSums(logs, na.rm = TRUE) * -1

  # Calculate percentile and flag suspicious entries
  testDF$Percentile <- round(stats::pnorm(testDF$REI, mean = mean(testDF$REI, na.rm = TRUE), sd = stats::sd(testDF$REI, na.rm = TRUE)), digits = 2) * 100
  testDF$Suspicious <- "No"
  testDF$Suspicious[testDF$Percentile <= 10 | testDF$Percentile >= 90] <- "Maybe"
  testDF$Suspicious[testDF$Percentile <= 5 | testDF$Percentile >= 95] <- "Yes"

  return(testDF)
}
