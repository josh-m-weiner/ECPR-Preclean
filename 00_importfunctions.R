# ##############################################################################
#                     SECTION 1: IMPORT & PARSING FUNCTIONS
# ##############################################################################

# ==============================================================================
# Parse binary/yes-no variables
# ==============================================================================

parse_binary <- function(x) {
  x <- tolower(trimws(x))
  result <- rep(NA_integer_, length(x))

  # Yes/true/1 patterns
  result[x %in% c("1", "yes", "y", "true", "t")] <- 1L

  # No/false/0 patterns
  result[x %in% c("0", "no", "n", "false", "f")] <- 0L

  result
}

# ==============================================================================
# Parse integers robustly (handles text mixed with numbers)
# ==============================================================================

parse_int_robust <- function(x) {
  x <- trimws(x)
  x[x == "" | is.na(x)] <- NA_character_

  result <- rep(NA_integer_, length(x))

  for (i in seq_along(x)) {
    if (is.na(x[i])) next

    val <- x[i]

    # Try direct integer conversion first
    direct <- suppressWarnings(as.integer(val))
    if (!is.na(direct)) {
      result[i] <- direct
      next
    }

    # Extract first number from string
    num_match <- regmatches(val, regexpr("-?[0-9]+", val))
    if (length(num_match) > 0 && nchar(num_match) > 0) {
      result[i] <- suppressWarnings(as.integer(num_match))
    }
  }

  result
}

# ==============================================================================
# Parse numeric robustly (handles text mixed with numbers)
# ==============================================================================

parse_num_robust <- function(x) {
  x <- trimws(x)
  x[x == "" | is.na(x)] <- NA_character_

  result <- rep(NA_real_, length(x))

  for (i in seq_along(x)) {
    if (is.na(x[i])) next

    val <- x[i]

    # Try direct numeric conversion first
    direct <- suppressWarnings(as.numeric(val))
    if (!is.na(direct)) {
      result[i] <- direct
      next
    }

    # Extract first number (including decimals) from string
    num_match <- regmatches(val, regexpr("-?[0-9]+\\.?[0-9]*", val))
    if (length(num_match) > 0 && nchar(num_match) > 0) {
      result[i] <- suppressWarnings(as.numeric(num_match))
    }
  }

  result
}

# ==============================================================================
# Parse dates - handle mixed formats (YYYY-MM-DD and MM-DD-YYYY)
# ==============================================================================

parse_mixed_date <- function(x) {
  x <- trimws(x)

  # Store as numeric days since epoch, convert at end
  days <- rep(NA_real_, length(x))

  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val) || val == "") next

    # Try Excel numeric date first (days since 1899-12-30)
    num_val <- suppressWarnings(as.numeric(val))
    if (!is.na(num_val) && num_val > 1000 && num_val < 100000) {
      # Convert Excel serial to days since Unix epoch (1970-01-01)
      days[i] <- num_val - 25569
      next
    }

    # Try various date formats
    formats <- c("%Y-%m-%d", "%m-%d-%Y", "%m/%d/%Y", "%Y/%m/%d")

    for (fmt in formats) {
      parsed <- suppressWarnings(as.Date(val, format = fmt))
      if (!is.na(parsed) && year(parsed) > 1900 && year(parsed) < 2100) {
        days[i] <- as.numeric(parsed)
        break
      }
    }
  }

  as.Date(days, origin = "1970-01-01")
}

parse_mixed_datetime <- function(x) {
  x <- trimws(x)

  # Store as numeric (seconds since epoch), convert at end
  epoch_seconds <- rep(NA_real_, length(x))

  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val) || val == "") next

    # Try Excel numeric datetime (days since 1899-12-30, with decimal for time)
    num_val <- suppressWarnings(as.numeric(val))
    if (!is.na(num_val) && num_val > 1000 && num_val < 100000) {
      # Convert Excel serial to Unix epoch
      epoch_seconds[i] <- (num_val - 25569) * 86400
      next
    }

    # Try various datetime formats
    formats <- c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%m-%d-%Y %H:%M:%S",
      "%m-%d-%Y %H:%M",
      "%m/%d/%Y %H:%M:%S",
      "%m/%d/%Y %H:%M",
      "%Y/%m/%d %H:%M:%S",
      "%Y/%m/%d %H:%M",
      "%Y-%m-%d",
      "%m-%d-%Y",
      "%m/%d/%Y"
    )

    for (fmt in formats) {
      parsed <- suppressWarnings(as.POSIXct(val, format = fmt, tz = "UTC"))
      if (!is.na(parsed) && year(parsed) > 1900 && year(parsed) < 2100) {
        epoch_seconds[i] <- as.numeric(parsed)
        break
      }
    }
  }

  as.POSIXct(epoch_seconds, origin = "1970-01-01", tz = "UTC")
}

parse_mixed_time <- function(x) {
  x <- trimws(x)

  # Convert to seconds, then to hms at end
  seconds <- rep(NA_real_, length(x))

  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val) || val == "") next

    # Try as numeric first (Excel decimal time: fraction of day)
    num_val <- suppressWarnings(as.numeric(val))
    if (!is.na(num_val) && num_val >= 0 && num_val < 2) {
      # Decimal fraction of day (0.5 = 12:00, 0.84 = ~20:10)
      seconds[i] <- num_val * 86400
      next
    }

    # Try HH:MM:SS format
    if (grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", val)) {
      parts <- as.integer(strsplit(val, ":")[[1]])
      seconds[i] <- parts[1] * 3600 + parts[2] * 60 + parts[3]
      next
    }

    # Try HH:MM format
    if (grepl("^[0-9]{1,2}:[0-9]{2}$", val)) {
      parts <- as.integer(strsplit(val, ":")[[1]])
      seconds[i] <- parts[1] * 3600 + parts[2] * 60
      next
    }
  }

  hms::hms(seconds = seconds)
}

# ==============================================================================
# Fix time-only Excel decimals by combining with arrest_date
# Some rows store datetime fields as time-only Excel decimals (0 to <1) rather
# than full serial datetimes. Add arrest_date to create parseable values.
# ==============================================================================

fix_time_only <- function(dt_col, date_col) {
  time_val <- suppressWarnings(as.numeric(dt_col))
  date_val <- suppressWarnings(as.numeric(date_col))
  ifelse(
    !is.na(time_val) & time_val >= 0 & time_val < 1 & !is.na(date_val),
    as.character(date_val + time_val),
    dt_col
  )
}

# ##############################################################################
#                  SECTION 2: DATA ANALYSIS & STATISTICAL TESTS
# ##############################################################################

# ==============================================================================
# Format mean (SD)
# ==============================================================================

mean_sd <- function(x) {
  if (all(is.na(x))) return("--")
  sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

# ==============================================================================
# Format n (%)
# ==============================================================================

n_pct <- function(x, condition = 1) {
  n <- sum(x == condition, na.rm = TRUE)
  total <- sum(!is.na(x))
  if (total == 0) return("--")
  sprintf("%d (%.1f%%)", n, n / total * 100)
}

# ==============================================================================
# Format count
# ==============================================================================

n_count <- function(x) {
  sum(!is.na(x))
}

# ==============================================================================
# Format p-value
# ==============================================================================

format_pval <- function(p) {
  if (is.na(p)) return("--")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

# ==============================================================================
# Compare continuous variables (t-test or Wilcoxon based on normality)
# ==============================================================================

compare_continuous <- function(x1, x2) {
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  if (length(x1) < 2 || length(x2) < 2) return(NA)

  # Check normality with Shapiro-Wilk (needs n >= 3; default to t-test if too few)
  if (length(x1) < 3 || length(x2) < 3) return(tryCatch(t.test(x1, x2)$p.value, error = function(e) NA))
  norm1 <- tryCatch(shapiro.test(head(x1, 5000))$p.value, error = function(e) 0)
  norm2 <- tryCatch(shapiro.test(head(x2, 5000))$p.value, error = function(e) 0)

  if (norm1 > 0.05 && norm2 > 0.05) {
    # Both normal: use t-test
    tryCatch(t.test(x1, x2)$p.value, error = function(e) NA)
  } else {
    # Non-normal: use Wilcoxon rank-sum, fall back to t-test on error
    tryCatch(wilcox.test(x1, x2)$p.value,
             error = function(e) tryCatch(t.test(x1, x2)$p.value, error = function(e) NA))
  }
}

# ==============================================================================
# Compare categorical variables (Fisher's exact test)
# ==============================================================================

compare_categorical <- function(var, group) {
  tbl <- table(var, group, useNA = "no")
  if (nrow(tbl) < 2 || ncol(tbl) < 2) return(NA)
  tryCatch(fisher.test(tbl)$p.value, error = function(e) NA)
}

# ==============================================================================
# Compare binary variables (Fisher's exact test)
# ==============================================================================

compare_binary <- function(x, group) {
  tbl <- table(x, group, useNA = "no")
  if (nrow(tbl) < 2 || ncol(tbl) < 2) return(NA)
  tryCatch(fisher.test(tbl)$p.value, error = function(e) NA)
}
