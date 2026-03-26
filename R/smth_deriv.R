# smth_deriv.R ####
## a function to generate a smooth for a gam, calculate the residuals 
## and identify the location of changepoints

## take gam model as input and calculate estimates with confidence intervals


#' Compute smooth estimates and first derivatives from a GAM, then flag
#' change points
#'
#' @param mod A fitted GAM model object (e.g., from mgcv)
#' @param n   Number of evaluation points per smooth (passed to
#' gratia::smooth_estimates
#' and ::derivatives)
#' @return    A tibble with smooth estimates, first derivatives,
#' significance flags, and a 'changepoint' indicator
#' @details   "Change points" are flagged where the direction of the
#' derivative (increasing/decreasing) switches between adjacent evaluated
#' points, based on simultaneous confidence intervals.

smth_deriv <- function(mod, n=400, load_pkgs = TRUE){
  
  
  # ---- Load required packages (if requested) ----
  if (load_pkgs) {
    pkgs <- c("gratia", "dplyr", "stringr", "stats")
    missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    
    if (length(missing) > 0) {
      stop(
        "The following required packages are missing: ",
        paste(missing, collapse = ", "),
        "\nPlease install them before continuing.",
        call. = FALSE
      )
    }
    
    invisible(lapply(pkgs, library, character.only = TRUE))
  }
  
  # --- 1) Get smooth estimates (values of the smooth for each term across its grid) ---
  # This pulls out the marginal smooth estimates for each smooth term at 'n' points,
  # adds the response variable name, removes random effects and all-NA columns,
  # then adds confidence intervals.
  
  gratia::smooth_estimates(mod,n = n) %>%
    # Add the response variable name (e.g., y in y ~ s(x) + ...),
    # and move it to the first column for convenience.
    mutate(resp = all.vars(formula(mod))[1]) %>% 
    relocate(resp) %>% 
    # Keep only proper smooths; drop random effects that gratia may include
    dplyr::filter(.type != "Random effect") %>% 
    # Drop columns that are entirely NA (can arise for some smooth types)
    select(., where(function(x) !all(is.na(x)))) %>% 
    # Compute confidence intervals for the smooth estimates
    gratia::add_confint() %>%
    # Work as a tibble (nice printing and dplyr compatibility)
    dplyr::as_tibble() %>% 
    # Create a trimmed label for the smooth (e.g., ".smooth" like "s(x)" -> "x")
    # This helps match the correct predictor column later when extracting values.
    dplyr::mutate(.smooth_trim = str_sub(.smooth, 3, -2)) %>% 
    # Put the trimmed label up front for easier inspectio
    relocate(.smooth_trim) -> sm
  
  # --- 2) Compute first derivatives of smooths (rate of change) ---
  # Derivatives are computed for all smooths using central differences at 'n' points,
  # with simultaneous confidence intervals to allow global significance decisions.
  gratia::derivatives(
    object = mod,
    select = gratia::smooths(mod),   # include all smooth terms
    order = 1,                       # first derivative
    interval = "simultaneous",       # simultaneous CI (more conservative)
    n = n,                           # grid size
    type = "central",                # central difference scheme
    unconditional = TRUE             # include smoothing parameter uncertainty
  ) %>% 
    # Drop columns that are entirely NA
    dplyr::select(., dplyr::where(function(x) !all(is.na(x)))) %>%
    # Identify statistically significant increasing/decreasing derivative segments:
    # If the entire CI is above 0 → increasing; below 0 → decreasing; else uncertain.
    dplyr::mutate(sig_pos = .lower_ci >0,
                  sig_neg = .upper_ci <0,
                  sig_dir = case_when(
                    sig_pos ~ "increasing",
                    sig_neg ~ "decreasing",
                    TRUE ~ "uncertain"
                  ),
                  # Track previous direction to detect changes along the evaluation grid
                  direction_prev = lag(sig_dir)
    ) %>%
    # Prefix derivative columns to avoid name collisions when binding with 'sm'
    # This makes it clear which columns are from the derivative pipeline.
    stats::setNames(paste0('fd_', names(.))) -> fd
  
  # --- 3) Combine smooth estimates and derivatives side-by-side ---
  # Bind the two tibbles. Remove duplicated smooth label from fd to avoid confusion.
  
  df_tmp <- cbind(sm, fd) %>% 
    dplyr::select(-fd_.smooth)
  
  # Clean up intermediate objects from the environment
  rm(sm,fd)
  
  # --- 4) Extract the correct predictor value column for each smooth row ---
  # gratia::smooth_estimates returns the grid column named by the underlying variable,
  # e.g., 'x' for s(x), 'z' for s(z), etc.
  # Dynamically select that column matching the trimmed smooth label)
  # and store it in 'pred.val' for sorting.
  # Build column index by matching the trimmed smooth name (e.g., "x")
  # to df_tmp names
  col_idx <- match(df_tmp$.smooth_trim, names(df_tmp))
  
  # Row indices (1..nrow) to pair with column indices for matrix-like extraction
  row_idx <- seq_len(nrow(df_tmp))
  
  # Extract each row's predictor value from the matched column and coerce to numeric
  df_tmp$pred.val <- as.numeric(df_tmp[cbind(row_idx, col_idx)])
  
  # Keep pred.val near CI columns for readability
  df_tmp <- dplyr::as_tibble(df_tmp) %>% 
    dplyr::relocate(pred.val,.before = .lower_ci)
  
  ## arrange by smoother values
  df_tmp %>% arrange(pred.val, .by_group = TRUE) -> df_tmp
  
  # --- 5) Order by predictor value within smooths ---
  # Ensures detection happens along a naturally ordered grid per smooth.
  
  df_tmp <- df_tmp %>%
    # group by smooth identity
    dplyr::group_by(.smooth, .smooth_trim) %>%
    # ensure sorted within each smooth
    dplyr::arrange(pred.val, .by_group = TRUE) %>%
    # Recompute lagged direction within each smooth to avoid cross-smooth artifacts
    dplyr::mutate(
      # recompute lag within group
      fd_direction_prev = dplyr::lag(fd_sig_dir),
      changepoint = fd_sig_dir != fd_direction_prev &
        fd_sig_dir %in% c("increasing", "decreasing")
    ) %>%
    dplyr::ungroup()
  
  return(df_tmp)
}
