# func_plot_smth_deriv_rug.R ####
#' Plot GAM smooths with CI, changepoints, and rugs (observed x; partial residuals y)
#'
#' @param mod    A fitted mgcv::gam model.
#' @param df_out Optional tibble from smth_deriv(mod, n). If NULL, it will be computed.
#' @param n      Grid size for smth_deriv() if df_out is NULL. Default: 400.
#' @param facet  Facet by ".smooth_trim" (default) or ".smooth".
#' @param show_deriv Overlay coloured line where fd_sig_dir != "uncertain". Default: TRUE.
#' @param show_chpt   Show changepoint points. Default: TRUE.
#' @param x_rug     Draw bottom rug of observed predictor values (per facet). Default: TRUE.
#' @param y_rug     Draw left rug of per-term partial residuals (link scale). Default: TRUE.
#'
#' @return A ggplot object.
#'
#' @details
#' - CI ribbon uses `.lower_ci`/`.upper_ci` from gratia::add_confint() on smooth estimates.
#' - y-rug is computed as term contribution (link scale) + working residuals (link scale),
#'   producing per-term partial residuals that align with `.estimate`.
#' - For tensor/factor-by smooths, `.smooth_trim` may not map to a single predictor column.
#'   The x-rug silently skips any trimmed names not present in `model.frame(mod)`.
plot_smth_deriv_rug <- function(
    mod,
    df_out = NULL,
    n = 400,
    facet = c(".smooth_trim", ".smooth"),
    show_deriv = TRUE,
    show_chpt = TRUE,
    x_rug = TRUE,
    y_rug = TRUE,
    
    response_scale = c("link", "response")   # NEW argument
    
) {
  facet <- match.arg(facet)
  response_scale <- match.arg(response_scale) # NEW argument
  
  # ===================================================================
  # 1) Compute (or accept) the prepared smooth + derivative output ====
  # ===================================================================
  if (is.null(df_out)) {
    df_out <- smth_deriv(mod, n = n)
  }
  
  # ===================================================================
  # 2) Per-term contributions and working residuals (link scale)   ====
  # ===================================================================
  #    These are used to construct y-rug on the same scale as the smooth.
  terms_mat <- predict(mod, type = "terms")          # n_obs x k terms (link scale)
  res_w     <- residuals(mod, type = "working")      # length n_obs (link scale)
  
  # smooth names in 'terms_mat' that match df_out$.smooth (e.g., "s(x)")
  smooth_names <- intersect(colnames(terms_mat), unique(df_out$.smooth))
  
  # Build a long tibble of partial residuals per smooth (link scale)
  obs_pr_long <- dplyr::bind_rows(lapply(smooth_names, function(snm) {
    tibble::tibble(
      .smooth      = snm,
      .smooth_trim = stringr::str_sub(snm, 3, -2),     # "s(x)" -> "x"
      y_pr         = as.numeric(terms_mat[, snm] + res_w)
    )
  }), .id = NULL)
  
  
  # ===================================================================
  # 3) Convert smooth & residuals to response scale                ====
  # ===================================================================
  if (response_scale == "response") {
    
    ### <<< NEW >>> Get inverse link (works for all families)
    inv_link <- mod$family$linkinv
    
    ### <<< NEW >>> Transform smooth estimates + CI
    df_out <- df_out %>%
      dplyr::mutate(
        .estimate  = inv_link(.estimate),
        .lower_ci  = inv_link(.lower_ci),
        .upper_ci  = inv_link(.upper_ci)
      )
    
    ### <<< NEW >>> Transform partial residuals (y_rug)
    obs_pr_long <- obs_pr_long %>%
      dplyr::mutate(y_pr = inv_link(y_pr))
  }
  # ===============================================================
  
  # ===================================================================
  # 4) Observed predictors (x-rug), pivoted long per facet         ====
  # ===================================================================
  obs_df_raw <- model.frame(mod)
  smooth_vars <- unique(df_out$.smooth_trim)
  smooth_vars <- smooth_vars[smooth_vars %in% names(obs_df_raw)]
  
  obs_x_long <- tidyr::pivot_longer(
    data     = obs_df_raw,
    cols     = tidyselect::all_of(smooth_vars),
    names_to = ".smooth_trim",
    values_to = "x_obs"
  )
  
  # 4) Construct the plot ----------------------------------------------------------
  p <- ggplot2::ggplot(df_out, ggplot2::aes(x = pred.val, y = .estimate)) +
    ggplot2::geom_hline(yintercept = 0, lty = 2) +
    
    # CI ribbon (NOTE: smooth CI columns are .lower/.upper)
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .lower_ci, ymax = .upper_ci, group = .smooth_trim),
      alpha = 0.2
    ) +
    
    # Base smooth line
    ggplot2::geom_line(ggplot2::aes(group = .smooth_trim)) +
    
    # Overlay line where derivative is "certain"
    { if (show_deriv) ggplot2::geom_line(
      data = dplyr::filter(df_out, fd_sig_dir != "uncertain"),
      ggplot2::aes(colour = fd_sig_dir),
      linewidth = 2
    ) } +
    
    # Changepoint markers
    { if (show_chpt) ggplot2::geom_point(
      data = dplyr::filter(df_out, changepoint),
      ggplot2::aes(fill = fd_sig_dir, shape = fd_sig_dir),
      size = 4
    ) } +
    
    # Bottom x-rug (observed predictor values per facet)
    { if (x_rug && length(smooth_vars) > 0) ggplot2::geom_rug(
      data = obs_x_long,
      ggplot2::aes(x = x_obs),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.5, colour = "grey40", linewidth = 0.4
    ) } +
    
    # Left y-rug: partial residuals on the same (link) scale as smooth estimate
    { if (y_rug) ggplot2::geom_rug(
      data = if (facet == ".smooth_trim") obs_pr_long else dplyr::mutate(obs_pr_long, .smooth_trim = NULL),
      ggplot2::aes(y = y_pr),
      inherit.aes = FALSE,
      sides = "l",
      alpha = 0.45, colour = "grey55", linewidth = 0.4
    ) } +
    
    # Faceting
    {
      if (facet == ".smooth_trim") {
        ggplot2::facet_wrap(~ .smooth_trim, scales = "free_x")
      } else {
        ggplot2::facet_wrap(~ .smooth, scales = "free_x")
      }
    } +
    
    # Scales, labels, theme
    ggplot2::scale_colour_manual(values = c(increasing = 2, decreasing = 4)) +
    ggplot2::scale_fill_manual(values   = c(increasing = 2, decreasing = 4)) +
    ggplot2::scale_shape_manual(values  = c(21, 24)) +
    ggplot2::labs(
      title   = paste0("Response variable: ",unique(df_out$resp)),
      x       = "Predictor value",
      y       = paste0("Smooth estimate (",response_scale," scale)"),
      caption = paste0("<b>Points indicate <span style='color:red'><b>increasing</b></span> and <span style='color:blue'><b>decreasing</b></span> changepoints<br>
                 Line colours indicate <span style='color:red'><b>increasing</b></span> and <span style='color:blue'><b>decreasing</b></span> trends<br>
                 Rugs: x = observed predictor values; y = partial residuals (",response_scale," scale)")
    ) +
    ggthemes::theme_few() +
    ggplot2::theme(
      strip.text   = ggplot2::element_text(face = 2),
      legend.position = "none",
      axis.title   = ggplot2::element_text(face = 2),
      axis.text    = ggplot2::element_text(face = 2),
      plot.caption = if (requireNamespace("ggtext", quietly = TRUE)) {
        ggtext::element_markdown(size = 10)
      } else {
        ggplot2::element_text(size = 10)
      }
    )
  
  return(p)
}
