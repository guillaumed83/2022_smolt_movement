historical_input <- read_input("data/historical_rst_summary.csv",
  c("year", "kedgwick_marked_n", "moses_recaptures_n", "butters_recaptures_n", "both_downstream_rsts_operational",
    "first_marking_date", "last_marking_date", "analysis_included"), "year")
discharge <- read_input("data/annual_discharge_metrics.csv",
  c("year", "mean_discharge_m3_s", "discharge_cv", "coverage_complete", "historical_correlation_included"), "year")
stopifnot(nrow(historical_input) == 16, all(historical_input$both_downstream_rsts_operational),
          all(historical_input$moses_recaptures_n >= 0), all(historical_input$butters_recaptures_n >= 0),
          identical(historical_input$year[!historical_input$analysis_included], c(2002L, 2013L, 2018L)),
          all(discharge$coverage_complete), nrow(discharge) == 14)
annual <- historical_input[historical_input$analysis_included, ]
annual <- annual[order(annual$year), ]
stopifnot(identical(annual$year, c(2003:2010, 2012L, 2014L, 2016L, 2017L, 2019L)))
moses <- annual$moses_recaptures_n
total <- moses + annual$butters_recaptures_n
stopifnot(all(total > 0))
pooled <- sum(moses) / sum(total)
pooled_ci <- unname(binom.test(sum(moses), sum(total))$conf.int)
dispersion_statistic <- function(successes, trials, proportion = sum(successes) / sum(trials)) {
  if (!is.finite(proportion) || proportion <= 0 || proportion >= 1) return(0)
  sum((successes - trials * proportion)^2 / (trials * proportion * (1 - proportion)))
}
dispersion_bootstrap <- function(successes, trials, proportion, replicates) {
  simulations <- replicate(replicates, rbinom(length(trials), size = trials, prob = proportion))
  simulations <- matrix(simulations, nrow = length(trials))
  p <- colSums(simulations) / sum(trials)
  result <- numeric(replicates)
  interior <- p > 0 & p < 1
  result[interior] <- colSums((simulations[, interior, drop = FALSE] - outer(trials, p[interior]))^2 /
                               outer(trials, p[interior] * (1 - p[interior])))
  result
}
beta_binomial_fit <- function(successes, trials) {
  start_mu <- min(max(sum(successes) / sum(trials), 1e-6), 1 - 1e-6)
  objective <- function(parameters) {
    mu <- plogis(parameters[1])
    kappa <- exp(parameters[2])
    alpha <- mu * kappa
    beta <- (1 - mu) * kappa
    -sum(lchoose(trials, successes) + lbeta(successes + alpha, trials - successes + beta) - lbeta(alpha, beta))
  }
  fits <- lapply(log(c(.5, 10, 1000)), function(start) {
    tryCatch(optim(c(qlogis(start_mu), start), objective, method = "L-BFGS-B",
                    lower = c(-15, log(1e-4)), upper = c(15, log(1e6)), control = list(maxit = 1000)),
             error = function(e) NULL)
  })
  valid <- vapply(fits, function(x) !is.null(x) && x$convergence == 0 && is.finite(x$value) && all(is.finite(x$par)), logical(1))
  if (!any(valid)) return(list(success = FALSE))
  fits <- fits[valid]
  best <- fits[[which.min(vapply(fits, `[[`, numeric(1), "value"))]]
  mu <- plogis(best$par[1])
  kappa <- exp(best$par[2])
  list(success = TRUE, mu = mu, kappa = kappa, rho = 1 / (kappa + 1), alpha = mu * kappa,
       beta = (1 - mu) * kappa, sd = sqrt(mu * (1 - mu) / (kappa + 1)), log_likelihood = -best$value)
}
dispersion <- dispersion_statistic(moses, total)
beta_fit <- beta_binomial_fit(moses, total)
if (!beta_fit$success) stop("Observed beta-binomial fit failed.")
likelihood_ratio <- max(0, 2 * (beta_fit$log_likelihood - sum(dbinom(moses, total, pooled, log = TRUE))))
annual$total_recaptures_n <- total
annual$conditional_moses <- moses / total
intervals <- t(vapply(seq_along(moses), function(i) unname(binom.test(moses[i], total[i])$conf.int), numeric(2)))
annual$ci_lower <- intervals[, 1]
annual$ci_upper <- intervals[, 2]
annual$eb_mean <- (beta_fit$alpha + moses) / (beta_fit$alpha + beta_fit$beta + total)
annual$eb_lower <- qbeta(.025, beta_fit$alpha + moses, beta_fit$beta + total - moses)
annual$eb_upper <- qbeta(.975, beta_fit$alpha + moses, beta_fit$beta + total - moses)
annual$mean_discharge_m3_s <- discharge$mean_discharge_m3_s[match(annual$year, discharge$year)]
annual$discharge_cv <- discharge$discharge_cv[match(annual$year, discharge$year)]
stopifnot(!anyNA(annual), identical(discharge$year[discharge$historical_correlation_included], annual$year))
dispersion_simulations <- lr_simulations <- numeric()
leave_one_out <- data.frame()
dispersion_p <- lr_p <- NA_real_
if (run_bootstraps) {
  # One seeded stream reproduces dispersion, likelihood-ratio, then leave-one-out simulations.
  set.seed(20260906)
  dispersion_simulations <- dispersion_bootstrap(moses, total, pooled, 10000L)
  dispersion_p <- (1 + sum(dispersion_simulations >= dispersion)) / 10001
  lr_simulations <- rep(NA_real_, 5000)
  for (i in seq_along(lr_simulations)) {
    simulated <- rbinom(length(total), size = total, prob = pooled)
    p <- sum(simulated) / sum(total)
    if (p > 0 && p < 1) {
      simulated_fit <- beta_binomial_fit(simulated, total)
      if (simulated_fit$success) lr_simulations[i] <- max(0, 2 * (simulated_fit$log_likelihood - sum(dbinom(simulated, total, p, log = TRUE))))
    }
    if (i %% 500 == 0) message("Likelihood-ratio bootstrap: ", i, "/5000")
  }
  failures <- sum(!is.finite(lr_simulations))
  if (failures > 250) stop("More than 5% of likelihood-ratio bootstrap fits failed.")
  if (failures > 0) warning(failures, " likelihood-ratio bootstrap fits failed; denominator uses successful fits.")
  lr_p <- (1 + sum(lr_simulations >= likelihood_ratio, na.rm = TRUE)) / (sum(is.finite(lr_simulations)) + 1)
  leave_one_out <- do.call(rbind, lapply(seq_along(moses), function(i) {
    keep <- seq_along(moses) != i
    p <- sum(moses[keep]) / sum(total[keep])
    statistic <- dispersion_statistic(moses[keep], total[keep], p)
    simulations <- dispersion_bootstrap(moses[keep], total[keep], p, 2000L)
    data.frame(omitted_year = annual$year[i], pooled_moses = p, dispersion = statistic,
               bootstrap_p = (1 + sum(simulations >= statistic)) / 2001)
  }))
  write_table(data.frame(replicate = seq_along(dispersion_simulations), dispersion = dispersion_simulations), "historical_dispersion_bootstrap")
  write_table(data.frame(replicate = seq_along(lr_simulations), likelihood_ratio = lr_simulations,
                         optimization_success = is.finite(lr_simulations)), "historical_likelihood_ratio_bootstrap")
  write_table(leave_one_out, "historical_leave_one_year_out")
}
historical_summary <- data.frame(marked_n = sum(annual$kedgwick_marked_n), moses_n = sum(moses), butters_n = sum(total - moses),
  pooled_conditional_moses = pooled, ci_lower = pooled_ci[1], ci_upper = pooled_ci[2], dispersion = dispersion,
  dispersion_p = dispersion_p, beta_binomial_mu = beta_fit$mu, beta_binomial_rho = beta_fit$rho,
  among_year_sd = beta_fit$sd, likelihood_ratio = likelihood_ratio, likelihood_ratio_p = lr_p)
correlations <- do.call(rbind, lapply(c("mean_discharge_m3_s", "discharge_cv"), function(metric) {
  do.call(rbind, lapply(c("pearson", "spearman"), function(method) {
    result <- cor.test(annual$conditional_moses, annual[[metric]], method = method, exact = FALSE)
    data.frame(metric = metric, method = method, estimate = unname(result$estimate), p_value = result$p.value,
               n_years = nrow(annual), historical_only = TRUE)
  }))
}))
regressions <- lapply(c("mean_discharge_m3_s", "discharge_cv"), function(metric) lm(reformulate(metric, "conditional_moses"), data = annual))
names(regressions) <- c("mean_discharge_m3_s", "discharge_cv")
regression_summary <- do.call(rbind, lapply(names(regressions), function(metric) {
  s <- coef(summary(regressions[[metric]]))
  data.frame(metric = metric, term = rownames(s), estimate = s[, 1], standard_error = s[, 2], t = s[, 3], p_value = s[, 4])
}))
write_table(annual, "historical_annual_analysis")
write_table(historical_summary, "historical_summary")
write_table(correlations, "discharge_correlations")
write_table(regression_summary, "discharge_regressions")
table_2 <- annual[c("year", "kedgwick_marked_n", "moses_recaptures_n", "butters_recaptures_n", "conditional_moses", "ci_lower", "ci_upper", "mean_discharge_m3_s", "discharge_cv")]
total_row <- data.frame(year = "Total", kedgwick_marked_n = sum(annual$kedgwick_marked_n), moses_recaptures_n = sum(moses),
  butters_recaptures_n = sum(total - moses), conditional_moses = pooled, ci_lower = pooled_ci[1], ci_upper = pooled_ci[2],
  mean_discharge_m3_s = NA_real_, discharge_cv = NA_real_)
table_2 <- rbind(table_2, total_row)
for (field in c("conditional_moses", "ci_lower", "ci_upper")) table_2[[field]] <- round(100 * table_2[[field]], 1)
table_2$mean_discharge_m3_s <- round(table_2$mean_discharge_m3_s, 1)
table_2$discharge_cv <- round(table_2$discharge_cv, 3)
write_table(table_2, "table_2_historical_recaptures")
save_figure("historical_moses_proportions", 8, 5.5, function() {
  par(mar = c(5, 4.5, 1, 6.5))
  x <- seq_len(nrow(annual))
  plot(x, annual$conditional_moses, type = "n", ylim = c(0, 1), xaxt = "n", yaxt = "n", xlab = "Year", ylab = "Conditional Moses proportion")
  axis(2, at = seq(0, 1, .2), labels = paste0(seq(0, 100, 20), "%"), las = 1, cex.axis = .8)
  abline(h = seq(0, 1, .2), col = "grey93")
  abline(h = beta_fit$mu, col = "firebrick", lty = 2)
  arrows(x, annual$eb_lower, x, annual$eb_upper, code = 3, angle = 90, length = .035, col = "steelblue4", lwd = 1.5)
  points(x, annual$conditional_moses, pch = 21, col = "grey35", cex = sqrt(total / 6))
  points(x, annual$eb_mean, pch = 19, col = "steelblue4", cex = sqrt(total / 6))
  axis(1, at = x, labels = annual$year, las = 2, cex.axis = .8)
  legend("bottomleft", c("Observed", "Empirical Bayes (95% interval)", sprintf("Mean annual proportion: %.1f%%", 100 * beta_fit$mu)),
         pch = c(21, 19, NA), col = c("grey45", "steelblue4", "firebrick"), pt.bg = "white", lty = c(NA, NA, 2), bty = "n", cex = .8)
  legend(par("usr")[2] + .2, .65, xpd = NA, legend = c(5, 10, 15), pch = 19, pt.cex = sqrt(c(5, 10, 15) / 6),
         col = "steelblue4", title = "Downstream\nrecaptures", bty = "n", cex = .65)
})
plot_discharge <- function(conditional_summary) {
  save_figure("moses_proportion_discharge", 8, 9, function() {
    par(mfrow = c(2, 1), mar = c(4.8, 4.7, 1, 6.5))
    for (metric in names(regressions)) {
      context <- discharge[discharge$year == 2022, metric]
      x <- annual[[metric]]
      xlim <- range(c(x, context)) + c(-1, 1) * diff(range(c(x, context))) * .07
      plot(x, annual$conditional_moses, type = "n", xlim = xlim, ylim = c(0, 1), yaxt = "n",
           xlab = if (metric == "mean_discharge_m3_s") expression("Mean discharge ("*m^3*"/s)") else "Discharge coefficient of variation",
           ylab = "Conditional Moses proportion")
      axis(2, at = seq(0, 1, .2), labels = paste0(seq(0, 100, 20), "%"), las = 1, cex.axis = .8)
      abline(h = .5, lty = 3, col = "grey55")
      abline(regressions[[metric]], col = "grey50", lwd = 1.5)
      segments(x, annual$ci_lower, x, annual$ci_upper, col = "steelblue4")
      points(x, annual$conditional_moses, pch = 21, bg = "#0078A8", col = "black", cex = .65 + .65 * sqrt(total / max(total)))
      segments(context, conditional_summary$q025, context, conditional_summary$q975, col = "darkorange2", lwd = 2)
      points(context, conditional_summary$mean, pch = 18, col = "darkorange2", cex = 1.8)
      r <- correlations[correlations$metric == metric & correlations$method == "pearson", ]
      legend("bottomright", c(sprintf("r = %.2f", r$estimate), sprintf("P = %.3f", r$p_value)), bty = "n", cex = .8)
      legend(par("usr")[2] + .02 * diff(par("usr")[1:2]), .65, xpd = NA, legend = c(1, 12, 19), pch = 19,
             pt.cex = .65 + .65 * sqrt(c(1, 12, 19) / max(total)), title = "Downstream\nrecaptures (n)", bty = "n", cex = .65)
    }
  })
}
