checks <- list()
check <- function(id, category, quantity, expected, observed, tolerance = 0, source = "Author-approved reporting target", notes = "", status = NULL) {
  expected <- unname(expected)
  observed <- unname(observed)
  if (is.null(status)) {
    matches <- length(expected) == length(observed) && identical(is.na(expected), is.na(observed))
    if (matches && is.numeric(expected) && is.numeric(observed)) {
      active <- !is.na(expected)
      matches <- all(is.finite(observed[active])) && all(abs(expected[active] - observed[active]) <= tolerance + 1e-12)
    } else if (matches) matches <- identical(as.character(expected), as.character(observed))
    status <- if (matches) "PASS" else "FAIL"
  }
  checks[[length(checks) + 1L]] <<- data.frame(check_id = id, category = category, quantity = quantity,
    expected = paste(expected, collapse = "; "), observed = paste(format(observed, digits = 16), collapse = "; "),
    tolerance = paste(tolerance, collapse = "; "), status = status, source = source, notes = notes)
}
check("H01", "historical", "Marked total", 21674, sum(annual$kedgwick_marked_n))
check("H02", "historical", "Moses and Butters recaptures", c(87, 40), c(sum(moses), sum(total - moses)))
check("H03", "historical", "Pooled conditional Moses proportion", .685, pooled, .0005)
check("H04", "historical", "Pooled exact 95% CI", c(.597, .765), pooled_ci, .0005)
check("H05", "historical", "Pearson-type dispersion", 29.12, dispersion, .005)
check("H06", "historical", "Beta-binomial mean and among-year SD", c(.688, .173), c(beta_fit$mu, beta_fit$sd), .0005)
check("H07", "historical", "Beta-binomial overdispersion", .1391303475212331, beta_fit$rho, 1e-7)
if (run_bootstraps) {
  check("H08", "bootstrap", "Dispersion replicates and exceedances", c(10000, 13), c(length(dispersion_simulations), sum(dispersion_simulations >= dispersion)))
  check("H09", "bootstrap", "Dispersion plus-one p-value", .0014, dispersion_p, .00005)
  check("H10", "bootstrap", "Successful LR replicates and exceedances", c(5000, 5), c(sum(is.finite(lr_simulations)), sum(lr_simulations >= likelihood_ratio, na.rm = TRUE)))
  check("H11", "bootstrap", "LR plus-one p-value", .0012, lr_p, .00005)
  check("H12", "bootstrap", "Leave-one-out pooled range", c(.648, .721), range(leave_one_out$pooled_moses), .0005)
  check("H13", "bootstrap", "Leave-one-out bootstrap p range", c(.0010, .0110), range(leave_one_out$bootstrap_p), .00005)
} else check("H_BOOT", "bootstrap", "Bootstrap calculations", "Executed", "Skipped by switch", status = "REVIEW", notes = "Set run_bootstraps <- TRUE for full verification.")
for (metric in c("mean_discharge_m3_s", "discharge_cv")) {
  x <- correlations[correlations$metric == metric & correlations$method == "pearson", ]
  check(paste0("D_", metric), "discharge", paste("Pearson r and p:", metric), if (metric == "mean_discharge_m3_s") c(.41, .17) else c(.64, .018),
        c(x$estimate, x$p_value), if (metric == "mean_discharge_m3_s") .005 else c(.005, .0005))
}
check("D03", "discharge", "2022 context mean and CV", c(447.1, .482),
      c(discharge$mean_discharge_m3_s[discharge$year == 2022], discharge$discharge_cv[discharge$year == 2022]), c(.05, .0005))
check("D04", "discharge", "Historical regressions exclude 2022", FALSE, 2022 %in% annual$year)
check("T01", "telemetry", "Tagged smolts", 45L, nrow(fish))
check("T02", "telemetry", "Fork length mean and SD (mm)", c(145, 8.3), c(mean(fish$fork_length_mm), sd(fish$fork_length_mm)), c(.5, .05))
check("T03", "telemetry", "Weight mean and SD (g)", c(27, 5.7), c(mean(fish$weight_g), sd(fish$weight_g)), c(.5, .05))
beyond <- rowSums(counts[, c("HoT", "Inner_Bay", "Outer_Bay", "all_SoBI")]) > 0
check("T04", "telemetry", "Head of tide or beyond", 44, sum(beyond))
check("T05", "telemetry", "Remaining fish detected at D10", TRUE, all(counts[!beyond, "R3a"] > 0))
check("T06", "telemetry", "Direct Moses; Butters; neither detections", c(2, 8, 7), colSums(route_detections[c("Moses", "Butters", "Neither")]))
check("T07", "telemetry", "Island-network transit fish", 21L, nrow(network_tracks))
check("T08", "telemetry", "Receiver detected-fish totals", c(23, 38, 25, 1, 16, 2, 15, 7, 8, 22, 9, 44), unname(colSums(counts[, 1:12] > 0)))
instant <- c(parse_time(fish$release_datetime), parse_time(events$first_detection_datetime), parse_time(events$last_detection_datetime))
instant <- instant[!is.na(instant)]
check("T09", "time", "Quebec offset throughout retained observations", "-0400", unique(format(instant, "%z", tz = timezone)), source = "Data provider convention confirmed by author")
check("S01", "posterior", "Published retained draws", 10200, prod(dim(published$draws)[1:2]))
check("S02", "posterior", "Draw-wise route partition", TRUE, all(Reduce(`+`, route_states) == 1))
targets <- list(Moses = c(.269, .044, .489), Butters = c(.322, .222, .356), Neither = c(.409, .178, .644), Conditional_Moses = c(.422, .118, .629))
for (route in names(targets)) {
  x <- route_summary[route_summary$route == route, ]
  check(paste0("S_", route), "posterior", paste(route, "mean and 95% CrI"), targets[[route]], c(x$mean, x$q025, x$q975), .0005)
  d <- aggregate_diagnostics[aggregate_diagnostics$quantity == route, ]
  check(paste0("A_", route), "diagnostics", paste(route, "R-hat < 1.01 and ESS >= 2900"), TRUE, d$rhat < 1.01 && d$ess >= 2900,
        notes = paste("R-hat", d$rhat, "; ESS", d$ess, "; MCSE", d$mcse, "; chain means", d$chain_1_mean, d$chain_2_mean, d$chain_3_mean))
}
check("S03", "diagnostics", "19 probability parameters: R-hat < 1.002 and ESS > 2900", TRUE,
      nrow(probability_diagnostics) == 19 && max(probability_diagnostics$rhat) < 1.002 && min(probability_diagnostics$ess) > 2900,
      notes = "coda full-chain estimators, autoburnin=FALSE. This claim does not apply to all monitored latent states.")
fish35 <- fish_diagnostics[fish_diagnostics$fish_index == 35 & fish_diagnostics$quantity == "Moses", ]
check("S04", "diagnostics", "Fish 35 route-probability chain agreement", TRUE, fish35$chain_mean_range < .01,
      notes = paste("Tag 61899; Moses range", fish35$chain_mean_range, "; intermediate latent-state exceptions remain documented."))
check("C01", "cmr", "Fixed passage probabilities", c(.270, .296, .434), fixed_passage$probability, 1e-12)
check("C01_DERIVATION", "cmr", "Fixed constants agree with rounded products of posterior transition means", fixed_passage$probability,
      round(fixed_passage$product_of_posterior_means, 3), 1e-12)
original <- cmr_diagnostics[cmr_diagnostics$model == "original" & !cmr_diagnostics$constant, ]
check("C02", "cmr", "Original diagnostic thresholds", TRUE, max(original$rhat) <= 1.012 && min(original$ess) >= 670,
      notes = paste("Maximum R-hat", max(original$rhat), "; minimum ESS", min(original$ess), "; structural constants excluded."))
for (parameter in c("theta_B", "theta_Mo", "theta_B_cond", "theta_Mo_cond", "Nm_tot")) {
  x <- cmr_comparison[cmr_comparison$model == "split" & cmr_comparison$quantity == parameter, ]
  check(paste0("C_", parameter), "cmr", paste(parameter, "flagged/modelled years"), if (parameter == "Nm_tot") c(16, 18) else c(13, 16),
        c(x$flagged_years, x$modeled_years))
}
check("C03", "cmr", "Simultaneously satisfactory split years", c(2009L, 2016L), sort(split_good_years))

references <- read_input("audit/reported_table_cells.csv", c("table", "key", "field", "expected", "tolerance"), c("table", "key", "field"))
table1_values <- rbind(table_1, table_1_summary)
table2_values <- data.frame(key = as.character(annual$year), marked = annual$kedgwick_marked_n, Moses = annual$moses_recaptures_n,
  Butters = annual$butters_recaptures_n, conditional_Moses_percent = 100 * annual$conditional_moses,
  CI_lower_percent = 100 * annual$ci_lower, CI_upper_percent = 100 * annual$ci_upper,
  mean_discharge_m3_s = annual$mean_discharge_m3_s, discharge_cv = annual$discharge_cv)
table2_values <- rbind(table2_values, data.frame(key = "Total", marked = sum(annual$kedgwick_marked_n), Moses = sum(moses), Butters = sum(total - moses),
  conditional_Moses_percent = 100 * pooled, CI_lower_percent = 100 * pooled_ci[1], CI_upper_percent = 100 * pooled_ci[2], mean_discharge_m3_s = NA_real_, discharge_cv = NA_real_))
table3_values <- data.frame(key = as.character(table_3$receiver_index), receiver_index = table_3$receiver_index,
  zone_index = table_3$zone_id, channel_index = table_3$channel_id, detected_fish = table_3$detected_fish)
for (i in seq_len(nrow(references))) {
  row <- references[i, ]
  observed <- switch(row$table,
    "Table 1" = as.numeric(table1_values[match(row$key, table1_values$fish_id), row$field]),
    "Table 2" = as.numeric(table2_values[match(row$key, table2_values$key), row$field]),
    "Table 3" = as.numeric(table3_values[match(row$key, table3_values$key), row$field]),
    "Table B1" = as.numeric(probability_summary[match(row$key, probability_summary$parameter), row$field]))
  check(sprintf("CELL_%04d", i), "reported_table_cell", paste(row$table, row$key, row$field), row$expected, observed, row$tolerance,
        source = "audit/reported_table_cells.csv", notes = "Audited reporting cells; author-approved 2008 correction. Calculations use analytical inputs, not these reference cells.")
}
verification <- do.call(rbind, checks)
if (file.exists("audit/repository_validation.csv")) {
  validation <- read_input("audit/repository_validation.csv", names(verification), "check_id")
  verification <- rbind(verification, validation[names(verification)])
}
stopifnot(!anyDuplicated(verification$check_id), all(verification$status %in% c("PASS", "FAIL", "REVIEW")))
if (write_outputs) write.csv(verification, "audit/verification_results.csv", row.names = FALSE, na = "NA")
print(table(verification$status))
if (any(verification$status == "FAIL")) {
  print(verification[verification$status == "FAIL", ], row.names = FALSE)
  if (!refit_state_space && !refit_cmr_models) stop("Reported-result verification failed; inspect audit/verification_results.csv.")
  warning("Refit results differ from the published archive; inspect verification results before use.")
}
