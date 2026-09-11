cmr_input <- readRDS("data/cmr_model_input.rds")
cmr_initial <- readRDS("data/cmr_initial_values.rds")
stopifnot(length(cmr_input) == 28, cmr_input$T == 18, identical(dim(cmr_input$I_w), c(18L, 5L)),
          all(cmr_input$I_w %in% c(0, 1)), identical(dim(cmr_input$alpha), c(18L, 4L)))
cmr_diagnostics <- read_input("derived-data/published_cmr_diagnostics.csv", c("model", "parameter", "variable", "year", "rhat", "ess", "constant"), c("model", "parameter"))
fixed_passage <- data.frame(route = c("Moses", "Butters", "Neither"), probability = c(.270, .296, 1 - .270 - .296),
                            derivation = "Rounded products of posterior mean transition probabilities")
transition_means <- vapply(p, mean, numeric(1))
moses_product <- transition_means["p_Z1a"] * transition_means["p_Z2a"]
butters_product <- (1 - transition_means["p_Z1a"]) * transition_means["p_Z2b"] * transition_means["p_Z3b"]
fixed_passage$product_of_posterior_means <- unname(c(moses_product, butters_product, 1 - moses_product - butters_product))
if (refit_cmr_models) {
  rjags::load.module("dic", quiet = TRUE)
  refit_diagnostics <- list()
  for (model_name in c("original", "split")) {
    settings <- cmr_initial$settings[[model_name]]
    initial <- cmr_initial$initial_values[[model_name]]
    for (i in seq_along(initial)) {
      initial[[i]]$.RNG.name <- "base::Mersenne-Twister"
      initial[[i]]$.RNG.seed <- 202107L + i - 1L
    }
    model_path <- if (model_name == "original") "model/cmr_original.jags" else "model/cmr_telemetry_split.jags"
    model <- rjags::jags.model(model_path, data = cmr_input, inits = initial, n.chains = 2L, n.adapt = 1000L, quiet = TRUE)
    update(model, n.iter = settings$burnin, progress.bar = "none")
    samples <- rjags::coda.samples(model, variable.names = settings$monitor,
                                    n.iter = settings$total_iterations - settings$burnin, thin = settings$thinning, progress.bar = "none")
    values <- as.matrix(samples)
    parameters <- colnames(values)
    constant <- apply(values, 2, sd) == 0
    rhat <- ess <- setNames(rep(NA_real_, length(parameters)), parameters)
    active <- parameters[!constant]
    chains <- coda::mcmc.list(lapply(samples, function(x) coda::mcmc(as.matrix(x)[, active, drop = FALSE])))
    rhat[active] <- coda::gelman.diag(chains, autoburnin = FALSE, multivariate = FALSE)$psrf[, 1]
    ess[active] <- coda::effectiveSize(chains)
    year <- rep(NA_integer_, length(parameters))
    indexed <- grepl("^(Nm_tot|theta_B|theta_Mo|theta_B_cond|theta_Mo_cond)\\[", parameters)
    year[indexed] <- 2001L + as.integer(sub(".*\\[([0-9]+)\\].*", "\\1", parameters[indexed]))
    refit_diagnostics[[model_name]] <- data.frame(model = model_name, parameter = parameters, variable = sub("\\[.*", "", parameters), year = year,
                                                  rhat = unname(rhat), ess = unname(ess), constant = constant)
    if (write_outputs) saveRDS(samples, paste0("output/cmr_", model_name, "_refit.rds"), compress = "xz")
  }
  cmr_diagnostics <- do.call(rbind, refit_diagnostics)
}
cmr_diagnostics$flagged <- !cmr_diagnostics$constant &
  (!is.finite(cmr_diagnostics$rhat) | !is.finite(cmr_diagnostics$ess) | cmr_diagnostics$rhat > 1.1 | cmr_diagnostics$ess < 200)
cmr_comparison <- do.call(rbind, lapply(c("original", "split"), function(model) {
  do.call(rbind, lapply(c("Nm_tot", "theta_B", "theta_Mo", "theta_B_cond", "theta_Mo_cond"), function(variable) {
    x <- cmr_diagnostics[cmr_diagnostics$model == model & cmr_diagnostics$variable == variable & !cmr_diagnostics$constant, ]
    if (!nrow(x)) return(NULL)
    data.frame(model = model, quantity = variable, modeled_years = nrow(x), flagged_years = sum(x$flagged), max_rhat = max(x$rhat), min_ess = min(x$ess))
  }))
}))
split_good_years <- Reduce(intersect, lapply(c("Nm_tot", "theta_B", "theta_Mo", "theta_B_cond", "theta_Mo_cond"), function(variable) {
  x <- cmr_diagnostics[cmr_diagnostics$model == "split" & cmr_diagnostics$variable == variable & !cmr_diagnostics$flagged & !cmr_diagnostics$constant, ]
  x$year
}))
write_table(fixed_passage, "cmr_fixed_passage_probabilities")
write_table(cmr_diagnostics, "cmr_parameter_diagnostics")
write_table(cmr_comparison, "cmr_convergence_comparison")
write_table(data.frame(year = sort(split_good_years)), "cmr_split_satisfactory_years")
