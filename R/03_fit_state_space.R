published <- readRDS("derived-data/published_state_space_draws.rds")
stopifnot(identical(dim(published$draws), c(3400L, 3L, 199L)), identical(published$fish_id, fish$fish_id),
          !anyDuplicated(dimnames(published$draws)[[3]]))
state_draws <- published$draws
if (refit_state_space) {
  initial <- readRDS("data/state_space_initial_values.rds")
  seeds <- c(20230122L, 20230123L, 20230124L)
  for (i in seq_along(initial)) {
    initial[[i]]$.RNG.name <- "base::Mersenne-Twister"
    initial[[i]]$.RNG.seed <- seeds[i]
  }
  total_iterations <- 350000L
  burnin_iterations <- 9800L
  thinning <- 100L
  rjags::load.module("dic", quiet = TRUE)
  model <- rjags::jags.model("model/channel_use_state_space.jags", data = model_data, inits = initial,
                             n.chains = 3L, n.adapt = 1000L, quiet = TRUE)
  update(model, n.iter = burnin_iterations, progress.bar = "none")
  refit_chains <- rjags::coda.samples(model, variable.names = unique(c(published$monitor, "deviance")),
                                     n.iter = total_iterations - burnin_iterations, thin = thinning, progress.bar = "none")
  refit_array <- simplify2array(lapply(refit_chains, as.matrix))
  state_draws <- aperm(refit_array, c(1, 3, 2))
  if (write_outputs) saveRDS(list(draws = state_draws, fish_id = fish$fish_id, seeds = seeds,
                                  burnin = burnin_iterations, total_iterations = total_iterations, thinning = thinning),
                             "output/state_space_refit.rds", compress = "xz")
}
