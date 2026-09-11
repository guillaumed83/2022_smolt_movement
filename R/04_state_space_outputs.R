probability_names <- c(receivers$detection_parameter[1:12], "p_Z1a", "p_Z2a", "p_Z2b", "p_dodge", "p_Z3b", "p_Z5b_Z4a", "p_Z5b_Z4ab")
parameter_names <- dimnames(state_draws)[[3]]
stopifnot(all(probability_names %in% parameter_names), !anyDuplicated(parameter_names))
summarize_draws <- function(x) {
  c(mean = mean(x), sd = sd(x), setNames(unname(quantile(x, c(.025, .25, .5, .75, .975))), c("q025", "q25", "median", "q75", "q975")))
}
diagnose_chains <- function(x, quantity) {
  stopifnot(is.matrix(x), all(is.finite(x)))
  chains <- coda::mcmc.list(lapply(seq_len(ncol(x)), function(j) coda::mcmc(x[, j])))
  constant <- length(unique(as.numeric(x))) == 1
  spectral <- vapply(seq_len(ncol(x)), function(j) if (var(x[, j]) == 0) 0 else coda::spectrum0.ar(x[, j])$spec, numeric(1))
  rhat <- if (constant) NA_real_ else unname(coda::gelman.diag(chains, autoburnin = FALSE, multivariate = FALSE)$psrf[1, 1])
  ess <- if (constant) NA_real_ else unname(coda::effectiveSize(chains)[1])
  mcse <- sqrt(sum(spectral / nrow(x))) / ncol(x)
  means <- colMeans(x)
  data.frame(quantity = quantity, mean = mean(x), sd = sd(as.numeric(x)), rhat = rhat, ess = ess, mcse = mcse,
             chain_1_mean = means[1], chain_2_mean = means[2], chain_3_mean = means[3],
             chain_mean_range = diff(range(means)), constant = constant)
}
extract_state <- function(variable) {
  names <- sprintf("%s[%d]", variable, fish$fish_index)
  index <- match(names, parameter_names)
  if (anyNA(index)) stop("Missing indexed states: ", paste(names[is.na(index)], collapse = ", "))
  state_draws[, , index, drop = FALSE]
}
route_states <- list(Moses = extract_state("Z2_a"), Butters = extract_state("Z4_b"),
                     Neither = extract_state("Z3_ab_dodge") + extract_state("Z4_ab"))
stopifnot(all(Reduce(`+`, route_states) == 1))
route_chains <- lapply(route_states, function(x) apply(x, c(1, 2), mean))
stopifnot(all(route_chains$Moses + route_chains$Butters > 0))
route_chains$Conditional_Moses <- route_chains$Moses / (route_chains$Moses + route_chains$Butters)
aggregate_diagnostics <- do.call(rbind, lapply(names(route_chains), function(name) diagnose_chains(route_chains[[name]], name)))
aggregate_diagnostics$status <- ifelse(is.finite(aggregate_diagnostics$rhat) & aggregate_diagnostics$rhat < 1.01 &
                                        is.finite(aggregate_diagnostics$ess) & aggregate_diagnostics$ess >= 2900, "PASS", "FAIL")
if (any(aggregate_diagnostics$status != "PASS")) stop("Aggregate route diagnostics failed; do not use these draws for manuscript outputs.")
route_summary <- data.frame(route = names(route_chains), do.call(rbind, lapply(route_chains, function(x) summarize_draws(as.numeric(x)))), row.names = NULL)
probability_summary <- data.frame(parameter = probability_names,
  do.call(rbind, lapply(probability_names, function(name) summarize_draws(as.numeric(state_draws[, , name])))), row.names = NULL)
probability_diagnostics <- do.call(rbind, lapply(probability_names, function(name) diagnose_chains(state_draws[, , name], name)))
individual_routes <- data.frame(fish_id = fish$fish_id, fish_index = fish$fish_index)
for (route in names(route_states)) individual_routes[[route]] <- apply(route_states[[route]], 3, mean)
fish_diagnostics <- do.call(rbind, lapply(seq_len(nrow(fish)), function(i) {
  do.call(rbind, lapply(names(route_states), function(route) {
    cbind(fish_id = fish$fish_id[i], fish_index = fish$fish_index[i], diagnose_chains(route_states[[route]][, , i], route))
  }))
}))
write_table(probability_summary, "posterior_probability_summary")
write_table(probability_diagnostics, "probability_parameter_diagnostics")
write_table(route_summary, "aggregate_route_summary")
write_table(aggregate_diagnostics, "aggregate_route_diagnostics")
write_table(individual_routes, "individual_route_probabilities")
write_table(fish_diagnostics, "fish_route_chain_diagnostics")
table_b1 <- probability_summary
table_b1$receiver_label <- c(receivers$receiver_label[1:12], rep(NA_character_, 7))
for (field in setdiff(names(probability_summary), "parameter")) table_b1[[field]] <- round(table_b1[[field]], 3)
write_table(table_b1, "table_b1_posterior_summary")
plot_discharge(route_summary[route_summary$route == "Conditional_Moses", ])

zone_colors <- adjustcolor(c("#A6BDDB", "#74A9CF", "#3690C0", "#0570B0", "#034E7B"), alpha.f = 170 / 255)
transition_labels <- c(expression(italic(pt)["1,1"]), expression(italic(pt)["2,1"]), expression(italic(pt)["2,3"]),
                       expression(italic(pt)["3,2"]), expression(italic(pt)["4,3"]), expression(italic(pt)["5,2a"]), expression(italic(pt)["5,2b"]))
plot_intervals <- function(index, labels, zones, name, width, xlab) {
  save_figure(name, width, 4.5, function() {
    par(mar = c(4.2, 4, 2, 1))
    summary <- probability_summary[index, ]
    n <- nrow(summary)
    summary <- rbind(summary, data.frame(parameter = "Prior", mean = .5, sd = sqrt(1 / 12), q025 = .025, q25 = .25, median = .5, q75 = .75, q975 = .975))
    plot(seq_len(n + 1), summary$median, type = "n", ylim = c(0, 1), xlim = c(.5, n + 1.5), xaxt = "n", xaxs = "i", yaxs = "i", xlab = xlab, ylab = "Probability")
    for (j in 1:n) if (!is.na(zones[j])) rect(j - .5, 0, j + .5, 1, col = zone_colors[zones[j]], border = NA)
    rect(n + .5, 0, n + 1.5, 1, col = "grey92", border = NA)
    abline(h = seq(.2, .8, .2), col = "grey85")
    abline(v = n + .5, lty = 2)
    colors <- c(rep("black", n), "red")
    segments(1:(n + 1), summary$q025, 1:(n + 1), summary$q975, col = colors)
    segments(1:(n + 1), summary$q25, 1:(n + 1), summary$q75, col = colors, lwd = 3)
    points(1:(n + 1), summary$median, col = colors, pch = 19, cex = .9)
    axis(1, at = 1:(n + 1), labels = c(labels, "Prior"), cex.axis = .8)
    legend("topleft", inset = c(0, -.14), xpd = NA, legend = paste("Zone", 1:5), fill = zone_colors, ncol = 5, bty = "n", cex = .75)
    box()
  })
}
receiver_labels <- as.expression(c(lapply(1:11, function(i) bquote(D[.(i)])), list(expression(D[12] / HoT)[[1]])))
plot_intervals(1:12, receiver_labels, receivers$zone_id[1:12], "receiver_detection_probabilities", 10, "Receivers")
plot_intervals(13:19, transition_labels, c(1, 2, 2, 3, 4, 5, 5), "transition_probabilities", 8, "Transition probability")
for (group in c("detection", "transition")) {
  selected <- if (group == "detection") 1:12 else 13:19
  labels <- if (group == "detection") lapply(1:12, function(i) bquote(italic(p)[.(i)])) else transition_labels
  save_figure(paste0(group, "_posterior_distributions"), 10, if (group == "detection") 8 else 6, function() {
    par(mfrow = c(if (group == "detection") 3 else 2, 4), mar = c(3.6, 3.3, 1, .8), mgp = c(2, .65, 0), cex = .8)
    for (i in seq_along(selected)) {
      x <- as.numeric(state_draws[, , probability_names[selected[i]]])
      hist(x, breaks = if (group == "detection") 50 else 40, freq = FALSE, xlim = c(0, 1),
           col = "grey75", border = "black", main = "", xlab = labels[[i]], ylab = if (i %% 4 == 1) "Density" else "")
      segments(0, 1, 1, 1, col = "red", lwd = 1)
    }
  })
}
display <- rev(order(individual_routes$Butters, individual_routes$Moses, individual_routes$fish_index))
route_plot_data <- do.call(rbind, lapply(c("Neither", "Moses", "Butters"), function(route) {
  data.frame(fish_id = factor(individual_routes$fish_id, levels = individual_routes$fish_id[display]),
             route = factor(route, levels = c("Butters", "Moses", "Neither")), probability = individual_routes[[route]])
}))
save_figure("individual_route_probabilities", 12, 6, function() {
  plot <- ggplot2::ggplot(route_plot_data, ggplot2::aes(x = fish_id, y = probability, fill = route)) +
    ggplot2::geom_col(position = "fill", width = .9) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(round(100 * probability) == 0, "", round(100 * probability))),
                       color = "white", position = ggplot2::position_fill(vjust = .5), size = 2.5) +
    ggplot2::scale_fill_manual(values = c(Moses = "skyblue2", Butters = "steelblue4", Neither = "grey65"),
                               breaks = c("Butters", "Moses", "Neither"), labels = c("Butters", "Moses", "Neither RST-monitored channel")) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), expand = c(0, 0)) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) + ggplot2::labs(x = "Smolt ID", y = NULL, fill = "Route") +
    ggplot2::theme_bw(base_size = 11) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5),
      panel.grid.major.x = ggplot2::element_blank(), legend.position = "bottom")
  print(plot)
})

nodes <- read_input("data/network_nodes.csv", c("node_id", "zone_id", "channel_id", "state_variable", "x", "y", "receiver_label"), "node_id")
edges <- read_input("data/network_edges.csv", c("edge_id", "from_node", "to_node", "marginal_key", "transition_parameter", "compulsory"), "edge_id")
stopifnot(nrow(nodes) == 15, nrow(edges) == 20, all(edges$from_node %in% nodes$node_id), all(edges$to_node %in% nodes$node_id))
p <- setNames(lapply(probability_names, function(name) as.numeric(state_draws[, , name])), probability_names)
links <- list(p1 = p$p_Z1a, p2 = 1 - p$p_Z1a)
links$p11 <- p$p_Z1a * p$p_Z2a
links$p12 <- p$p_Z1a * (1 - p$p_Z2a)
links$p22 <- (1 - p$p_Z1a) * (1 - p$p_Z2b)
links$p23 <- (1 - p$p_Z1a) * p$p_Z2b
links$px11 <- links$p11
links$px22 <- (links$p12 + links$p22) * p$p_dodge
links$px23 <- (links$p12 + links$p22) * (1 - p$p_dodge)
links$px34 <- links$p23
links$pxx11 <- links$px11
links$pxx21 <- links$px22
links$pxx32 <- links$px23
links$pxx42 <- links$px34 * (1 - p$p_Z3b)
links$pxx43 <- links$px34 * p$p_Z3b
links$pxxx11 <- (links$pxx11 + links$pxx21) * (1 - p$p_Z5b_Z4a)
links$pxxx12 <- (links$pxx11 + links$pxx21) * p$p_Z5b_Z4a
links$pxxx21 <- (links$pxx32 + links$pxx42) * (1 - p$p_Z5b_Z4ab)
links$pxxx22 <- (links$pxx32 + links$pxx42) * p$p_Z5b_Z4ab
links$pxxx32 <- links$pxx43
edges$posterior_mean <- vapply(links[edges$marginal_key], mean, numeric(1))
edges$posterior_sd <- vapply(links[edges$marginal_key], sd, numeric(1))
write_table(edges, "network_marginal_link_probabilities")
save_figure("channel_network", 12, 6.5, function() {
  par(mar = c(.5, .5, .5, .5))
  plot(0, 0, type = "n", xlim = c(-.65, 5.65), ylim = c(1.5, 4.5), axes = FALSE, xlab = "", ylab = "")
  palette <- c("skyblue1", "deepskyblue", "deepskyblue2", "deepskyblue3", "deepskyblue4", "dodgerblue4")
  for (i in seq_len(nrow(edges))) {
    from <- nodes[match(edges$from_node[i], nodes$node_id), ]
    to <- nodes[match(edges$to_node[i], nodes$node_id), ]
    dx <- to$x - from$x
    dy <- to$y - from$y
    distance <- sqrt(dx^2 + dy^2)
    start <- c(from$x, from$y) + .13 * c(dx, dy) / distance
    end <- c(to$x, to$y) - .13 * c(dx, dy) / distance
    color <- palette[min(from$zone_id + 1, 6)]
    arrows(start[1], start[2], end[1], end[2], length = .11, angle = 22, code = 2, lwd = 1 + 9 * edges$posterior_mean[i], col = color)
    offset <- if (abs(dy) < .05) .095 else .11
    fraction <- if (edges$edge_id[i] == "E17") .22 else if (edges$edge_id[i] == "E18") .75 else .5
    if (edges$edge_id[i] == "E17") offset <- -.10
    if (edges$edge_id[i] == "E15") offset <- -.095
    if (edges$edge_id[i] == "E14") offset <- .035
    x <- from$x + fraction * dx - offset * dy / distance
    y <- from$y + fraction * dy + offset * dx / distance
    angle <- atan2(dy * par("pin")[2] / diff(par("usr")[3:4]), dx * par("pin")[1] / diff(par("usr")[1:2])) * 180 / pi
    text(x, y, sprintf("%.2f %s %.2f", edges$posterior_mean[i], intToUtf8(177), edges$posterior_sd[i]),
         srt = angle, cex = .68, col = color)
  }
  points(nodes$x, nodes$y, pch = 21, bg = "white", col = "steelblue4", cex = 3.3)
  for (i in seq_len(nrow(nodes))) {
    label <- if (nodes$zone_id[i] == 0) "" else paste(nodes$zone_id[i], nodes$channel_id[i], sep = ",")
    text(nodes$x[i], nodes$y[i], label, col = "black", cex = .7)
    if (!is.na(nodes$receiver_label[i])) {
      receiver <- receivers$receiver_index[match(nodes$receiver_label[i], receivers$receiver_label)]
      dx <- if (receiver == 3) -.40 else if (receiver %in% 10:11) .40 else .16
      dy <- if (receiver %in% c(3, 10, 11)) 0 else if (receiver %in% c(5, 7, 9)) -.25 else .25
      x <- nodes$x[i] + dx
      y <- nodes$y[i] + dy
      segments(nodes$x[i], nodes$y[i], x, y, col = "grey65")
      rect(x - .12, y - .06, x + .12, y + .06, col = "#FFF2AE", border = "grey60")
      text(x, y, receiver, cex = .65)
      if (receiver %in% c(6, 9)) {
        x <- nodes$x[i] + .40
        y <- nodes$y[i] - .13
        polygon(x + c(-.08, -.08, .13), y + c(0, .10, 0), col = "grey75", border = "grey50")
        segments(x - .12, y, x + .18, y, col = "grey50", lwd = 2)
        text(x + .03, y - .075, if (receiver == 6) "Moses" else "Butters", cex = .55, col = "grey35")
      }
    }
  }
})
