fish <- read_input("data/tagged_smolt_metadata.csv", c("fish_id", "fish_index", "release_datetime", "fork_length_mm", "weight_g"), "fish_id")
receivers <- read_input("data/receiver_metadata.csv", c("location_id", "receiver_label", "display_order", "zone_id", "channel_id", "model_observation", "detection_parameter"), "location_id")
events <- read_input("data/telemetry_location_events.csv", c("fish_id", "location_id", "first_detection_datetime", "last_detection_datetime", "detection_count", "detected"), c("fish_id", "location_id"))
segments <- read_input("data/segment_distances.csv", c("segment_id", "start_location", "start_event", "end_location", "end_event", "distance_km"), "segment_id")
fish <- fish[order(fish$fish_index), ]
receivers <- receivers[order(receivers$display_order), ]
stopifnot(nrow(fish) == 45, nrow(receivers) == 15, nrow(events) == 675, identical(fish$fish_index, 1:45),
          all(events$fish_id %in% fish$fish_id), all(events$location_id %in% receivers$location_id),
          all(events$detection_count >= 0), all(events$detection_count == floor(events$detection_count)),
          all(events$detected == as.integer(events$detection_count > 0)))
key <- paste(events$fish_id, events$location_id)
index <- match(as.vector(t(outer(fish$fish_id, receivers$location_id, paste))), key)
stopifnot(!anyNA(index))
events <- events[index, ]
counts <- matrix(events$detection_count, nrow = 45, byrow = TRUE, dimnames = list(fish$fish_id, receivers$location_id))
first <- matrix(as.numeric(parse_time(events$first_detection_datetime)), nrow = 45, byrow = TRUE, dimnames = dimnames(counts))
last <- matrix(as.numeric(parse_time(events$last_detection_datetime)), nrow = 45, byrow = TRUE, dimnames = dimnames(counts))
release <- as.numeric(parse_time(fish$release_datetime))
stopifnot(!anyNA(release), identical(is.na(first), counts == 0), identical(is.na(last), counts == 0),
          all(last[counts > 0] >= first[counts > 0]), all(first[counts > 0] >= matrix(release, 45, 15)[counts > 0]))
model_data <- list(n_fish = 45L)
for (j in which(!is.na(receivers$model_observation))) model_data[[receivers$model_observation[j]]] <- as.integer(counts[, j] > 0)
travel <- do.call(rbind, lapply(seq_len(nrow(segments)), function(i) {
  s <- segments[i, ]
  start <- if (s$start_location == "release") release else if (s$start_event == "first") first[, s$start_location] else last[, s$start_location]
  end_locations <- strsplit(s$end_location, ";", fixed = TRUE)[[1]]
  end <- if (length(end_locations) == 1) {
    if (s$end_event == "first") first[, end_locations] else last[, end_locations]
  } else apply(last[, end_locations, drop = FALSE], 1, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
  days <- (end - start) / 86400
  if (any(days <= 0, na.rm = TRUE)) stop("Nonpositive travel time in segment ", s$segment_id)
  data.frame(fish_id = fish$fish_id, segment_id = s$segment_id, start_epoch_seconds = start, end_epoch_seconds = end,
             travel_days = days, distance_km = s$distance_km, speed_km_day = s$distance_km / days)
}))
travel_summary <- do.call(rbind, lapply(segments$segment_id, function(id) {
  x <- travel[travel$segment_id == id & is.finite(travel$travel_days), ]
  data.frame(segment_id = id, n = nrow(x), mean_days = mean(x$travel_days), sd_days = sd(x$travel_days),
             mean_speed_km_day = mean(x$speed_km_day), sd_speed_km_day = sd(x$speed_km_day))
}))
network_tracks <- travel[travel$segment_id == "network" & is.finite(travel$travel_days), ]
network_tracks$duration_minutes <- network_tracks$travel_days * 1440
stopifnot(nrow(network_tracks) == 21)
network_tracks$duration_class <- cut(network_tracks$duration_minutes, c(-Inf, 100, 480, Inf), right = FALSE,
                                     labels = c("<100 min", "100 min to <8 hours", ">=8 hours"))
biology_summary <- data.frame(n = nrow(fish), fork_length_mean_mm = mean(fish$fork_length_mm), fork_length_sd_mm = sd(fish$fork_length_mm),
                              weight_mean_g = mean(fish$weight_g), weight_sd_g = sd(fish$weight_g))
route_detections <- data.frame(fish_id = fish$fish_id, Moses = counts[, "R2a"] > 0, Butters = counts[, "R2b"] > 0, Neither = counts[, "Rx2a"] > 0)
table_1 <- data.frame(fish_id = fish$fish_id, fork_length_mm = fish$fork_length_mm, weight_g = fish$weight_g, counts, total_detections = rowSums(counts), check.names = FALSE)
table_segments <- c("release_Rx1", "release_HoT", "HoT_inner", "HoT_outer", "HoT_SoBI")
for (segment in table_segments) table_1[[paste0(segment, "_days")]] <- travel$travel_days[match(paste(fish$fish_id, segment), paste(travel$fish_id, travel$segment_id))]
table_1_summary <- do.call(rbind, lapply(c("Mean", "SD"), function(label) {
  x <- table_1[1, ]
  x$fish_id <- label
  x$fork_length_mm <- x$weight_g <- NA_real_
  fun <- if (label == "Mean") mean else sd
  for (field in c(colnames(counts), "total_detections", paste0(table_segments, "_days"))) {
    value <- table_1[[field]]
    if (field %in% c(colnames(counts), "total_detections")) value <- value[value > 0]
    x[[field]] <- fun(value, na.rm = TRUE)
  }
  x
}))
table_1_display <- rbind(table_1, table_1_summary)
for (field in paste0(table_segments, "_days")) table_1_display[[field]] <- round(table_1_display[[field]], 2)
for (field in c(colnames(counts), "total_detections")) table_1_display[[field]][46:47] <- round(table_1_display[[field]][46:47], 1)
table_3 <- receivers[!is.na(receivers$receiver_index), c("receiver_index", "receiver_label", "zone_id", "channel_id", "location_id")]
table_3$detected_fish <- colSums(counts[, table_3$location_id, drop = FALSE] > 0)
write_table(table_1_display, "table_1_tagged_smolts")
write_table(table_3, "table_3_receiver_indices")
write_table(biology_summary, "tagged_smolt_biology")
write_table(travel, "individual_travel_times_speeds")
write_table(travel_summary, "segment_travel_summary")
write_table(network_tracks, "island_network_tracks")
write_table(route_detections, "direct_route_detections")

tide <- read_input("data/tide_light_series.csv", c("datetime", "tide_height_m", "daylight_state"), "datetime")
light <- read_input("data/daylight_boundaries.csv", c("nautical_twilight_start_datetime", "sunrise_datetime", "sunset_datetime", "nautical_twilight_end_datetime"), "sunrise_datetime")
tide_time <- as.numeric(parse_time(tide$datetime))
light_time <- lapply(light, function(x) as.numeric(parse_time(x)))
tide_at <- approxfun(tide_time, tide$tide_height_m)
track_colors <- c("#E8DD39", "#F28E00", "#E73D26")
save_figure("island_passage_tide_light", 8.5, 11, function() {
  par(mfrow = c(2, 1), mar = c(4.8, 4.4, .7, .7))
  windows <- list(c("2022-05-25", "2022-05-29"), c("2022-05-30", "2022-06-05"))
  for (panel in 1:2) {
    limits <- as.numeric(as.POSIXct(windows[[panel]], tz = timezone))
    limits <- limits + c(-1, 1) * diff(limits) * .04
    ix <- tide_time >= limits[1] & tide_time <= limits[2]
    ylim <- if (panel == 1) c(.8, 3.12) else c(.65, 3.2)
    plot(tide_time[ix], tide$tide_height_m[ix], type = "n", xlim = limits, ylim = ylim, xaxt = "n", xaxs = "i", yaxs = "i",
         xlab = "Date", ylab = "Water height (m)")
    rect(limits[1], ylim[1], limits[2], ylim[2], col = "grey55", border = NA)
    for (i in seq_len(nrow(light))) {
      rect(light_time$nautical_twilight_start_datetime[i], ylim[1], light_time$nautical_twilight_end_datetime[i], ylim[2], col = "grey75", border = NA)
      rect(light_time$sunrise_datetime[i], ylim[1], light_time$sunset_datetime[i], ylim[2], col = "grey95", border = NA)
    }
    lines(tide_time, tide$tide_height_m, col = "royalblue3", lwd = 1.5)
    ticks <- if (panel == 1) seq(as.POSIXct("2022-05-25", tz = timezone), as.POSIXct("2022-05-29", tz = timezone), by = "day") else
      seq(as.POSIXct("2022-05-31", tz = timezone), as.POSIXct("2022-06-04", tz = timezone), by = "2 days")
    axis(1, at = as.numeric(ticks), labels = format(ticks, "%b %d", tz = timezone), cex.axis = .8)
    label_y <- tide_at(network_tracks$start_epoch_seconds) + .014
    label_y[match(c("61837", "61838", "61841", "61692", "61693", "61907"), network_tracks$fish_id)] <- c(1.82, 1.91, 1.73, 1.07, 1.24, 1.15)
    for (i in seq_len(nrow(network_tracks))) {
      x <- network_tracks[i, ]
      if (x$start_epoch_seconds < limits[1] || x$start_epoch_seconds > limits[2]) next
      y <- tide_at(x$start_epoch_seconds)
      color <- track_colors[as.integer(x$duration_class)]
      segments(x$start_epoch_seconds, y, x$end_epoch_seconds, y, col = color, lwd = 1)
      points(c(x$start_epoch_seconds, x$end_epoch_seconds), c(y, y), col = color, pch = 15, cex = .55)
      if (abs(label_y[i] - y) > .025) segments(x$start_epoch_seconds - 7500, label_y[i], x$start_epoch_seconds - 1000, y, col = "grey30", lwd = .5)
      text(x$start_epoch_seconds - 9000, label_y[i], x$fish_id, cex = .62, adj = 1)
    }
    if (panel == 2) legend("topright", c("Day (sunrise-sunset)", "Nautical twilight", "Night", "Water height", "First D3 to last D10/D11", levels(network_tracks$duration_class)),
                           col = c("grey95", "grey75", "grey55", "royalblue3", NA, track_colors),
                           pch = c(15, 15, 15, NA, NA, 15, 15, 15), lty = c(NA, NA, NA, 1, NA, 1, 1, 1),
                           bg = "white", cex = .75, box.col = "grey70")
    box()
  }
})
save_figure("riverine_migration_speeds", 11, 5.5, function() {
  par(mar = c(4.8, 4.5, 1, .8))
  plot(0, 0, type = "n", xlim = c(0, 20.6), ylim = c(-6, 156), xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", xlab = "", ylab = expression("Migration speed (km "*day^{-1}*")"))
  rect(0, -6, 10, 156, col = "grey85", border = NA)
  rect(10, -6, 15, 156, col = "grey75", border = NA)
  rect(15, -6, 20.6, 156, col = "grey65", border = NA)
  axis(2, at = seq(0, 150, 25), las = 1)
  axis(1, at = c(1, 10, 15, 20), labels = c("Release site", "Rx1", "R3a/R3b", "HoT"))
  for (j in 1:3) {
    start <- c(1, 10, 1)[j]
    end <- c(10, 15, 20)[j]
    id <- c("release_Rx1", "network", "release_HoT")[j]
    value <- na.omit(travel$speed_km_day[travel$segment_id == id])
    average <- mean(value)
    spread <- sd(value)
    x <- seq(start, end, length.out = 200)
    shape <- plogis(c(1.2, 2.5, .5)[j] * (x - (start + end) / 2))
    color <- c("#FDE725", "#90D743", "#35B779")[j]
    polygon(c(x, rev(x)), c(shape * (average + spread), rev(shape * max(average - spread, 0))), col = adjustcolor(color, alpha.f = .39), border = NA)
    points(rep(end, length(value)), value, pch = 16, col = adjustcolor("grey20", alpha.f = .39), cex = .7)
    segments(end, max(average - spread, 0), end, average + spread, col = color, lwd = 1.5)
    points(end, average, pch = 16, col = color)
    text(end, c(81, 144, 67)[j], paste("n =", length(value)), cex = .8)
  }
  text(c(5, 12.5, 18), 148, c("lotic\nhabitat", "island\nnetwork", "pelagic\nhabitat"), cex = .9)
  mtext(c("101 km", "6.35 km", "9.5 km"), side = 1, at = c(5.5, 12.5, 17.5), line = 2.7, cex = .8)
  for (j in 1:3) {
    left <- c(1, 10, 15)[j]
    right <- c(10, 15, 20)[j]
    middle <- (left + right) / 2
    arrows(middle - .7, -25, left, -25, code = 2, angle = 25, length = .08, xpd = NA)
    arrows(middle + .7, -25, right, -25, code = 2, angle = 25, length = .08, xpd = NA)
  }
  box()
})
