library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(readr)

df <- read_csv("data/test_preds_shiny.csv") %>%
  mutate(time_frame = factor(time_frame,
                             levels = c("PreHW", "PostHW"),
                             labels = c("Pre Heat Wave", "Post Heat Wave")))

pred_vars <- c("log_d_mean_gb", "oxygen_mean_0_50", "oxygen_surface",
               "salt_surface",  "temp_mean_0_50")

var_labels <- c(
  log_d_mean_gb    = "Log Mean Groundfish Biomass (log units)",
  oxygen_mean_0_50 = "Mean Oxygen 0-50m (umol/kg)",
  oxygen_surface   = "Surface Oxygen (umol/kg)",
  salt_surface     = "Surface Salinity (PSU)",
  temp_mean_0_50   = "Mean Temperature 0-50m (C)"
)

colors <- c("Pre Heat Wave" = "#2196F3", "Post Heat Wave" = "#F44336")

# ── Summary stats table ───────────────────────────────────────────────────────
stats <- df %>%
  pivot_longer(all_of(pred_vars), names_to = "variable", values_to = "value") %>%
  group_by(variable, time_frame) %>%
  summarise(
    mean   = round(mean(value,   na.rm = TRUE), 3),
    median = round(median(value, na.rm = TRUE), 3),
    sd     = round(sd(value,     na.rm = TRUE), 3),
    min    = round(min(value,    na.rm = TRUE), 3),
    max    = round(max(value,    na.rm = TRUE), 3),
    .groups = "drop"
  )

cat("=== Summary Statistics ===\n")
print(as.data.frame(stats), row.names = FALSE)

# Statistical tests (Wilcoxon, non-parametric)
cat("\n=== Wilcoxon Rank-Sum Tests (Pre vs Post) ===\n")
for (v in pred_vars) {
  pre_vals  <- df[df$time_frame == "Pre Heat Wave",  v]
  post_vals <- df[df$time_frame == "Post Heat Wave", v]
  wt <- wilcox.test(pre_vals, post_vals)
  delta_mean <- mean(post_vals, na.rm=TRUE) - mean(pre_vals, na.rm=TRUE)
  delta_pct  <- round(delta_mean / abs(mean(pre_vals, na.rm=TRUE)) * 100, 2)
  cat(sprintf("%-25s  W = %-10.0f  p = %-10.4f  delta_mean = %+.4f  (%+.2f%%)\n",
              v, wt$statistic, wt$p.value, delta_mean, delta_pct))
}


# ── Plot 1: Density plots (one per variable) ──────────────────────────────────
p_density <- lapply(pred_vars, function(v) {
  ggplot(df, aes(x = .data[[v]], fill = time_frame, color = time_frame)) +
    geom_density(alpha = 0.35, linewidth = 0.7) +
    geom_vline(data = df %>% group_by(time_frame) %>%
                 summarise(m = mean(.data[[v]], na.rm=TRUE), .groups="drop"),
               aes(xintercept = m, color = time_frame),
               linetype = "dashed", linewidth = 0.7) +
    scale_fill_manual(values = colors)  +
    scale_color_manual(values = colors) +
    labs(title = var_labels[v], x = NULL, y = "Density",
         fill = NULL, color = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position  = "bottom",
          panel.grid.minor = element_blank(),
          plot.title       = element_text(size = 10, face = "bold"))
})

fig1 <- wrap_plots(p_density, ncol = 2) +
  plot_annotation(
    title    = "Distribution Comparison: Pre vs Post Heat Wave",
    subtitle = "Dashed lines show group means",
    theme    = theme(plot.title    = element_text(size = 14, face = "bold"),
                     plot.subtitle = element_text(size = 10, color = "grey40"))
  )

ggsave("plot1_densities.png", fig1, width = 12, height = 12, dpi = 150)
cat("\nSaved: plot1_densities.png\n")


# ── Plot 2: Boxplots with jittered points (subsampled) ───────────────────────
df_long <- df %>%
  pivot_longer(all_of(pred_vars), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = pred_vars,
                           labels = var_labels[pred_vars]))

set.seed(42)
df_jitter <- df_long %>% sample_frac(0.08)  # 8% sample to avoid overplotting

fig2 <- ggplot(df_long, aes(x = time_frame, y = value, fill = time_frame)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, linewidth = 0.6, width = 0.5) +
  geom_jitter(data = df_jitter,
              aes(color = time_frame), alpha = 0.25, size = 0.6, width = 0.2) +
  scale_fill_manual(values = colors,  guide = "none") +
  scale_color_manual(values = colors, guide = "none") +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(title    = "Boxplots: Pre vs Post Heat Wave",
       subtitle = "Jittered points show 8% random sample",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor  = element_blank(),
        strip.text        = element_text(size = 9, face = "bold"),
        axis.text.x       = element_text(size = 9),
        plot.title        = element_text(size = 14, face = "bold"),
        plot.subtitle     = element_text(size = 10, color = "grey40"))

ggsave("plot2_boxplots.png", fig2, width = 11, height = 11, dpi = 150)
cat("Saved: plot2_boxplots.png\n")


# ── Plot 3: Difference map — spatial distribution of Post - Pre ──────────────
pre  <- df %>% filter(time_frame == "Pre Heat Wave")
post <- df %>% filter(time_frame == "Post Heat Wave")

diff_df <- pre %>%
  select(cell_coord_id, lat, lon) %>%
  bind_cols(
    setNames(
      lapply(pred_vars, function(v) {
        post[[v]][match(pre$cell_coord_id, post$cell_coord_id)] - pre[[v]]
      }),
      pred_vars
    )
  )

p_diff <- lapply(pred_vars, function(v) {
  lim <- max(abs(diff_df[[v]]), na.rm = TRUE)
  ggplot(diff_df, aes(x = lon, y = lat, color = .data[[v]])) +
    geom_point(size = 0.4, alpha = 0.7) +
    scale_color_gradient2(low  = "#2166ac", mid = "white", high = "#b2182b",
                          midpoint = 0, limits = c(-lim, lim),
                          name = "Post - Pre") +
    coord_fixed(ratio = 1.2) +
    labs(title = var_labels[v], x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(size = 9, face = "bold"),
          legend.key.height = unit(0.5, "cm"))
})

fig3 <- wrap_plots(p_diff, ncol = 2) +
  plot_annotation(
    title    = "Spatial Difference: Post minus Pre Heat Wave",
    subtitle = "Red = increased post-HW   |   Blue = decreased post-HW",
    theme    = theme(plot.title    = element_text(size = 14, face = "bold"),
                     plot.subtitle = element_text(size = 10, color = "grey40"))
  )

ggsave("plot3_difference_maps.png", fig3, width = 12, height = 12, dpi = 150)
cat("Saved: plot3_difference_maps.png\n")


# ── Plot 4: Cumulative distribution functions (ECDF) ─────────────────────────
p_ecdf <- lapply(pred_vars, function(v) {
  ggplot(df, aes(x = .data[[v]], color = time_frame)) +
    stat_ecdf(linewidth = 0.8) +
    scale_color_manual(values = colors) +
    labs(title = var_labels[v], x = NULL, y = "Cumulative Probability",
         color = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position  = "bottom",
          panel.grid.minor = element_blank(),
          plot.title       = element_text(size = 10, face = "bold"))
})

fig4 <- wrap_plots(p_ecdf, ncol = 2) +
  plot_annotation(
    title    = "Empirical Cumulative Distributions: Pre vs Post Heat Wave",
    subtitle = "Horizontal separation indicates distributional shift",
    theme    = theme(plot.title    = element_text(size = 14, face = "bold"),
                     plot.subtitle = element_text(size = 10, color = "grey40"))
  )

ggsave("plot4_ecdf.png", fig4, width = 12, height = 12, dpi = 150)
cat("Saved: plot4_ecdf.png\n")


# ── Plot 5: Delta summary bar chart ──────────────────────────────────────────
delta_summary <- data.frame(
  variable  = pred_vars,
  label     = var_labels[pred_vars],
  pct_delta = sapply(pred_vars, function(v) {
    pre_mn  <- mean(pre[[v]], na.rm = TRUE)
    post_mn <- mean(post[[v]], na.rm = TRUE)
    (post_mn - pre_mn) / abs(pre_mn) * 100
  })
) %>%
  mutate(direction = ifelse(pct_delta > 0, "Increase", "Decrease"),
         label     = factor(label, levels = label[order(pct_delta)]))

fig5 <- ggplot(delta_summary, aes(x = pct_delta, y = label, fill = direction)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  geom_text(aes(label = paste0(round(pct_delta, 2), "%"),
                hjust = ifelse(pct_delta >= 0, -0.1, 1.1)),
            size = 3.5) +
  scale_fill_manual(values = c("Increase" = "#F44336", "Decrease" = "#2196F3"),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  labs(title    = "Mean % Change: Post minus Pre Heat Wave",
       subtitle = "Positive = higher post-HW   |   Negative = lower post-HW",
       x = "% change in mean", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor    = element_blank(),
        panel.grid.major.y  = element_blank(),
        plot.title          = element_text(size = 14, face = "bold"),
        plot.subtitle       = element_text(size = 10, color = "grey40"))

ggsave("plot5_delta_summary.png", fig5, width = 10, height = 5, dpi = 150)
cat("Saved: plot5_delta_summary.png\n")

cat("\nDone. 5 plots saved.\n")

