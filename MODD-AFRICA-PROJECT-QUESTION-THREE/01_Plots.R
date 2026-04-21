library(ggplot2)
ggplot(df, aes(x = T, y = d)) +
  geom_line(size = 1) +
  labs(title = "Martens (1997) death Function",
       x = "Temperature (°C)",
       y = "death rate") +
  ylim(c(0,1))+
  theme_minimal()

ggplot(df, aes(x = T, y = p)) +
  geom_line(size = 1) +
  labs(title = "Martens (1997) Survival Function",
       x = "Temperature (°C)",
       y = "survival probability") +
  theme_minimal()

ggplot(df, aes(x = h, y = Hum_eff_vals)) +
  geom_line(size = 1) +
  labs(title = "Humidity eff Function",
       x = "Humidity (%)",
       y = "Humidity effect") +
  theme_minimal()



# # 3. Plot the 3D surface
# surv_vals = survival_fraction(T_vals, h_vals, h_min, h_max)
# surv_vals[surv_vals<0] <-0
# persp(T_vals, h_vals, surv_vals, theta = 30, phi = 30, expand = 0.5, col = "lightblue", shade = 0.5)
# df$surv_vals[df$surv_vals==0] <- 0.00000001
library(ggplot2)
library(metR)

bw <- 0.05
label_levels <- c(0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

ggplot(df, aes(x = T, y = h)) +
  
  # Filled contours
  geom_contour_filled(
    aes(z = surv_vals),
    binwidth = bw
  ) +
  
  # Fine background contour lines
  geom_contour(
    aes(z = surv_vals),
    binwidth  = bw,
    colour    = "white",
    linewidth = 0.2,
    alpha     = 0.35
  ) +
  
  # Bold lines at key levels only
  geom_contour(
    aes(z = surv_vals),
    breaks    = label_levels,
    colour    = "white",
    linewidth = 1.0,
    alpha     = 0.9
  ) +
  
  # Labels placed in right-hand portion of plot only
  geom_text_contour(
    aes(z = surv_vals),
    breaks        = label_levels,
    colour        = "white",
    size          = 4.0,
    fontface      = "bold",
    stroke        = 0.3,
    stroke.colour = "black",
    skip          = 0,
    rotate        = FALSE,
    xlim          = c(33, 39)   # pin labels to right side
  ) +
  
  # Mako palette: dark purple (low) → bright cyan/white (high)
  # Much easier to read than viridis yellow dominance
  scale_fill_viridis_d(
    option    = "mako",
    direction = 1
  ) +
  
  scale_x_continuous(
    name   = "Temperature (°C)",
    breaks = seq(0, 45, by = 5),
    limits = c(0, 45),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name   = "Relative Humidity (%)",
    breaks = seq(0, 100, by = 20),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  
  # Shaded out-of-range zones
  annotate("rect",
           xmin = 0,  xmax = 5,
           ymin = 0,  ymax = 100,
           fill = "black", alpha = 0.45) +
  annotate("rect",
           xmin = 40, xmax = 45,
           ymin = 0,  ymax = 100,
           fill = "black", alpha = 0.45) +
  
  # Thermal limit dashed lines
  annotate("segment",
           x = 5, xend = 5, y = 0, yend = 100,
           linetype  = "dashed", colour = "grey90",
           linewidth = 0.8) +
  annotate("segment",
           x = 40, xend = 40, y = 0, yend = 100,
           linetype  = "dashed", colour = "grey90",
           linewidth = 0.8) +
  
  # Thermal limit labels — placed INSIDE the shaded zones
  annotate("label",
           x = 2.5, y = 50,
           label    = "T < T[min]",
           parse    = TRUE,
           fill     = alpha("black", 0.5),
           colour   = "white",
           size     = 3.2,
           fontface = "italic",
           label.size = 0) +
  annotate("label",
           x = 42.5, y = 50,
           label    = "T > T[max]",
           parse    = TRUE,
           fill     = alpha("black", 0.5),
           colour   = "white",
           size     = 3.2,
           fontface = "italic",
           label.size = 0) +
  
  # Thermal min/max tick labels on x axis
  annotate("text",
           x = 5, y = -6,
           label    = "T[min]~(5~degree*C)",
           parse    = TRUE,
           colour   = "grey30",
           size     = 3.0,
           hjust    = 0.5,
           fontface = "italic") +
  annotate("text",
           x = 40, y = -6,
           label    = "T[max]~(40~degree*C)",
           parse    = TRUE,
           colour   = "grey30",
           size     = 3.0,
           hjust    = 0.5,
           fontface = "italic") +
  
  # Survival = 1 region label inside the high-survival zone
  annotate("label",
           x = 20, y = 75,
           label    = "Survival > 0.95",
           fill     = alpha("black", 0.25),
           colour   = "white",
           size     = 3.8,
           fontface = "bold.italic",
           label.size = 0) +
  
  labs(
    title   = "Survival Probability as a Function of Temperature and Humidity",
    caption = paste0(
      "Filled colours show survival probability (0 = no survival, 1 = full survival).\n",
      "Bold contour lines and labels mark key thresholds (0.10, 0.25, 0.50, 0.75, 0.90, 0.95).\n",
      "Shaded regions outside dashed lines indicate temperatures beyond thermal limits."
    )
  ) +
  
  guides(fill = "none") +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0,
                                    margin = margin(b = 8)),
    plot.caption     = element_text(size = 8.5, colour = "grey45",
                                    hjust = 0, lineheight = 1.4,
                                    margin = margin(t = 10)),
    axis.title.x     = element_text(face = "bold", size = 12,
                                    margin = margin(t = 18)),  # room for Tmin/Tmax labels
    axis.title.y     = element_text(face = "bold", size = 12),
    axis.text        = element_text(size = 10),
    panel.grid.major = element_line(colour = "grey70", linewidth = 0.2,
                                    linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(12, 15, 20, 10)
  )

# Save at high resolution
ggsave("survival_contour.png",
       width = 10, height = 6.5, dpi = 300, bg = "white")
