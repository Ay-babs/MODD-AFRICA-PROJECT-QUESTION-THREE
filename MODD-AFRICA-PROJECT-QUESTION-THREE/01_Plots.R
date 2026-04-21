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
library(metR)
bw <- 0.05
ggplot(df,
       aes(y = h,
           x = T)) +
  #facet_wrap(~species) +
  geom_contour_filled(
    aes(z = surv_vals),
    binwidth = bw
  ) +
  geom_contour(
    aes(z = surv_vals),
    binwidth = bw,
    colour = grey(0.2),
    linewidth = 0.5
  ) +
  geom_text_contour(
    aes(z = surv_vals),
    binwidth = bw,
    nudge_y = -5,
    skip = 0
  )
