# Define the survival function
# Create a temperature sequence
T_vals <- seq(0, 45, by = 1)
h_vals <- seq(0, 100, by = 1)
h_min <- 5
h_max <- 42

# Death rate function
death_safe <- function(T) {
  denom <- -4.4 + 1.31*T - 0.03*T^2
  
  denom_safe <-ifelse(denom <= 0, 0.000001, denom)
  return( 1/denom_safe)
}

# Survival probability function depending on T
survival_safe <- function(T) {
  denom <- -4.4 + 1.31*T - 0.03*T^2
  denom_safe <-ifelse(denom <= 0, 0.000001, denom)
  return( exp(-1/denom_safe))
}

# Humidity function
S_h <- function(h, h_min, h_max) {
  ifelse(h <= h_min, 0,
         ifelse(h >= h_max, 1,
                (h - h_min) / (h_max - h_min)))
}

#  Final surviving fraction depending on T and H ---
# includes 1/12 scaling (2-hour intervals)
survival_fraction <- function(T, h, h_min, h_max) {
  exp(-death_safe(T) / 12) * S_h(h, h_min, h_max)
}

df <- expand.grid(
  T = T_vals,
  h = h_vals
)
df$Hum_eff_vals <- S_h(df$h, h_min, h_max)
df$p = survival_safe(df$T)
df$d = death_safe(df$T)
df$surv_vals = survival_fraction(df$T, df$h, h_min, h_max)
