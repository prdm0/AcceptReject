simulation <- function(parallel = FALSE){
  time <- system.time(
    x <- accept_reject(
      n = 2e6L,
      f = dnorm,
      continuous = TRUE,
      args_f = list(mean = 0, sd = 1),
      xlim = c(-4, 4),
      parallel = FALSE
   ))
  list(time = time, x = x)
}

# Serial
cat(simulation(parallel = FALSE))

# Parallel
cat(simulation(parallel = TRUE))
