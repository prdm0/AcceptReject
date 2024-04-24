simulation <- function(n, parallel = FALSE){
  start_time <- Sys.time()
  x <- accept_reject(
    n = n,
    f = dnorm,
    continuous = TRUE,
    args_f = list(mean = 0, sd = 1),
    xlim = c(-4, 4),
    parallel = parallel
  )

  end_time <- Sys.time()
  execution_time <- end_time - start_time

  cat("Time: ", execution_time, " seconds\n")

  cat("First observations:\n")
  print(head(x))

  return(x)
}
simulation(n = 1000, parallel = FALSE)
