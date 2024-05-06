# Example 1 ---------------------------------------------------------------
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

inspect(
  f = dnorm,
  args_f = list(mean = 0, sd = 1),
  f_base = dt,
  args_f_base = list(df = 30),
  xlim = c(-4, 4),
  c = 1.0167
)

# Example 2 ---------------------------------------------------------------
x <- accept_reject(
  n = 100000L,
  f = dnorm,
  continuous = TRUE,
  args_f = list(mean = 0, sd = 1),
  f_base = dt,
  args_f_base = list(df = 30),
  random_base = rt,
  xlim = c(-4, 4)
)
print(x)
plot(x)

# Example 3 ---------------------------------------------------------------

x <- accept_reject(
  n = 1000L,
  f = dnorm,
  continuous = TRUE,
  args_f = list(mean = 0, sd = 1),
  xlim = c(-4, 4)
)
print(x)
plot(x)

# Example 4 ---------------------------------------------------------------
x <- accept_reject(
  n = 100000L,
  f = dbeta,
  continuous = TRUE,
  args_f = list(shape1 = 2, shape2 = 2),
  xlim = c(0, 1)
)
print(x)
plot(x)

# Example 5 ---------------------------------------------------------------
inspect(
  f = dbeta,
  args_f = list(shape1 = 2, shape2 = 2),
  xlim = c(0, 1),
  f_base = dunif,
  args_f_base = list(min = 0, max = 1),
  c = 1.5
)

x <- accept_reject(
  n = 100000L,
  f = dbeta,
  continuous = TRUE,
  args_f = list(shape1 = 0.5, shape2 = 0.5),
  xlim = c(0, 1)
)
print(x)
plot(x)
