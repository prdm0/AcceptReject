library(tictoc)

tic()
x <-
  accept_reject(
    n = 2e6L,
    f = dnorm,
    continuous = TRUE,
    args_f = list(mean = 0, sd = 1),
    xlim = c(-4, 4),
    parallel = FALSE
  )
toc()

print(x)


