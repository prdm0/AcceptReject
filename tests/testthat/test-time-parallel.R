simulation <- function(n, parallel = FALSE){
  # Iniciar o tempo
  start_time <- Sys.time()

  # Executar a função accept_reject
  x <- accept_reject(
    n = n,
    f = dnorm,
    continuous = TRUE,
    args_f = list(mean = 0, sd = 1),
    xlim = c(-4, 4),
    parallel = parallel
  )

  # Calcular o tempo de execução
  end_time <- Sys.time()
  execution_time <- end_time - start_time

  # Imprimir o tempo de execução
  cat("Tempo de execução: ", execution_time, " segundos\n")

  # Imprimir as primeiras observações
  cat("Primeiras observações:\n")
  print(head(x))

  # Retornar o resultado
  return(x)
}
simulation(n = 1000, parallel = FALSE)

simulation(n = 1000, parallel = TRUE)
