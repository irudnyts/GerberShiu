# test2

set.seed(1)
prc <- simulate_process(lambda_p = 0.5, lambda_n = 1, eps = 0.3)
plot(prc$process, type = "l")
