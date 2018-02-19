# test2

set.seed(1)
prc <- simulate_process(lambda_p = 2, lambda_n = 0.5, eps = 0.3)
plot(prc$process, type = "l")

plot(prc$process[1:100, ], type = "l")

mean(prc$jumps_p)
mean(prc$jumps_n)

mean(diff(prc$time_p))
mean(diff(prc$time_n))

var(diff(prc$time_p))
var(diff(prc$time_n))
