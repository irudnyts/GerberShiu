# test2

set.seed(10)
prc <- simulate_process(lambda_p = 2, lambda_n = 0.5, eps = 0.2)
plot(prc$process, type = "l")

plot(prc$process[1:100, ], type = "l")

mean(prc$jumps_p)
mean(prc$jumps_n)

mean(diff(prc$time_p))
mean(diff(prc$time_n))

var(diff(prc$time_p))
var(diff(prc$time_n))


plot(sapply(1:length(prc$time_p), function(x) mean(diff(prc$time_p[1:x]))), type = "l")
plot(sapply(1:length(prc$time_n), function(x) mean(diff(prc$time_n[1:x]))), type = "l")
