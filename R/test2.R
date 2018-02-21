# # # test2
# #
# # set.seed(1000)
# # prc <- simulate_process()
# # # par(mfrow = c(1, 2))
# # # plot(sapply(1:length(prc$time_p), function(x) mean(diff(prc$time_p[1:x]))), type = "l"); abline(h = 1, col = 2)
# # # plot(sapply(1:length(prc$time_n), function(x) mean(diff(prc$time_n[1:x]))), type = "l"); abline(h = 1, col = 2)
# #
# # plot(prc$process, type = "l")
# #
# # plot(prc$process[1:100, ], type = "l")
# #
# # mean(prc$jumps_p)
# # mean(prc$jumps_n)
# #
# # mean(diff(prc$time_p))
# # mean(diff(prc$time_n))
# #
# # var(diff(prc$time_p))
# # var(diff(prc$time_n))
# #
# #
# # plot(sapply(1:length(prc$time_p), function(x) mean(diff(prc$time_p[1:x]))), type = "l")
# # plot(sapply(1:length(prc$time_n), function(x) mean(diff(prc$time_n[1:x]))), type = "l")
# #
# #
# # library(magrittr)
# # mean(prc$jumps_p)
# # mean(prc$jumps_n)
# # sapply(1:length(prc$jumps_p), function(x) mean(prc$jumps_p[1:x])) %>% plot(type = "l")
# # sapply(1:length(prc$jumps_n), function(x) mean(prc$jumps_n[1:x])) %>% plot(type = "l")
#
#
# set.seed(1000)
# prc <- simulate_process()
# par(mfrow = c(1, 1))
# plot(prc$path[1:100, ], type = "l")
# nrow(prc$path)
# plot(prc$path[53950:54089, ], type = "l")
# plot(prc$path, type = "l")
# par(mfrow = c(1, 2))
# plot(sapply(1:length(prc$time_p), function(x) mean(diff(prc$time_p[1:x]))), type = "l"); abline(h = 1, col = 2)
# plot(sapply(1:length(prc$time_n), function(x) mean(diff(prc$time_n[1:x]))), type = "l"); abline(h = 1, col = 2)
#
#
# set.seed(1)
# b <- simulate_process()
# system.time(a <- simulate_process(max_jumps_number = 100))
#
# set.seed(1)
# b <- simulate_process()
# system.time(a <- simulate_process(max_jumps_number = 1000))
#
# set.seed(1)
# b <- simulate_process()
# system.time(a <- simulate_process(max_jumps_number = 10000))
#
# set.seed(1)
# b <- simulate_process()
# system.time(a <- simulate_process(max_jumps_number = 100000))


