set.seed(1)
prc <- simulate_process(initial_capital = 10,
                        premium_rate = 1,
                        capital_injections_rate = 1,
                        claims_rate = 1)

prc <- simulate_process(initial_capital = 10,
                        premium_rate = 1,
                        capital_injections_rate = 1,
                        claims_rate = 1)

plot(sapply(1:length(n_time), function(x) mean(diff(n_time[1:x]))), type = "l")
length(n_time) - length(p_time)


set.seed(1)
prc <- simulate_process(u = 7, premiums = 1, lambda1 = 1, lambda2 = 2)
plot(prc, type = "l")
plot(prc[1:100, ], type = "l")

prc <- simulate_process(u = 7, premiums = 1, lambda1 = 1, lambda2 = 1.3)
# fast matrix append without creating new matrix
# a <- matrix(1:4, ncol = 2, byrow = TRUE)
# a <- t(a)
# a[c(5, 6)] <- c(5, 6)
# matrix(a, ncol = 2, byrow = TRUE)

# how break works
# i <- 1
# while(i < 10) {
#     j <- 1
#     repeat {
#         j <- j + 1
#         if(j > 7) break
#     }
#     i <- i + 1
# }




# add_njump <- function(jump_time, jump_value) {
#     process[nrow(process) + 1, 1] <- process[nrow(process), 1] + jump_time
#     process[nrow(process), 2] <- process[nrow(process) - 1, 2] + jump_time * premiums
#     process[nrow(process) + 1, 1] <- process[nrow(process), 1]
#     process[nrow(process), 2] <- process[nrow(process) - 1, 2] - jump_time
# }
#
# add_pjump <- function(jump_time, jump_value) {
#     process[nrow(process) + 1, 1] <- process[nrow(process), 1] + jump_time
#     process[nrow(process), 2] <- process[nrow(process) - 1, 2] + jump_time * premiums
#     process[nrow(process) + 1, 1] <- process[nrow(process), 1]
#     process[nrow(process), 2] <- process[nrow(process) - 1, 2] + jump_time
#
# }















# simulate_process_until_ruin <- function(u, premiums, lambda1, lambda2,
#                                         max_iterations, max_time) {
#
#     browser()
#
#     process <- matrix(NA, nrow = 1, ncol = 2)
#     colnames(process) <- c("time", "X")
#     process[1, ] <- c(0, u)
#
#     add_njump <- function(jump_time, jump_value) {
#         time_to_jump <- jump_time - process[nrow(process), 1]
#         process <<- rbind(
#             process,
#             c(process[nrow(process), 1] + jump_time,
#               process[nrow(process), 2] + time_to_jump * premiums)
#         )
#
#         process <<- rbind(
#             process,
#             c(process[nrow(process), 1],
#               process[nrow(process), 2] - jump_value)
#         )
#     }
#
#     add_pjump <- function(jump_time, jump_value) {
#         time_to_jump <- jump_time - process[nrow(process), 1]
#         process <<- rbind(
#             process,
#             c(process[nrow(process), 1] + jump_time,
#               process[nrow(process), 2] + time_to_jump * premiums)
#         )
#
#         process <<- rbind(
#             process,
#             c(process[nrow(process), 1],
#               process[nrow(process), 2] + jump_value)
#         )
#     }
#
#     get_process_last <- function() process[nrow(process), 2]
#
#     last <- function(x) x[length(x)]
#
#     p_time <- numeric(); p_time[1] <- 0 # time of positive jumps
#     n_time <- numeric(); n_time[1] <- 0 # time of negative jumps
#
#     # iterator
#     # i <- 0
#
#     p_time <-  c(p_time, last(p_time) + rexp(1, lambda1))
#     n_time <-  c(n_time, last(n_time) + rexp(1, lambda2))
#
#     repeat{
#
#         if(last(p_time) > last(n_time)) {
#
#             add_njump(last(n_time), 1)
#             if(get_process_last() < 0) break
#
#             while(last(p_time) > last(n_time)) {
#                 n_time <-  c(n_time, last(n_time) + rexp(1, lambda2))
#                 add_njump(last(n_time), 1)
#                 if(get_process_last() < 0) break
#             }
#
#             add_pjump(last(p_time), 1)
#
#         } else {
#
#             add_pjump(last(p_time), 1)
#
#             while(last(p_time) < last(n_time)) {
#                 p_time <-  c(p_time, last(p_time) + rexp(1, lambda1))
#                 add_pjump(last(p_time), 1)
#             }
#
#             add_njump(last(n_time), 1)
#             if(get_process_last() < 0) break
#         }
#     }
#     return(process)
# }
