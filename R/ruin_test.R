# See Table 2 in Breur and Andrei 2011
# Should be the same

simulation_number <- 1000
set.seed(1)
processes <- pbapply::pbreplicate(
    n = simulation_number,
    expr = simulate_process(u = 10, pr = 0, lambda_p = 1, f_p = rexp,
                            param_p = list(rate = 1),
                            lambda_n = 1, f_n1 = rexp,
                            param_n1 = list(rate = 1),
                            eps = 0, max_jumps_number = 10000),
    simplify = FALSE)

rp <- function(prc, t) {
    ifelse(
        test = prc$is_ruined,
        yes = ifelse(test = prc$time_to_ruin < t, yes = 1, no = 0),
        no = ifelse(test = prc$path[nrow(prc$path) > t, 1], yes = 0, no = NA)
           )
}

ruined <- sapply(processes, rp, t = 10)
mean(ruined, na.rm = TRUE)


simulation_number <- 10000
set.seed(1)
processes <- pbapply::pbreplicate(
    n = simulation_number,
    expr = simulate_process(u = 0, pr = 0, lambda_p = 1, f_p = rexp,
                            param_p = list(rate = 1),
                            lambda_n = 1, f_n1 = rexp,
                            param_n1 = list(rate = 1),
                            eps = 0, max_jumps_number = 10000),
    simplify = FALSE)

rp <- function(prc, t) {
    ifelse(
        test = prc$is_ruined,
        yes = ifelse(test = prc$time_to_ruin < t, yes = 1, no = 0),
        no = ifelse(test = prc$path[nrow(prc$path) > t, 1], yes = 0, no = NA)
    )
}

ruined <- sapply(processes, rp, t = 10)
mean(ruined, na.rm = TRUE)
