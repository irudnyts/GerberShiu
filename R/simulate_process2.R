simulate_process2 <- function(u = 10,
                              pr = 1,
                              lambda_p = 1,
                              f_p = rexp,
                              param_p = list(rate = 1),
                              lambda_n = 1,
                              f_n1 = rexp,
                              param_n1 = list(rate = 1 / 2),
                              f_n2 = actuar::rpareto1,
                              param_n2 = list(shape = 3 / 2, min = 2 / 3),
                              eps = 0.1,
                              max_time_span = 7500) {

    number_p_jumps <- rpois(n = 1, lambda = lambda_p * max_time_span)
    number_n_jumps <- rpois(n = 1, lambda = lambda_n * max_time_span)

    time_p <- sort(runif(n = number_p_jumps)) * max_time_span
    time_n <- sort(runif(n = number_n_jumps)) * max_time_span

    jumps_p <- rexp(n = number_p_jumps, rate = 1)
    jumps_n <- rexp(n = number_n_jumps, rate = 1)

    rval <- list()

    rval[["time_p"]] <- time_p
    rval[["time_n"]] <- time_n

    rval[["jumps_p"]] <- jumps_p
    rval[["jumps_n"]] <- jumps_n

    path <- matrix(data = c(0, u), nrow = 1, ncol = 2)


    while(length(time_p) != 0 | length(time_n) != 0) {

        if(length(time_p) != 0 & length(time_n) != 0) {
            if(time_p[1] < time_n[1]) {

                path <- rbind(
                    path,
                    c(time_p[1],
                      path[nrow(path), 2] + (time_p[1] - path[nrow(path), 1]) * pr)
                )

                path <- rbind(
                    path,
                    c(path[nrow(path), 1],
                      path[nrow(path), 2] + jumps_p[1])
                )

                time_p <- time_p[-1]
                jumps_p <- jumps_p[-1]

            } else {

                path <- rbind(
                    path,
                    c(time_n[1],
                      path[nrow(path), 2] + (time_n[1] - path[nrow(path), 1]) * pr)
                )

                path <- rbind(
                    path,
                    c(path[nrow(path), 1],
                      path[nrow(path), 2] - jumps_n[1])
                )

                time_n <- time_n[-1]
                jumps_n <- jumps_n[-1]

            }
        } else {
            if(length(time_p) != 0) {

                path <- rbind(
                    path,
                    c(time_p[1],
                      path[nrow(path), 2] + (time_p[1] - path[nrow(path), 1]) * pr)
                )

                path <- rbind(
                    path,
                    c(path[nrow(path), 1],
                      path[nrow(path), 2] + jumps_p[1])
                )

                time_p <- time_p[-1]
                jumps_p <- jumps_p[-1]

            }
            if(length(time_n) != 0) {

                path <- rbind(
                    path,
                    c(time_n[1],
                      path[nrow(path), 2] + (time_n[1] - path[nrow(path), 1]) * pr)
                )

                path <- rbind(
                    path,
                    c(path[nrow(path), 1],
                      path[nrow(path), 2] - jumps_n[1])
                )

                time_n <- time_n[-1]
                jumps_n <- jumps_n[-1]

            }
        }
    }

    # add last step
    path <- rbind(
        path,
        c(max_time_span,
          path[nrow(path), 2] + (max_time_span - path[nrow(path), 1]) * pr)
    )

    rval[["path"]] <- path

    return(rval)

}
