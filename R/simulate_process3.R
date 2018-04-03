simulate_process3 <- function(u = 10,
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
                              max_jumps_number = 10000,
                              max_time_span = 7500) {
    # add n = 1 to all distributions parameters in order to generate only
    # one r.v.
    param_p[["n"]] <- 1
    param_n1[["n"]] <- 1
    param_n2[["n"]] <- 1

    # initialize process
    path <- matrix(NA, nrow = 1, ncol = 2)
    colnames(path) <- c("time", "X")
    path[1, ] <- c(0, u)

    add_jump_to_path <- function(path, arrival, size) {

        path <- rbind(
            path,
            c(arrival,
              path[nrow(path), 2] + (arrival - path[nrow(path), 1]) * pr)
        )

        path <- rbind(
            path,
            c(arrival,
              path[nrow(path), 2] + size)
        )
        path
    }

    s_pos <- numeric() # positive jumps' sizes
    s_neg <- numeric() # positive jumps' sizes

    a_pos <- numeric() # arrival times of positive jumps
    a_neg <- numeric() # arrival times of negative jumps

    ca_pos <- rexp(1, lambda_p) # current arrival of a positive jump
    ca_neg <- rexp(1, lambda_n) # current arrival of a negative jump

    repeat{

        if(ca_pos < max_time |
           ca_neg < max_time) {

            if(ca_pos > ca_neg) {

                cs_neg <- do.call(f_n, param_n) # current negative jump's size

                path <- add_jump_to_path(path, ca_neg, -cs_neg)

                s_neg <- c(s_neg, cs_neg)
                a_neg <- c(a_neg, ca_neg)

                if(path[nrow(path), 2] < 0) break

                ca_neg <- ca_neg + rexp(1, lambda_n)


            } else if(ca_pos == ca_neg) {

                cs_pos <- do.call(f_p, param_p) # current positive jump's size
                cs_neg <- do.call(f_n, param_n) # current negative jump's size

                path <- add_jump_to_path(path, ca_pos, cs_pos - cs_neg)

                s_pos <- c(s_pos, cs_pos)
                s_neg <- c(s_neg, cs_neg)

                a_pos <- c(a_pos, ca_pos)
                a_neg <- c(a_neg, ca_neg)

                ca_pos <- ca_pos + rexp(1, lambda_p)
                ca_neg <- ca_neg + rexp(1, lambda_n)

            } else if(ca_pos < ca_neg) {

                cs_pos <- do.call(f_p, param_p) # current positive jump's size

                path <- add_jump_to_path(path, ca_pos, cs_pos)
                s_pos <- c(s_pos, cs_pos)
                a_pos <- c(a_pos, ca_pos)

                ca_pos <- ca_pos + rexp(1, lambda_p)

            }

        } else {

            break

        }

    }

}
