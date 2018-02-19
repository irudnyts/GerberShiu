# max_jumps_number can be Inf
simulate_process <- function(u = 10,
                             pr = 1,

                             lambda_p = 1,
                             f_p = rexp,
                             param_p = list(rate = 1),

                             lambda_n = 1,
                             f_n1 = rexp,
                             param_n1 = list(rate = 1),
                             f_n2 = rpareto,
                             param_n2 = list(shape = 1, scale = 1),
                             eps = 0.1,

                             max_jumps_number = 10000) {

    # initialize iterator variable
    jumps_number <- 0

    # initialize process
    process <- matrix(NA, nrow = 1, ncol = 2)
    colnames(process) <- c("time", "X")
    process[1, ] <- c(0, u)

    # function for adding negative jump to a process
    add_jump_n <- function(jump_time, jump_value) {
        time_to_jump <- jump_time - process[nrow(process), 1]
        process <<- rbind(
            process,
            c(jump_time,
              process[nrow(process), 2] + time_to_jump * pr)
        )

        process <<- rbind(
            process,
            c(process[nrow(process), 1],
              process[nrow(process), 2] - jump_value)
        )

        jumps_number <<- jumps_number + 1
    }

    # function for adding positive jump to a process
    add_jump_p <- function(jump_time, jump_value) {
        time_to_jump <- jump_time - process[nrow(process), 1]
        process <<- rbind(
            process,
            c(jump_time,
              process[nrow(process), 2] + time_to_jump * pr)
        )

        process <<- rbind(
            process,
            c(process[nrow(process), 1],
              process[nrow(process), 2] + jump_value)
        )

        jumps_number <<- jumps_number + 1
    }

    # get last value of a process
    get_process_last_value <- function() process[nrow(process), 2]

    # add n = 1 to all distribution parameters in order to generate only one r.v.
    param_p[["n"]] <- 1
    param_n1[["n"]] <- 1
    param_n2[["n"]] <- 1

    # function to generate positive jump
    jump_size_p <- function() do.call(f_p, param_p)

    # function to generate negative jump
    jump_size_n <- function() {
        ifelse(test = rbinom(n = 1, size = 1, prob = eps) == 1,
               yes = do.call(f_n2, param_n2),
               no = do.call(f_n1, param_n1))

    }

    # get last element of a vector
    last <- function(x) x[length(x)]

    jumps_n <- numeric() # negative jumps' sizes
    jumps_p <- numeric() # positive jumps' sizes

    time_n <- numeric() # time of negative jumps
    time_p <- numeric() # time of positive jumps

    time_n[1] <- rexp(1, lambda_n)
    time_p[1] <- rexp(1, lambda_p)

    repeat{

        if(last(time_p) > last(time_n)) {

            jumps_n <-  c(jumps_n, jump_size_n())
            add_jump_n(last(time_n), last(jumps_n))

            if(get_process_last_value() < 0) break
            if(jumps_number > max_jumps_number) break

            repeat {

                time_n <- c(time_n, last(time_n) + rexp(1, lambda_n))
                if(last(time_p) < last(time_n)) break

                jumps_n <-  c(jumps_n, jump_size_n())
                add_jump_n(last(time_n), last(jumps_n))

                if(get_process_last_value() < 0) break

                if(jumps_number > max_jumps_number) break
            }

            if(get_process_last_value() < 0) break
            if(jumps_number > max_jumps_number) break


        } else {

            jumps_p <-  c(jumps_p, jump_size_p())
            add_jump_p(last(time_p), last(jumps_p))

            if(jumps_number > max_jumps_number) break

            repeat {
                time_p <-  c(time_p, last(time_p) + rexp(1, lambda_p))
                if(last(time_p) > last(time_n)) break
                jumps_p <-  c(jumps_p, jump_size_p())
                add_jump_p(last(time_p), last(jumps_p))
                if(jumps_number > max_jumps_number) break
            }

            if(jumps_number > max_jumps_number) break

        }
    }
    return(list(
        process = process,
        jumps_p = jumps_p,
        time_p = time_p,
        jumps_n = jumps_n,
        time_n = time_n
    ))
}


# process <- function(process, positive_jumps_time, positive_jumps_sizes,
#                     negative_jumps_time, negative_jumps_sizes) {
#     rval <- list(process = process,
#                  positive_jumps_time = positive_jumps_time,
#                  positive_jumps_sizes = positive_jumps_sizes,
#                  negative_jumps_time = negative_jumps_time,
#                  negative_jumps_sizes = negative_jumps_sizes)
#     class(rval) <- "process"
# }




