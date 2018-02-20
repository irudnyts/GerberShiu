# max_jumps_number can be Inf
#' @export
simulate_process <- function(u = 10,
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
                             max_jumps_number = 1000000) {

    # add n = 1 to all distributions parameters in order to generate only
    # one r.v.
    param_p[["n"]] <- 1
    param_n1[["n"]] <- 1
    param_n2[["n"]] <- 1

    # initialize iterator variable
    jumps_number <- 0

    # initialize process
    path <- matrix(NA, nrow = 1, ncol = 2)
    colnames(path) <- c("time", "X")
    path[1, ] <- c(0, u)

    # function for adding negative jump to a path
    add_jump_n <- function() {
        jump_value <- ifelse(test = rbinom(n = 1, size = 1, prob = eps) == 1,
                             yes = do.call(f_n2, param_n2),
                             no = do.call(f_n1, param_n1))
        jumps_n <<-  c(jumps_n, jump_value)

        jump_time <- last(time_n)
        time_to_jump <- jump_time - path[nrow(path), 1]

        path <<- rbind(
            path,
            c(jump_time,
              path[nrow(path), 2] + time_to_jump * pr)
        )

        path <<- rbind(
            path,
            c(path[nrow(path), 1],
              path[nrow(path), 2] - jump_value)
        )

        jumps_number <<- jumps_number + 1
    }

    # function for adding positive jump to a path
    add_jump_p <- function(jump_time, jump_value) {
        jump_value <- do.call(f_p, param_p)
        jumps_p <<-  c(jumps_p, jump_value)
        jump_time <- last(time_p)
        time_to_jump <- jump_time - path[nrow(path), 1]
        path <<- rbind(
            path,
            c(jump_time,
              path[nrow(path), 2] + time_to_jump * pr)
        )

        path <<- rbind(
            path,
            c(path[nrow(path), 1],
              path[nrow(path), 2] + jump_value)
        )

        jumps_number <<- jumps_number + 1
    }

    # get last value of a path
    get_path_last_value <- function() path[nrow(path), 2]

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

            add_jump_n()

            if(get_path_last_value() < 0) break
            if(jumps_number > max_jumps_number) break

            repeat {

                time_n <- c(time_n, last(time_n) + rexp(1, lambda_n))
                if(last(time_p) < last(time_n)) break

                add_jump_n()

                if(get_path_last_value() < 0) break

                if(jumps_number > max_jumps_number) break
            }

            if(get_path_last_value() < 0) break
            if(jumps_number > max_jumps_number) break


        } else {

            add_jump_p()

            if(jumps_number > max_jumps_number) break

            repeat {
                time_p <-  c(time_p, last(time_p) + rexp(1, lambda_p))
                if(last(time_p) > last(time_n)) break
                add_jump_p()
                if(jumps_number > max_jumps_number) break
            }

            if(jumps_number > max_jumps_number) break

        }
    }

    if(jumps_number > max_jumps_number)
        warning("max_jumps_number is achieved")

    rval <- list(
        path = path,
        jumps_p = jumps_p,
        time_p = time_p,
        jumps_n = jumps_n,
        time_n = time_n
    )

    class(rval) <- "process"

    return(rval)
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

#' @export
plot.process <- function(prc) {
    plot(prc$path, type = "l")

}
