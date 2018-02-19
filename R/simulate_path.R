# max_jumps_number can be Inf
simulate_process <- function(initial_capital = 10,
                             premium_rate = 1,
                             capital_injections_rate = 1,
                             capital_injections_distribution = rexp,
                             capital_injections_distribution_parameters = list(rate = 1),
                             claims_rate = 1,
                             claims_ht_distribution = rexp,
                             claims_ht_distribution_parameters = list(rate = 1),
                             claims_pt_distribution = rpareto,
                             claims_pt_distribution_parameters = list(shape = 1, scale = 1),
                             mixing_parameter = 0.5,
                             max_jumps_number = 10000) {
    # rename arguments
    u <- initial_capital
    pr <- premium_rate
    l1 <- capital_injections_rate
    l2 <- claims_rate

    # initialize process
    process <- matrix(NA, nrow = 1, ncol = 2)
    colnames(process) <- c("time", "X")
    process[1, ] <- c(0, u)


    # adding negative jump to a process
    add_njump <- function(jump_time, jump_value) {
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

    # adding positive jump to a process
    add_pjump <- function(jump_time, jump_value) {
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
    get_process_last <- function() process[nrow(process), 2]

    # get last element of a process
    last <- function(x) x[length(x)]

    p_time <- numeric() # time of positive jumps
    n_time <- numeric() # time of negative jumps

    p_time[1] <-  rexp(1, l1)
    n_time[1] <-  rexp(1, l2)


    jumps_number <- 0

    repeat{

        if(last(p_time) > last(n_time)) {

            add_njump(last(n_time), 1)
            if(get_process_last() < 0) return(process)
            if(jumps_number > max_jumps_number) {
                warning(paste0("max_jumps_number attained.",
                        "Process stoped before being negative."))
                browser()
                return(process)
            }

            repeat {
                n_time <-  c(n_time, last(n_time) + rexp(1, l2))
                if(last(p_time) < last(n_time)) break
                add_njump(last(n_time), 1)
                if(get_process_last() < 0) return(process)
                if(jumps_number > max_jumps_number) {
                    warning(paste0("max_jumps_number attained.",
                                   "Process stoped before being negative."))
                    browser()
                    return(process)
                }
            }

        } else {

            add_pjump(last(p_time), 1)
            if(jumps_number > max_jumps_number) {
                warning(paste0("max_jumps_number attained.",
                               "Process stoped before being negative."))
                browser()
                return(process)
            }

            repeat {
                p_time <-  c(p_time, last(p_time) + rexp(1, l1))
                if(last(p_time) > last(n_time)) break
                add_pjump(last(p_time), 1)
                if(jumps_number > max_jumps_number) {
                    warning(paste0("max_jumps_number attained.",
                                   "Process stoped before being negative."))
                    browser()
                    return(process)
                }
            }

        }
    }
    # return(process)
}
