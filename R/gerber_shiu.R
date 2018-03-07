#' @export
gerber_shiu <- function(q = 1,
                        w = function(x, y) x + y,
                        simulation_number = 1000,
                        seed = 1,
                        ...) {
    # browser()
    gs <- function(prc) {
        exp(-q * prc$time_to_ruin *
            w(prc$deficit_at_ruin, prc$surplus_prior_to_ruin))
    }

    set.seed(seed)

    processes <- pbapply::pbreplicate(n = simulation_number,
                                      expr = simulate_process(...),
                                      simplify = FALSE)

    gs_values <- sapply(processes, gs)
    mean(gs_values) #, na.rm = TRUE)

    # simulate 100000 process and add to a list
    # estimate for these processes a value of Gerber Shiu function
    # take an average of this values

}
