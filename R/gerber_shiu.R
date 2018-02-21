#' @export
gerber_shiu <- function(q = 1,
                        w = function(x, y) x + y,
                        simulation_number = 1000,
                        ...) {
    # browser()
    gs <- function(prc) {
        exp(-q * get_time_to_ruin(prc)) *
            w(get_deficit_at_ruin(prc), get_surplus_prior_to_ruin(prc))
    }

    processes <- pbapply::pbreplicate(n = simulation_number,
                                      expr = simulate_process(...),
                                      simplify = FALSE)

    gs_values <- sapply(processes, gs)
    mean(gs_values)

    # simulate 100000 process and add to a list
    # estimate for these processes a value of Gerber Shiu function
    # take an average of this values

}
