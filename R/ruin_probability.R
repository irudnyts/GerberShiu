#' @export
ruin_probability <- function(t,
                             simulation_number = 10000,
                             seed = NULL,
                             ci_level = 0.95,
                             ...) {

    if(!is.null(seed)) set.seed(seed)

    process_params <- list(...)

    if(!is.null(process_params[["max_time"]]))
        message("max_time will be overwritten")
    if(!is.null(process_params[["max_jumps_number"]]))
        message("max_jumps_number will be overwritten")

    process_params[["max_time"]] <- t
    process_params[["max_jumps_number"]] <- Inf

    processes <- pbapply::pbreplicate(
        n = simulation_number,
        expr = do.call(simulate_process, process_params),
        simplify = FALSE
    )

    ruined <- sapply(processes, function(prc) prc$is_ruined)

    p <- mean(ruined)
    std <- sd(ruined)

    z <- qnorm(0.5 + ci_level / 2)

    list(
        lower_bound = p - z * std / sqrt(simulation_number),
        estimate = p,
        upper_bound = p + z * std / sqrt(simulation_number)
    )

}
