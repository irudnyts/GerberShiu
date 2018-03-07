#' @export
ruin_probability <- function(t,
                             simulation_number = 1000,
                             seed = 1,
                             ...) {

    set.seed(seed)

    process_params <- list(...)

    if(!is.null(process_params[["max_time_span"]]))
        message("max_time_span will be overwritten")
    if(!is.null(process_params[["max_jumps_number"]]))
        message("max_time_span will be overwritten")

    process_params[["max_time_span"]] <- t
    process_params[["max_jumps_number"]] <- Inf

    processes <- pbapply::pbreplicate(
        n = simulation_number,
        expr = do.call(simulate_process, process_params),
        simplify = FALSE
    )

    ruined <- sapply(processes, function(prc) prc$is_ruined)
    mean(ruined) #, na.rm = TRUE)

}
