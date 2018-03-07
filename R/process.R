get_time_to_ruin <- function(prc) {
    ifelse(prc$is_ruined,
           yes = prc$path[nrow(prc$path), 1],
           no = NA)
}

get_deficit_at_ruin <- function(prc) {
    ifelse(prc$is_ruined,
           yes = -prc$path[nrow(prc$path), 2],
           no = NA)
}

get_surplus_prior_to_ruin <- function(prc) {
    ifelse(prc$is_ruined,
           yes = prc$path[nrow(prc$path) - 1, 2],
           no = NA)
}

get_path_value <- function(prc, t) {
    if(prc$path[nrow(prc$path), 1] >= t) {
        if(t %in% prc$path[, 1]) {
            indices <- which(prc$path[, 1] == t)
            prc$path[last(indices), 2]
        } else {
            indices <- which(prc$path[, 1] < t)
            prc$path[last(indices), 2] +
                prc$pr * (t - prc$path[last(indices), 1])

        }
    } else {
        NA
    }
}
