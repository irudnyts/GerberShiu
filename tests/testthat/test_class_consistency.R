context("Class' elements consistency")

for(seed in 1:100) {
    set.seed(seed)
    process <- simulate_process(max_jumps_number = 100)

    test_that(
        paste0("Seed: ", seed,
               ". Number of positive jumps is consistent with the number of",
               " positive jumps' arrivals time"),
        expect_equal(length(process$jumps_p), length(process$time_p))
    )

    test_that(
        paste0("Seed: ", seed,
               ". Number of negative jumps is consistent with the number of",
               " negative jumps' arrivals time"),
        expect_equal(length(process$jumps_n), length(process$time_n))
    )

    test_that(
        paste0("Seed: ", seed,
               ". Number of negative jumps is consistent with the number of",
               " negative jumps' arrivals time"),
        expect_equal(process$jumps_number,
                     length(process$jumps_p) + length(process$jumps_n))
    )
}
