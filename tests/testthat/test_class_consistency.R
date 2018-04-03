context("Class' elements consistency")

for(seed in 1:100) {

    set.seed(seed)
    process <- simulate_process(u = 100, pr = 0, max_time = Inf,
                                max_jumps_number = 1000)

    test_that(
        paste0("Seed: ", seed,
               ". Number of positive jumps is consistent with the number of",
               " positive jumps' arrivals time"),
        expect_equal(length(process$positive_jump_sizes),
                     length(process$positive_arrival_times))
    )

    test_that(
        paste0("Seed: ", seed,
               ". Number of negative jumps is consistent with the number of",
               " negative jumps' arrivals time"),
        expect_equal(length(process$negative_jump_sizes),
                     length(process$negative_arrival_times))
    )

    test_that(
        paste0("Seed: ", seed,
               ". The total number of jumps is consistent with numbers of",
               " positive and negative jumps."),
        expect_equal(process$number_jumps,
                     length(process$negative_arrival_times) +
                         length(process$positive_arrival_times))
    )
}
