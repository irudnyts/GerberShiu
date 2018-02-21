context("Jumps' interarrival times")

for(seed in 1:100) {
    set.seed(seed)
    process <- simulate_process(u = 100, max_jumps_number = 1000)

    test_that(
        paste0("Seed: ", seed,
               ". Mean of positive jumps' interarrival time is around 1"),
        expect_true(abs(mean(diff(process$time_p)) - 1) < 0.2)
    )

    test_that(
        paste0("Seed: ", seed,
               ". Variance of positive jumps' interarrival time is around 1"),
        expect_true(abs(var(diff(process$time_p)) - 1) < 0.5)
    )

    test_that(
        paste0("Seed: ", seed,
               ". Mean of negative jumps' interarrival time is around 1"),
        expect_true(abs(mean(diff(process$time_n)) - 1) < 0.2)
    )

    test_that(
        paste0("Seed: ", seed,
               ". Variance of negative jumps' interarrival time is around 1"),
        expect_true(abs(var(diff(process$time_n)) - 1) < 0.5)
    )
}
