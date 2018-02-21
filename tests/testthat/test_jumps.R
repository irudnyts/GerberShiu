context("Jumps' interarrival times")


for(seed in 1:100) {
    set.seed(seed)
    process <- simulate_process(u = 100, max_jumps_number = 1000)

    test_that(
        paste0("Seed: ", seed, ". Mean of positive jumps' is around 1"),
        expect_true(abs(mean(process$jumps_p) - 1) < 0.5)
    )

    test_that(
        paste0("Seed: ", seed, ". Mean of negative jumps' is around 2"),
        expect_true(abs(mean(process$jumps_n) - 2) < 0.5)
    )
}
