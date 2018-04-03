context("Jumps' sizes")


for(seed in 1:100) {

    set.seed(seed)
    process <- simulate_process(u = 100, pr = 0, max_time = Inf,
                                max_jumps_number = 1000)

    test_that(
        paste0("Seed: ", seed, ". Mean of positive jumps' is around 1"),
        expect_true(abs(mean(process$positive_jump_sizes) - 1) < 0.2)
    )

    test_that(
        paste0("Seed: ", seed, ". Mean of negative jumps' is around 2"),
        expect_true(abs(mean(process$negative_jump_sizes) - 1) < 0.2)
    )
}
