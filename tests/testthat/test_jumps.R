context("Jumps' interarrival times")

set.seed(1000)
process <- simulate_process()

test_that("Mean of positive jumps' is around 1", {
    expect_true(abs(mean(process$jumps_p) - 1) < 0.1)
})

test_that("Mean of negative jumps' is around 2", {
    expect_true(abs(mean(process$jumps_n) - 2) < 0.1)
})
