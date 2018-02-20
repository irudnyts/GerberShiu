context("Jumps' interarrival times")

set.seed(1000)
process <- simulate_process()

test_that("Mean of positive jumps' interarrival time is around 1", {
    expect_true(abs(mean(diff(process$time_p)) - 1) < 0.1)
})

test_that("Variance of positive jumps' interarrival time is around 1", {
    expect_true(abs(var(diff(process$time_p)) - 1) < 0.1)
})

test_that("Mean of negative jumps' interarrival time is around 1", {
    expect_true(abs(mean(diff(process$time_n)) - 1) < 0.1)
})

test_that("Variance of negative jumps' interarrival time is around 1", {
    expect_true(abs(var(diff(process$time_n)) - 1) < 0.1)
})
