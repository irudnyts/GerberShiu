context("Class' elements consistency")

set.seed(1000)
process <- simulate_process()

test_that(
    paste0("Number of positive jumps is consistent with the number of",
                 " positive jumps' arrivals time"),
    expect_equal(length(process$jumps_p), length(process$time_p))
)

test_that(
    paste0("Number of negative jumps is consistent with the number of",
           " negative jumps' arrivals time"),
    expect_equal(length(process$jumps_n), length(process$time_n))
)

test_that(
    paste0("Number of negative jumps is consistent with the number of",
           " negative jumps' arrivals time"),
    expect_equal(process$jumps_number,
                 length(process$jumps_p) + length(process$jumps_n))
)
