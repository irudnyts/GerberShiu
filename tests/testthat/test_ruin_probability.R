context("Ruin probability")

probs <- data.frame(
    pr = c(
        0, 0.1, 0.9, 1, 1.1,
        0, 0.1, 0.9, 1, 1.1,
        0, 0.1, 0.9, 1, 1.1
        # 0, 0.1, 0.9, 1, 1.1,
        # 0, 0.1, 0.9, 1, 1.1,
        # 0, 0.1, 0.9, 1, 1.1
    ),

    u = c(
        rep(0, 5),
        rep(10, 5),
        rep(0, 5)
        # rep(10, 5),
        # rep(0, 5),
        # rep(10, 5)
    ),

    t = c(
        rep(10, 5),
        rep(10, 5),
        rep(100, 5)
        # rep(100, 5),
        # rep(Inf, 5), # Inf
        # rep(Inf, 5)
    ),

    prob = c(
        0.872239, 0.846041, 0.605712, 0.578405, 0.552538,
        0.086230, 0.067593, 0.008527, 0.006604, 0.008142,
        0.960050, 0.930334, 0.616260, 0.585779, 0.557662
        # 0.581408, 0.423523, 0.013280, 0.009308, 0.006685,
        # 1.000000, 0.950124, 0.616264, 0.587861, 0.557663,
        # 1.000000, 0.576998, 0.013282, 0.009307, 0.006685
    ),

    tolerance = c(
        rep(0.1, 5),
        rep(0.01, 5),
        rep(0.1, 5)
        # rep(0.01, 5), # different probabilites
        # rep(0.1, 5),
        # rep(0.01, 5)
    )
)

for(row in 1:nrow(probs)) {

    test_that(
        paste0("u: ", probs[row, "u"],
               ", pr: ", probs[row, "pr"],
               ", t: ", probs[row, "t"],
               ". A simulated probability is similar to an exact one."),
        expect_true(
            abs(
                ruin_probability(
                    t = probs[row, "t"],
                    simulation_number = 10000,
                    u = probs[row, "u"],
                    pr = probs[row, "pr"]
                )$estimate -
                probs[row, "prob"]
            ) < probs[row, "tolerance"]
        )
    )

}
