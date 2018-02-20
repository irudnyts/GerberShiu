#' @export
gerber_shiu <- function(u = 10,
                        q = 1,
                        w = function(x, y) x + y,
                        pr = 1,
                        lambda_p = 1,
                        f_p = rexp,
                        param_p = list(rate = 1),
                        lambda_n = 1,
                        f_n1 = rexp,
                        param_n1 = list(rate = 1 / 2),
                        f_n2 = actuar::rpareto1,
                        param_n2 = list(shape = 3 / 2, min = 2 / 3),
                        eps = 0.1,
                        max_jumps_number = 1000000,
                        simulation_number = 100000) {

}
