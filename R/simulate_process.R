#' @name simulate_process
#' @title Simulate a path of Cramer–Lundberg model extenssion with capital
#' injections.
#'
#' @description \code{simulate_process} returns an object of S3 \code{process}
#' class, which is a simulated stochastic process of Cramer–Lundberg model
#' extenssion with capital injections (i.e. positive jumps).
#'
#' @details This function simulates a stochastic process, which follows
#' Cramer–Lundberg model extenssion with capital injections (i.e. positive
#' jumps). The model of the process to be simulated is as follows:
#' \deqn{X_{\epsilon}(t) = u + ct + \sum_{k=1}^{N^{(+)}(t)} U^{(+)}_k -
#' \sum_{k=1}^{N^{(-)}(t)} U^{(-)}_k}
#' The stopping time of the algorithm is (1) when ruin is occured,
#' (2) the time horizon (\code{max_time}) is attained, (3) the maximum number of
#' jumps (\code{max_jumps_number}) is achieved. Values of (\code{max_time}) and
#' (\code{max_jumps_number}) can be set to \code{Inf} allowing the process to be
#' simulated until ruin.
#'
#' @section Warning:
#' The function should be used with a great care when both
#' \code{max_jumps_number} and \code{max_time} are set to \code{Inf}. In this
#' case a simulating \code{repeat} loop will be stopped only by ruin. This can
#' cause an infinite loop.
#'
#' @param u a numeric vector of length one specifying an initial capital
#' (\eqn{u} in the formula representation). Default value is 10.
#'
#' @param pr a numeric vector of length one specifying a premium rate (\eqn{c}
#' in the formula representation). Defaulr value is 1.
#'
#' @param lambda_p a numeric vector of length one specifying the rate of a
#' Poisson process of capital injections. Default value is 1.
#'
#' @param f_p a function specifying a random generator of a distribution of
#' capital injections. Default value is \code{rexp} (an exponential
#' distribution).
#'
#' @param param_p a list of parameters of capital injections' distribution
#' passed to f_p. The default value is \code{list(rate = 1)}.
#'
#' @param lambda_n a numeric vector of length one specifying the rate of a
#' Poisson process of claims. Default value is 1.
#'
#' @param f_n1 a function specifying a random generator for a phase-type
#' distribution of claims. Default value is \code{rexp} (an exponential
#' distribution).
#'
#' @param param_n1 a list of parameters of claims' distribution passed to f_n1.
#' The default value is \code{list(rate = 1)}.
#'
#' @param f_n2 a function specifying a random generator for a heavy-tailed
#' distribution of claims. Default value is \code{actuar::rpareto1} (a Pareto
#' Type I distribution, from \code{actuar} package).
#'
#' @param param_n2 a list of parameters of claims' distribution passed to f_n2.
#' The default value is \code{list(shape = 3 / 2, min = 2 / 3)}.
#'
#' @param eps a numeric vector of length one specifying the mixing parameter for
#' claims' mixed distribution. Default value is \code{0}.
#'
#' @param max_jumps_number a numeric vector of length one specifying the maximum
#' number of positive and negative jumps, after which the loop stops.
#' Default value is \code{Inf}, i.e. infinite number of loops. See details.
#'
#' @param max_time a numeric vector of length one specifying the time horizon
#' until whith the process will be simulated. Default valie is 10. Can be set to
#' \code{Inf}. See details.
#'
#' @return An object of class \code{process} which is represented by a list of
#' elements:
#' \itemize{
#' \item path: a matrix of two columns. The first column is the time, the second
#'     is the value of a process.
#' \item positive_jump_sizes: a vector of positive (capital injections) jumps'
#'     values.
#' \item negative_jump_sizes: a vector of negative (claims) jumps' values.
#' \item positive_arrival_times: a vector indicating arrival times of positive
#'     jumps.
#' \item negative_arrival_times: a vector indicating arrival times of negative
#'     jumps.
#' \item number_jumps: the number of jumps.
#' \item time: the final time of the process.
#' \item seed: the value of .Random.seed before simulation.
#' \item u: an initial capital.
#' \item pc: a premium rate.
#' \item lambda_p: the rate of a Poisson process of capital injections.
#' \item f_p: a random generator of a distribution of capital injections.
#' \item param_p: a list of parameters of capital injections' distribution
#'     passed to f_p.
#' \item lambda_n: the rate of a Poisson process of claims.
#' \item f_n1: a random generator for a phase-type distribution of claims.
#' \item param_n1: a list of parameters of claims' distribution passed to f_n1.
#' \item f_n2: a random generator for a heavy-tailed distribution of claims.
#' \item param_n2: a list of parameters of claims' distribution passed to f_n2.
#' \item eps: the mixing parameter for claims' mixed distribution.
#' \item max_jumps_number: the maximum number of positive and negative jumps.
#' \item max_time: the maximum of the time horizon.
#' }
#'
#' @examples
#' \dontrun{
#' prc <- simulate_process()
#' }
#' @export
simulate_process <- function(u = 10,
                             pr = 1,
                             lambda_p = 1,
                             f_p = rexp,
                             param_p = list(rate = 1),
                             lambda_n = 1,
                             f_n1 = rexp,
                             param_n1 = list(rate = 1),
                             f_n2 = actuar::rpareto1,
                             param_n2 = list(shape = 3 / 2, min = 2 / 3),
                             eps = 0,
                             max_jumps_number = Inf,
                             max_time = 10) {

    seed <- .Random.seed

    # add n = 1 to all distributions parameters in order to generate only
    # one r.v.
    param_p[["n"]] <- 1
    param_n1[["n"]] <- 1
    param_n2[["n"]] <- 1

    # initialize process
    path <- matrix(NA, nrow = 1, ncol = 2)
    colnames(path) <- c("time", "X")
    path[1, ] <- c(0, u)

    # auxiliary function for adding jumps to a path
    add_jump_to_path <- function(path, arrival, size) {

        path <- rbind(
            path,
            c(arrival,
              path[nrow(path), 2] + (arrival - path[nrow(path), 1]) * pr)
        )

        path <- rbind(
            path,
            c(arrival,
              path[nrow(path), 2] + size)
        )
        path
    }

    s_pos <- numeric() # positive jumps' sizes
    s_neg <- numeric() # positive jumps' sizes

    a_pos <- numeric() # arrival times of positive jumps
    a_neg <- numeric() # arrival times of negative jumps

    ca_pos <- rexp(1, lambda_p) # current arrival time of a positive jump
    ca_neg <- rexp(1, lambda_n) # current arrival time of a negative jump

    nj <- 0 # number of jumps

    repeat{

        if((ca_pos < max_time | ca_neg < max_time) & nj < max_jumps_number) {

            if(ca_pos > ca_neg) {

                # current negative jump's size
                cs_neg <- ifelse(test = rbinom(n = 1, size = 1, prob = eps) == 1,
                                 yes = do.call(f_n2, param_n2),
                                 no = do.call(f_n1, param_n1))

                path <- add_jump_to_path(path, ca_neg, -cs_neg)

                s_neg <- c(s_neg, cs_neg)
                a_neg <- c(a_neg, ca_neg)

                nj <- nj + 1

                if(path[nrow(path), 2] < 0) break

                ca_neg <- ca_neg + rexp(1, lambda_n)


            } else if(ca_pos == ca_neg) {

                cs_pos <- do.call(f_p, param_p) # current positive jump's size
                # current negative jump's size
                cs_neg <- ifelse(test = rbinom(n = 1, size = 1, prob = eps) == 1,
                                 yes = do.call(f_n2, param_n2),
                                 no = do.call(f_n1, param_n1))

                path <- add_jump_to_path(path, ca_pos, cs_pos - cs_neg)

                s_pos <- c(s_pos, cs_pos)
                s_neg <- c(s_neg, cs_neg)

                a_pos <- c(a_pos, ca_pos)
                a_neg <- c(a_neg, ca_neg)

                nj <- nj + 1

                if(path[nrow(path), 2] < 0) break

                ca_pos <- ca_pos + rexp(1, lambda_p)
                ca_neg <- ca_neg + rexp(1, lambda_n)

            } else if(ca_pos < ca_neg) {

                cs_pos <- do.call(f_p, param_p) # current positive jump's size

                path <- add_jump_to_path(path, ca_pos, cs_pos)

                s_pos <- c(s_pos, cs_pos)
                a_pos <- c(a_pos, ca_pos)

                nj <- nj + 1

                ca_pos <- ca_pos + rexp(1, lambda_p)

            }

        } else {

            break

        }

    }


    # add max_time to a path, if the path is not ruined
    if(path[nrow(path), 2] >= 0)
        path <- rbind(
            path,
            c(max_time,
              path[nrow(path), 2] + (max_time - path[nrow(path), 1]) * pr)
        )

    # generate a returning value
    rval <- list(
        path = path,
        positive_jump_sizes = s_pos,
        negative_jump_sizes = s_neg,
        positive_arrival_times = a_pos,
        negative_arrival_times = a_neg,
        number_jumps = nj,
        time = path[nrow(path), 1],
        seed = seed,
        u = u,
        pr = pr,
        lambda_p = lambda_p,
        f_p = f_p,
        param_p = param_p,
        lambda_n = lambda_n,
        f_n1 = f_n1,
        param_n1 = param_n1,
        f_n2 = f_n2,
        param_n2 = param_n2,
        eps = eps,
        max_jumps_number = max_jumps_number,
        max_time = max_time
    )

    class(rval) <- "process"

    return(rval)

}
