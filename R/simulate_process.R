# max_jumps_number can be Inf
# set seed
# premium rate should be positive

#' Simulate a path of Cramer–Lundberg model extenssion with capital injections
#'
#' \code{simulate_process} returns an object of S3 \code{process} class, which
#' is simulated stochastic process of Cramer–Lundberg model extenssion with
#' capital injections (i.e. positive jumps).
#'
#' This function simulates a stochastic process, which follows Cramer–Lundberg
#' model extenssion with capital injections (i.e. positive jumps) until either
#' ruin or when maximum number of jumps is attained.
#'
#' @section Warning:
#' The function should be used with a great care when \code{max_jumps_number} is
#' set to \code{Inf}. In this case a simulating \code{repeat} loop will be
#' stopped only by ruin. This can cause an infinite loop. The model is as
#' follows:
#' \deqn{X_{\epsilon}(t) = u + ct + \sum_{k=1}^{N^{(+)}(t)} U^{(+)}_k -
#' \sum_{k=1}^{N^{(-)}(t)} U^{(-)}_k}
#'
#' @param u a numeric vector of length one specifying an initial capital
#' (\eqn{u} in the formula representation). Default value is 10.
#' @param pr a numeric vector of length one specifying a premium rate (\eqn{c}
#' in the formula representation). Defaulr value is 1.
#' @param lambda_p a numeric vector of length one specifying the rate of a
#' Poisson process of capital injections. Default value is 1.
#' @param f_p a function specifying a random generator for a distribution of
#' capital injections. Default value is \code{rexp} (an exponential
#' distribution).
#' @param param_p a list of parameters of capital injections' distribution
#' passed to f_p. The default value is \code{list(rate = 1)}.
#' @param lambda_n a numeric vector of length one specifying the rate of a
#' Poisson process of claims. Default value is 1.
#' @param f_n1 a function specifying a random generator for a phase-type
#' distribution of claims. Default value is \code{rexp} (an exponential
#' distribution).
#' @param param_n1 a list of parameters of claims' distribution passed to f_n1.
#' The default value is \code{list(rate = 1 / 2)}.
#' @param f_n2 a function specifying a random generator for a heavy-tailed
#' distribution of claims. Default value is \code{actuar::rpareto1} (a Pareto
#' Type I distribution, from \code{actuar} package).
#' @param param_n1 a list of parameters of claims' distribution passed to f_n2.
#' The default value is \code{list(shape = 3 / 2, min = 2 / 3)}.
#' @param eps a numeric vector of length one specifying the mixing parameter for
#' claims' mixed distribution.
#' @param max_jumps_number a numeric vector of length one specifying the maximum
#' number of positive and negative jumps, after which the function stops.
#' Default value is 10000. Can be set to \code{Inf}.
#' @return An object of class \code{process} which is represented by a list of
#' elements:
#' \itemize{
#' \item path: a matrix of two columns. The first column is the time, the second
#' is the value of a process.
#' \item jumps_p a vector of positive (capital injections) jumps' values.
#' \item time_p a vector indicating times of positive jumps.
#' \item jumps_n a vector of negative (claims) jumps' values.
#' \item time_p a vector indicating times of negative jumps.
#' \item jumps_number a vector indicating the number of all jumps.
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
                             param_n1 = list(rate = 1 / 2),
                             f_n2 = actuar::rpareto1,
                             param_n2 = list(shape = 3 / 2, min = 2 / 3),
                             eps = 0.1,
                             max_jumps_number = 10000,
                             max_time_span = 7500) {


    # utility function: get last element of a vector
    last <- function(x) ifelse(length(x) > 0, yes = x[length(x)], no = 0)

    # add n = 1 to all distributions parameters in order to generate only
    # one r.v.
    param_p[["n"]] <- 1
    param_n1[["n"]] <- 1
    param_n2[["n"]] <- 1

    # initialize iterator variable
    jumps_number <- 0

    # initialize process
    path <- matrix(NA, nrow = 1, ncol = 2)
    colnames(path) <- c("time", "X")
    path[1, ] <- c(0, u)

    # function for adding negative jump to a path
    add_jump_n <- function() {

        jump_value <- ifelse(test = rbinom(n = 1, size = 1, prob = eps) == 1,
                             yes = do.call(f_n2, param_n2),
                             no = do.call(f_n1, param_n1))
        jumps_n <<-  c(jumps_n, jump_value)

        jump_time <- current_time_n
        time_n <<- c(time_n, current_time_n)

        time_to_jump <- jump_time - path[nrow(path), 1]

        path <<- rbind(
            path,
            c(jump_time,
              path[nrow(path), 2] + time_to_jump * pr)
        )

        path <<- rbind(
            path,
            c(path[nrow(path), 1],
              path[nrow(path), 2] - jump_value)
        )

        jumps_number <<- jumps_number + 1
    }

    # function for adding positive jump to a path
    add_jump_p <- function(jump_time, jump_value) {

        jump_value <- do.call(f_p, param_p)
        jumps_p <<-  c(jumps_p, jump_value)

        jump_time <- current_time_p
        time_p <<- c(time_p, current_time_p)

        time_to_jump <- jump_time - path[nrow(path), 1]
        path <<- rbind(
            path,
            c(jump_time,
              path[nrow(path), 2] + time_to_jump * pr)
        )

        path <<- rbind(
            path,
            c(path[nrow(path), 1],
              path[nrow(path), 2] + jump_value)
        )

        jumps_number <<- jumps_number + 1
    }

    # check whether the path is ruined or not
    is_ruined <- function() path[nrow(path), 2] < 0

    # check whether the maximum number of jumps attained or not
    is_max_jumps_number_attained <- function() jumps_number >= max_jumps_number

    # check whether the path is reached maximum time span
    is_max_time_span_attained <- function() path[nrow(path), 1] >= max_time_span

    jumps_n <- numeric() # negative jumps' sizes
    jumps_p <- numeric() # positive jumps' sizes

    time_n <- numeric() # time of negative jumps
    time_p <- numeric() # time of positive jumps

    current_time_n <- rexp(1, lambda_n)
    current_time_p <- rexp(1, lambda_p)

    # browser()

    repeat{

        if(current_time_p > current_time_n) {

            add_jump_n()

            if(is_ruined()) break
            if(is_max_jumps_number_attained()) break
            if(is_max_time_span_attained()) break

            repeat {

                current_time_n <- last(time_n) + rexp(1, lambda_n)
                if(current_time_p < current_time_n) break

                add_jump_n()

                if(is_ruined()) break
                if(is_max_jumps_number_attained()) break
                if(is_max_time_span_attained()) break
            }

            if(is_ruined()) break
            if(is_max_jumps_number_attained()) break
            if(is_max_time_span_attained()) break


        } else {

            add_jump_p()

            if(is_max_jumps_number_attained()) break
            if(is_max_time_span_attained()) break

            repeat {
                current_time_p <- last(time_p) + rexp(1, lambda_p)
                if(current_time_p > current_time_n) break

                add_jump_p()

                if(is_max_jumps_number_attained()) break
                if(is_max_time_span_attained()) break
            }

            if(is_max_jumps_number_attained()) break
            if(is_max_time_span_attained()) break

        }
    }

    if(jumps_number >= max_jumps_number)
        warning("max_jumps_number is achieved")

    rval <- list(
        path = path,
        jumps_p = jumps_p,
        time_p = time_p,
        jumps_n = jumps_n,
        time_n = time_n,
        jumps_number = jumps_number,
        is_ruined = is_ruined(),
        is_max_jumps_number_attained = is_max_jumps_number_attained(),
        is_max_time_span_attained = is_max_time_span_attained(),
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
        max_time_span = max_time_span
    )

    class(rval) <- "process"

    return(rval)
}


#' @export
plot.process <- function(prc) {
    plot(prc$path, type = "l")

}
