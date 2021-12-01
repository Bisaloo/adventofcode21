test_input <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
test_output_puzzle1 <- 7
test_output_puzzle2 <- 5

solve_puzzle1 <- function(input) {

  sum(diff(input)>0)

}

solve_puzzle2 <- function(input) {

  sum(diff(RcppRoll::roll_sum(input, 3)) > 0)

}


