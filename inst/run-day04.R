library(adventofcode21)
x <- scan("./inst/input04.txt", what = character())

x <- list(
  "draw" = as.numeric(strsplit(x[1], ",")[[1]]),
  "boards" = lapply(
    seq(from = 2, to = length(x), by = 25),
    function(i) {
      matrix(as.numeric(x[seq(i, i+24)]), nrow = 5, ncol = 5, byrow = TRUE)
    }
  )
)

p1 <- f04a(x)
p2 <- f04b(x)

stopifnot(p1 == aoc_solutions$day04a)
stopifnot(p2 == aoc_solutions$day04b)
