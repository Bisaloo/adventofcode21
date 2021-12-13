library(adventofcode21)
x <- readLines("./inst/input13.txt")
x <- list(
  positions = strsplit(grep("^[^fold]", x, value = TRUE), ","),
  folds = grep("^fold", x, value = TRUE)
)
x$positions <- do.call(rbind, x$positions)
storage.mode(x$positions) <- "numeric"

x$folds <- lapply(
  x$folds,
  function(e) {
    if (startsWith(e, "fold along x=")) {
      return(c(as.numeric(gsub("fold along x=", "", e)), 0))
    } else {
      return(c(0, as.numeric(gsub("fold along y=", "", e))))
    }
  }
)

p1 <- f13a(x)
p2 <- f13b(x)
image(p2)

stopifnot(p1 == aoc_solutions$day13a)
