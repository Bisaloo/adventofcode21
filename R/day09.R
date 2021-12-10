#' Day 09: Smoke Basin
#'
#' [Smoke Basin](https://adventofcode.com/2021/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' These caves seem to be [lava
#' tubes](https://en.wikipedia.org/wiki/Lava_tube). Parts are even still
#' volcanically active; small hydrothermal vents release smoke into the
#' caves that slowly [settles like
#' rain]{title="This was originally going to be a puzzle about watersheds, but we're already under water."}.
#'
#' If you can model how the smoke flows through the caves, you might be
#' able to avoid it and be that much safer. The submarine generates a
#' heightmap of the floor of the nearby caves for you (your puzzle input).
#'
#' Smoke flows to the lowest point of the area it\'s in. For example,
#' consider the following heightmap:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Each number corresponds to the height of a particular location, where
#' `9` is the highest and `0` is the lowest a location can be.
#'
#' Your first goal is to find the *low points* - the locations that are
#' lower than any of its adjacent locations. Most locations have four
#' adjacent locations (up, down, left, and right); locations on the edge or
#' corner of the map have three or two adjacent locations, respectively.
#' (Diagonal locations do not count as adjacent.)
#'
#' In the above example, there are *four* low points, all highlighted: two
#' are in the first row (a `1` and a `0`), one is in the third row (a `5`),
#' and one is in the bottom row (also a `5`). All other locations on the
#' heightmap have some lower adjacent location, and so are not low points.
#'
#' The *risk level* of a low point is *1 plus its height*. In the above
#' example, the risk levels of the low points are `2`, `1`, `6`, and `6`.
#' The sum of the risk levels of all low points in the heightmap is
#' therefore `15`.
#'
#' Find all of the low points on your heightmap. *What is the sum of the
#' risk levels of all low points on your heightmap?*
#'
#' **Part Two**
#'
#' Next, you need to find the largest basins so you know what areas are
#' most important to avoid.
#'
#' A *basin* is all locations that eventually flow downward to a single low
#' point. Therefore, every low point has a basin, although some basins are
#' very small. Locations of height `9` do not count as being in any basin,
#' and all other locations will always be part of exactly one basin.
#'
#' The *size* of a basin is the number of locations within the basin,
#' including the low point. The example above has four basins.
#'
#' The top-left basin, size `3`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The top-right basin, size `9`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The middle basin, size `14`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' The bottom-right basin, size `9`:
#'
#'     2199943210
#'     3987894921
#'     9856789892
#'     8767896789
#'     9899965678
#'
#' Find the three largest basins and multiply their sizes together. In the
#' above example, this is `9 * 14 * 9 = 1134`.
#'
#' *What do you get if you multiply together the sizes of the three largest
#' basins?*
#'
#' @param x some data
#' @return For Part One, `f09a(x)` returns .... For Part Two,
#'   `f09b(x)` returns ....
#' @export
#' @examples
#' f09a(example_data_09())
#' f09b(example_data_09())
f09a <- function(x) {

  lows <- f09_find_lows(x)

  return(sum(x[lows] + 1))

}

f09_find_lows <- function(x) {

  low_rows <- x < t(apply(x, 1, slider::slide_dbl, median, .before = 1, .after = 1))
  low_cols <- x < apply(x, 2, slider::slide_dbl, median, .before = 1, .after = 1)

  return(low_rows & low_cols)

}


#' @rdname day09
#' @export
f09b <- function(x) {

  # Dirty hack to convert binary matrix to bitmap array and then magick object
  basins <- rep(apply(x != 9, 2, as.numeric), 3)
  dim(basins) <- c(dim(x), 3)

  basins_landscape <- magick::image_read(basins)

  lows <- which(f09_find_lows(x), arr.ind = TRUE)

  # Convert lows to image coords instead of matrix coords
  lows <- lows[, c(2,1)] - 1

  sizes <- apply(lows, 1, f09_find_basin, basins_landscape)

  return(prod(sort(sizes, decreasing = TRUE)[1:3]))
}


f09_find_basin <- function(low, landscape) {

  # Fill basin with water (in blue)
  basin <- magick::image_fill(landscape, "blue", paste0("+", low[1], "+", low[2]))

  # Count blue pixels
  return(sum(as.raster(basin) == "#0000ffff"))

}

#' Create christmas lights gif/video
#'
#' @param x puzzle input
#'
#' @export
f09_video <- function(x) {

  # Dirty hack to convert binary matrix to bitmap array and then magick object
  basins <- rep(apply(x != 9, 2, as.numeric), 3)
  dim(basins) <- c(dim(x), 3)

  landscape <- magick::image_read(basins)

  lows <- which(f09_find_lows(x), arr.ind = TRUE)

  # Convert lows to image coords instead of matrix coords
  lows <- lows[, c(2,1)] - 1

  imgs <- lapply(seq_len(nrow(lows)), function(i) {
    img <- magick::image_fill(landscape, "red", paste0("+", lows[i,1], "+", lows[i,2]))
    magick::image_transparent(img, "white")
  })

  # Randomize order and create groups of 15 to have a christmas lights effect
  frames <- replicate(10, {
    f <- purrr::reduce(imgs[sample(length(imgs), 15)], magick::image_composite, "Blend")
    f <- magick::image_background(f, "green")
    f <- magick::image_flatten(f)
  })

  frames <- do.call(c, frames)
  frames <- magick::image_scale(frames, 400)

  magick::image_animate(frames, fps = 1)

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = matrix(c(
      2, 1, 9, 9, 9, 4, 3, 2, 1, 0,
      3, 9, 8, 7, 8, 9, 4, 9, 2, 1,
      9, 8, 5, 6, 7, 8, 9, 8, 9, 2,
      8, 7, 6, 7, 8, 9, 6, 7, 8, 9,
      9, 8, 9, 9, 9, 6, 5, 6, 7, 8), nrow = 5, byrow = TRUE
    )
  )
  l[[example]]
}
