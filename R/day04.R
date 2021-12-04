#' Day 04: Giant Squid
#'
#' [Giant Squid](https://adventofcode.com/2021/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' You\'re already almost 1.5km (almost a mile) below the surface of the
#' ocean, already so deep that you can\'t see any sunlight. What you *can*
#' see, however, is a giant squid that has attached itself to the outside
#' of your submarine.
#'
#' Maybe it wants to play
#' [bingo](https://en.wikipedia.org/wiki/Bingo_(American_version))?
#'
#' Bingo is played on a set of boards each consisting of a 5x5 grid of
#' numbers. Numbers are chosen at random, and the chosen number is *marked*
#' on all boards on which it appears. (Numbers may not appear on all
#' boards.) If all numbers in any row or any column of a board are marked,
#' that board *wins*. (Diagonals don\'t count.)
#'
#' The submarine has a *bingo subsystem* to help passengers (currently, you
#' and the giant squid) pass the time. It automatically generates a random
#' order in which to draw numbers and a random set of boards (your puzzle
#' input). For example:
#'
#'     7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
#'
#'     22 13 17 11  0
#'      8  2 23  4 24
#'     21  9 14 16  7
#'      6 10  3 18  5
#'      1 12 20 15 19
#'
#'      3 15  0  2 22
#'      9 18 13 17  5
#'     19  8  7 25 23
#'     20 11 10 24  4
#'     14 21 16 12  6
#'
#'     14 21 17 24  4
#'     10 16 15  9 19
#'     18  8 23 26 20
#'     22 11 13  6  5
#'      2  0 12  3  7
#'
#' After the first five numbers are drawn (`7`, `4`, `9`, `5`, and `11`),
#' there are no winners, but the boards are marked as follows (shown here
#' adjacent to each other to save space):
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' After the next six numbers are drawn (`17`, `23`, `2`, `0`, `14`, and
#' `21`), there are still no winners:
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' Finally, `24` is drawn:
#'
#'     22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
#'      8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
#'     21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
#'      6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
#'      1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
#'
#' At this point, the third board *wins* because it has at least one
#' complete row or column of marked numbers (in this case, the entire top
#' row is marked: `14 21 17 24  4`).
#'
#' The *score* of the winning board can now be calculated. Start by finding
#' the *sum of all unmarked numbers* on that board; in this case, the sum
#' is `188`. Then, multiply that sum by *the number that was just called*
#' when the board won, `24`, to get the final score, `188 * 24 = 4512`.
#'
#' To guarantee victory against the giant squid, figure out which board
#' will win first. *What will your final score be if you choose that
#' board?*
#'
#' **Part Two**
#'
#' On the other hand, it might be wise to try a different strategy: [let
#' the giant squid
#' win]{title="That's 'cuz a submarine don't pull things' antennas out of their
#' sockets when they lose. Giant squid are known to do that."}.
#'
#' You aren\'t sure how many bingo boards a giant squid could play at once,
#' so rather than waste time counting its arms, the safe thing to do is to
#' *figure out which board will win last* and choose that one. That way, no
#' matter which boards it picks, it will win for sure.
#'
#' In the above example, the second board is the last to win, which happens
#' after `13` is eventually called and its middle column is completely
#' marked. If you were to keep playing until this point, the second board
#' would have a sum of unmarked numbers equal to `148` for a final score of
#' `148 * 13 = 1924`.
#'
#' Figure out which board will win last. *Once it wins, what would its
#' final score be?*
#'
#' @param x some data
#' @return For Part One, `f04a(x)` returns .... For Part Two,
#'   `f04b(x)` returns ....
#' @export
#' @examples
#' f04a(example_data_04())
#' f04b(example_data_04())
f04a <- function(x) {

  wins <- vapply(
    x[["boards"]],
    get_winning_round, x[["draw"]],
    FUN.VALUE = numeric(1)
  )

  get_winning_score(
    x[["boards"]][[which.min(wins)]],
    draw = x[["draw"]],
    winning_round = min(wins)
  )

}

get_winning_round <- function(board, draw) {

  win_row <- apply(board, 1, function(r) max(match(r, draw)))
  win_col <- apply(board, 2, function(c) max(match(c, draw)))

  return(min(win_row, win_col))

}

get_winning_score <- function(board, draw, winning_round) {

  sum_unmarked <- sum(board[!board %in% draw[seq_len(winning_round)]])
  last_called <- draw[winning_round]

  return(sum_unmarked * last_called)

}

#' @rdname day04
#' @export
f04b <- function(x) {

  wins <- vapply(
    x[["boards"]],
    get_winning_round, x[["draw"]],
    FUN.VALUE = numeric(1)
  )

  get_winning_score(
    x[["boards"]][[which.max(wins)]],
    draw = x[["draw"]],
    winning_round = max(wins)
  )

}


f04_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
example_data_04 <- function(example = 1) {
  l <- list(
    a = list(
      draw = c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1),
      boards = list(
        matrix(
          c(22, 13, 17, 11, 0,
            8, 2, 23, 4, 24,
            21, 9, 14, 16, 7,
            6, 10, 3, 18, 5,
            1, 12, 20, 15, 19), nrow = 5, ncol = 5, byrow = TRUE),
        matrix(
          c(3, 15, 0, 2, 22,
            9, 18, 13, 17, 5,
            19, 8, 7, 25, 23,
            20, 11, 10, 24, 4,
            14, 21, 16, 12, 6), nrow = 5, ncol = 5, byrow = TRUE),
        matrix(
          c(14, 21, 17, 24, 4,
            10, 16, 15, 9, 19,
            18, 8, 23, 26, 20,
            22, 11, 13, 6, 5,
            2, 0, 12, 3, 7), nrow = 5, ncol = 5, byrow = TRUE)
      )
    )
  )
  l[[example]]
}
