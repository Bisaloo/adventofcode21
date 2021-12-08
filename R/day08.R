#' Day 08: Seven Segment Search
#'
#' [Seven Segment Search](https://adventofcode.com/2021/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' You barely reach the safety of the cave when the whale smashes into the
#' cave mouth, collapsing it. Sensors indicate another exit to this cave at
#' a much greater depth, so you have no choice but to press on.
#'
#' As your submarine slowly makes its way through the cave system, you
#' notice that the four-digit [seven-segment
#' displays](https://en.wikipedia.org/wiki/Seven-segment_display) in your
#' submarine are malfunctioning; [they must have been
#' damaged]{title="Yes, just the four-digit seven-segment ones. Whole batch must have been faulty."}
#' during the escape. You\'ll be in a lot of trouble without them, so
#' you\'d better figure out what\'s wrong.
#'
#' Each digit of a seven-segment display is rendered by turning on or off
#' any of seven segments named `a` through `g`:
#'
#'       0:      1:      2:      3:      4:
#'      aaaa    ....    aaaa    aaaa    ....
#'     b    c  .    c  .    c  .    c  b    c
#'     b    c  .    c  .    c  .    c  b    c
#'      ....    ....    dddd    dddd    dddd
#'     e    f  .    f  e    .  .    f  .    f
#'     e    f  .    f  e    .  .    f  .    f
#'      gggg    ....    gggg    gggg    ....
#'
#'       5:      6:      7:      8:      9:
#'      aaaa    aaaa    aaaa    aaaa    aaaa
#'     b    .  b    .  .    c  b    c  b    c
#'     b    .  b    .  .    c  b    c  b    c
#'      dddd    dddd    ....    dddd    dddd
#'     .    f  e    f  .    f  e    f  .    f
#'     .    f  e    f  .    f  e    f  .    f
#'      gggg    gggg    ....    gggg    gggg
#'
#' So, to render a `1`, only segments `c` and `f` would be turned on; the
#' rest would be off. To render a `7`, only segments `a`, `c`, and `f`
#' would be turned on.
#'
#' The problem is that the signals which control the segments have been
#' mixed up on each display. The submarine is still trying to display
#' numbers by producing output on signal wires `a` through `g`, but those
#' wires are connected to segments *randomly*. Worse, the wire/segment
#' connections are mixed up separately for each four-digit display! (All of
#' the digits *within* a display use the same connections, though.)
#'
#' So, you might know that only signal wires `b` and `g` are turned on, but
#' that doesn\'t mean *segments* `b` and `g` are turned on: the only digit
#' that uses two segments is `1`, so it must mean segments `c` and `f` are
#' meant to be on. With just that information, you still can\'t tell which
#' wire (`b`/`g`) goes to which segment (`c`/`f`). For that, you\'ll need
#' to collect more information.
#'
#' For each display, you watch the changing signals for a while, make a
#' note of *all ten unique signal patterns* you see, and then write down a
#' single *four digit output value* (your puzzle input). Using the signal
#' patterns, you should be able to work out which pattern corresponds to
#' which digit.
#'
#' For example, here is what you might see in a single entry in your notes:
#'
#'     acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
#'     cdfeb fcadb cdfeb cdbaf
#'
#' (The entry is wrapped here to two lines so it fits; in your notes, it
#' will all be on a single line.)
#'
#' Each entry consists of ten *unique signal patterns*, a `|` delimiter,
#' and finally the *four digit output value*. Within an entry, the same
#' wire/segment connections are used (but you don\'t know what the
#' connections actually are). The unique signal patterns correspond to the
#' ten different ways the submarine tries to render a digit using the
#' current wire/segment connections. Because `7` is the only digit that
#' uses three segments, `dab` in the above example means that to render a
#' `7`, signal lines `d`, `a`, and `b` are on. Because `4` is the only
#' digit that uses four segments, `eafb` means that to render a `4`, signal
#' lines `e`, `a`, `f`, and `b` are on.
#'
#' Using this information, you should be able to work out which combination
#' of signal wires corresponds to each of the ten digits. Then, you can
#' decode the four digit output value. Unfortunately, in the above example,
#' all of the digits in the output value (`cdfeb fcadb cdfeb cdbaf`) use
#' five segments and are more difficult to deduce.
#'
#' For now, *focus on the easy digits*. Consider this larger example:
#'
#'     be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
#'     fdgacbe cefdb cefbgd gcbe
#'     edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
#'     fcgedb cgb dgebacf gc
#'     fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
#'     cg cg fdcagb cbg
#'     fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
#'     efabcd cedba gadfec cb
#'     aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
#'     gecf egdcabf bgf bfgea
#'     fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
#'     gebdcfa ecba ca fadegcb
#'     dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
#'     cefg dcbef fcge gbcadfe
#'     bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
#'     ed bcgafe cdgba cbgef
#'     egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
#'     gbdfcae bgc cg cgb
#'     gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
#'     fgae cfgab fg bagce
#'
#' Because the digits `1`, `4`, `7`, and `8` each use a unique number of
#' segments, you should be able to tell which combinations of signals
#' correspond to those digits. Counting *only digits in the output values*
#' (the part after `|` on each line), in the above example, there are `26`
#' instances of digits that use a unique number of segments (highlighted
#' above).
#'
#' *In the output values, how many times do digits `1`, `4`, `7`, or `8`
#' appear?*
#'
#' **Part Two**
#'
#' Through a little deduction, you should now be able to determine the
#' remaining digits. Consider again the first example above:
#'
#'     acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
#'     cdfeb fcadb cdfeb cdbaf
#'
#' After some careful analysis, the mapping between signal wires and
#' segments only make sense in the following configuration:
#'
#'      dddd
#'     e    a
#'     e    a
#'      ffff
#'     g    b
#'     g    b
#'      cccc
#'
#' So, the unique signal patterns would correspond to the following digits:
#'
#' -   `acedgfb`: `8`
#' -   `cdfbe`: `5`
#' -   `gcdfa`: `2`
#' -   `fbcad`: `3`
#' -   `dab`: `7`
#' -   `cefabd`: `9`
#' -   `cdfgeb`: `6`
#' -   `eafb`: `4`
#' -   `cagedb`: `0`
#' -   `ab`: `1`
#'
#' Then, the four digits of the output value can be decoded:
#'
#' -   `cdfeb`: `5`
#' -   `fcadb`: `3`
#' -   `cdfeb`: `5`
#' -   `cdbaf`: `3`
#'
#' Therefore, the output value for this entry is `5353`.
#'
#' Following this same process for each entry in the second, larger example
#' above, the output value of each entry can be determined:
#'
#' -   `fdgacbe cefdb cefbgd gcbe`: `8394`
#' -   `fcgedb cgb dgebacf gc`: `9781`
#' -   `cg cg fdcagb cbg`: `1197`
#' -   `efabcd cedba gadfec cb`: `9361`
#' -   `gecf egdcabf bgf bfgea`: `4873`
#' -   `gebdcfa ecba ca fadegcb`: `8418`
#' -   `cefg dcbef fcge gbcadfe`: `4548`
#' -   `ed bcgafe cdgba cbgef`: `1625`
#' -   `gbdfcae bgc cg cgb`: `8717`
#' -   `fgae cfgab fg bagce`: `4315`
#'
#' Adding all of the output values in this larger example produces `61229`.
#'
#' For each entry, determine all of the wire/segment connections and decode
#' the four-digit output values. *What do you get if you add up all of the
#' output values?*
#'
#' @param x some data
#' @return For Part One, `f08a(x)` returns .... For Part Two,
#'   `f08b(x)` returns ....
#' @export
#' @examples
#' f08a(example_data_08())
#' f08b(example_data_08())
f08a <- function(x) {

  x <- vapply(x, function(e) sum(nchar(e[[2]]) %in% c(2, 3, 4, 7)), numeric(1))

  return(sum(x))
}


#' @rdname day08
#' @export
f08b <- function(x) {

  sum(
    vapply(x, function(e) {
      key <- f08_find_key(e[[1]])
      as.numeric(paste0(f08_decode(e[[2]], key), collapse = ""))
    }, numeric(1))
  )

}


f08_find_key <- function(y) {

  y2 <- strsplit(y, "")

  # "Easy digits"
  d1 <- which(lengths(y2) == 2)
  d4 <- which(lengths(y2) == 4)
  d7 <- which(lengths(y2) == 3)
  d8 <- which(lengths(y2) == 7)

  # 9 is almost 4 and 7
  d9 <- which(
    vapply(y2, function(e) length(setdiff(e, union(y2[[d7]], y2[[d4]]))) == 1, logical(1))
    &
    lengths(y2) == 6
  )

  # 9 is 5 and 1
  d5 <-setdiff(
    which(vapply(y2, function(e) setequal(union(e, y2[[d1]]), y2[[d9]]), logical(1))),
    d9
  )

  # 6 has one less than 8, one more than 5 and is not 9
  d6 <- setdiff(which(
    vapply(y2, function(e) length(setdiff(y2[[d8]], e)) == 1, logical(1))
    &
    vapply(y2, function(e) length(setdiff(e, y2[[d5]])) == 1, logical(1))
  ), d9)

  # 0 has 6 segments and it not 6 or 9
  d0 <- setdiff(which(lengths(y2) == 6), c(d6, d9))
  d2 <-


  # 3 and 5 have one less than 9
  d3 <- setdiff(
    which(
      vapply(y2, function(e) length(setdiff(y2[[d9]], e)) == 1, logical(1))
      &
      lengths(y2) == 5
    ),
    d5
  )

  d2 <- setdiff(seq_along(y), c(d0, d1, d3, d4, d5, d6, d7, d8, d9))

  y2 <- lapply(y2, sort)

  return(y2[c(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)])

}

f08_decode <- function(y, key) {

  y2 <- strsplit(y, "")
  y2 <- lapply(y2, sort)

  # -1 because we start indexing at 0
  v <- match(y2, key) - 1

  return(v)

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day08
#' @export
example_data_08 <- function(example = 1) {
  l <- list(
    a = list(
      list(c("be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"), c("fdgacbe", "cefdb", "cefbgd", "gcbe")),
      list(c("edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"), c("fcgedb", "cgb", "dgebacf", "gc")),
      list(c("fgaebd", "cg", "bdaec", "gdafb", "agbcfd", "gdcbef", "bgcad", "gfac", "gcb", "cdgabef"), c("cg", "cg", "fdcagb", "cbg")),
      list(c("fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab", "fgdeca", "fcdbega"), c("efabcd", "cedba", "gadfec", "cb")),
      list(c("aecbfdg", "fbg", "gf", "bafeg", "dbefa", "fcge", "gcbea", "fcaegb", "dgceab", "fcbdga"), c("gecf", "egdcabf", "bgf", "bfgea")),
      list(c("fgeab", "ca", "afcebg", "bdacfeg", "cfaedg", "gcfdb", "baec", "bfadeg", "bafgc", "acf"), c("gebdcfa", "ecba", "ca", "fadegcb")),
      list(c("dbcfg", "fgd", "bdegcaf", "fgec", "aegbdf", "ecdfab", "fbedc", "dacgb", "gdcebf", "gf"), c("cefg", "dcbef", "fcge", "gbcadfe")),
      list(c("bdfegc", "cbegaf", "gecbf", "dfcage", "bdacg", "ed", "bedf", "ced", "adcbefg", "gebcd"), c("ed", "bcgafe", "cdgba", "cbgef")),
      list(c("egadfb", "cdbfeg", "cegd", "fecab", "cgb", "gbdefca", "cg", "fgcdab", "egfdb", "bfceg"), c("gbdfcae", "bgc", "cg", "cgb")),
      list(c("gcafb", "gcf", "dcaebfg", "ecagb", "gf", "abcdeg", "gaef", "cafbge", "fdbac", "fegbdc"), c("fgae", "cfgab", "fg", "bagce"))
    ),
    b = list(
      list(c("acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"),
           c("cdfeb", "fcadb", "cdfeb", "cdbaf"))
    )
  )
  l[[example]]
}
