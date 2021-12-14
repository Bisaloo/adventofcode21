#' Day 14: Extended Polymerization
#'
#' [Extended Polymerization](https://adventofcode.com/2021/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' The incredible pressures at this depth are starting to put a strain on
#' your submarine. The submarine has
#' [polymerization](https://en.wikipedia.org/wiki/Polymerization) equipment
#' that would produce suitable materials to reinforce the submarine, and
#' the nearby volcanically-active caves should even have the necessary
#' input elements in sufficient quantities.
#'
#' The submarine manual contains [instructions]{title="HO
#'
#' HO -> OH"} for finding the optimal polymer formula; specifically, it
#' offers a *polymer template* and a list of *pair insertion* rules (your
#' puzzle input). You just need to work out what polymer would result after
#' repeating the pair insertion process a few times.
#'
#' For example:
#'
#'     NNCB
#'
#'     CH -> B
#'     HH -> N
#'     CB -> H
#'     NH -> C
#'     HB -> C
#'     HC -> B
#'     HN -> C
#'     NN -> C
#'     BH -> H
#'     NC -> B
#'     NB -> B
#'     BN -> B
#'     BB -> N
#'     BC -> B
#'     CC -> N
#'     CN -> C
#'
#' The first line is the *polymer template* - this is the starting point of
#' the process.
#'
#' The following section defines the *pair insertion* rules. A rule like
#' `AB -> C` means that when elements `A` and `B` are immediately adjacent,
#' element `C` should be inserted between them. These insertions all happen
#' simultaneously.
#'
#' So, starting with the polymer template `NNCB`, the first step
#' simultaneously considers all three pairs:
#'
#' -   The first pair (`NN`) matches the rule `NN -> C`, so element `C` is
#'     inserted between the first `N` and the second `N`.
#' -   The second pair (`NC`) matches the rule `NC -> B`, so element `B` is
#'     inserted between the `N` and the `C`.
#' -   The third pair (`CB`) matches the rule `CB -> H`, so element `H` is
#'     inserted between the `C` and the `B`.
#'
#' Note that these pairs overlap: the second element of one pair is the
#' first element of the next pair. Also, because all pairs are considered
#' simultaneously, inserted elements are not considered to be part of a
#' pair until the next step.
#'
#' After the first step of this process, the polymer becomes `NCNBCHB`.
#'
#' Here are the results of a few steps using the above rules:
#'
#'     Template:     NNCB
#'     After step 1: NCNBCHB
#'     After step 2: NBCCNBBBCBHCB
#'     After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
#'     After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
#'
#' This polymer grows quickly. After step 5, it has length 97; After step
#' 10, it has length 3073. After step 10, `B` occurs 1749 times, `C` occurs
#' 298 times, `H` occurs 161 times, and `N` occurs 865 times; taking the
#' quantity of the most common element (`B`, 1749) and subtracting the
#' quantity of the least common element (`H`, 161) produces
#' `1749 - 161 = 1588`.
#'
#' Apply 10 steps of pair insertion to the polymer template and find the
#' most and least common elements in the result. *What do you get if you
#' take the quantity of the most common element and subtract the quantity
#' of the least common element?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f14a(x)` returns .... For Part Two,
#'   `f14b(x)` returns ....
#' @export
#' @examples
#' f14a(example_data_14())
#' f14b(example_data_14())
f14a <- function(x) {

  for (i in 1:10) {
    x$template <- f14_polymerize(x$template, x$rules)
  }

  f14_count_elements(x$template)

}

f14_polymerize <- function(template, rules) {

  oligos <- slider::slide_chr(
    strsplit(template, "")[[1]],
    ~ f14_insert(.x, rules = rules),
    .after = 1,
  )

  return(paste(na.omit(oligos), collapse = ""))

}

f14_insert <- function(pair, rules) {

  return(paste0(pair[1], rules$insert[rules$pair == paste0(pair[1], pair[2])]))

}

f14_count_elements <- function(poly) {

  monos <- strsplit(poly, "")[[1]]

  return(diff(range(table(monos))))

}


#' @rdname day14
#' @export
f14b <- function(x) {

  # Old pair -->> 2 new pairs
  rules <- x$rules %>%
    dplyr::mutate(new_pair1 = paste0(substr(pair, 1, 1), insert),
                  new_pair2 = paste0(insert, substr(pair, 2, 2))) %>%
    tidyr::pivot_longer(dplyr::starts_with("new_pair"),
                        names_to = c(".value", "id"),
                        names_pattern = "(new_pair)(\\d)")

  # Create transition matrix
  polym_matrix <- matrix(
    0,
    ncol = nrow(x$rules),
    nrow = nrow(x$rules),
    dimnames = rep(list(sort(x$rules$pair)), 2)
  )

  for (i in seq_len(nrow(rules))) {
    polym_matrix[rules$pair[i], rules$new_pair[i]] <- 1
  }

  # Create initial vector of pair counts
  initial <- na.omit(slider::slide_chr(
    strsplit(x$template, "")[[1]],
    paste,
    collapse = "",
    .after = 1,
    .complete = TRUE
  ))
  initial <- factor(initial, levels = sort(x$rules$pair))

  tab_dimers <- table(initial)

  for (i in 1:40) {
    tab_dimers <- tab_dimers %*% polym_matrix
  }

  tab_monomers <- c(by(c(tab_dimers), substr(colnames(tab_dimers), 2, 2), sum))
  first_monomer <- setNames(rep_len(0, length(tab_monomers)), names(tab_monomers))
  first_monomer[substr(x$template, 1, 1)] <- 1

  tab_monomers <- tab_monomers + first_monomer

  return(max(tab_monomers) - min(tab_monomers))

}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day14
#' @export
example_data_14 <- function(example = 1) {
  l <- list(
    a = list(
      template = "NNCB",
      rules = tibble::tribble(
        ~ pair, ~ insert,
         "CH" ,    "B"  ,
         "HH" ,    "N"  ,
         "CB" ,    "H"  ,
         "NH" ,    "C"  ,
         "HB" ,    "C"  ,
         "HC" ,    "B"  ,
         "HN" ,    "C"  ,
         "NN" ,    "C"  ,
         "BH" ,    "H"  ,
         "NC" ,    "B"  ,
         "NB" ,    "B"  ,
         "BN" ,    "B"  ,
         "BB" ,    "N"  ,
         "BC" ,    "B"  ,
         "CC" ,    "N"  ,
         "CN" ,    "C"
      )
    )
  )
  l[[example]]
}
