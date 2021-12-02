test_input <- tibble::tribble(
  ~ direction, ~ distance,
  "forward", 5,
  "down", 5,
  "forward", 8,
  "up", 3,
  "down", 8,
  "forward", 2
)

solve_puzzle1 <- function(input) {

  input$distance[input$direction == "up"] <- - input$distance[input$direction == "up"]

  sum(input$distance[input$direction != "forward"]) *
    sum(input$distance[input$direction == "forward"])

}

solve_puzzle2 <- function(input) {

  input$vertical_dist <- 0
  input$vertical_dist[input$direction == "down"] <- input$distance[input$direction == "down"]
  input$vertical_dist[input$direction == "up"] <- - input$distance[input$direction == "up"]

  input$horizontal_dist <- 0
  input$horizontal_dist[input$direction == "forward"] <- input$distance[input$direction == "forward"]

  aim <- cumsum(input$vertical_dist)

  depth <- sum(aim * input$horizontal_dist)

  depth * sum(input$horizontal_dist)

}
