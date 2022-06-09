#' Graph Options
#'
#' what_graph is a function used to analyze what graphs can be created from certain variables depending on the quantity and data type of the variables.
#'
#' @param df A data frame.
#' @param ... 1-3 unquoted column names of the specified df argument, separated by commas.
#'
#' @return what_graph returns a character vector of possible graphs that can be created from the variables that are input.
#'
#' @details
#' It is important that before this function is used, the columns of interest have the correct data type.
#' Specifically, a categorical or discrete variable needs to have a data type of 'factor' and a continuous variable needs to have a data type of 'numeric'
#'
#' If the variable types are not numeric or factor the function will not return a list of possible graphs.
#'
#' @examples
#' what_graph(iris, Sepal.Length, Sepal.Width)
#'
#' @import dplyr
#'
#' @export
what_graph <- function(df, ...){

  dots <- rlang::enexprs(...)

  numeric_count <- 0
  factor_count <- 0
  total_count <- 0

  # Classify First Variable

  one <- df %>%
    pull(dots[[1]])

  if (is.numeric(one)){

    numeric_count <- numeric_count + 1
    total_count <- total_count + 1

  } else if (is.factor(one)){

    total_count <- total_count + 1

  }

  else {stop("please input your first variable as numeric or factor type")}

  # Second Variable

  if (length(dots) == 2 | length(dots) == 3){

    two <- df %>%
      pull(dots[[2]])

    if (is.numeric(two)){

      numeric_count <- numeric_count + 1
      total_count <- total_count + 1

    } else if (is.factor(two)){

      total_count <- total_count + 1

    }

    else {stop("please input your second variable as numeric or factor type")}

  }

  # Third Variable

  if (length(dots) == 3){

    three <- df %>%
      pull(dots[[3]])

    if (is.numeric(three)){

      total_count <- total_count + 1
      numeric_count <- numeric_count + 1

    } else if (is.factor(three)){

      total_count <- total_count + 1

    }

    else {stop("please input your third variable as numeric or factor type")}

  }

  if (length(dots) > 3){stop("This function takes a maximum of three variables")}


  # Choose Which Graph to use
  # If input 1 variable

  if (total_count == 1 & is.numeric(one)){

    graphs <- c("Histogram", "Density Plot", "Normal Probability Plot")
    return(graphs)

  } else if (total_count == 1 & is.factor(one)){

    graphs <- c("Bar Chart")
    return(graphs)


    # If input 2 variables

  } else if (total_count == 2 & numeric_count == 2){

    graphs <- c("Scatter Plot")

  } else if (total_count == 2 & numeric_count == 1){

    graphs <- c("Column Chart", "Stacked Histogram", "Box Plot", "Violin Plot", "Multi Density Plot")
    return(graphs)

  } else if (total_count == 2 & numeric_count == 0) {

    graphs <- c("Stacked Bar Chart", "Grouped Bar Chart", "Bubble Plot")
    return(graphs)
  }


  # If input 3 variables

  else if (total_count == 3 & numeric_count == 3){

    graphs <- c("Gradient Scatter Plot")
    return(graphs)

  } else if (total_count == 3 & numeric_count == 2){

    graphs <- c("Grouped Scatter Plot")
    return(graphs)

  } else if (total_count == 3 & numeric_count == 1) {

    graphs <- c("Stacked Column Chart", "Grouped Box Plot", "Grouped Violin Plot" )
    return(graphs)

  } else if (total_count == 3 & numeric_count == 0) {

    print("No known ggplot graphs can be created with three seperate discrete variables")

  }

}

