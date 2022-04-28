#' Gives a list of potential graphs and charts
#'
#' @param df A dataframe
#' @param n Number of variables to graph
#' @param var1 A variable from dataframe
#' @param var2 A variable from dataframe (optional)
#' @param fill A variable from dataframe used as the fill/color aesthetic (optional)
#'
#' @return A vector of possible graphs
#' @examples
#' what_graph(iris, 1, Sepal.Length)
#'
#' @import tidyverse
#'
#' @export

what_graph <- function(df, n, var1, var2 = NULL, fill = NULL){

  library(tidyverse)
  numeric_count <- 0
  factor_count <- 0
  total_count <- 0

  # Classify First Variable
  one <- df %>%
    pull({{var1}})
  if (n == 1){

    one <- df %>%
      pull({{var1}})

    if (is.numeric(one)){

      numeric_count <- numeric_count + 1
      total_count <- total_count + 1

    } else if (is.factor(one)){

      total_count <- total_count + 1


    }

    else {

      stop("please input your first variable as numeric or factor type")

    }

  }


  else if (n == 2){

    one <- df %>%
      pull({{var1}})

    if (is.numeric(one)){

      numeric_count <- numeric_count + 1
      total_count <- total_count + 1

    } else if (is.factor(one)){

      total_count <- total_count + 1


    }

    else {

      stop("please input your first variable as numeric or factor type")

    }

    two <- df %>%
      pull({{var2}})

    if (is.numeric(two)){

      numeric_count <- numeric_count + 1
      total_count <- total_count + 1



    } else if (is.factor(two)){

      total_count <- total_count + 1

    }

    else {

      stop("please input your second variable as numeric or factor type")

    }


  }



  else if (n == 3){

    one <- df %>%
      pull({{var1}})

    if (is.numeric(one)){

      numeric_count <- numeric_count + 1
      total_count <- total_count + 1

    } else if (is.factor(one)){

      total_count <- total_count + 1


    }

    else {

      stop("please input your first variable as numeric or factor type")

    }

    two <- df %>%
      pull({{var2}})

    if (is.numeric(two)){

      numeric_count <- numeric_count + 1
      total_count <- total_count + 1



    } else if (is.factor(two)){

      total_count <- total_count + 1

    }

    else {

      stop("please input your second variable as numeric or factor type")

    }

    three <- df %>%
      pull({{fill}})

    if (is.numeric(three)){

      fill_col <- three
      total_count <- total_count + 1
      numeric_count <- numeric_count + 1


    } else if (is.factor(three)){

      fill_col <- three
      total_count <- total_count + 1

    }

    else {

      stop("please input your third variable as numeric or factor type")

    }
  }


  # Choose Which Graph to use

  if (total_count == 1 & is.numeric(one)){

    graphs <- c("Histogram", "Density Plot")
    return(graphs)
  }
  else if (total_count == 1 & is.factor(one)){

    graphs <- c("Barplot", "Lollipop", "Waffle", "Word Cloud", " Dougnut", "Treemap", "Pie")
    return(graphs)
  }

  # If input 2 variables
  else if (total_count == 2 & numeric_count == 2){

    graphs <- c("Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot")
    return(graphs)
  }

  else if (total_count == 2 & numeric_count == 1){

    graphs <- c("Grouped Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot", "Ridge Line", "PCA", "Lollipop")
    return(graphs)
  }


  else if (total_count == 2 & numeric_count == 0) {

    graphs <- c("Venn Diagram", "Treemap", "Sunburst", "Barplot", "Stacked Barplot", "Grouped Barplot", "Lollipop", "Spider Plot")
    return(graphs)
  }


  # If input 3 variables

  else if (total_count == 3 & numeric_count == 3){

    graphs <- c("Scatter Plot", "Stacked Area Plot", " Box Plot", "Violin Plot", "Stream Graph", "Line Plot", "3D Scatter")
    return(graphs)
  }

  else if (total_count == 3 & numeric_count == 2){

    graphs <- c("Grouped Scatter Plot", "Stacked Area Plot", " Box Plot", "Violin Plot", " Stream Graph", "Line Plot", "PCA")
    return(graphs)
  }


  else if (total_count == 3 & numeric_count == 1) {

    graphs <- c("Barplot", "Boxplot", "Violin", "Treemap", "Sunburst", "Arc", "Network")
    return(graphs)
  }

  else if (total_count == 3 & numeric_count == 0) {

    graphs <- c("Venn Diagram", "Treemap", "Sunburst", "Barplot", "Stacked Barplot",     "Grouped Barplot", "Lollipop", "Spider Plot")
    return(graphs)

  }

  else {

    return(NULL)
  }

}


#hi

# hi this is a test from sam
#hello sam!

#hey guys!!

#meowwwwwwww


