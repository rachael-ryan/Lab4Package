

what_graph <- function(df, var1, var2 = as.numeric(), fill = as.numeric()){

  numeric_count <- 0
  factor_count <- 0
  total_count <- 0

  # Classify First Variable
  one <- df %>%
    pull({{var1}})
  print(one)

  if (is.numeric(one)){

    numeric_count <- numeric_count + 1
    total_count <- total_count + 1

  } else if (is.factor(one)){

    total_count <- total_count + 1


  }

  else {

    stop("please input your first variable as numeric or factor type")

  }



  # Classify Second Variable

  two <- df %>%
    pull({{var2}})
  print(two)

  if (is.numeric(two)){

    numeric_count <- numeric_count + 1
    total_count <- total_count + 1



  } else if (is.factor(two)){

    total_count <- total_count + 1


  }

  else {

    stop("please input your second variable as numeric or factor type")

  }


  # Classify Third Variable  (Variable used for fill/color)
  three <- df %>%
    pull({{fill}})

  if (is.numeric(three)){

    fill_col <- fill
    total_count <- append(total_count, fill)

  } else if (is.factor(three)){

    fill_col <- fill
    total_count <- append(total_count, fill)
  }

  else {

    stop("please input your third variable as numeric or factor type")

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

    graphs <- c("Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot", "Ridge Line", "PCA", "Lollipop")
    return(graphs)
  }


  else if (total_count == 2 & numeric_count == 0) {

    graphs <- c("Venn Diagram", "Treemap", "Sunburst", "Barplot", "Stacked Bartplot", "Grouped Barplot", "Lollipop", "Spider Plot")
    return(graphs)
  }   else {

    return(NULL)
  }


}
iris %>%
  what_graph(Sepal.Length)
