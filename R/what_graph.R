what_graph <- function(df, var1, var2 = as.numeric(), fill = as.numeric(), palette = c()){

  numeric_count <- 0
  factor_count <- 0
  total_count <- c()

  # Classify First Variable
  if (is.numeric(var1)){

    numeric_count <- numeric_count + 1
    total_count <- append(total_count, var1)

  } else if (is.factor(var1)){

    factor_count <- factor_count + 1
    total_count <- append(total_count, var1)

  }

  else {

    stop("please input your first variable as numeric or factor type")

  }


  # Classify Second Variable
  if (is.numeric(var2)){

    numeric_count <- numeric_count + 1
    total_count <- append(total_count, var2)


  } else if (is.factor(var2){

    factor_count <- factor_count + 1
    total_count <- append(total_count, var2)

  }

  else {

    stop("please input your second variable as numeric or factor type")

  }


  # Classify Third Variable  (Variable used for fill/color)
  if (is.numeric(fill)){

    fill_col <- fill
    total_count <- append(total_count, fill)

  } else if (is.factor(fill)){

    fill_col <- fill
    total_count <- append(total_count, fill)
  }

  else {

    stop("please input your third variable as numeric or factor type")

  }


  # Store Color Palette in object

  palette <- palette


  # Choose Which Graph to use

  if (length(total_count) == 1 & is.numeric(var1)){

    graphs <- c("Histogram", "Density Plot")
    return(graphs)
  }
  else if (length(total_count) == 1 & is.factor(var1)){

    graphs <- c("Barplot", "Lollipop", "Waffle", "Word Cloud", " Dougnut", "Treemap", "Pie")
    return(graphs)
  }

  # If input 2 variables
  else if (length(total_count) == 2 & is.numeric(var1) & is.numeric(var2)){

    graphs <- c("Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot")
    return(graphs)
  }

  else if (length(total_count) == 2 & is.numeric(var1) & is.factor(var2)){

    graphs <- c("Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot", "Ridge Line", "PCA", "Lollipop")
    return(graphs)
  }

  else if (length(total_count) == 2 & is.factor(var1) & is.numeric(var2)){

    graphs <- c("Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot", "Ridge Line", "PCA", "Lollipop")
    return(graphs)

  } else if (length(total_count) == 2 & is.factor(var1) & is.factor(var2)) {

    graphs <- c("Venn Diagram", "Treemap", "Sunburst", "Barplot", "Stacked Bartplot", "Grouped Barplot", "Lollipop", "Spider Plot")
    return(graphs)
  }   else {

    return(NULL)
  }


}
