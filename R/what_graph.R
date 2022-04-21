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
  if (is.numeric(var2) == TRUE){

    numeric_count <- numeric_count + 1
    total_count <- append(total_count, var2)


  } else if (is.factor(var2) == TRUE){

    factor_count <- factor_count + 1
    total_count <- append(total_count, var2)

  }

  else {

    stop("please input your second variable as numeric or factor type")

  }


  # Classify Third Variable  (Variable used for fill/color)
  if (is.numeric(fill) == TRUE){

    fill_col <- fill
    total_count <- append(total_count, fill)

  } else if (is.factor(fill) == TRUE){

    fill_col <- fill
    total_count <- append(total_count, fill)
  }

  else {

    stop("please input your third variable as numeric or factor type")

  }


  # Store Color Palette in object

  palette <- palette


  # Choose Which Graph to use

  if (length(total_count) == 1 & is.numeric(var1) == TRUE){

    graphs <- c("Histogram", "Density Plot")
    return(graphs)
  }
  else if (length(total_count) == 1 & is.factor(var1) == TRUE){

    graphs <- c("Barplot", "Lollipop", "Waffle", "Word Cloud", " Dougnut", "Treemap", "Pie")
    return(graphs)
  }

  # If input 2 variables
  else if (length(total_count) == 2 & is.numeric(var1) == TRUE & is.numeric(var2) == TRUE){

    graphs <- c("Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot")
    return(graphs)
  }

  else if (length(total_count) == 2 & is.numeric(var1) == TRUE & is.factor(var2) == TRUE){

    graphs <- c("Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot", "Ridge Line", "PCA", "Lollipop")
    return(graphs)
  }

  else if (length(total_count) == 2 & is.factor(var1) == TRUE & is.numeric(var2) == TRUE){

    graphs <- c("Scatter Plot", "Histogram", " Box Plot", "Violin Plot", " Density Plot", "Line Plot", "Ridge Line", "PCA", "Lollipop")
    return(graphs)

  } else if (length(total_count) == 2 & is.factor(var1) == TRUE & is.factor(var2) == TRUE) {

    graphs <- c("Venn Diagram", "Treemap", "Sunburst", "Barplot", "Stacked Bartplot", "Grouped Barplot", "Lollipop", "Spider Plot")
    return(graphs)
  }   else {

    return(NULL)
  }


}

#hi

# hi this is a test from sam
#hello sam!

#hey guys!!

#meowwwwwwww
