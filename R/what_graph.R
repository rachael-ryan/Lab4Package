what_graph <- function(df, var1, var2, fill, palette = "Dark2"){
  cat = 0
  cont = 0
  if(is.factor(var1) == TRUE) {cat = cat + 1}
  else if(is.numeric(var1) == TRUE){cont = cont + 1}
  else(stop("Your first variable entered is not in a graphable form. Please change it to a factor or numeric type."))
  if(is.factor(var2) == TRUE) {cat = cat + 1}
  else if(is.numeric(var2) == TRUE){cont = cont + 1}
  else(stop("Your second variable entered is not in a graphable form. Please change it to a factor or numeric type."))
  if(is.factor(fill) == FALSE & is.numeric(fill) == FALSE)
    {stop("Your third variable entered is not in a graphable form. Please change it to a factor or numeric type.")}
  else(){}
}

#hi

# hi this is a test from sam
