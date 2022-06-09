test_that("The what_graph funciton works correctly", {

  scatter <- what_graph(iris, Sepal.Length, Sepal.Width)

  box <- what_graph(iris, Sepal.Length, Species)[3]

  expect_equal(scatter, "Scatter Plot")
  expect_equal(box, "Box Plot")

})

