test_that("what_graph works", {
  expect_equal(what_graph(iris, 1, Sepal.Length), "Histogram"    "Density Plot")
})
