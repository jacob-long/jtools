context("just/not")

x <- 1:5
y <- 3:8

test_that("vector subsetting works", {
  expect_equal(x %just% y, c(3, 4, 5))
  expect_equal(x %not% y, c(1, 2))
})

test_that("vector subassignment works", {
  x %just% y <- 5
  expect_equal(x, c(1, 2, 5, 5, 5))
  x <- 1:5
  x %not% y <- 5
  expect_equal(x, c(5, 5, 3, 4, 5))
})

df <- tibble::tribble(
  ~A,      ~B,    ~C,
   1,   "red",  TRUE,
   2,   "red", FALSE,
   3,  "blue",  TRUE
)

test_that("data frame subsetting works [by name]", {
  expect_equal(df %just% "A", tibble::tibble(A = c(1, 2, 3)))
  expect_equal(df %not% c("A", "B"), tibble::tibble(C = c(TRUE, FALSE, TRUE)))
})

test_that("data frame subsetting works [by index]", {
  expect_equal(df %just% 1, tibble::tibble(A = c(1, 2, 3)))
  expect_equal(df %not% c(1, 2), tibble::tibble(C = c(TRUE, FALSE, TRUE)))
})

test_that("data frame subassignment works [by name]", {
  df %just% "A" <- c(3, 2, 1)
  expect_equal(df, 
               tibble::tribble(
                 ~A,      ~B,    ~C,
                 3,   "red",  TRUE,
                 2,   "red", FALSE,
                 1,  "blue",  TRUE
               ))
  df <- tibble::tribble(
    ~A,      ~B,    ~C,
    1,   "red",  TRUE,
    2,   "red", FALSE,
    3,  "blue",  TRUE
  )
  df %not% c("A", "B") <- c(FALSE, TRUE, FALSE)
  expect_equal(df, 
               tibble::tribble(
                 ~A,      ~B,    ~C,
                 1,   "red",  FALSE,
                 2,   "red",   TRUE,
                 3,  "blue",  FALSE
               ))
  df <- tibble::tribble(
    ~A,      ~B,    ~C,
    1,   "red",  TRUE,
    2,   "red", FALSE,
    3,  "blue",  TRUE
  )
})


test_that("data frame subassignment works [by index]", {
  df %just% 1 <- c(3, 2, 1)
  expect_equal(df, 
               tibble::tribble(
                 ~A,      ~B,    ~C,
                 3,   "red",  TRUE,
                 2,   "red", FALSE,
                 1,  "blue",  TRUE
               ))
  df <- tibble::tribble(
    ~A,      ~B,    ~C,
    1,   "red",  TRUE,
    2,   "red", FALSE,
    3,  "blue",  TRUE
  )
  df %not% c(1, 2) <- c(FALSE, TRUE, FALSE)
  expect_equal(df, 
               tibble::tribble(
                 ~A,      ~B,    ~C,
                 1,   "red",  FALSE,
                 2,   "red",   TRUE,
                 3,  "blue",  FALSE
               ))
  df <- tibble::tribble(
    ~A,      ~B,    ~C,
    1,   "red",  TRUE,
    2,   "red", FALSE,
    3,  "blue",  TRUE
  )
})

df <- as.matrix(df[1:2])

test_that("matrix subsetting works [by name]", {
  expect_equal(df %just% "A", c("1", "2", "3"))
  expect_equal(df %not% c("B"), c("1", "2", "3"))
})

test_that("matrix subsetting works [by index]", {
  expect_equal(df %just% 1, c("1", "2", "3"))
  expect_equal(df %not% c(1), c("red", "red", "blue"))
})

test_that("matrix subassignment works [by name]", {
  df %just% "A" <- c("3", "2", "1")
  expect_equal(df, 
               as.matrix(tibble::tribble(
                 ~A,      ~B, 
                 3,   "red",
                 2,   "red",
                 1,  "blue",
               )))
  df <- as.matrix(tibble::tribble(
    ~A,      ~B,  
    1,   "red",  
    2,   "red", 
    3,  "blue", 
  ))
  df %not% c("A") <- c("blue", "blue", "red")
  expect_equal(df, 
               as.matrix(tibble::tribble(
                 ~A,      ~B, 
                 1,   "blue",  
                 2,   "blue",  
                 3,  "red",  
               )))
  df <- as.matrix(tibble::tribble(
    ~A,      ~B,
    1,   "red",  
    2,   "red", 
    3,  "blue",  
  ))
})

test_that("matrix subassignment works [by index]", {
  df %just% 1 <- c("3", "2", "1")
  expect_equal(df, 
               as.matrix(tibble::tribble(
                 ~A,      ~B,    
                 3,   "red",  
                 2,   "red", 
                 1,  "blue",  
               )))
  df <- as.matrix(tibble::tribble(
    ~A,      ~B,  
    1,   "red",  
    2,   "red", 
    3,  "blue",  
  ))
  df %not% c(1) <- c("blue", "blue", "red")
  expect_equal(df, 
               as.matrix(tibble::tribble(
                 ~A,      ~B,   
                 1,   "blue",  
                 2,   "blue",   
                 3,  "red",  
               )))
})