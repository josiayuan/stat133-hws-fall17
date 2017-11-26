# ===================================================================
# Title:Function Tests for HW04
# Description:
#   This script test functions created for HW04
# Input(s): R script "functions.R"
# Output(s): R script "tests.R"
# Author: Josia Yuan
# Date: 11-11-2017
# ===================================================================

#load packages
library(testthat)

#source
source('../code/functions.R',chdir = TRUE)


#Unit test for Function remove_missing
context('testing Function remove-missing()')
test_that("Function integrity", {
  #Value returned is without missing value
  a <- c(1, 4, 7, NA, 10)
  expect_that(remove_missing(a), equals(c(1,4,7,10)))
  #Function works with vector w/o missing value
  b <- c(1,4,7,10)
  expect_that(remove_missing(b), equals(c(1,4,7,10)))
  #Function works with types other than numeric
  d <- c('a','b',NA)
  expect_that(remove_missing(d), equals(c('a','b')))
  #Function works with NA vector
  e <- c(NA,NA)
  expect_that(remove_missing(e), equals(logical(0)))
  })

#Unit test for Function get_minimum
context('testing Function get_minimum()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_minimum(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_minimum(c("a","b")), throws_error() )
  #output is of the correct value
  expect_that(get_minimum(a), equals(1))
  #output is of type numeric
  expect_that(is.numeric(get_minimum(a)),equals(TRUE))
}
)


#Unit test for Function get_maximum
context('testing Function get_maximum()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_maximum(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_maximum(c("a","b")), throws_error() )
  #output is of the correct value
  expect_that(get_maximum(a), equals(10))
  #output is of type numeric
  expect_that(is.numeric(get_maximum(a)),equals(TRUE))
}
)

#Unit test for Function get_range
context('testing Function get_range()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_range(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_range(c("a","b")), throws_error() )
  #output is of the correct value
  expect_that(get_range(a), equals(9))
  #output is of type numeric
  expect_that(is.numeric(get_range(a)),equals(TRUE))
}
)

#Unit test for Function get_percentile10()
context('testing Function get_percentile10()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_percentile10(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_percentile10()(c("a","b")), throws_error() )
  #output is correct
  expect_that(get_percentile10(a),equals(1.9))
  #output is of type numeric
  expect_that(is.numeric(get_percentile10(a)),equals(TRUE))
}
)

#Unit test for Function get_percentile90()
context('testing Function get_percentile90()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_percentile90(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_percentile90()(c("a","b")), throws_error() )
  #output is correct
  expect_that(get_percentile90(a),equals(9.1))
  #output is of type numeric
  expect_that(is.numeric(get_percentile90(a)),equals(TRUE))
}
)

#Unit test for Function get_median
context('testing Function get_median()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_median(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_median(c("a","b")), throws_error() )
  #output is correct
  expect_that(get_median(a),equals(median(a,na.rm = TRUE)))
  #output is of type numeric
  expect_that(is.numeric(get_median(a)),equals(TRUE))
}
)

#Unit test for Function get_averge
context('testing Function get_averge()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_averge(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_averge(c("a","b")), throws_error() )
  #output is correct
  expect_that(get_averge(a),equals(mean(a,na.rm = TRUE)))
  #output is of type numeric
  expect_that(is.numeric(get_averge(a)),equals(TRUE))
}
)

#Unit test for Function get_stdev
context('testing Function get_stdev()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_stdev(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_stdev(c("a","b")), throws_error() )
  #output is correct
  expect_that(get_stdev(a),equals(sd(a,na.rm = TRUE)))
  #output is of type numeric
  expect_that(is.numeric(get_stdev(a)),equals(TRUE))
}
)

#Unit test for Function get_quartile1
context('testing Function get_quartile1()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_quartile1(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_quartile1(c("a","b")), throws_error() )
  #output is correct
  expect_that(get_quartile1(a),equals(3.25))
  #output is of type numeric
  expect_that(is.numeric(get_quartile1(a)),equals(TRUE))
}
)

#Unit test for Function get_quartile3
context('testing Function get_quartile3()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- get_quartile1(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #throw error message if input not numeric
  expect_that( get_quartile3(c("a","b")), throws_error() )
  #output is correct
  expect_that(get_quartile3(a),equals(7.75))
  #output is of type numeric
  expect_that(is.numeric(get_quartile3(a)),equals(TRUE))
}
)

#Unit test for Function count_missing
context('testing Function count_missing()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- count_missing(a)
  #output is of length 1
  expect_that(length(test),equals(1))
  #output is correct
  expect_that(count_missing(a),equals(1))
  #Function works with vector without missing value
  b <- c(1,2,3)
  test <- count_missing(b)
  expect_that(test,equals(0))
  #output is of type numeric
  expect_that(is.numeric(count_missing(a)),equals(TRUE))
}
)

#Unit test for Function summary_stats()
context('testing Function summary_stats()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  test <- summary_stats(a)
  #output is of length 11
  expect_that(length(test),equals(11))
  #output is of the right type list
  expect_that(is.list(test),equals(TRUE))
  #throw error message if input not numeric
  b <-c('a','b','c')
  expect_that(summary_stats(b), throws_error() )
  #Works with vector w/p missing value
  c <- c(1, 4, 7, 10)
  test2 <- summary_stats(c)
  expect_output(str(test2), "List of 11")
}
)


#Unit test for Function print_stats()
context('testing Function print_stats()')
test_that("function integrity",{
  a <- c(1, 4, 7, NA, 10)
  #output is not of any type but printing function
  expect_is(print_stats(a),'NULL')
  #throw error message if input not numeric
  b <-c('a','b','c')
  expect_that(pringt_stats(b), throws_error() )
  #functions takes only one vector
  expect_that(pringt_stats(a,b), throws_error() )
  #functions takes only one vector
  c <- c(1,2,3)
  expect_that(pringt_stats(a,b,c), throws_error() )
  
}
)

#Unit test for Function rescale100()
context('testing Function rescale100()')
test_that("function integrity",{
  b <- c(18,15,16,4,17,9)
  test <- rescale100(b,xmin=0,xmax=20)
  #output is of the same length
  expect_that(length(test),equals(length(b)))
  #output is correct
  expect_that(test,equals(c(90,75,80,20,85,45)))
  #output is of type numeric
  expect_that(is.numeric(test),equals(TRUE))
  #no argument for NA
  test2 <- rescale100(a,xmin=0,xmax=10)
  expect_that(is.double(test2),equals(TRUE))
}
)

#Unit test for Function drop_lowest()
context('testing Function drop_lowest()')
test_that("function integrity",{
  b <- c(10, 10, 8.5, 4, 7, 9)
  test <- drop_lowest(b)
  #output is of length 6
  expect_that(length(test),equals(5))
  #output is correct
  expect_that(test,equals(c(7.0,8.5,9.0,10.0,10.0)))
  #output is of type numeric
  expect_that(is.numeric(test),equals(TRUE))
  #function only drops one of the lowest
  c <- c(1,1,2,3)
  test2 <- drop_lowest(c)
  expect_that(test2,equals(c(1,2,3)))
}
)

#Unit test for Function score_homework()
context('testing Function score_homework()')
test_that("function integrity",{
  hws <- c(100, 80, 30, 70, 75, 85)
  test <- score_homework(hws, drop = TRUE)
  test2 <- score_homework(hws, drop = FALSE)
  #output is of length 1
  expect_that(length(test),equals(1))
  expect_that(length(test2),equals(1))
  #output is of correct average
  expect_that(test,equals(82))
  #output is of type numeric
  expect_that(is.numeric(test),equals(TRUE))
  #output is of type numeric
  expect_that(is.numeric(test2),equals(TRUE))
}
)

#Unit test for Function score_quiz()
context('testing Function score_quiz()')
test_that("function integrity",{
  quizzes <- c(100, 80, 70, 0)
  test <- score_quiz(quizzes, drop = TRUE)
  test2 <- score_quiz(quizzes, drop = FALSE)
  #output is of length 1
  expect_that(length(test),equals(1))
  expect_that(length(test2),equals(1))
  #output is of correct average
  expect_that(test2,equals(62.5))
  #output is of type numeric
  expect_that(is.numeric(test),equals(TRUE))
  #output is of type numeric
  expect_that(is.numeric(test2),equals(TRUE))
}
)

#Unit test for Function score_lab()
context('testing Function score_lab()')
test_that("function integrity",{
  test <- score_lab(12)
  test2 <- score_lab(2)
  #output is of length 1
  expect_that(length(test),equals(1))
  #output is of correct average
  expect_that(test,equals(100))
  #output is of correct average
  expect_that(test2,equals(0))
  #output is of type numeric
  expect_that(is.numeric(test),equals(TRUE))
}
)
