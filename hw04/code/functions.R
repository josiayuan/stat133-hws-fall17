# ===================================================================
# Title: Functions for HW04
# Description:
#   This script prepares functions to be used
# in hw04
# Input(s): N/A
# Output(s): data file 'hw04-josia-yuan.Rmd'
# Author: Josia Yuan
# Date: 11-011-2017
# ===================================================================

#' @title remove-missing()
#' @description remove missing value
#' @param x a vector
#' @return a vector without missing value
remove_missing <- function(x){
  x<-x[is.na(x)==FALSE]
  return(x)
}
a <- c(1, 4, 7, NA, 10)
remove_missing(a)


#' @title get_minimun()
#' @description find the minimum value
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the minimum numeric element within the vector
get_minimum <- function(x,na.rm=FALSE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){remove_missing(x)
    x<-sort(x)
    return(x[1])}
  else{x<-sort(x)
  return(x[1])}
}
get_minimum(a)



#' @title get_maximum()
#' @description find the maximum value
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the maximum numeric element within the vector
get_maximum <- function(x,na.rm=FALSE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){remove_missing(x)
    x<-sort(x,decreasing = TRUE)
    print(x[1])}
  else{x<-sort(x,decreasing = TRUE)
  return(x[1])}
}
get_maximum(a)



#' @title get_range()
#' @description compute the overall range of the input vector
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the overall range of the input vectorS
get_range <- function(x,na.rm=FALSE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){x <- remove_missing(x)
  max<-get_maximum(x)
  min<-get_minimum(x)
  max-min}
  else{max<-get_maximum(x)
  min<-get_minimum(x)
  max-min}
}
get_range(a,na.rm=TRUE)



#' @title get_percentile10()
#' @description compute the 10th percentile of the input vector
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the 10th percentile of the input vector
get_percentile10 <- function(x,na.rm=TRUE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){x <- remove_missing(x)
  p <- quantile(x,probs = 0.1,na.rm=TRUE)
  names(p)<-NULL
  return(p)
  }else{
    stop("can't do it with missing value")
  }
}
get_percentile10(a,na.rm=TRUE)



#' @title get_percentile90()
#' @description compute the 90th percentile of the input vector
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the 90th percentile of the input vector
get_percentile90 <- function(x,na.rm=TRUE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){x <- remove_missing(x)
  p <- quantile(x,probs = 0.9,na.rm=TRUE)
  names(p)<-NULL
  return(p)
  }else{
    stop("can't do it with missing value")
  }
}
get_percentile90(a,na.rm=TRUE)



#' @title get_median()
#' @description compute the median of the input vector
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the median of the input vector
median_odd <- function(x){x <-sort(x)
num <- (length(x)+1)/2
median <- x[num]
return(median)}
median_even <- function(x){x <-sort(x)
num_1<- length(x)/2
num_2 <- num_1 +1
median <- (x[num_1]+x[num_2])/2
return(median)}

get_median <- function(x,na.rm=TRUE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){x <- remove_missing(x)
  if(length(x) %% 2 ==0){
    median_even(x)
  }else{median_odd(x)}
  }else{if(length(x) %% 2 ==0){
    median_even(x)
  }else{median_odd(x)}}
}
get_median(a,na.rm=TRUE)



#' @title get_average()
#' @description compute the average of the input vector
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the average of the input vector
get_averge <- function(x,na.rm=TRUE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){x <- remove_missing(x)
  sum <- 0
  for(i in 1:length(x)){sum <- x[i] + sum}
  avg <- sum/length(x)
  return(avg)
  }else{stop("can't do it with missing value")}
}
get_averge(a, na.rm=TRUE)



#' @title get_stdev()
#' @description compute the standard deviation of the input vector
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return  the standard deviation of the input vector
get_stdev <- function(x,na.rm=TRUE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){
    avg<-get_averge(x,na.rm=TRUE)
    minu_square <- 0
    x <- remove_missing(x)
    for(i in 1:length(x)){
      minu_square <- minu_square + ((x[i]-avg)^2)}
    sd <- sqrt(minu_square/(length(x)-1))
    return(sd)
  }else{stop("can't do it with missing value")}
}
get_stdev(a, na.rm=TRUE)



#' @title get-quartile1()
#' @description compute the first quartile of the input vector
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the first quartile of the input vector
get_quartile1 <- function(x,na.rm=TRUE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){x <- remove_missing(x)
  p <- quantile(x,probs = 0.25,na.rm=TRUE)
  names(p)<-NULL
  return(p)
  }else{
    stop("can't do it with missing value")
  }
}
get_quartile1(a)



#' @title get-quartile3()
#' @description compute the third quartile of the input vector
#' @param x a numeric vector
#' @param na.rm an optional logical na.rm argument
#' @return the third quartile of the input vector
get_quartile3 <- function(x,na.rm=TRUE){
  if (!is.numeric(x)) {
    stop("x is not numeric")
  }
  if(na.rm==TRUE){x <- remove_missing(x)
  p <- quantile(x,probs = 0.75,na.rm=TRUE)
  names(p)<-NULL
  return(p)
  }else{
    stop("can't do it with missing value")
  }
}
get_quartile3(a)


#' @title Count_missing
#' @description calculates the number of missing values NA
#' @param x a numeric vector
#' @return the number of missing values NA
count_missing <- function(x){
  which <- which(is.na(x))
  return(length(which))
}
count_missing(a)



#' @title summary_states()
#' @description returns a list of summary statistics
#' @param x a numeric vector
#' @return a list of summary statistics
summary_stats<-function(x){
  list <- list(
    minimum = get_minimum(x),
    percent10 = get_percentile10(x),
    quartile1 = get_quartile1(x),
    median = get_median(x),
    mean = get_averge(x),
    quartile3 = get_quartile3(x),
    percent90 = get_percentile90(x),
    maximum = get_maximum(x),
    range = get_range(x),
    stdev = get_stdev(x),
    missing = count_missing(x))
  return(list)
}
summary_stats(a)


#' @title print_stats
#' @description prints the values in a nice format
#' @param x a list of summary statistics
#' @return the values in a nice format
num <- function(x){ number <- c(get_minimum(x),
                                get_percentile10(x),
                                get_quartile1(x),
                                get_median(x),
                                get_averge(x),
                                get_quartile3(x),
                                get_percentile90(x),
                                get_maximum(x),
                                get_range(x),
                                get_stdev(x),
                                count_missing(x))
return(number)
}
print_stats <- function(x){
  num(x)
  names_sum <- names(summary_stats(x))
  Num <- num(x)
  first <- format(names_sum,width = 9,justify = "left")
  second <- sprintf("%.4f",Num)
  names(second)<-NULL
  third <- paste(first,": ",second,sep = "")
  cat(third,sep = "\n")
}
print_stats(a)


#' @title rescale100
#' @description compute a rescaled vector with a potential scale from 0 to 100
#' @param x a numeric vector
#' @param xmin a numeric value
#' @param xmax a numeric value
#' @return  a rescaled vector with a potential scale from 0 to 100
rescale100 <- function(x,xmin,xmax){
  z <- 100*((x-xmin)/(xmax-xmin))
  return(z)
}
b <- c(18,15,16,4,17,9)
rescale100(b,xmin=0,xmax=20)



#' @title drop_lowest
#' @description drop the lowest value
#' @param x a numeric vector of length n
#' @return a vector of length n ??? 1 without the lowest value
drop_lowest <- function(x){
  x <- sort(x)
  x <- x[-1]
  return(x)
}
b <- c(10, 10, 8.5, 4, 7, 9)
drop_lowest(b)



#' @title score_homework()
#' @description: compute a single homework average
#' @param x a numeric vector of homework scores (of length n)
#' @param drop an optional logical argument
#' @return  a single homework average
score_homework <- function(x,drop=TRUE){
  if(drop==TRUE){
    x <- drop_lowest(x)
    average <- get_averge(x)
    return(average)
  }else{average <- get_averge(x)
  return(average)}
}
hws <- c(100, 80, 30, 70, 75, 85)
score_homework(hws, drop = TRUE)
score_homework(hws, drop = FALSE)



#' @title score_quiz
#' @description compute a single quiz average
#' @param x a numeric vector of quiz scores (of length n)
#' @param drop an optional logical argument
#' @return a single quiz average
score_quiz <- function(x,drop=TRUE){
  if(drop==TRUE){
    x <- drop_lowest(x)
    Average <- get_averge(x)
    return(Average)
  }else{Average <- get_averge(x)
  return(Average)}
}
quizzes <- c(100, 80, 70, 0)
score_quiz(quizzes, drop = TRUE)
score_quiz(quizzes, drop = FALSE)



#' @title score_lab()
#' @description compute the lab score
#' @param x a numeric value of lab attendance
#' @return  a single attendance value ranges between 0 and 12
score_lab <- function(x){
  if(x==11){print(100)}
  else if(x==12){print(100)}
  else if(x==10){print(80)}
  else if(x==9){print(60)}
  else if(x==8){print(40)}
  else if(x==7){print(20)}
  else if(x<=6){print(0)}
}
score_lab(12)
score_lab(10)
score_lab(6)

