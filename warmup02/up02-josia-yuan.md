warmup 02
================
josiayuan
9/8/2017

``` r
#load the objects
load("nba2017-salary-points.RData")
##list available objects
ls()
```

    ## [1] "experience" "player"     "points"     "points1"    "points2"   
    ## [6] "points3"    "position"   "salary"     "team"

``` r
#inspection
typeof(player)
```

    ## [1] "character"

``` r
length(player)
```

    ## [1] 441

``` r
length(team)
```

    ## [1] 441

``` r
length(position)
```

    ## [1] 441

``` r
length(points)
```

    ## [1] 441

mean (i.e. average) standard deviation minimum value maximum value median quartiles

``` r
##Quantitative Variable - salary
summary(salary)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##     5145  1286160  3500000  6187014  9250000 30963450

``` r
mean(salary)
```

    ## [1] 6187014

``` r
sd(salary)
```

    ## [1] 6571890

``` r
min(salary)
```

    ## [1] 5145

``` r
max(salary)
```

    ## [1] 30963450

``` r
median(salary)
```

    ## [1] 3500000

``` r
quantile(salary)
```

    ##       0%      25%      50%      75%     100% 
    ##     5145  1286160  3500000  9250000 30963450

``` r
hist(salary)
```

![](up02-josia-yuan_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)

``` r
boxplot(salary)
```

![](up02-josia-yuan_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-2.png)

### Comments

#### Statistics summary

The **mean**, aka **average**, of the nba players' salaries for 2016-2017 is *6187000* dollars, with **minimum** being *5145* and **maximun** *30960000* dollars, which gives a range of 30954855.

#### Observations & Analysis

The value 5145 in comparison to a relatively much larger typical value with units 1000000 appears to be noticable, together with a large standard deviation 6571890, such renders a tentative guess that 5145 may be an outlier, which affect data such as average and standard deviation.

``` r
##Qualitative Variable - position
is.factor(position)
```

    ## [1] FALSE

``` r
position_fac <- factor(position)
a = table(position_fac)
prop.table(a)
```

    ## position_fac
    ##         C        PF        PG        SF        SG 
    ## 0.2018141 0.2018141 0.1927438 0.1882086 0.2154195

``` r
barplot(a)
```

![](up02-josia-yuan_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

### Comments

With the barplot, different positions appear to have faily similar frequency. This may imply every position is valued as important as all the counterparts, because every position playes different role within a basketball team to ensure a comprehensive performance.
