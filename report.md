---
title: "Final results report for the mtcars example"
author: You
output: html_document
---

# The weight and fuel efficiency of cars

Is there an association between the weight and the fuel efficiency of cars? To find out, we use the `mtcars` dataset from the `datasets` package. The `mtcars` data originally came from the 1974 Motor Trend US magazine, and it contains design and performance data on 32 models of automobile.


```r
# ?mtcars # more info
head(mtcars)
```

```
##                    mpg cyl disp  hp drat    wt  qsec vs
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1
##                   am gear carb
## Mazda RX4          1    4    4
## Mazda RX4 Wag      1    4    4
## Datsun 710         1    4    1
## Hornet 4 Drive     0    3    1
## Hornet Sportabout  0    3    2
## Valiant            0    3    1
```

Here, `wt` is weight in tons, and `mpg` is fuel efficiency in miles per gallon. We want to figure out if there is an association between `wt` and `mpg`. The `mtcars` dataset itself only has 32 rows, so we generated two larger bootstrapped datasets. We called them `small` and `large`.


```r
head(drake::readd(small)) # 48 rows
```

```
##       x    y
## 1 3.780 15.2
## 2 3.440 17.8
## 3 3.215 21.4
## 4 3.440 19.2
## 5 2.770 19.7
## 6 2.465 21.5
```

```r
drake::loadd(large)       # 64 rows
head(large)
```

```
##       x    y
## 1 2.140 26.0
## 2 3.730 17.3
## 3 1.935 27.3
## 4 2.780 21.4
## 5 2.320 22.8
## 6 2.780 21.4
```

Then, we fit a couple regression models to the `small` and `large` to try to detect an association between `wt` and `mpg`. Here are the coefficients and p-values from one of the model fits.


```r
drake::readd(coef_regression2_small)
```

```
##               Estimate Std. Error   t value     Pr(>|t|)
## (Intercept) 26.4616178 0.86674313  30.52994 3.491905e-32
## x2          -0.6589527 0.06457111 -10.20507 2.125733e-13
```

Since the p-value on `x2` is so small, there may be an association between weight and fuel efficiency after all.

# A note on knitr reports in drake projects.

Because of the calls to `readd()` and `loadd()`, `drake` knows that `small`, `large`, and `coef_regression2_small` are dependencies of this R Markdown report. This dependency relationship is what causes the report to be processed at the very end.
