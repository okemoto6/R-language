diamonds
================
okemoto
2023-12-04

# Analysis of diamonds dataset (part of ggplot2 library)

## containing the prices and other attributes of almost 54,000 diamonds

#### Data overview

``` r
library(ggplot2)

head(diamonds)
```

    ## # A tibble: 6 × 10
    ##   carat cut       color clarity depth table price     x     y     z
    ##   <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
    ## 1  0.23 Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
    ## 2  0.21 Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
    ## 3  0.23 Good      E     VS1      56.9    65   327  4.05  4.07  2.31
    ## 4  0.29 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
    ## 5  0.31 Good      J     SI2      63.3    58   335  4.34  4.35  2.75
    ## 6  0.24 Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48

    ## 
    ## Dołączanie pakietu: 'dplyr'

    ## Następujące obiekty zostały zakryte z 'package:stats':
    ## 
    ##     filter, lag

    ## Następujące obiekty zostały zakryte z 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Rows: 53,940
    ## Columns: 10
    ## $ carat   <dbl> 0.23, 0.21, 0.23, 0.29, 0.31, 0.24, 0.24, 0.26, 0.22, 0.23, 0.…
    ## $ cut     <ord> Ideal, Premium, Good, Premium, Good, Very Good, Very Good, Ver…
    ## $ color   <ord> E, E, E, I, J, J, I, H, E, H, J, J, F, J, E, E, I, J, J, J, I,…
    ## $ clarity <ord> SI2, SI1, VS1, VS2, SI2, VVS2, VVS1, SI1, VS2, VS1, SI1, VS1, …
    ## $ depth   <dbl> 61.5, 59.8, 56.9, 62.4, 63.3, 62.8, 62.3, 61.9, 65.1, 59.4, 64…
    ## $ table   <dbl> 55, 61, 65, 58, 58, 57, 57, 55, 61, 61, 55, 56, 61, 54, 62, 58…
    ## $ price   <int> 326, 326, 327, 334, 335, 336, 336, 337, 337, 338, 339, 340, 34…
    ## $ x       <dbl> 3.95, 3.89, 4.05, 4.20, 4.34, 3.94, 3.95, 4.07, 3.87, 4.00, 4.…
    ## $ y       <dbl> 3.98, 3.84, 4.07, 4.23, 4.35, 3.96, 3.98, 4.11, 3.78, 4.05, 4.…
    ## $ z       <dbl> 2.43, 2.31, 2.31, 2.63, 2.75, 2.48, 2.47, 2.53, 2.49, 2.39, 2.…

``` r
library(skimr)
skim_without_charts(diamonds)
```

|                                                  |          |
|:-------------------------------------------------|:---------|
| Name                                             | diamonds |
| Number of rows                                   | 53940    |
| Number of columns                                | 10       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| factor                                           | 3        |
| numeric                                          | 7        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts                                    |
|:--------------|----------:|--------------:|:--------|---------:|:----------------------------------------------|
| cut           |         0 |             1 | TRUE    |        5 | Ide: 21551, Pre: 13791, Ver: 12082, Goo: 4906 |
| color         |         0 |             1 | TRUE    |        7 | G: 11292, E: 9797, F: 9542, H: 8304           |
| clarity       |         0 |             1 | TRUE    |        8 | SI1: 13065, VS2: 12258, SI2: 9194, VS1: 8171  |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |      sd |    p0 |    p25 |     p50 |     p75 |     p100 |
|:--------------|----------:|--------------:|--------:|--------:|------:|-------:|--------:|--------:|---------:|
| carat         |         0 |             1 |    0.80 |    0.47 |   0.2 |   0.40 |    0.70 |    1.04 |     5.01 |
| depth         |         0 |             1 |   61.75 |    1.43 |  43.0 |  61.00 |   61.80 |   62.50 |    79.00 |
| table         |         0 |             1 |   57.46 |    2.23 |  43.0 |  56.00 |   57.00 |   59.00 |    95.00 |
| price         |         0 |             1 | 3932.80 | 3989.44 | 326.0 | 950.00 | 2401.00 | 5324.25 | 18823.00 |
| x             |         0 |             1 |    5.73 |    1.12 |   0.0 |   4.71 |    5.70 |    6.54 |    10.74 |
| y             |         0 |             1 |    5.73 |    1.14 |   0.0 |   4.72 |    5.71 |    6.54 |    58.90 |
| z             |         0 |             1 |    3.54 |    0.71 |   0.0 |   2.91 |    3.53 |    4.04 |    31.80 |

#### Min and max values for weight and price

``` r
min_carat <- min(diamonds$carat)
max_carat <- max(diamonds$carat)
min_price <- min(diamonds$price)
max_price <- max(diamonds$price)
max_carat_index <- which.max(diamonds$carat)
max_price_index <- which.max(diamonds$price)

cat('Miminum diamond weight is', min_carat, 'ct, maximum diamond weight is', max_carat, 'ct \n\n')  
```

    ## Miminum diamond weight is 0.2 ct, maximum diamond weight is 5.01 ct

``` r
cat('Minimum diamond price is $', min_price, ', maximum diamond price is $', max_price, '\n\n')  
```

    ## Minimum diamond price is $ 326 , maximum diamond price is $ 18823

``` r
cat('The most expensive diamond cost $', max_price, 'and its weight was', diamonds$carat[max_price_index], '\n\n')  
```

    ## The most expensive diamond cost $ 18823 and its weight was 2.29

``` r
cat('The heaviest diamonds weight', max_carat, 'ct and its cost was', diamonds$price[max_carat_index])
```

    ## The heaviest diamonds weight 5.01 ct and its cost was 18018

#### Dataset summary by cut

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.0
    ## ✔ readr     2.1.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
diamonds_cut <- diamonds %>% 
  group_by(cut) %>% 
  summarise(total_number = n(),
            mean_price = mean(price),
            median_price = median(price),
            mean_carat = mean(carat),
            median_carat = median(carat),
            sd_carat = sd(carat)
            ) %>% 
  arrange(-total_number)

library(knitr)

kable(diamonds_cut)
```

| cut       | total_number | mean_price | median_price | mean_carat | median_carat |  sd_carat |
|:----------|-------------:|-----------:|-------------:|-----------:|-------------:|----------:|
| Ideal     |        21551 |   3457.542 |       1810.0 |  0.7028370 |         0.54 | 0.4328763 |
| Premium   |        13791 |   4584.258 |       3185.0 |  0.8919549 |         0.86 | 0.5152616 |
| Very Good |        12082 |   3981.760 |       2648.0 |  0.8063814 |         0.71 | 0.4594354 |
| Good      |         4906 |   3928.864 |       3050.5 |  0.8491847 |         0.82 | 0.4540544 |
| Fair      |         1610 |   4358.758 |       3282.0 |  1.0461366 |         1.00 | 0.5164043 |

![](diamonds_files/figure-gfm/plot_1-1.png)<!-- -->

#### Dataset summary by color

``` r
library(tidyverse)

diamonds_color <- diamonds %>% 
  group_by(color) %>% 
  summarise(total_number = n(),
            mean_price = mean(price),
            median_price = median(price),
            mean_carat = mean(carat),
            median_carat = median(carat),
            sd_carat = sd(carat)
            ) %>% 
  arrange(-total_number)

library(knitr)

kable(diamonds_color)
```

| color | total_number | mean_price | median_price | mean_carat | median_carat |  sd_carat |
|:------|-------------:|-----------:|-------------:|-----------:|-------------:|----------:|
| G     |        11292 |   3999.136 |       2242.0 |  0.7711902 |         0.70 | 0.4414357 |
| E     |         9797 |   3076.752 |       1739.0 |  0.6578667 |         0.53 | 0.3685656 |
| F     |         9542 |   3724.886 |       2343.5 |  0.7365385 |         0.70 | 0.3975884 |
| H     |         8304 |   4486.669 |       3460.0 |  0.9117991 |         0.90 | 0.5212355 |
| D     |         6775 |   3169.954 |       1838.0 |  0.6577948 |         0.53 | 0.3595733 |
| I     |         5422 |   5091.875 |       3730.0 |  1.0269273 |         1.00 | 0.5791727 |
| J     |         2808 |   5323.818 |       4234.0 |  1.1621368 |         1.11 | 0.5958012 |

![](diamonds_files/figure-gfm/plot_2-1.png)<!-- -->

#### Dataset summary by clarity

``` r
library(tidyverse)

diamonds_clarity <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(total_number = n(),
            mean_price = mean(price),
            median_price = median(price),
            mean_carat = mean(carat),
            median_carat = median(carat),
            sd_carat = sd(carat)
            ) %>% 
  arrange(-total_number)

library(knitr)

kable(diamonds_clarity)
```

| clarity | total_number | mean_price | median_price | mean_carat | median_carat |  sd_carat |
|:--------|-------------:|-----------:|-------------:|-----------:|-------------:|----------:|
| SI1     |        13065 |   3996.001 |         2822 |  0.8504822 |         0.76 | 0.4496517 |
| VS2     |        12258 |   3924.989 |         2054 |  0.7639346 |         0.63 | 0.4462921 |
| SI2     |         9194 |   5063.029 |         4072 |  1.0776485 |         1.01 | 0.5166532 |
| VS1     |         8171 |   3839.455 |         2005 |  0.7271582 |         0.57 | 0.4235294 |
| VVS2    |         5066 |   3283.737 |         1311 |  0.5962021 |         0.44 | 0.3596968 |
| VVS1    |         3655 |   2523.115 |         1093 |  0.5033215 |         0.39 | 0.2995573 |
| IF      |         1790 |   2864.839 |         1080 |  0.5051229 |         0.35 | 0.3134331 |
| I1      |          741 |   3924.169 |         3344 |  1.2838462 |         1.12 | 0.6324360 |

![](diamonds_files/figure-gfm/plot_3-1.png)<!-- -->

## Calculations

#### which of the variables - weight, quality of cut, color or clarity - has the strongest linear correlation with price?

``` r
price_carat_cor <- cor(diamonds$price, diamonds$carat)
price_cut_cor <- cor(diamonds$price, as.numeric(diamonds$cut))
price_color_cor <- cor(diamonds$price, as.numeric(diamonds$color))
price_clarity_cor <- cor(diamonds$price, as.numeric(diamonds$clarity))

cat('Value of correlation coefficient between price and weight:', price_carat_cor, '\n')
```

    ## Value of correlation coefficient between price and weight: 0.9215913

``` r
cat('Value of correlation coefficient between price and cut:', price_cut_cor, '\n')
```

    ## Value of correlation coefficient between price and cut: -0.05349066

``` r
cat('Value of correlation coefficient between price and color:', price_color_cor, '\n')
```

    ## Value of correlation coefficient between price and color: 0.1725109

``` r
cat('Value of correlation coefficient between price and clarity:', price_clarity_cor)
```

    ## Value of correlation coefficient between price and clarity: -0.1468001

Two variables has positive linear correlation with price factor (weight
of diamond and its color) and two variables has negative linear
correlation (cut and clarity). While correlations between price and
three of variables - cut, color and clarity are weak, a correlation
between price and weight is strong: while price is growing the weight is
growing/while weight is growing the price is growing (correlation
doesn’t show causation).

#### Correlation between diamond weight and its price

``` r
ggplot(diamonds, aes(x = carat, y = price, color = carat)) +
  geom_jitter() +
  labs(x = 'Weight [ct]', y = 'Price [$]', title = 'Weight of diamond', subtitle = 'by its price', caption = 'data source: diamonds dataset (ggplot2 library)')
```

![](diamonds_files/figure-gfm/plot_4-1.png)<!-- -->

#### Weight - price correlation with trend line:

``` r
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = 'Weight [ct]', y = 'Price [$]', title = 'Weight of diamond', subtitle = 'by its price', caption = 'data source: diamonds dataset (ggplot2 library)')
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](diamonds_files/figure-gfm/plot_5-1.png)<!-- -->
