Statistical assignment 3
================
Eliot Barrett - Holman 087675
12/02/20

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.5.3

``` r
# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             # Select the folder where the files are stored.
             "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "C:/Users/eliot/Documents/Politics and IR/Year 2 Term 2/Data 3/Data3_blank/data/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
# first select the variables that we care about

Long <- all7 %>% pivot_longer(c(a_memorig:g_vote6), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("Wave", "Variable") , sep = "_", extra = "merge") %>%
  pivot_wider(names_from = Variable, values_from = value)

  
Long
```

    ## # A tibble: 584,234 x 6
    ##        pidp Wave  memorig sex_dv age_dv vote6
    ##       <int> <chr>   <int>  <int>  <int> <int>
    ##  1 68001367 a           1      1     39     3
    ##  2 68001367 b          NA     NA     NA    NA
    ##  3 68001367 c          NA     NA     NA    NA
    ##  4 68001367 d          NA     NA     NA    NA
    ##  5 68001367 e          NA     NA     NA    NA
    ##  6 68001367 f          NA     NA     NA    NA
    ##  7 68001367 g          NA     NA     NA    NA
    ##  8 68004087 a           1      1     59     2
    ##  9 68004087 b           1      1     60     2
    ## 10 68004087 c           1      1     61     2
    ## # ... with 584,224 more rows

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex = recode(sex_dv,
                            `1` = "male",
                            `2` = "female",
                            .default = NA_character_)) %>%

       mutate(vote6 = ifelse(vote6 < 0, NA_integer_, vote6)
                             )

table(Long$sex)
```

    ## 
    ## female   male 
    ## 117665 100342

``` r
table(Long$vote6)
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%
  group_by(sex, Wave) %>%
   summarise( meanVote6 = mean(vote6, na.rm = TRUE))
      
        
meanVote6
```

    ## # A tibble: 20 x 3
    ## # Groups:   sex [3]
    ##    sex    Wave  meanVote6
    ##    <chr>  <chr>     <dbl>
    ##  1 female a          2.84
    ##  2 female b          2.82
    ##  3 female c          2.87
    ##  4 female d          2.89
    ##  5 female e          2.87
    ##  6 female f          2.81
    ##  7 female g          2.73
    ##  8 male   a          2.53
    ##  9 male   b          2.51
    ## 10 male   c          2.54
    ## 11 male   d          2.55
    ## 12 male   e          2.51
    ## 13 male   f          2.47
    ## 14 male   g          2.42
    ## 15 <NA>   a          4   
    ## 16 <NA>   b          4   
    ## 17 <NA>   c          4   
    ## 18 <NA>   d          4   
    ## 19 <NA>   e          3   
    ## 20 <NA>   f          4

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
meanVote6
```

    ## # A tibble: 20 x 3
    ## # Groups:   sex [3]
    ##    sex    Wave  meanVote6
    ##    <chr>  <chr>     <dbl>
    ##  1 female a          2.84
    ##  2 female b          2.82
    ##  3 female c          2.87
    ##  4 female d          2.89
    ##  5 female e          2.87
    ##  6 female f          2.81
    ##  7 female g          2.73
    ##  8 male   a          2.53
    ##  9 male   b          2.51
    ## 10 male   c          2.54
    ## 11 male   d          2.55
    ## 12 male   e          2.51
    ## 13 male   f          2.47
    ## 14 male   g          2.42
    ## 15 <NA>   a          4   
    ## 16 <NA>   b          4   
    ## 17 <NA>   c          4   
    ## 18 <NA>   d          4   
    ## 19 <NA>   e          3   
    ## 20 <NA>   f          4

``` r
meanVote6 %>% pivot_wider(names_from = Wave, values_from = meanVote6)
```

    ## # A tibble: 3 x 8
    ## # Groups:   sex [3]
    ##   sex        a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42
    ## 3 <NA>    4     4     4     4     3     4    NA

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
#set all negative values of vote 6 to NA so its easier to remove them
all7$a_vote6 <- ifelse(all7$a_vote6 < 0, NA, all7$a_vote6)
all7$b_vote6 <- ifelse(all7$b_vote6 < 0, NA, all7$b_vote6)
all7$c_vote6 <- ifelse(all7$c_vote6 < 0, NA, all7$c_vote6)
all7$d_vote6 <- ifelse(all7$d_vote6 < 0, NA, all7$d_vote6)
all7$e_vote6 <- ifelse(all7$e_vote6 < 0, NA, all7$e_vote6)
all7$f_vote6 <- ifelse(all7$f_vote6 < 0, NA, all7$f_vote6)
all7$g_vote6 <- ifelse(all7$g_vote6 < 0, NA, all7$g_vote6)

NONA <- all7 %>% filter(!is.na(a_vote6))%>% filter(!is.na(b_vote6))%>% filter(!is.na(c_vote6))%>% filter(!is.na(d_vote6))%>% filter(!is.na(e_vote6))%>% filter(!is.na(f_vote6)) %>% filter(!is.na(g_vote6))
# now we have a data frame with only respondents with non-missing values for political interest in all 7 waves

NONA <- NONA %>% mutate( delta = abs(b_vote6 - a_vote6) + abs(c_vote6 - b_vote6) + abs(d_vote6 - c_vote6) + abs(d_vote6 - e_vote6) + abs(e_vote6 - f_vote6) + abs(f_vote6 - g_vote6))
# now we have delta for each person in the dataset

# mean delta for men and women 

NONA %>% group_by(a_sex_dv) %>% summarise(mean_delta = mean(delta))
```

    ## # A tibble: 2 x 2
    ##   a_sex_dv mean_delta
    ##      <int>      <dbl>
    ## 1        1       2.61
    ## 2        2       2.59

``` r
# 1 is male and 2 is female
# male mean delta = 2.610134
# female mean delta = 2.586267

?scatter.smooth
# calculate mean delta by age at wave 1
mean_delta_age <- NONA %>% group_by(a_age_dv) %>% summarise(mean_delta = mean(delta))


scatter.smooth(mean_delta_age,
               xlab = "age at first wave",
               ylab = "mean delta",
               main = "graph showing mean delta against age")
```

![](assignment3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# here we see that stability of political interest is strongest for the oldest members of the survey at wave 1
# while political interest is fairly stable for the younger groups as well (around 2.7)
# interestingly we see a dip in stability of political interest in middle-aged groups at the beginning of the survey
# we see outliers at the oldest ages in stability of political interest, perhaps due to such a low number of these #involved in the sample and who completed each wave 
```
