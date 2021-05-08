Project 1: General Analysis Worldwide Suicide rate in 2016
================
Trinh Pham (SDS348)
2021-05-08

# Introduction

-   Suicide is a global phenomenon and occurs throughout the lifespan.
    Effective and evidence-based intervention can implement population.
    Suicide is a complex issue and therefore suicide prevention efforts
    need to collaboration among multiple aspects and other sectors such
    as society, mental business, justice. These efforts must be
    comprehensive and become broader over worldwide.
-   Data from this project carried out to investigate the relationship
    between the suicide number and the population of each country and by
    gender. By using the additional data of the facilities, the variable
    of health unit and mental hospital shown the inversely proportional
    to the suicide number variable by each country.
-   This project is make an overall analysis of suicide rate and explain
    these rate in detail by dyplyr and PCA.

# Tidy & Join/Merge

``` r
#imported the dataset
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(readr)
library(readxl)
library(usethis) 
use_git_config(user.name = "trinh-pham1094", user.email = "trinh.pham1094@gmail.com")
Facilities <- read_excel("~/Desktop/Facilities_new.xlsx")

suicide <- read_excel("~/Desktop/suicide_rate.xlsx")
Facilities
```

    ## # A tibble: 112 x 7
    ##    country     year mental_hospitals health_units outpatient_faci… day_treatment
    ##    <chr>      <dbl>            <dbl>        <dbl>            <dbl>         <dbl>
    ##  1 Afghanist…  2016            0.003        0.012            0.006        NA    
    ##  2 Albania     2016            0.068        0.068            0.41         NA    
    ##  3 Algeria     2016            0.048        0.068            0.048        NA    
    ##  4 Angola      2016            0.011       NA               NA            NA    
    ##  5 Antigua a…  2016            1.00        NA               NA            NA    
    ##  6 Argentina   2016            0.937        1.07             1.72         NA    
    ##  7 Armenia     2016           NA            0.069            1.37          0.034
    ##  8 Azerbaijan  2016            0.114        0.021            0.156         0.094
    ##  9 Bangladesh  2016            0.001        0.035            0.055        NA    
    ## 10 Belarus     2016            0.2          0.063            0.116         0.285
    ## # … with 102 more rows, and 1 more variable: residential_facilities <dbl>

``` r
suicide
```

    ## # A tibble: 27,820 x 12
    ##    country  year sex    age         suicides_no population `suicides/100k_pop`
    ##    <chr>   <dbl> <chr>  <chr>             <dbl>      <dbl>               <dbl>
    ##  1 Albania  1987 male   15-24 years          21     312900                6.71
    ##  2 Albania  1987 male   35-54 years          16     308000                5.19
    ##  3 Albania  1987 female 15-24 years          14     289700                4.83
    ##  4 Albania  1987 male   75+ years             1      21800                4.59
    ##  5 Albania  1987 male   25-34 years           9     274300                3.28
    ##  6 Albania  1987 female 75+ years             1      35600                2.81
    ##  7 Albania  1987 female 35-54 years           6     278800                2.15
    ##  8 Albania  1987 female 25-34 years           4     257200                1.56
    ##  9 Albania  1987 male   55-74 years           1     137500                0.73
    ## 10 Albania  1987 female 5-14 years            0     311000                0   
    ## # … with 27,810 more rows, and 5 more variables: country-year <chr>,
    ## #   HDI_for_year <dbl>, gdp_for_year($) <dbl>, gdp_per_capita($) <dbl>,
    ## #   generation <chr>

``` r
glimpse(suicide)
```

    ## Rows: 27,820
    ## Columns: 12
    ## $ country             <chr> "Albania", "Albania", "Albania", "Albania", "Alban…
    ## $ year                <dbl> 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1987, 19…
    ## $ sex                 <chr> "male", "male", "female", "male", "male", "female"…
    ## $ age                 <chr> "15-24 years", "35-54 years", "15-24 years", "75+ …
    ## $ suicides_no         <dbl> 21, 16, 14, 1, 9, 1, 6, 4, 1, 0, 0, 0, 2, 17, 1, 1…
    ## $ population          <dbl> 312900, 308000, 289700, 21800, 274300, 35600, 2788…
    ## $ `suicides/100k_pop` <dbl> 6.71, 5.19, 4.83, 4.59, 3.28, 2.81, 2.15, 1.56, 0.…
    ## $ `country-year`      <chr> "Albania1987", "Albania1987", "Albania1987", "Alba…
    ## $ HDI_for_year        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ `gdp_for_year($)`   <dbl> 2156624900, 2156624900, 2156624900, 2156624900, 21…
    ## $ `gdp_per_capita($)` <dbl> 796, 796, 796, 796, 796, 796, 796, 796, 796, 796, …
    ## $ generation          <chr> "Generation X", "Silent", "Generation X", "G.I. Ge…

``` r
glimpse(Facilities)
```

    ## Rows: 112
    ## Columns: 7
    ## $ country                <chr> "Afghanistan", "Albania", "Algeria", "Angola", …
    ## $ year                   <dbl> 2016, 2016, 2016, 2016, 2016, 2016, 2016, 2016,…
    ## $ mental_hospitals       <dbl> 0.003, 0.068, 0.048, 0.011, 1.001, 0.937, NA, 0…
    ## $ health_units           <dbl> 0.012, 0.068, 0.068, NA, NA, 1.071, 0.069, 0.02…
    ## $ outpatient_facilities  <dbl> 0.006, 0.410, 0.048, NA, NA, 1.720, 1.371, 0.15…
    ## $ day_treatment          <dbl> NA, NA, NA, NA, NA, NA, 0.034, 0.094, NA, 0.285…
    ## $ residential_facilities <dbl> NA, 0.445, NA, 0.014, NA, 0.152, NA, 0.031, 0.0…

``` r
#tidy data set by using pipe
suicide_tidy<- suicide %>% ## join/merge 2 dataset
   left_join(Facilities,by="country") %>%
  select(-'country-year',-'HDI_for_year',-'outpatient_facilities',-'residential_facilities' ,-'day_treatment',-'gdp_for_year($)',-'gdp_per_capita($)',-'suicides/100k_pop') %>%
  filter(year.x=="2016") %>%
  select(-year.y,-year.x) %>%  
  arrange(desc(suicides_no)) %>%
  na.omit()  %>%
  mutate_if(is.character,as.factor)
suicide_tidy
```

    ## # A tibble: 80 x 8
    ##    country   sex    age      suicides_no population generation  mental_hospitals
    ##    <fct>     <fct>  <fct>          <dbl>      <dbl> <fct>                  <dbl>
    ##  1 Thailand  male   35-54 y…        1421   10084647 Generation…            0.028
    ##  2 Thailand  male   55-74 y…         734    5203957 Boomers                0.028
    ##  3 Thailand  male   25-34 y…         646    4796355 Millenials             0.028
    ##  4 Romania   male   35-54 y…         632    2945568 Generation…            0.171
    ##  5 Romania   male   55-74 y…         615    2068747 Boomers                0.171
    ##  6 Thailand  female 35-54 y…         375   10629684 Generation…            0.028
    ##  7 Thailand  male   15-24 y…         322    4736305 Millenials             0.028
    ##  8 Lithuania male   35-54 y…         266     376689 Generation…            0.136
    ##  9 Thailand  female 55-74 y…         222    6049756 Boomers                0.028
    ## 10 Lithuania male   55-74 y…         212     277763 Boomers                0.136
    ## # … with 70 more rows, and 1 more variable: health_units <dbl>

*My main point in this research is focus on the suicide number of
population in 2016 and how the facilities such as mental hospital and
health unit effects on the number of suicide by each country. Thus, my
dataset have been drop 9 variable include
country-year’,‘HDI\_for\_year’,‘outpatient\_facilities’,‘residential\_facilities’
,‘day\_treatment’,‘gdp\_for\_year()′, ′*g**d**p*<sub>*p*</sub>*e**r*<sub>*c*</sub>*a**p**i**t**a*()’,‘suicides/100k\_pop’
after combining two dataset.*

\#\#Summary statistic

``` r
#explore the suicide_tidy dataset by dplyr function
# Summarize data by finding the mean suicide_no, number of rows, and number of distinct (country)
suicide_tidy %>%
   summarize(mean(suicides_no, na.rm=T), # calculate mean by ignoring NA values
            n(), # number of rows
            n_distinct(country))
```

    ## # A tibble: 1 x 3
    ##   `mean(suicides_no, na.rm = T)` `n()` `n_distinct(country)`
    ##                            <dbl> <int>                 <int>
    ## 1                           101.    80                     8

``` r
# Summarize data by finding the mean population, number of rows, and number of distinct (country)
suicide_tidy %>%
   summarize(mean(population, na.rm=T), # calculate mean by ignoring NA values
            n(), # number of rows
            n_distinct(country))
```

    ## # A tibble: 1 x 3
    ##   `mean(population, na.rm = T)` `n()` `n_distinct(country)`
    ##                           <dbl> <int>                 <int>
    ## 1                      1010347.    80                     8

``` r
# Summarize data by finding the mean mental_hospitals, number of rows, and number of distinct (country)
suicide_tidy %>%
   summarize(mean(mental_hospitals, na.rm=T), # calculate mean by ignoring NA values
            n(), # number of rows
            n_distinct(country))
```

    ## # A tibble: 1 x 3
    ##   `mean(mental_hospitals, na.rm = T)` `n()` `n_distinct(country)`
    ##                                 <dbl> <int>                 <int>
    ## 1                               0.226    80                     8

``` r
# Summarize data by finding the mean mental_hospitals, number of rows, and number of distinct (country)
suicide_tidy %>%
   summarize(mean(health_units, na.rm=T), # calculate mean by ignoring NA values
            n(), # number of rows
            n_distinct(country))
```

    ## # A tibble: 1 x 3
    ##   `mean(health_units, na.rm = T)` `n()` `n_distinct(country)`
    ##                             <dbl> <int>                 <int>
    ## 1                           0.966    80                     8

``` r
# Find summaries under certain conditions: find mean suicide_no, mental_hospital and population for female in Thailand
suicide_tidy %>% 
  filter(sex=="male", country=="Thailand") %>% 
  summarize(mean(suicides_no), mean(population),mean(mental_hospitals))
```

    ## # A tibble: 1 x 3
    ##   `mean(suicides_no)` `mean(population)` `mean(mental_hospitals)`
    ##                 <dbl>              <dbl>                    <dbl>
    ## 1                 655           5189063.                    0.028

``` r
# Find summaries by subgroups: mean suicides_no per country
suicide_tidy %>%# number of distinct values 
  group_by(country)%>%
   summarize(mean_suicides_noNA = mean(suicides_no,na.rm=T)) # ignoring NA values
```

    ## # A tibble: 8 x 2
    ##   country   mean_suicides_noNA
    ##   <fct>                  <dbl>
    ## 1 Croatia                 68.3
    ## 2 Grenada                  0  
    ## 3 Iceland                  4  
    ## 4 Lithuania               82.2
    ## 5 Mongolia                42.3
    ## 6 Qatar                    6.8
    ## 7 Romania                195. 
    ## 8 Thailand               412.

``` r
# Find mean suicides_no, sd and se per country per sex
suicide_tidy %>% 
  group_by(country,sex) %>% 
  summarize(mean_suicides=mean(suicides_no, na.rm=T), 
            sd_vol=sd(suicides_no, na.rm=T), 
            n=n(),
            se_vol=sd_vol/sqrt(n)) # use recently defined variables
```

    ## `summarise()` has grouped output by 'country'. You can override using the `.groups` argument.

    ## # A tibble: 16 x 6
    ## # Groups:   country [8]
    ##    country   sex    mean_suicides sd_vol     n  se_vol
    ##    <fct>     <fct>          <dbl>  <dbl> <int>   <dbl>
    ##  1 Croatia   female          33.6  27.4      5  12.2  
    ##  2 Croatia   male           103    65.7      5  29.4  
    ##  3 Grenada   female           0     0        5   0    
    ##  4 Grenada   male             0     0        5   0    
    ##  5 Iceland   female           0.8   1.10     5   0.490
    ##  6 Iceland   male             7.2   3.96     5   1.77 
    ##  7 Lithuania female          25.8  18.2      5   8.12 
    ##  8 Lithuania male           139.   95.7      5  42.8  
    ##  9 Mongolia  female          10.4   7.80     5   3.49 
    ## 10 Mongolia  male            74.2  56.6      5  25.3  
    ## 11 Qatar     female           1     1.41     5   0.632
    ## 12 Qatar     male            12.6  11.7      5   5.24 
    ## 13 Romania   female          62    42.4      5  19.0  
    ## 14 Romania   male           329.  271.       5 121.   
    ## 15 Thailand  female         168.  132.       5  59.1  
    ## 16 Thailand  male           655   489.       5 219.

``` r
# Stop grouping: count how many observations per country then count how many of these observations per country are distinct
suicide_tidy  %>% 
  group_by(country) %>% 
  summarize(count=n()) %>% 
  ungroup() %>% 
  summarize(n_distinct(count))
```

    ## # A tibble: 1 x 1
    ##   `n_distinct(count)`
    ##                 <int>
    ## 1                   1

``` r
#Describe function include analysis of all the numeric dataset. For this, mean, quantile, min, max, median, 1st quarantile, 3rd qurantile.
summary(suicide_tidy) 
```

    ##       country       sex              age      suicides_no    
    ##  Croatia  :10   female:40   15-24 years:16   Min.   :   0.0  
    ##  Grenada  :10   male  :40   25-34 years:16   1st Qu.:   2.0  
    ##  Iceland  :10               35-54 years:16   Median :  24.0  
    ##  Lithuania:10               55-74 years:16   Mean   : 101.3  
    ##  Mongolia :10               75+ years  :16   3rd Qu.:  98.0  
    ##  Qatar    :10                                Max.   :1421.0  
    ##  (Other)  :20                                                
    ##    population              generation mental_hospitals  health_units   
    ##  Min.   :    1312   Boomers     :16   Min.   :0.0280   Min.   :0.0380  
    ##  1st Qu.:   26274   Generation X:16   1st Qu.:0.0370   1st Qu.:0.3317  
    ##  Median :  244144   Millenials  :32   Median :0.1505   Median :0.4945  
    ##  Mean   : 1010347   Silent      :16   Mean   :0.2264   Mean   :0.9663  
    ##  3rd Qu.:  675837                     3rd Qu.:0.2040   3rd Qu.:0.7455  
    ##  Max.   :10629684                     Max.   :0.9360   Max.   :4.5420  
    ## 

*The mean of suicide number per country is 101. The mean of population
per country is 1010347 The mean of mental\_hospital per country is 0.226
The mean of health\_unit is 0.966 The mean suicide number, mental
hospital and population for female in Thailand are 655, 5189063, 0.028.*

## Plotting

``` r
library(ggplot2) 
suicide_tidy<-as.data.frame(suicide_tidy)
# Display the distribution of country and sex of number of suicide. 
ggplot(suicide_tidy, aes(x=country,fill=sex))+
   geom_bar(aes(y = suicides_no), stat="summary", fun="mean")
```

![](project-1-$_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
## Considering age values as categories to display the distribution of number of suicide for age.
ggplot(suicide_tidy, aes(x=suicides_no, y=mental_hospitals,color=country))+
  geom_point() 
```

![](project-1-$_files/figure-gfm/unnamed-chunk-3-2.png)<!-- --> \*
Thailand is the country have the highest number of suicide rate in 2016.
The male have suicide rate is extremely higher than female.\* The lowest
is Grenada, which is have 0 number of suicide in 2016. According the
geom\_point plot, Grenada have the highest number of mental\_hospitals
in their country and Thailands have the lowest. Therefore, the mental
hospitals rate is inversely proportional to the suicides number. \*

``` r
##Making a heatmap with geom_title
## Visualize the relationship first
ggplot(suicide_tidy, aes(x = suicides_no, y = population)) +
  geom_point()
```

![](project-1-$_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Find the correlation between two variables
cor(suicide_tidy$suicides_no,suicide_tidy$population, use = "pairwise.complete.obs")
```

    ## [1] 0.7531811

``` r
#Build a correlation matrix between all numeric variables
suicide_num <- suicide_tidy %>%
  select_if(is.numeric)
suicide_num
```

    ##    suicides_no population mental_hospitals health_units
    ## 1         1421   10084647            0.028        0.151
    ## 2          734    5203957            0.028        0.151
    ## 3          646    4796355            0.028        0.151
    ## 4          632    2945568            0.171        0.392
    ## 5          615    2068747            0.171        0.392
    ## 6          375   10629684            0.028        0.151
    ## 7          322    4736305            0.028        0.151
    ## 8          266     376689            0.136        0.682
    ## 9          222    6049756            0.028        0.151
    ## 10         212     277763            0.136        0.682
    ## 11         187     480263            0.165        0.519
    ## 12         171     579838            0.171        0.392
    ## 13         152    1124052            0.028        0.151
    ## 14         146     571003            0.165        0.519
    ## 15         134    1359178            0.171        0.392
    ## 16         127     290660            0.034        0.470
    ## 17         127     375631            0.034        0.470
    ## 18         116    4702656            0.028        0.151
    ## 19         112    2462909            0.171        0.392
    ## 20         104     134519            0.165        0.519
    ## 21          96     191346            0.136        0.682
    ## 22          96    2823207            0.171        0.392
    ## 23          91    1101638            0.171        0.392
    ## 24          85     252851            0.034        0.470
    ## 25          78      80426            0.136        0.682
    ## 26          69     551758            0.165        0.519
    ## 27          69    1589015            0.028        0.151
    ## 28          61     997868            0.171        0.392
    ## 29          60    4525574            0.028        0.151
    ## 30          48     274466            0.165        0.519
    ## 31          47     570380            0.165        0.519
    ## 32          45     390385            0.136        0.682
    ## 33          41     177888            0.136        0.682
    ## 34          41     406249            0.136        0.682
    ## 35          40     246002            0.165        0.519
    ## 36          30     242287            0.165        0.519
    ## 37          29     193001            0.136        0.682
    ## 38          29     113527            0.034        0.470
    ## 39          27     675747            0.038        0.038
    ## 40          27    1264645            0.171        0.392
    ## 41          21     676108            0.038        0.038
    ## 42          18     246805            0.034        0.470
    ## 43          15     292237            0.034        0.470
    ## 44          15     406028            0.034        0.470
    ## 45          14    1041307            0.171        0.392
    ## 46          13     293525            0.038        0.038
    ## 47          11      44024            0.303        4.542
    ## 48          10      33247            0.303        4.542
    ## 49           8      25029            0.303        4.542
    ## 50           8     182632            0.136        0.682
    ## 51           6     231038            0.165        0.519
    ## 52           6     265327            0.165        0.519
    ## 53           6      24099            0.303        4.542
    ## 54           6     168917            0.136        0.682
    ## 55           3      15349            0.034        0.470
    ## 56           3      26305            0.034        0.470
    ## 57           3     179374            0.038        0.038
    ## 58           2      23233            0.303        4.542
    ## 59           2      32963            0.303        4.542
    ## 60           2      99266            0.038        0.038
    ## 61           2     164901            0.038        0.038
    ## 62           1       8723            0.303        4.542
    ## 63           1     148196            0.034        0.470
    ## 64           0       9543            0.936        0.936
    ## 65           0      10271            0.936        0.936
    ## 66           0      11222            0.936        0.936
    ## 67           0       6392            0.936        0.936
    ## 68           0       2227            0.936        0.936
    ## 69           0       9860            0.936        0.936
    ## 70           0      10718            0.936        0.936
    ## 71           0      11746            0.936        0.936
    ## 72           0       5721            0.936        0.936
    ## 73           0       1312            0.936        0.936
    ## 74           0      22971            0.303        4.542
    ## 75           0      43216            0.303        4.542
    ## 76           0      11268            0.303        4.542
    ## 77           0      88675            0.038        0.038
    ## 78           0      26182            0.038        0.038
    ## 79           0       3250            0.038        0.038
    ## 80           0       6093            0.038        0.038

``` r
cor(suicide_num, use = "pairwise.complete.obs")
```

    ##                  suicides_no population mental_hospitals health_units
    ## suicides_no        1.0000000  0.7531811       -0.2361953   -0.2199091
    ## population         0.7531811  1.0000000       -0.2790597   -0.2587414
    ## mental_hospitals  -0.2361953 -0.2790597        1.0000000    0.2476835
    ## health_units      -0.2199091 -0.2587414        0.2476835    1.0000000

``` r
# Make it pretty using a heatmap with geom_tile!
cor(suicide_num, use = "pairwise.complete.obs") %>%
  # Save as a data frame
  as.data.frame %>%
  # Convert row names to an explicit variable
  rownames_to_column %>%
  # Pivot so that all correlations appear in the same column
pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>% ggplot(aes(rowname, other_var, fill=correlation)) +
# Heatmap with geom_tile
geom_tile() +
  # Change the scale to make the middle appear neutral
  scale_fill_gradient2(low="red",mid="white",high="blue") +
  # Overlay values
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
  # Give title and labels
  labs(title = "Correlation matrix for the dataset of Suicide", x = "variable 1", y = "variable 2")
```

![](project-1-$_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# A package for building a correlation matrix with univariate and bivariate graphs # install.packages(psych)
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
pairs.panels(suicide_tidy,
method = "pearson", # correlation coefficient method
hist.col = "blue", # color of histogram
smooth = FALSE, density = FALSE, ellipses = FALSE)
```

![](project-1-$_files/figure-gfm/unnamed-chunk-4-3.png)<!-- --> *the
pair of distinct is mean of suicides\_no and mean of population, which
content has the strongest relationship.*

``` r
# From the dataset suicide_tidy, only consider country Iceland and Thailand, and the suicides_no, population, and mental_hospitals
Iceland_Thailand <- suicide_tidy %>%
  filter(country %in% c("Iceland","Thailand"))%>%
  select(suicides_no,population,mental_hospitals )
Iceland_Thailand
```

    ##    suicides_no population mental_hospitals
    ## 1         1421   10084647            0.028
    ## 2          734    5203957            0.028
    ## 3          646    4796355            0.028
    ## 4          375   10629684            0.028
    ## 5          322    4736305            0.028
    ## 6          222    6049756            0.028
    ## 7          152    1124052            0.028
    ## 8          116    4702656            0.028
    ## 9           69    1589015            0.028
    ## 10          60    4525574            0.028
    ## 11          11      44024            0.303
    ## 12          10      33247            0.303
    ## 13           8      25029            0.303
    ## 14           6      24099            0.303
    ## 15           2      23233            0.303
    ## 16           2      32963            0.303
    ## 17           1       8723            0.303
    ## 18           0      22971            0.303
    ## 19           0      43216            0.303
    ## 20           0      11268            0.303

``` r
# Calculate distances with R function 
dist(Iceland_Thailand,method="euclidean")
```

    ##               1            2            3            4            5
    ## 2  4.880690e+06                                                    
    ## 3  5.288292e+06 4.076020e+05                                       
    ## 4  5.450380e+05 5.425727e+06 5.833329e+06                          
    ## 5  5.348342e+06 4.676522e+05 6.005087e+04 5.893379e+06             
    ## 6  4.034891e+06 8.457992e+05 1.253401e+06 4.579928e+06 1.313451e+06
    ## 7  8.960595e+06 4.079905e+06 3.672303e+06 9.505632e+06 3.612253e+06
    ## 8  5.381991e+06 5.013014e+05 9.370050e+04 5.927028e+06 3.364963e+04
    ## 9  8.495632e+06 3.614942e+06 3.207340e+06 9.040669e+06 3.147290e+06
    ## 10 5.559073e+06 6.783833e+05 2.707816e+05 6.104110e+06 2.107312e+05
    ## 11 1.004062e+07 5.159933e+06 4.752331e+06 1.058566e+07 4.692281e+06
    ## 12 1.005140e+07 5.170710e+06 4.763108e+06 1.059644e+07 4.703058e+06
    ## 13 1.005962e+07 5.178928e+06 4.771326e+06 1.060466e+07 4.711276e+06
    ## 14 1.006055e+07 5.179858e+06 4.772256e+06 1.060559e+07 4.712206e+06
    ## 15 1.006141e+07 5.180724e+06 4.773122e+06 1.060645e+07 4.713072e+06
    ## 16 1.005168e+07 5.170994e+06 4.763392e+06 1.059672e+07 4.703342e+06
    ## 17 1.007592e+07 5.195234e+06 4.787632e+06 1.062096e+07 4.727582e+06
    ## 18 1.006168e+07 5.180986e+06 4.773384e+06 1.060671e+07 4.713334e+06
    ## 19 1.004143e+07 5.160741e+06 4.753139e+06 1.058647e+07 4.693089e+06
    ## 20 1.007338e+07 5.192689e+06 4.785087e+06 1.061842e+07 4.725037e+06
    ##               6            7            8            9           10
    ## 2                                                                  
    ## 3                                                                  
    ## 4                                                                  
    ## 5                                                                  
    ## 6                                                                  
    ## 7  4.925704e+06                                                    
    ## 8  1.347100e+06 3.578604e+06                                       
    ## 9  4.460741e+06 4.649630e+05 3.113641e+06                          
    ## 10 1.524182e+06 3.401522e+06 1.770820e+05 2.936559e+06             
    ## 11 6.005732e+06 1.080028e+06 4.658632e+06 1.544991e+06 4.481550e+06
    ## 12 6.016509e+06 1.090805e+06 4.669409e+06 1.555768e+06 4.492327e+06
    ## 13 6.024727e+06 1.099023e+06 4.677627e+06 1.563986e+06 4.500545e+06
    ## 14 6.025657e+06 1.099953e+06 4.678557e+06 1.564916e+06 4.501475e+06
    ## 15 6.026523e+06 1.100819e+06 4.679423e+06 1.565782e+06 4.502341e+06
    ## 16 6.016793e+06 1.091089e+06 4.669693e+06 1.556052e+06 4.492611e+06
    ## 17 6.041033e+06 1.115329e+06 4.693933e+06 1.580292e+06 4.516851e+06
    ## 18 6.026785e+06 1.101081e+06 4.679685e+06 1.566044e+06 4.502603e+06
    ## 19 6.006540e+06 1.080836e+06 4.659440e+06 1.545799e+06 4.482358e+06
    ## 20 6.038488e+06 1.112784e+06 4.691388e+06 1.577747e+06 4.514306e+06
    ##              11           12           13           14           15
    ## 2                                                                  
    ## 3                                                                  
    ## 4                                                                  
    ## 5                                                                  
    ## 6                                                                  
    ## 7                                                                  
    ## 8                                                                  
    ## 9                                                                  
    ## 10                                                                 
    ## 11                                                                 
    ## 12 1.077700e+04                                                    
    ## 13 1.899500e+04 8.218000e+03                                       
    ## 14 1.992500e+04 9.148001e+03 9.300022e+02                          
    ## 15 2.079100e+04 1.001400e+04 1.796010e+03 8.660092e+02             
    ## 16 1.106100e+04 2.841127e+02 7.934002e+03 8.864001e+03 9.730000e+03
    ## 17 3.530100e+04 2.452400e+04 1.630600e+04 1.537600e+04 1.451000e+04
    ## 18 2.105300e+04 1.027600e+04 2.058016e+03 1.128016e+03 2.620076e+02
    ## 19 8.080749e+02 9.969005e+03 1.818700e+04 1.911700e+04 1.998300e+04
    ## 20 3.275600e+04 2.197900e+04 1.376100e+04 1.283100e+04 1.196500e+04
    ##              16           17           18           19
    ## 2                                                     
    ## 3                                                     
    ## 4                                                     
    ## 5                                                     
    ## 6                                                     
    ## 7                                                     
    ## 8                                                     
    ## 9                                                     
    ## 10                                                    
    ## 11                                                    
    ## 12                                                    
    ## 13                                                    
    ## 14                                                    
    ## 15                                                    
    ## 16                                                    
    ## 17 2.424000e+04                                       
    ## 18 9.992000e+03 1.424800e+04                          
    ## 19 1.025300e+04 3.449300e+04 2.024500e+04             
    ## 20 2.169500e+04 2.545000e+03 1.170300e+04 3.194800e+04

``` r
# Clustering with kmeans function for suicides_no and population
clust_data <- suicide_tidy %>%
  select(suicides_no,population)
#install.packages ("cluster")
library(cluster)
# Use the function kmeans to find 3 clusters
kmeans1 <- clust_data %>% kmeans(3)
kmeans1
```

    ## K-means clustering with 3 clusters of sizes 8, 61, 11
    ## 
    ## Cluster means:
    ##   suicides_no population
    ## 1   487.00000  6341116.8
    ## 2    36.18033   185584.8
    ## 3   182.09091  1707103.1
    ## 
    ## Clustering vector:
    ##  [1] 1 1 1 3 3 1 1 2 1 2 2 2 3 2 3 2 2 1 3 2 2 3 3 2 2 2 3 3 1 2 2 2 2 2 2 2 2 2
    ## [39] 2 3 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [77] 2 2 2 2
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 4.472631e+13 2.191933e+12 5.465114e+12
    ##  (between_SS / total_SS =  84.0 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
# Available components
kmeans1$size
```

    ## [1]  8 61 11

``` r
# Save cluster assignment as a column in your dataset
kmeansclust <- clust_data%>%
  mutate(cluster=as.factor(kmeans1$cluster))
# Make a plot of data colored by final cluster assignment
kmeansclust %>%
  ggplot(aes(suicides_no,population,color = cluster)) +
  geom_point()
```

![](project-1-$_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Covariance matrix
suicide_tidy %>%
  select_if(is.numeric) %>%
  cov
```

    ##                    suicides_no    population mental_hospitals  health_units
    ## suicides_no       4.571010e+04  3.273933e+08    -1.433144e+01 -6.515221e+01
    ## population        3.273933e+08  4.133602e+12    -1.610179e+05 -7.289707e+05
    ## mental_hospitals -1.433144e+01 -1.610179e+05     8.054252e-02  9.740687e-02
    ## health_units     -6.515221e+01 -7.289707e+05     9.740687e-02  1.920258e+00

``` r
# Relationships among variables
library(psych)
suicide_num
```

    ##    suicides_no population mental_hospitals health_units
    ## 1         1421   10084647            0.028        0.151
    ## 2          734    5203957            0.028        0.151
    ## 3          646    4796355            0.028        0.151
    ## 4          632    2945568            0.171        0.392
    ## 5          615    2068747            0.171        0.392
    ## 6          375   10629684            0.028        0.151
    ## 7          322    4736305            0.028        0.151
    ## 8          266     376689            0.136        0.682
    ## 9          222    6049756            0.028        0.151
    ## 10         212     277763            0.136        0.682
    ## 11         187     480263            0.165        0.519
    ## 12         171     579838            0.171        0.392
    ## 13         152    1124052            0.028        0.151
    ## 14         146     571003            0.165        0.519
    ## 15         134    1359178            0.171        0.392
    ## 16         127     290660            0.034        0.470
    ## 17         127     375631            0.034        0.470
    ## 18         116    4702656            0.028        0.151
    ## 19         112    2462909            0.171        0.392
    ## 20         104     134519            0.165        0.519
    ## 21          96     191346            0.136        0.682
    ## 22          96    2823207            0.171        0.392
    ## 23          91    1101638            0.171        0.392
    ## 24          85     252851            0.034        0.470
    ## 25          78      80426            0.136        0.682
    ## 26          69     551758            0.165        0.519
    ## 27          69    1589015            0.028        0.151
    ## 28          61     997868            0.171        0.392
    ## 29          60    4525574            0.028        0.151
    ## 30          48     274466            0.165        0.519
    ## 31          47     570380            0.165        0.519
    ## 32          45     390385            0.136        0.682
    ## 33          41     177888            0.136        0.682
    ## 34          41     406249            0.136        0.682
    ## 35          40     246002            0.165        0.519
    ## 36          30     242287            0.165        0.519
    ## 37          29     193001            0.136        0.682
    ## 38          29     113527            0.034        0.470
    ## 39          27     675747            0.038        0.038
    ## 40          27    1264645            0.171        0.392
    ## 41          21     676108            0.038        0.038
    ## 42          18     246805            0.034        0.470
    ## 43          15     292237            0.034        0.470
    ## 44          15     406028            0.034        0.470
    ## 45          14    1041307            0.171        0.392
    ## 46          13     293525            0.038        0.038
    ## 47          11      44024            0.303        4.542
    ## 48          10      33247            0.303        4.542
    ## 49           8      25029            0.303        4.542
    ## 50           8     182632            0.136        0.682
    ## 51           6     231038            0.165        0.519
    ## 52           6     265327            0.165        0.519
    ## 53           6      24099            0.303        4.542
    ## 54           6     168917            0.136        0.682
    ## 55           3      15349            0.034        0.470
    ## 56           3      26305            0.034        0.470
    ## 57           3     179374            0.038        0.038
    ## 58           2      23233            0.303        4.542
    ## 59           2      32963            0.303        4.542
    ## 60           2      99266            0.038        0.038
    ## 61           2     164901            0.038        0.038
    ## 62           1       8723            0.303        4.542
    ## 63           1     148196            0.034        0.470
    ## 64           0       9543            0.936        0.936
    ## 65           0      10271            0.936        0.936
    ## 66           0      11222            0.936        0.936
    ## 67           0       6392            0.936        0.936
    ## 68           0       2227            0.936        0.936
    ## 69           0       9860            0.936        0.936
    ## 70           0      10718            0.936        0.936
    ## 71           0      11746            0.936        0.936
    ## 72           0       5721            0.936        0.936
    ## 73           0       1312            0.936        0.936
    ## 74           0      22971            0.303        4.542
    ## 75           0      43216            0.303        4.542
    ## 76           0      11268            0.303        4.542
    ## 77           0      88675            0.038        0.038
    ## 78           0      26182            0.038        0.038
    ## 79           0       3250            0.038        0.038
    ## 80           0       6093            0.038        0.038

``` r
pairs.panels(suicide_num[1:4], 
             smooth = FALSE, density = FALSE, ellipses = FALSE,
             bg = c("red","yellow","blue")[iris$Species], pch = 21)
```

![](project-1-$_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
## PCA
# Prepare data for PCA and run PCA
pca <- suicide_num %>%
  # Scale to 0 mean and unit variance (standardize)
  scale() %>% 
  prcomp()
# Results from PCA
names(pca)
```

    ## [1] "sdev"     "rotation" "center"   "scale"    "x"

``` r
# Visualize the result PCA
pca
```

    ## Standard deviations (1, .., p=4):
    ## [1] 1.4347142 0.9721625 0.8669921 0.4947928
    ## 
    ## Rotation (n x k) = (4 x 4):
    ##                         PC1       PC2         PC3         PC4
    ## suicides_no      -0.5942403 0.4040137 -0.03010845  0.69479843
    ## population       -0.6109356 0.3338745 -0.02043527 -0.71754295
    ## mental_hospitals  0.3782495 0.5665467 -0.73112055 -0.03761439
    ## health_units      0.3613356 0.6358660  0.68127720 -0.03118339

``` r
# Visualize the rotated data
head(pca$x)
```

    ##            PC1       PC2          PC3        PC4
    ## [1,] -6.871665 3.2138267 -0.166808221  1.1307237
    ## [2,] -3.495593 1.1141175 -0.021004270  0.6206531
    ## [3,] -3.128522 0.8808897 -0.004514723  0.4785265
    ## [4,] -2.280033 0.9465586 -0.233850140  1.0618442
    ## [5,] -1.969306 0.7704448 -0.222643034  1.3160510
    ## [6,] -4.128157 1.3267180 -0.024982630 -2.4608945

``` r
# Add the information about the different groups back into PCA data
pca_data <- data.frame(pca$x, country = suicide_tidy$country)
head(pca_data)
```

    ##         PC1       PC2          PC3        PC4  country
    ## 1 -6.871665 3.2138267 -0.166808221  1.1307237 Thailand
    ## 2 -3.495593 1.1141175 -0.021004270  0.6206531 Thailand
    ## 3 -3.128522 0.8808897 -0.004514723  0.4785265 Thailand
    ## 4 -2.280033 0.9465586 -0.233850140  1.0618442  Romania
    ## 5 -1.969306 0.7704448 -0.222643034  1.3160510  Romania
    ## 6 -4.128157 1.3267180 -0.024982630 -2.4608945 Thailand

``` r
# Plot the data according to the new coordinate system: PC1 and PC2
ggplot(pca_data, aes(x = PC1, y = PC2, color = country)) + 
  geom_point()
```

![](project-1-$_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Take a look at the rotation matrix
pca$rotation
```

    ##                         PC1       PC2         PC3         PC4
    ## suicides_no      -0.5942403 0.4040137 -0.03010845  0.69479843
    ## population       -0.6109356 0.3338745 -0.02043527 -0.71754295
    ## mental_hospitals  0.3782495 0.5665467 -0.73112055 -0.03761439
    ## health_units      0.3613356 0.6358660  0.68127720 -0.03118339

``` r
# Save the rotation matrix in a data frame
rotation_data <- data.frame(
  pca$rotation, 
  variable = row.names(pca$rotation))

# Define an arrow style
arrow_style <- arrow(length = unit(0.05, "inches"), type = "closed")

# Plot the contribution of variables to PCs using geom_segment() for arrows and geom_text() for labels
ggplot(rotation_data) + 
  geom_segment(aes(xend = PC1, yend = PC2), x = 0, y = 0, arrow = arrow_style) + 
  geom_text(aes(x = PC1, y = PC2, label = variable), hjust = 0, size = 3, color = "red") + 
  xlim(-1., 1.25) + 
  ylim(-1., 1.) +
  coord_fixed()
```

![](project-1-$_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
\*Dimensionality Reduction: - I have used PCA on numeric variable. - In
the rotation matrix provided in the PCA object, the variable
suicides\_no and population contribute positive to PC2 but negatively to
PC1. The variable health\_units and mental\_hospitals contribute
positive to both PC1 and PC2.

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
