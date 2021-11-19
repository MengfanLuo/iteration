iteration and list cols
================
Mengfan Luo
23/10/2021

## Lists

``` r
l  =list(
  vec_num = 5:8,
  vec_log = c(TRUE,FALSE),
  summary = summary(rnorm(1000,mean = 5, sd = 3))
)

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.495   3.082   4.940   4.926   6.799  14.740

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.495   3.082   4.940   4.926   6.799  14.740

## List of norms

``` r
list_norms = 
list(
  a = rnorm(50,mean  =2, sd = 1),
  b = rnorm(50,mean  =5, sd = 3),
  c = rnorm(50,mean  =20, sd = 1.2),
  d = rnorm(50,mean  =-12, sd = 0.5)
)
```

## Define function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if (length(x)<3){
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = tibble(
    mean = mean_x,
    sd = sd_x
  )
  
  return(output_df)
}

mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.19 0.914

## for loop

``` r
output = vector("list", length = 4)

output[[1]] = mean_and_sd(list_norms[[1]])

for (i in 1:4){
  output[[i]] = mean_and_sd(list_norms[[i]])
}

## Can add more line to output list

output[[7]] = 1
```

Use `map` instead

``` r
output2 = map(list_norms,mean_and_sd)

output3 = map(list_norms,mean)
output4 = map_dbl(list_norms,mean)
```

List columna

``` r
list_col_df = 
  tibble(
    names = c("a","b","c","d"),
    norms = list_norms
  )

list_col_df %>% 
  filter(names == "a")
```

    ## # A tibble: 1 x 2
    ##   names norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
list_col_df$norms[[1]]
```

    ##  [1] 0.9411537 2.1915560 1.0299960 2.0353616 2.2424669 3.7576723 1.1538523
    ##  [8] 0.8649143 1.6849788 2.1919925 3.4134958 2.3128769 1.8057871 2.3046675
    ## [15] 2.5587121 1.5425755 2.4475428 4.2552529 1.0587616 2.9143324 2.3895391
    ## [22] 2.6604158 2.3163178 0.6216191 1.6595276 1.7537638 3.1216632 2.9147574
    ## [29] 1.4888306 3.7134160 2.3142023 3.7769708 3.4849042 2.2746255 2.5192549
    ## [36] 1.2248280 1.2686050 1.9143870 3.7683106 1.3187426 1.8354084 1.4415829
    ## [43] 0.9011392 2.5102836 2.1734667 3.2810919 2.2488023 0.4927518 2.2581782
    ## [50] 3.0635298

``` r
map(list_col_df$norms,mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.19 0.914
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.61  3.14
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.0  1.18
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.0 0.489

``` r
list_col_df %>% 
  mutate(summaries = map(norms,mean_and_sd))
```

    ## # A tibble: 4 x 3
    ##   names norms        summaries       
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [50]>   <tibble [1 x 2]>
    ## 2 b     <dbl [50]>   <tibble [1 x 2]>
    ## 3 c     <dbl [50]>   <tibble [1 x 2]>
    ## 4 d     <dbl [50]>   <tibble [1 x 2]>

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\asus\AppData\Local/Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-12 10:27:32 (7.62)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: C:\Users\asus\AppData\Local/Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-12 10:27:47 (1.701)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: C:\Users\asus\AppData\Local/Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-12 10:27:54 (0.914)

    ## file min/max dates: 1999-09-01 / 2021-10-31

Nest data within location

``` r
weather_nested = nest(weather_df,data = date:tmin)

weather_nested %>% 
  filter(name == "CentralPark_NY") %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ... with 355 more rows

``` r
weather_nested %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ... with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # ... with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # ... with 355 more rows

``` r
weather_nested$data[[1]]
```

    ## # A tibble: 365 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ... with 355 more rows

``` r
lm(tmax~tmin, data = weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nested$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
weather_lm = function(df){
  lm(tmax~tmin, data = df)
}

weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data,weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>% 
  mutate(lm_results = map(data,weather_lm)) 
```

    ## # A tibble: 3 x 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 x 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 x 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 x 4]> <lm>

``` r
  unnest(weather_nested)
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(data)`

    ## # A tibble: 1,095 x 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # ... with 1,085 more rows

## Napolean

Functions to get reviews/stars

``` r
get_page_reviews = function(page_url){
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  return(reviews)
}
```

``` r
base_url  =  "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url,1:5)

map(urls,get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 x 3
    ##    title                            stars              text                     
    ##    <chr>                            <chr>              <chr>                    
    ##  1 no brainer                       5.0 out of 5 stars "\n  watched this with m~
    ##  2 Yeah., it was pretty good.       5.0 out of 5 stars "\n  Yeah, it was pretty~
    ##  3 Love it                          5.0 out of 5 stars "\n  Didn't like this wh~
    ##  4 it was                           5.0 out of 5 stars "\n  mad good yo\n"      
    ##  5 Fun!                             4.0 out of 5 stars "\n  Fun and entertainin~
    ##  6 Vintage                          5.0 out of 5 stars "\n  Easy to order. I th~
    ##  7 too many commercials             1.0 out of 5 stars "\n  5 minutes into the ~
    ##  8 this film is so good!            5.0 out of 5 stars "\n  VOTE FOR PEDRO!\n"  
    ##  9 Good movie                       5.0 out of 5 stars "\n  Weird story, goofy ~
    ## 10 I Just everyone to know this.... 5.0 out of 5 stars "\n  VOTE FOR PEDRO !!!!~
    ## 
    ## [[2]]
    ## # A tibble: 10 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  2 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ##  3 Classic Film                                          5.0 ou~ "\n  Had to or~
    ##  4 hehehehe                                              5.0 ou~ "\n  goodjobbo~
    ##  5 Painful                                               1.0 ou~ "\n  I think I~
    ##  6 GRAND                                                 5.0 ou~ "\n  GRAND\n"  
    ##  7 Hello, 90s                                            5.0 ou~ "\n  So nostal~
    ##  8 Cult Classic                                          5.0 ou~ "\n  Watched i~
    ##  9 Format was inaccurate                                 4.0 ou~ "\n  There was~
    ## 10 Good funny                                            3.0 ou~ "\n  Would rec~
    ## 
    ## [[3]]
    ## # A tibble: 10 x 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Not available w/in 48 hour window           1.0 out of 5 stars "\n  I couldn~
    ##  2 Your mom went to college.                   5.0 out of 5 stars "\n  Classic ~
    ##  3 Very funny movie                            5.0 out of 5 stars "\n  I watch ~
    ##  4 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing ~
    ##  5 A classic                                   5.0 out of 5 stars "\n  If you d~
    ##  6 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g~
    ##  7 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t~
    ##  8 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf~
    ##  9 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ## 10 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo~
    ## 
    ## [[4]]
    ## # A tibble: 10 x 3
    ##    title                                           stars              text      
    ##    <chr>                                           <chr>              <chr>     
    ##  1 "Hilarious"                                     5.0 out of 5 stars "\n  Funn~
    ##  2 "Love it"                                       5.0 out of 5 stars "\n  What~
    ##  3 "WORTH IT!"                                     5.0 out of 5 stars "\n  It's~
    ##  4 "Funny movie."                                  5.0 out of 5 stars "\n  Grea~
    ##  5 "Best movie ever!"                              5.0 out of 5 stars "\n  Got ~
    ##  6 "I was stuck in the oil patch back in the day." 5.0 out of 5 stars "\n  I wa~
    ##  7 "Funny Dork humor"                              5.0 out of 5 stars "\n  Humo~
    ##  8 "Still funny!"                                  5.0 out of 5 stars "\n  Stil~
    ##  9 "Love it!! \U0001f49c"                          5.0 out of 5 stars "\n  Love~
    ## 10 "LOVE it"                                       5.0 out of 5 stars "\n  cult~
    ## 
    ## [[5]]
    ## # A tibble: 10 x 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 "Perfect"                         5.0 out of 5 stars "\n  Exactly what I ask~
    ##  2 "Love this movie!"                5.0 out of 5 stars "\n  Great movie and se~
    ##  3 "Love it"                         5.0 out of 5 stars "\n  Love this movie. H~
    ##  4 "As described"                    3.0 out of 5 stars "\n  Book is as describ~
    ##  5 "GOSH!!!"                         5.0 out of 5 stars "\n  Just watch the mov~
    ##  6 "Watch it right now"              5.0 out of 5 stars "\n  You need to watch ~
    ##  7 "At this point it’s an addiction" 5.0 out of 5 stars "\n  I watch this movie~
    ##  8 "\U0001f495"                      5.0 out of 5 stars "\n  Hands down, one of~
    ##  9 "Good dumb movie"                 5.0 out of 5 stars "\n  I really wanted to~
    ## 10 "funny"                           5.0 out of 5 stars "\n  so funny and inven~

``` r
napreviews_df = 
  tibble(
    urls = urls
  )


napreviews_df %>% 
  mutate(reviews = map(urls,get_page_reviews)) %>% 
  select(reviews) %>% 
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

    ## # A tibble: 50 x 3
    ##    title                            stars              text                     
    ##    <chr>                            <chr>              <chr>                    
    ##  1 no brainer                       5.0 out of 5 stars "\n  watched this with m~
    ##  2 Yeah., it was pretty good.       5.0 out of 5 stars "\n  Yeah, it was pretty~
    ##  3 Love it                          5.0 out of 5 stars "\n  Didn't like this wh~
    ##  4 it was                           5.0 out of 5 stars "\n  mad good yo\n"      
    ##  5 Fun!                             4.0 out of 5 stars "\n  Fun and entertainin~
    ##  6 Vintage                          5.0 out of 5 stars "\n  Easy to order. I th~
    ##  7 too many commercials             1.0 out of 5 stars "\n  5 minutes into the ~
    ##  8 this film is so good!            5.0 out of 5 stars "\n  VOTE FOR PEDRO!\n"  
    ##  9 Good movie                       5.0 out of 5 stars "\n  Weird story, goofy ~
    ## 10 I Just everyone to know this.... 5.0 out of 5 stars "\n  VOTE FOR PEDRO !!!!~
    ## # ... with 40 more rows

``` r
bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5])
)
```

    ## # A tibble: 50 x 3
    ##    title                            stars              text                     
    ##    <chr>                            <chr>              <chr>                    
    ##  1 no brainer                       5.0 out of 5 stars "\n  watched this with m~
    ##  2 Yeah., it was pretty good.       5.0 out of 5 stars "\n  Yeah, it was pretty~
    ##  3 Love it                          5.0 out of 5 stars "\n  Didn't like this wh~
    ##  4 it was                           5.0 out of 5 stars "\n  mad good yo\n"      
    ##  5 Fun!                             4.0 out of 5 stars "\n  Fun and entertainin~
    ##  6 Vintage                          5.0 out of 5 stars "\n  Easy to order. I th~
    ##  7 too many commercials             1.0 out of 5 stars "\n  5 minutes into the ~
    ##  8 this film is so good!            5.0 out of 5 stars "\n  VOTE FOR PEDRO!\n"  
    ##  9 Good movie                       5.0 out of 5 stars "\n  Weird story, goofy ~
    ## 10 I Just everyone to know this.... 5.0 out of 5 stars "\n  VOTE FOR PEDRO !!!!~
    ## # ... with 40 more rows
