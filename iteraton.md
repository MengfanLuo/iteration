iteration
================
Mengfan Luo
23/10/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1] -0.03865847 -1.98222979 -0.29207551 -0.25766755 -1.85520777  0.63270985
    ##  [7] -0.29390663 -0.36283128 -0.59448931 -0.67295561 -0.78758096 -0.70633004
    ## [13] -0.73154755 -0.02676072  0.93909895 -0.06785764  1.29796142  0.79203046
    ## [19]  0.38942663  0.30119596  0.32338218  1.21465525  2.72136960 -0.66644491
    ## [25]  0.72471344

``` r
z_scores = function(x) {
  z = (x - mean(x))/sd(x)
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -0.03865847 -1.98222979 -0.29207551 -0.25766755 -1.85520777  0.63270985
    ##  [7] -0.29390663 -0.36283128 -0.59448931 -0.67295561 -0.78758096 -0.70633004
    ## [13] -0.73154755 -0.02676072  0.93909895 -0.06785764  1.29796142  0.79203046
    ## [19]  0.38942663  0.30119596  0.32338218  1.21465525  2.72136960 -0.66644491
    ## [25]  0.72471344

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores(c("a","b","c"))
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

Improve:

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)){
    stop("x needs to be numeric")
  }
  
  if (length(x)<3){
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x))/sd(x)
  return(z)
}
```

## Multiple outcomes

Return a output dataframe

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
```

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.96  3.58

How to debug?

``` r
# create an x
x = x_vec

#Then can check those inside the function

#Don't forget to remove x
rm(x)
```

## different sample sizes, means and sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.31  3.05

Functions that simulates data, compute mean and sd

``` r
sim_mean_sd = function(n, mu = 0, sigma = 1){
  sim_data = tibble(
    x = rnorm(n,mean = mu,sd = sigma)
  )
  
   sim_data %>% 
     summarize(mean = mean(x),
                     sd = sd(x))
}

sim_mean_sd(30)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.338  1.17

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

A lot of pages for reviews…

A function that gives reviews based on url

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

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

get_page_reviews(url)
```

    ## # A tibble: 10 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Yeah., it was pretty good.                            5.0 ou~ "\n  Yeah, it ~
    ##  2 Love it                                               5.0 ou~ "\n  Didn't li~
    ##  3 it was                                                5.0 ou~ "\n  mad good ~
    ##  4 Fun!                                                  4.0 ou~ "\n  Fun and e~
    ##  5 Vintage                                               5.0 ou~ "\n  Easy to o~
    ##  6 too many commercials                                  1.0 ou~ "\n  5 minutes~
    ##  7 this film is so good!                                 5.0 ou~ "\n  VOTE FOR ~
    ##  8 Good movie                                            5.0 ou~ "\n  Weird sto~
    ##  9 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ## 10 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~

``` r
base_url  =  "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url,1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5])
)
```

    ## # A tibble: 50 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Yeah., it was pretty good.                            5.0 ou~ "\n  Yeah, it ~
    ##  2 Love it                                               5.0 ou~ "\n  Didn't li~
    ##  3 it was                                                5.0 ou~ "\n  mad good ~
    ##  4 Fun!                                                  4.0 ou~ "\n  Fun and e~
    ##  5 Vintage                                               5.0 ou~ "\n  Easy to o~
    ##  6 too many commercials                                  1.0 ou~ "\n  5 minutes~
    ##  7 this film is so good!                                 5.0 ou~ "\n  VOTE FOR ~
    ##  8 Good movie                                            5.0 ou~ "\n  Weird sto~
    ##  9 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ## 10 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ## # ... with 40 more rows
