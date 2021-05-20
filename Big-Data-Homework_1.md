Big Data Homework \#1
================

### **Chapter 3, exercise 14**

``` r
 set.seed(1)
 x1=runif(100)
 x2=0.5*x1+rnorm(100)/10
 y=2+2*x1+0.3*x2+rnorm(100)
```

1)  The regression coefficients are 2 and 0.3, while 2 is the intercept.
    rnorm(100) represents the error term.

2)  The correlation between x1 and x2 is 0.8351212

<!-- end list -->

``` r
plot(x1,x2)
```

![](Big-Data-Homework_1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

3)  
<!-- end list -->

``` r
df <- cbind(y,x1,x2) %% 
  as_tibble()
model1 <- lm(y~ x1+x2, data = df)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x1 + x2, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8311 -0.7273 -0.0537  0.6338  2.3359 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   2.1305     0.2319   9.188 7.61e-15 ***
    ## x1            1.4396     0.7212   1.996   0.0487 *  
    ## x2            1.0097     1.1337   0.891   0.3754    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.056 on 97 degrees of freedom
    ## Multiple R-squared:  0.2088, Adjusted R-squared:  0.1925 
    ## F-statistic:  12.8 on 2 and 97 DF,  p-value: 1.164e-05

``` r
p_value_b1 <- summary(model1)$coefficients[2,4]
p_value_b2 <- summary(model1)$coefficients[3,4]

judge_h0 <- function(p, significance_level = 0.05){
  
  if (p < significance_level){
    answer = paste0("As the p-value is ~",round(p,4)," < ",significance_level,", we reject H0 at the ", significance_level," level of significance.")
  }
  
  else answer = paste0("As the p-value is ~",round(p,4)," >= ",significance_level,", we cannot reject H0 at the ", significance_level," level of significance.")
  
  
  return(answer)
}
```

The intercept is 2.1304996, the estimated b1 is 1.4395554, while the
estimated b2 is 1.0096742.

b1: As the p-value is \~0.0487 \< 0.05, we reject H0 at the 0.05 level
of significance.

b2: As the p-value is \~0.3754 \>= 0.05, we cannot reject H0 at the 0.05
level of significance.

4)  
<!-- end list -->

``` r
model2 <- lm(y~x1,data=df)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x1, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.89495 -0.66874 -0.07785  0.59221  2.45560 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   2.1124     0.2307   9.155 8.27e-15 ***
    ## x1            1.9759     0.3963   4.986 2.66e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.055 on 98 degrees of freedom
    ## Multiple R-squared:  0.2024, Adjusted R-squared:  0.1942 
    ## F-statistic: 24.86 on 1 and 98 DF,  p-value: 2.661e-06

``` r
m2_p_value_b1 <- summary(model2)$coefficients[2,4]
```

As the p-value is \~0 \< 0.05, we reject H0 at the 0.05 level of
significance.

5)  
<!-- end list -->

``` r
model3 <- lm(y~x2,data=df)
summary(model3)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x2, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.62687 -0.75156 -0.03598  0.72383  2.44890 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   2.3899     0.1949   12.26  < 2e-16 ***
    ## x2            2.8996     0.6330    4.58 1.37e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.072 on 98 degrees of freedom
    ## Multiple R-squared:  0.1763, Adjusted R-squared:  0.1679 
    ## F-statistic: 20.98 on 1 and 98 DF,  p-value: 1.366e-05

``` r
m3_p_value_b2 <- summary(model3)$coefficients[2,4]
```

As the p-value is \~0 \< 0.05, we reject H0 at the 0.05 level of
significance.

6)  Not necessarily, because the variables x1 and x2 are strongly
    correlated. So when we omit x1, omitted variable bias occurs and x2
    appears to be statistically significant.

7)  
### Old models

``` r
sjPlot::tab_model(model1, model2, model2, show.ci = FALSE, show.se = TRUE)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

y

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

y

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

y

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

std. Error

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

std. Error

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">

std. Error

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.13

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.23

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.11

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.23

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

2.11

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.23

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x1

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

1.44

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.72

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>0.049</strong>

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

1.98

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.40

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

1.98

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.40

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x2

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

1.01

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

1.13

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.375

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

100

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

100

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

100

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup> / R<sup>2</sup> adjusted

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.209 / 0.193

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.202 / 0.194

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.202 / 0.194

</td>

</tr>

</table>

### Updated models

``` r
 x1=c(x1, 0.1)
 x2=c(x2, 0.8)
 y=c(y,6)
 
 df <- cbind(y,x1,x2) %% 
  as_tibble()
 
model1 <- lm(y~ x1+x2, data = df)

model2 <- lm(y~x1,data=df)

model3 <- lm(y~x2,data=df)

sjPlot::tab_model(model1, model2, model3, show.ci = FALSE, show.se = TRUE)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

y

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

y

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

y

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

std. Error

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

std. Error

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">

std. Error

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.23

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.23

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.26

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.24

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

2.35

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.19

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x1

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.54

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.59

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.365

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

1.77

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.41

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

x2

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

2.51

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.90

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>0.006</strong>

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

3.12

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.60

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

101

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

101

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

101

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup> / R<sup>2</sup> adjusted

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.219 / 0.203

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.156 / 0.148

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.212 / 0.204

</td>

</tr>

</table>

  - In the multivariate regression, the inclusion of the new observation
    shifts the results: x2 becomes statistically significant, whereas x1
    is no longer so. Thus, for this model, the observation is both an
    outlier and a high leverage point.

  - In the second model, the additional observation reduces the adjusted
    R squared, meaning that the model leaves less variation explained.
    The intercept for x1 is slightly lower. Overall, it does not seem
    like the observation is a high leverage point for this model.

  - In the third model, the intercept for x2 increases by over 1 unit,
    meaning the observation could be called a high leverage point. The
    adjusted R squared remains similar as before.

### **Chapter 8, exercise 4**

4.  This question relates to the plots in Figure 8.12.

<!-- end list -->

1)  Sketch the tree corresponding to the partition of the predictor
    space illustrated in the left-hand panel of Figure 8.12. The numbers
    inside the boxes indicate the mean of Y within each region.

![](https://raw.githubusercontent.com/klimaviu/BigDataAnalysis/main/hw1_e4_a.png)

2)  Create a diagram similar to the left-hand panel of Figure 8.12,
    using the tree illustrated in the right-hand panel of the same
    figure. You should divide up the predictor space into the correct
    regions, and indicate the mean for each region

![](https://raw.githubusercontent.com/klimaviu/BigDataAnalysis/main/hw1_e4_b.png)

### **Chapter 6, exercise 9**

1)  
<!-- end list -->

``` r
library(ISLR)

dataset <- ISLR::College %>% 
  mutate(
    Private = ifelse(Private == "Yes",1,0)
  )

training <- dataset %>% 
  sample_n(600)

testing <- dataset %>% 
  anti_join(training)
```

2)  OLS model

<!-- end list -->

``` r
lm_college <- lm(Apps~., data = training)
summary(lm_college)
```

    ## 
    ## Call:
    ## lm(formula = Apps ~ ., data = training)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4830.7  -435.6   -15.7   341.8  7371.9 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -542.61853  485.75635  -1.117 0.264430    
    ## Private     -611.79409  162.12197  -3.774 0.000177 ***
    ## Accept         1.62228    0.04460  36.372  < 2e-16 ***
    ## Enroll        -1.17330    0.21063  -5.570 3.89e-08 ***
    ## Top10perc     51.55845    6.55405   7.867 1.78e-14 ***
    ## Top25perc    -14.46237    5.23551  -2.762 0.005920 ** 
    ## F.Undergrad    0.09391    0.03740   2.511 0.012302 *  
    ## P.Undergrad    0.02739    0.03724   0.735 0.462364    
    ## Outstate      -0.08182    0.02272  -3.601 0.000344 ***
    ## Room.Board     0.17776    0.05729   3.103 0.002011 ** 
    ## Books          0.04394    0.29351   0.150 0.881036    
    ## Personal       0.06322    0.07436   0.850 0.395559    
    ## PhD          -10.14009    5.52455  -1.835 0.066948 .  
    ## Terminal      -3.69358    6.13515  -0.602 0.547385    
    ## S.F.Ratio     21.77874   15.15596   1.437 0.151262    
    ## perc.alumni    5.07854    5.02618   1.010 0.312715    
    ## Expend         0.07554    0.01368   5.521 5.09e-08 ***
    ## Grad.Rate      7.59008    3.50723   2.164 0.030861 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1086 on 582 degrees of freedom
    ## Multiple R-squared:  0.9332, Adjusted R-squared:  0.9312 
    ## F-statistic: 477.9 on 17 and 582 DF,  p-value: < 2.2e-16

``` r
evaluate_model <- function(model){
  
  
prediction <- predict(model, newdata = testing)
actual_value <- testing$Apps

SSE <- sum((prediction -actual_value)^2)
SST <- sum((actual_value - mean(actual_value))^2)
r_squared <- 1 - SSE / SST
rmse <-  sqrt(SSE/nrow(testing))
  
model_name <- deparse(substitute(model))

return(cbind(model_name, rmse, r_squared) %>% as_tibble())
  
}

(m1_eval <- evaluate_model(lm_college))
```

    ## # A tibble: 1 x 3
    ##   model_name rmse             r_squared        
    ##   <chr>      <chr>            <chr>            
    ## 1 lm_college 897.428715825018 0.891940042194879

3)  Ridge regression model

<!-- end list -->

``` r
library(glmnet)

x = training %>% 
  select(-Apps)

y_train = training$Apps

x_test = testing %>% 
  select(-Apps)
y_test = testing$Apps

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

cv_ridge <- cv.glmnet(as.matrix(x), y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min




evaluate_ridge_model <- function(model) {
  
  prediction <- predict(model, s = optimal_lambda, newx = as.matrix(x_test))
  
  actual_value <- testing$Apps

  SSE <- sum((prediction -actual_value)^2)
  SST <- sum((actual_value - mean(actual_value))^2)
  r_squared <- 1 - SSE / SST
  rmse <-  sqrt(SSE/nrow(testing))
    
  model_name <- deparse(substitute(model))
  
  return(cbind(model_name, rmse,  r_squared) %>% as_tibble())
  
}

(m2_eval <- evaluate_ridge_model(ridge_reg))
```

    ## # A tibble: 1 x 3
    ##   model_name rmse             r_squared       
    ##   <chr>      <chr>            <chr>           
    ## 1 ridge_reg  897.376454053946 0.89195262757406

4)  Lasso model

<!-- end list -->

``` r
lasso_reg <- cv.glmnet(as.matrix(x), y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 


lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

(m3_eval <- evaluate_ridge_model(lasso_model))
```

    ## # A tibble: 1 x 3
    ##   model_name  rmse             r_squared        
    ##   <chr>       <chr>            <chr>            
    ## 1 lasso_model 895.629598108723 0.892372873699022

5)  PCR model

<!-- end list -->

``` r
library(pls)

pcr_model <- pcr(Apps~., data = training, scale = TRUE, validation = "CV")
summary(pcr_model)
```

    ## Data:    X dimension: 600 17 
    ##  Y dimension: 600 1
    ## Fit method: svdpc
    ## Number of components considered: 17
    ## 
    ## VALIDATION: RMSEP
    ## Cross-validated using 10 random segments.
    ##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
    ## CV            4144     4066     2124     2122     1970     1799     1738
    ## adjCV         4144     4067     2122     2120     1958     1754     1735
    ##        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## CV        1704     1687     1618      1615      1616      1618      1629
    ## adjCV     1705     1680     1615      1612      1613      1616      1627
    ##        14 comps  15 comps  16 comps  17 comps
    ## CV         1633      1566      1232      1183
    ## adjCV      1630      1542      1225      1176
    ## 
    ## TRAINING: % variance explained
    ##       1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
    ## X      31.813    57.22    64.08    69.75    75.11    80.26    83.83    87.33
    ## Apps    4.003    74.10    74.17    78.31    83.13    83.20    83.86    84.49
    ##       9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
    ## X       90.43     92.88      95.0     96.81     97.90     98.74     99.37
    ## Apps    85.46     85.64      85.7     85.70     85.71     85.71     91.06
    ##       16 comps  17 comps
    ## X        99.84    100.00
    ## Apps     92.77     93.32

We can see that the lowest RMSEP (root mean square error of prediction)
occurs when M = 17.

``` r
pcr_m2 <- pcr(Apps~., data = training, scale = TRUE, ncomp = 17)

evaluate_pcr_model <- function(model, optimal_M){
  
  prediction <-  predict(model, x_test, ncomp=optimal_M)

  actual_value <- testing$Apps

  SSE <- sum((prediction -actual_value)^2)
  SST <- sum((actual_value - mean(actual_value))^2)
  r_squared <- 1 - SSE / SST
  rmse <-  sqrt(SSE/nrow(testing))
    
  model_name <- deparse(substitute(model))
  
  return(cbind(model_name, rmse,  r_squared) %>% as_tibble())

  
}

(m4_eval <- evaluate_pcr_model(pcr_m2, optimal_M = 17))
```

    ## # A tibble: 1 x 3
    ##   model_name rmse             r_squared        
    ##   <chr>      <chr>            <chr>            
    ## 1 pcr_m2     897.428715825017 0.891940042194879

6)  PLS model

<!-- end list -->

``` r
pls_model <-  plsr(Apps~., data = training, scale = TRUE, validation = "CV")
summary(pls_model)
```

    ## Data:    X dimension: 600 17 
    ##  Y dimension: 600 1
    ## Fit method: kernelpls
    ## Number of components considered: 17
    ## 
    ## VALIDATION: RMSEP
    ## Cross-validated using 10 random segments.
    ##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
    ## CV            4144     1996     1745     1575     1489     1336     1253
    ## adjCV         4144     1991     1745     1568     1474     1318     1243
    ##        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## CV        1236     1227     1224      1224      1222      1222      1218
    ## adjCV     1228     1219     1215      1215      1213      1213      1210
    ##        14 comps  15 comps  16 comps  17 comps
    ## CV         1218      1219      1219      1219
    ## adjCV      1210      1210      1210      1210
    ## 
    ## TRAINING: % variance explained
    ##       1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
    ## X       25.81    45.31    62.36    64.99    67.68    72.99    76.96    80.57
    ## Apps    78.30    83.87    87.59    90.82    92.75    93.02    93.09    93.15
    ##       9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
    ## X       82.34     84.85     87.03     90.36     92.77     95.31     97.17
    ## Apps    93.25     93.28     93.30     93.31     93.32     93.32     93.32
    ##       16 comps  17 comps
    ## X        98.77    100.00
    ## Apps     93.32     93.32

The lowest RMSEP occurs when M = 13

``` r
(m5_eval <- evaluate_pcr_model(pls_model, optimal_M = 13))
```

    ## # A tibble: 1 x 3
    ##   model_name rmse            r_squared        
    ##   <chr>      <chr>           <chr>            
    ## 1 pls_model  896.65542007203 0.892126188020192

7)  
<!-- end list -->

``` r
final_results <- rbind(
  m1_eval, m2_eval, m3_eval, m4_eval, m5_eval
) %>% 
  as_tibble() %>% 
  mutate(
    rmse = round(as.numeric(rmse),2),
    r_squared = round(as.numeric(r_squared),4)
  )

knitr::kable(final_results)
```

| model\_name  |   rmse | r\_squared |
| :----------- | -----: | ---------: |
| lm\_college  | 897.43 |     0.8919 |
| ridge\_reg   | 897.38 |     0.8920 |
| lasso\_model | 895.63 |     0.8924 |
| pcr\_m2      | 897.43 |     0.8919 |
| pls\_model   | 896.66 |     0.8921 |

All models have similar values of RMSE and R squared. The R squared is
around 89% for all of them, meaning they do a very good job of
explaining the variation in the test data.
