Big Data Homework \#2
================

### **Chapter 10, exercise 8**

``` r
library(factoextra)

dataset <- datasets::USArrests %>% 
  as_tibble()

pca_model <- prcomp(dataset, scale. = TRUE)
summary(pca_model) 
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4
    ## Standard deviation     1.5749 0.9949 0.59713 0.41645
    ## Proportion of Variance 0.6201 0.2474 0.08914 0.04336
    ## Cumulative Proportion  0.6201 0.8675 0.95664 1.00000

``` r
pca_info <- get_eigenvalue(pca_model) %>% 
  clean_names()

mvar <- pca_model$sdev^2
pves <- mvar/sum(mvar)
```

1)  Using the first method, the PVEs are 0.6200604, 0.2474413,
    0.0891408, 0.0433575, respectively.

<!-- end list -->

``` r
loadings <- pca_model$rotation
scaled_dataset <- scale(dataset)
sumvar <- sum(apply(as.matrix(scaled_dataset)^2, 2, sum))
pves2 <- apply((as.matrix(scaled_dataset) %*% loadings)^2, 2, sum) / sumvar
```

2)  Applying Equation 10.8 directly, the PVEs are 0.6200604, 0.2474413,
    0.0891408, 0.0433575.

### **Chapter 4, exercise 12**

1)  
<!-- end list -->

``` r
Power <- function(){
  return(2^3)
}
```

2)  
<!-- end list -->

``` r
Power2 <- function(x,a){
x^a
}

ans_1 <- Power2(10,3)
ans_2 <- Power2(8,17)
ans_3 <- Power2(131,3)
```

3)  
<!-- end list -->

  - 10^3: 1000
  - 8^17: 2.251799810^{15}
  - 131^3: 2.24809110^{6}

<!-- end list -->

4)  
<!-- end list -->

``` r
Power3 <- function(x,a){
  return(x^a)
}
```

5)  
<!-- end list -->

``` r
x <- seq(1,10,1)
y <- Power3(x,2)

plot(x,y,type="l",
     main = "X vs. x squared",
     xlab = "x",
     ylab = "x, squared")
```

![](Big-Data-Homework_2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

6)  
<!-- end list -->

``` r
PlotPower <- function(x_vec, a){
  
  y <- Power3(x_vec,a)
  
  plot(x_vec,y,type="l",
       main = paste0("X vs. x raised to the power of ", a),
       xlab = "x",
       ylab = paste0("x, raised to the power of ",a))
  
}


PlotPower(1:10, 3)
```

![](Big-Data-Homework_2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### **Chapter 7, exercise 6**

1)  
<!-- end list -->

``` r
Wage <- ISLR::Wage



poly_cv <- function(K, degree){
  
  df <- Wage %>% 
    filter(!is.na(age))
  
  df.shuffled <- df[sample(nrow(df)),]

folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

mse = matrix(data=NA,nrow=K,ncol=degree)

for(i in 1:K){

    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- df.shuffled[testIndexes, ]
    trainData <- df.shuffled[-testIndexes, ]

    for (j in 1:degree){
        fit.train = lm(wage ~ poly(age,j), data=trainData)
        fit.test = predict(fit.train, newdata=testData)
        mse[i,j] = mean((fit.test-testData$wage)^2, na.rm=TRUE) 
    }
}

mses <- colMeans(mse)

optimal_d <- which.min(mses)

print(paste0("According to ",K,"-fold cross-validation, the optimal d is: ",optimal_d,"."))

plot(1:degree,colMeans(mse), type = "l", lwd = 2,
     main = "Average test MSE vs. polynomial degree",
     ylab = "Average test MSE",
     xlab = "Polynomial degree")

abline(v = optimal_d, col = "blue", lwd = 2, lty =2)

optimal_model <- lm(wage ~ poly(age,optimal_d), data=trainData)
testData$prediction <-  predict(optimal_model, newdata=testData)

plot <- testData %>% 
  ggplot(aes(age, prediction))+
  geom_line(colour = "blue", size = 1.5)+
  geom_point(aes(x = age, y = wage))+
  labs(
    title =  "Prediction fit"
  )

print(plot)

return(optimal_d)

}

optimal_d <- poly_cv(15, 20)
```

    ## [1] "According to 15-fold cross-validation, the optimal d is: 9."

![](Big-Data-Homework_2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](Big-Data-Homework_2_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

ANOVA: exploiting the fact that poly() creates orthogonal polynomials

``` r
max.fit =  lm(wage ~ poly(age,20), data=Wage)

fit_summary <- coef(summary(max.fit)) %>% 
  as_tibble() %>% 
  mutate(
    degree = row_number()
  ) 

fit_summary%>% 
  kable()
```

|     Estimate | Std. Error |      t value | Pr(\>|t|) | degree |
| -----------: | ---------: | -----------: | --------: | -----: |
|   111.703608 |  0.7290723 |  153.2133574 | 0.0000000 |      1 |
|   447.067853 | 39.9329321 |   11.1954677 | 0.0000000 |      2 |
| \-478.315806 | 39.9329321 | \-11.9779786 | 0.0000000 |      3 |
|   125.521686 | 39.9329321 |    3.1433125 | 0.0016869 |      4 |
|  \-77.911181 | 39.9329321 |  \-1.9510508 | 0.0511446 |      5 |
|  \-35.812889 | 39.9329321 |  \-0.8968259 | 0.3698843 |      6 |
|    62.707716 | 39.9329321 |    1.5703259 | 0.1164455 |      7 |
|    50.549790 | 39.9329321 |    1.2658672 | 0.2056596 |      8 |
|  \-11.254732 | 39.9329321 |  \-0.2818409 | 0.7780851 |      9 |
|  \-83.691799 | 39.9329321 |  \-2.0958090 | 0.0361833 |     10 |
|     1.624050 | 39.9329321 |    0.0406694 | 0.9675622 |     11 |
|    10.158814 | 39.9329321 |    0.2543969 | 0.7992065 |     12 |
|   \-2.607628 | 39.9329321 |  \-0.0653002 | 0.9479394 |     13 |
|    13.766887 | 39.9329321 |    0.3447502 | 0.7303065 |     14 |
|  \-15.573009 | 39.9329321 |  \-0.3899791 | 0.6965799 |     15 |
|  \-28.189593 | 39.9329321 |  \-0.7059234 | 0.4802909 |     16 |
|    35.807251 | 39.9329321 |    0.8966847 | 0.3699596 |     17 |
|  \-48.292005 | 39.9329321 |  \-1.2093278 | 0.2266329 |     18 |
|     7.911025 | 39.9329321 |    0.1981078 | 0.8429743 |     19 |
|    23.361139 | 39.9329321 |    0.5850094 | 0.5585857 |     20 |
|    26.672861 | 39.9329321 |    0.6679415 | 0.5042227 |     21 |

``` r
highest_significant_degree <- fit_summary %>% 
  clean_names() %>% 
  filter(pr_t < 0.05) %>% 
  slice_max(degree, n = 1)


anova_vs_cv_answer <- ifelse(
  highest_significant_degree$degree == optimal_d, "Both methods produce the same result.",
  "Anova and cross-validation produced different results."
)
```

  - According to ANOVA, the highest significant degree is 10.

  - According to cross-validation, the highest significant degree is 9.

  - Anova and cross-validation produced different results.

<!-- end list -->

2)  
<!-- end list -->

``` r
cut_cv <- function(K, nr_cuts){
  
  df <- Wage %>% 
    filter(!is.na(age))
  
  df.shuffled <- df[sample(nrow(df)),]

folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

mse = matrix(data=NA,nrow=K,ncol=nr_cuts)

for (i in 1:K) {
  
  testIndexes <- which(folds == i, arr.ind = TRUE)
  
  for (j in 2:nr_cuts) {
    
  df.shuffled$age_level <- cut(df.shuffled$age, j)
    
  testData <- df.shuffled[testIndexes,]
  trainData <- df.shuffled[-testIndexes,]
  
  testData <- testData %>% filter(age_level %in% unique(trainData$age_level))
    
    fit.train = lm(wage ~ age_level, data = trainData)
    
    fit.test = predict(
      fit.train,
      newdata = testData )
    
    mse[i, j] = mean((fit.test - testData$wage) ^ 2, na.rm = TRUE)
  }
}

mses <- colMeans(mse)

optimal_nr <- which.min(mses)

print(paste0("According to ",K,"-fold cross-validation, the optimal number of cuts is: ",optimal_nr,"."))

plot(1:nr_cuts,colMeans(mse), type = "l", lwd = 2,
     main = "Average test MSE vs. number of cuts",
     ylab = "Average test MSE",
     xlab = "Number of cuts")

abline(v = optimal_nr, col = "blue", lwd = 2, lty =2)

optimal_model <- lm(wage ~ cut(age,optimal_nr), data=trainData)
testData$prediction <-  predict(optimal_model, newdata=testData)

plot <- testData %>% 
  ggplot(aes(age, prediction))+
  geom_line(colour = "blue", size = 1.5)+
  geom_point(aes(x = age, y = wage))+
  labs(
    title =  "Prediction fit"
  )

print(plot)

return(optimal_nr)

}

set.seed(1)

cut_cv(5,10)
```

    ## [1] "According to 5-fold cross-validation, the optimal number of cuts is: 8."

![](Big-Data-Homework_2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->![](Big-Data-Homework_2_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

    ## [1] 8
