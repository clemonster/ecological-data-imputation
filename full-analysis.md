---
title: "Imputing Ecological Data"
author: "Clement Ponsonnet"
date: "November 2017"
output:
  html_document:
    keep_md : true
  html_notebook: default
---



##Questions

**1 - When you suggest methods to deal with missing values to users, the recurrent question is "What is the percentage of missing values that I can have in my data set, is 50% too much but 20% OK?" What is your answer to this question?**

*It depends on the dataset. If your data is highly correlated, you can have many missing values and impute precisely. If the data is vey noisy, even just a few missing values can be problematic. Multiple imputation can always be performed and enables us to estimate the variability of the predictions, which will tell us how much we can trust the result of the imputation*

**2 - Explain the aims of multiple imputation in comparison to single imputation.**

*With single imputation we underestimate the variance of our data that comes from the missing values. Multiple imputation is a way of conserving this variance because we impute both the missing values and their variability*

**3 - Your aim is to impute a data set (predict as well as possible the missing values). You have 3 imputation methods available. How could you compare them?**

*Do cross-validation: take a dataset where there is no missing data. Take out some data, and impute those missing values 3 times using each method. Each time, compute the errors of the predictions. Choose the method which yields the lowest mean square error (or other error metric)*


## Continuous data with missing values - Regression with missing data via Multiple Imputation
First of all you will need to install the following packages


```r
install.packages("VIM")
install.packages("missMDA")
install.packages("Amelia")
```


```r
library(VIM)
library(missMDA)
library(Amelia)
library(FactoMineR)
```

Air pollution is currently one of the most serious public health worries worldwide. Many epidemiological studies
have proved the influence that some chemical compounds, such as sulphur dioxide (SO2), nitrogen dioxide
(NO2), ozone (O3) can have on our health. Associations set up to monitor air quality are active all over the
world to measure the concentration of these pollutants. They also keep a record of meteorological conditions
such as temperature, cloud cover, wind, etc.  

We have at our disposal 112 observations collected
during the summer of 2001 in Rennes. The variables available are 

* maxO3 (maximum daily ozone) 
* maxO3v (maximum daily ozone the previous day) 
* T12 (temperature at midday) 
* T9 
* T15 (Temp at 3pm)
* Vx12 (projection of the wind speed vector on the east-west axis at midday)
* Vx9 and Vx15 as well as the Nebulosity (cloud) Ne9, Ne12, Ne15

Here the final aim is to analyse the relationship between the
maximum daily ozone (maxO3) level and the other meteorological variables. To do so we will perform regression to explain maxO3 in function of all the other variables. This data is incomplete (there are missing values). Indeed, it occurs frenquently to have machines that fail one day, leading to some information not recorded. We will therefore perform regression via multiple imputation.

__(R1)__ Import the data.


```r
ozo <- read.table("data/ozoneNA.csv",header=TRUE,
sep=",", row.names=1)
WindDirection <- ozo[,12]
don <- ozo[,1:11]   #### keep only the continuous variables
summary(don)
```

```
##      maxO3              T9             T12             T15       
##  Min.   : 42.00   Min.   :11.30   Min.   :14.30   Min.   :14.90  
##  1st Qu.: 71.00   1st Qu.:16.00   1st Qu.:18.60   1st Qu.:18.90  
##  Median : 81.50   Median :17.70   Median :20.40   Median :21.40  
##  Mean   : 91.24   Mean   :18.22   Mean   :21.46   Mean   :22.41  
##  3rd Qu.:108.25   3rd Qu.:19.90   3rd Qu.:23.60   3rd Qu.:25.65  
##  Max.   :166.00   Max.   :25.30   Max.   :33.50   Max.   :35.50  
##  NA's   :16       NA's   :37      NA's   :33      NA's   :37     
##       Ne9             Ne12            Ne15           Vx9         
##  Min.   :0.000   Min.   :0.000   Min.   :0.00   Min.   :-7.8785  
##  1st Qu.:3.000   1st Qu.:4.000   1st Qu.:3.00   1st Qu.:-3.0000  
##  Median :5.000   Median :5.000   Median :5.00   Median :-0.8671  
##  Mean   :4.987   Mean   :4.986   Mean   :4.60   Mean   :-1.0958  
##  3rd Qu.:7.000   3rd Qu.:7.000   3rd Qu.:6.25   3rd Qu.: 0.6919  
##  Max.   :8.000   Max.   :8.000   Max.   :8.00   Max.   : 5.1962  
##  NA's   :34      NA's   :42      NA's   :32     NA's   :18       
##       Vx12              Vx15            maxO3v      
##  Min.   :-7.8785   Min.   :-9.000   Min.   : 42.00  
##  1st Qu.:-3.6941   1st Qu.:-3.759   1st Qu.: 70.00  
##  Median :-1.9284   Median :-1.710   Median : 82.50  
##  Mean   :-1.6853   Mean   :-1.830   Mean   : 89.39  
##  3rd Qu.:-0.1302   3rd Qu.: 0.000   3rd Qu.:101.00  
##  Max.   : 6.5778   Max.   : 3.830   Max.   :166.00  
##  NA's   :10        NA's   :21       NA's   :12
```

```r
head(don)
```

```
##          maxO3   T9  T12  T15 Ne9 Ne12 Ne15     Vx9    Vx12    Vx15 maxO3v
## 20010601    87 15.6 18.5   NA   4    4    8  0.6946 -1.7101 -0.6946     84
## 20010602    82   NA   NA   NA   5    5    7 -4.3301 -4.0000 -3.0000     87
## 20010603    92 15.3 17.6 19.5   2   NA   NA  2.9544      NA  0.5209     82
## 20010604   114 16.2 19.7   NA   1    1    0      NA  0.3473 -0.1736     92
## 20010605    94   NA 20.5 20.4  NA   NA   NA -0.5000 -2.9544 -4.3301    114
## 20010606    80 17.7 19.8 18.3   6   NA    7 -5.6382 -5.0000 -6.0000     94
```

__(R2)__ Load the libraries.



First, we perfom some descriptive statistics (how many missing? how many variables, individuals with missing?) and try to **inspect and vizualize the pattern of missing entries and get hints on the mechanism** that generated the missingness.  For this purpose, we use the R package **VIM** (Visualization and Imputation of Missing Values - Mathias Templ) as well as Multiple Correspondence Analysis (FactoMineR package). The package VIM provides tools for the visualization of missing or imputed values, which can be used for exploring the data and the structure of the missing or imputed values. Depending on this structure, they may help to identify the mechanism generating the missing values or errors, which may have happened in the imputation process. You can check the documentation for the VIM package by executing


```r
?VIM
```

The VIM function **aggr** calculates and plots the amount of missing entries in each variables and in some combinations of variables (that tend to be missing simultaneously).


```r
paste("dimensions of total dataset: ", dim(don))
paste("dimensions of dataset if we remove rows with missing values: ", dim(na.omit(don)))
dim(na.omit(don))
res<-summary(aggr(don, sortVar=TRUE))$combinations
```


```r
head(res[rev(order(res[,2])),])
```
In the combinations column, 1 means the variable is missing, 0 means it is observed.

So the most  frequent combination is the one where all the variables are observed (13 values). Then, the second one is  where T9, T12 and T15 are simultaneously missing (7 rows) (1 is missing, 0 is observed - there is a 1 for the second, third and fourth variables). The graph on the right panel represents these pattern, with blue for observed and red for missing.


```r
aggr(don, sortVar=TRUE)
```

![](full-analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```
## 
##  Variables sorted by number of missings: 
##  Variable      Count
##      Ne12 0.37500000
##        T9 0.33035714
##       T15 0.33035714
##       Ne9 0.30357143
##       T12 0.29464286
##      Ne15 0.28571429
##      Vx15 0.18750000
##       Vx9 0.16071429
##     maxO3 0.14285714
##    maxO3v 0.10714286
##      Vx12 0.08928571
```


The VIM function **matrixplot ** creates a matrix plot in which all cells of a data matrix are visualized by rectangles. Available data is coded according to a continuous color scheme (gray scale), while missing/imputed data is visualized by a clearly distinguishable color (red). If you use Rstudio the plot is not interactive (thus the warnings), but if you use R directly, you can click on a column of your choice, this will result in sorting the rows in the decreasing order of the values of this column. This is useful to check if there is an association between the value of a variable and the missingness of another one.


```r
matrixplot(don,sortby=2) # marche pas sur Rstudio
```
Here, "red" means data is missing. Non-missing data are on a gray scale with darker shades indicating higher values.  
We notice that often when T9 is missing, T12 and T15 are also missing. When T9 is missing, we do not see more black or white values associated to other variables which should imply that when T9 is missing it would have corresponded to high or low values in another variable. This would have suggested missing at random (MAR) missing values for instance. Here everything points to missing completely at random (MCAR) values.

By playing around with the *sortby* argument, we can make the following observations about the structure of our missing data
* *maxO3 seems uncorrelated in terms with others of missing values* 
* *T9, T12, T15 seem to all be correlated (in the sense that when one variable is missing, the others are likely to be missing)*
* *Ne9, Ne12 and Ne15 are correlated.*
* *Vx9 seems uncorrelated*
* *Vx12 and Vx15 seem correlated*
* *maxO3v seems uncorrelated*


The VIM function **marginplot** creates a scatterplot with additional information on the missing values. If you plot the variables (x,y), the points with no missing values are represented as in a standard scatterplot. The points for which x (resp. y) is missing are represented in red along the y (resp. x) axis. In addition, boxplots of the x and y variables are represented along the axes with and without missing values (in red all variables x where y is missing, in blue all variables x where y is observed).


```r
marginplot(don[,c("T9","maxO3")])
```

We can see here that the distribution of T9 is the same when maxO3 is oberved and when maxO3 is missing. If the two boxplots (red and blue) would have been very different it would imply that when maxO3 is missing the values of T9 can be very high or very low which lead to suspect the MAR hypothesis

**The plots we have showed so far are useful because they may help to identify the mechanism generating the missing values or errors, which may have happened in the imputation process. This is useful to check if there is an association between the value of a variable and the missingness of another one. Do you observe any associations between the missing entries ? When values are missing on a variable does it correspond to small or large values on another one ?**

__(R3)__ Create a categorical dataset with "o" when the value of the cell is observed and "m" when it is missing, and with the same row and column names as in the original data. Then, you can perform Multiple Correspondence Analysis on the categories to visualize the association with the **MCA** function.


```r
class(don)
```

```
## [1] "data.frame"
```

```r
cat <- don
cat <- apply(cat, c(1,2), function(x) return(ifelse(is.na(x),'m','o')))
MCA(cat)
```

![](full-analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->![](full-analysis_files/figure-html/unnamed-chunk-8-2.png)<!-- -->![](full-analysis_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

```
## **Results of the Multiple Correspondence Analysis (MCA)**
## The analysis was performed on 112 individuals, described by 11 variables
## *The results are available in the following objects:
## 
##    name              description                       
## 1  "$eig"            "eigenvalues"                     
## 2  "$var"            "results for the variables"       
## 3  "$var$coord"      "coord. of the categories"        
## 4  "$var$cos2"       "cos2 for the categories"         
## 5  "$var$contrib"    "contributions of the categories" 
## 6  "$var$v.test"     "v-test for the categories"       
## 7  "$ind"            "results for the individuals"     
## 8  "$ind$coord"      "coord. for the individuals"      
## 9  "$ind$cos2"       "cos2 for the individuals"        
## 10 "$ind$contrib"    "contributions of the individuals"
## 11 "$call"           "intermediate results"            
## 12 "$call$marge.col" "weights of columns"              
## 13 "$call$marge.li"  "weights of rows"
```

**Interpretation**  
*If variables are close on the variables factor map (eg. T15_m and T9_m), this means that the missing values are correlated : when T15_m is missing, so is T9_m. The point T_15m corresponds to the barycenter of all the individuals for which T15 is missing. You can see the projection of the individuals on the second graph (which is called the individuals factor map)*


Then, before modeling the data, we perform a **PCA with missing values** to explore the correlation between variables. Use the R package **missMDA** dedicated to perform principal components methods with missing values and to impute data with PC methods.


```r
library(missMDA)
?missMDA
```


__(R4)__ Determine the number of components ncp to keep using the 
**estim_ncpPCA** function. Perform PCA with missing values using the 
**imputePCA** function and ncp components. Then plot the correlation circle.


```r
?estim_ncpPCA
?imputePCA
```


```r
estim_ncpPCA(don)
```

```
## $ncp
## [1] 2
## 
## $criterion
##         0         1         2         3         4         5 
## 170.77775  88.60110  84.97487 104.60004  92.98638 115.43620
```

```r
plot((0:5), estim_ncpPCA(don)$criterion)
```

![](full-analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#Donne le nombre de composantes "optimal" par cross-validation
estim_ncpPCA(don)$ncp
```

```
## [1] 2
```

```r
#Complete les missing values
completed_don <- imputePCA(don, ncp = estim_ncpPCA(don)$ncp)$completeObs


PCA(completed_don)
```

![](full-analysis_files/figure-html/unnamed-chunk-11-2.png)<!-- -->![](full-analysis_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```
## **Results for the Principal Component Analysis (PCA)**
## The analysis was performed on 112 individuals, described by 11 variables
## *The results are available in the following objects:
## 
##    name               description                          
## 1  "$eig"             "eigenvalues"                        
## 2  "$var"             "results for the variables"          
## 3  "$var$coord"       "coord. for the variables"           
## 4  "$var$cor"         "correlations variables - dimensions"
## 5  "$var$cos2"        "cos2 for the variables"             
## 6  "$var$contrib"     "contributions of the variables"     
## 7  "$ind"             "results for the individuals"        
## 8  "$ind$coord"       "coord. for the individuals"         
## 9  "$ind$cos2"        "cos2 for the individuals"           
## 10 "$ind$contrib"     "contributions of the individuals"   
## 11 "$call"            "summary statistics"                 
## 12 "$call$centre"     "mean of the variables"              
## 13 "$call$ecart.type" "standard error of the variables"    
## 14 "$call$row.w"      "weights for the individuals"        
## 15 "$call$col.w"      "weights for the variables"
```



__(Q4)__ Could you guess how cross-validation is performed to select the number of components?

*Probably by doing the imputePCA on a test set for different numbers of components. And choosing the number of components that gives the lowest mean square error*

Then, to run the regression with missing values, we use **Multiple Imputation**. We impute the data either assuming 1) a Gaussian distribution (library Amelia) or 2) a PCA based model (library missMDA). 
Note that there are two ways to impute either using a Joint Modeling (one joint probabilitisc model for the variables all together)
or a Condional Modeling (one model per variable) approach. We refer to the references given in the slides for more details.  We use the R package **Amelia**. We generate 100 imputed data sets with the amelia method:


```r
library(Amelia)
```


```r
?amelia
```


```r
res.amelia <- amelia(don, m=100)  
```

```
## -- Imputation 1 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51
## 
## -- Imputation 2 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61
## 
## -- Imputation 3 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64
## 
## -- Imputation 4 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
## 
## 
## -- Imputation 5 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50
## 
## -- Imputation 6 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42
## 
## -- Imputation 7 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55
## 
## -- Imputation 8 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48
## 
## -- Imputation 9 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
## 
## -- Imputation 10 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25
## 
## -- Imputation 11 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88
## 
## -- Imputation 12 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57
## 
## -- Imputation 13 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34
## 
## -- Imputation 14 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
## 
## 
## -- Imputation 15 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53
## 
## -- Imputation 16 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41
## 
## -- Imputation 17 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29
## 
## -- Imputation 18 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105
## 
## -- Imputation 19 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52
## 
## -- Imputation 20 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
## 
## -- Imputation 21 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
## 
## -- Imputation 22 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49
## 
## -- Imputation 23 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
## 
## -- Imputation 24 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45
## 
## -- Imputation 25 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
## 
## -- Imputation 26 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
## 
## -- Imputation 27 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57
## 
## -- Imputation 28 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67
## 
## -- Imputation 29 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49
## 
## -- Imputation 30 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54
## 
## -- Imputation 31 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51
## 
## -- Imputation 32 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63
## 
## -- Imputation 33 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
## 
## -- Imputation 34 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
## 
## -- Imputation 35 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72
## 
## -- Imputation 36 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41
## 
## -- Imputation 37 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89
## 
## -- Imputation 38 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
##  141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
##  161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
##  181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200
##  201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220
##  221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240
##  241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260
##  261 262 263 264 265 266
## 
## -- Imputation 39 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47
## 
## -- Imputation 40 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45
## 
## -- Imputation 41 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65
## 
## -- Imputation 42 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44
## 
## -- Imputation 43 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
## 
## -- Imputation 44 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
##  141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
##  161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
##  181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200
##  201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220
##  221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240
##  241 242 243
## 
## -- Imputation 45 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
##  141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
##  161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
##  181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200
##  201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220
##  221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240
##  241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260
##  261 262 263 264 265 266 267 268 269 270 271 272 273 274 275 276 277 278 279
## 
## -- Imputation 46 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
## 
## -- Imputation 47 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127
## 
## -- Imputation 48 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93
## 
## -- Imputation 49 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
## 
## -- Imputation 50 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50
## 
## -- Imputation 51 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
##  141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
##  161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
##  181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200
##  201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220
##  221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240
##  241 242 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260
##  261 262 263 264 265 266 267 268 269 270 271 272 273
## 
## -- Imputation 52 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45
## 
## -- Imputation 53 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
## 
## 
## -- Imputation 54 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43
## 
## -- Imputation 55 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
## 
## -- Imputation 56 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73
## 
## -- Imputation 57 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53
## 
## -- Imputation 58 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125
## 
## -- Imputation 59 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
##  141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
##  161 162 163 164 165 166 167
## 
## -- Imputation 60 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48
## 
## -- Imputation 61 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
## 
## -- Imputation 62 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63
## 
## -- Imputation 63 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
## 
## 
## -- Imputation 64 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53
## 
## -- Imputation 65 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54
## 
## -- Imputation 66 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47
## 
## -- Imputation 67 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83
## 
## -- Imputation 68 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61
## 
## -- Imputation 69 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53
## 
## -- Imputation 70 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38
## 
## -- Imputation 71 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
## 
## 
## -- Imputation 72 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76
## 
## -- Imputation 73 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32
## 
## -- Imputation 74 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42
## 
## -- Imputation 75 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28
## 
## -- Imputation 76 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
## 
## -- Imputation 77 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38
## 
## -- Imputation 78 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44
## 
## -- Imputation 79 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31
## 
## -- Imputation 80 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70
## 
## -- Imputation 81 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91
## 
## -- Imputation 82 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
##  141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
##  161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
##  181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198 199 200
##  201 202 203 204 205 206 207
## 
## -- Imputation 83 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43
## 
## -- Imputation 84 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62
## 
## -- Imputation 85 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49
## 
## -- Imputation 86 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46
## 
## -- Imputation 87 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
## 
## -- Imputation 88 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
##  141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
## 
## -- Imputation 89 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43
## 
## -- Imputation 90 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
## 
## -- Imputation 91 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43
## 
## -- Imputation 92 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62
## 
## -- Imputation 93 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
## 
## 
## -- Imputation 94 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42
## 
## -- Imputation 95 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81
## 
## -- Imputation 96 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66
## 
## -- Imputation 97 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52
## 
## -- Imputation 98 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48
## 
## -- Imputation 99 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
##  101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
##  121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
##  141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
##  161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
## 
## -- Imputation 100 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63
```

```r
#names(res.amelia$imputations) 
res.amelia$imputations$imp1 
```

```
##              maxO3       T9      T12      T15         Ne9      Ne12
## 20010601  87.00000 15.60000 18.50000 19.38559  4.00000000  4.000000
## 20010602  82.00000 16.76597 17.73622 18.55485  5.00000000  5.000000
## 20010603  92.00000 15.30000 17.60000 19.50000  2.00000000  6.149045
## 20010604 114.00000 16.20000 19.70000 24.24131  1.00000000  1.000000
## 20010605  94.00000 18.42734 20.50000 20.40000  6.01816777  5.501731
## 20010606  80.00000 17.70000 19.80000 18.30000  6.00000000  4.571503
## 20010607  79.00000 16.80000 15.60000 14.90000  7.00000000  8.000000
## 20010610  79.00000 14.90000 17.50000 18.90000  5.00000000  5.000000
## 20010611 101.00000 16.10000 19.60000 21.40000  2.00000000  3.532964
## 20010612 106.00000 18.30000 22.30190 22.90000  5.00000000  4.985235
## 20010613 101.00000 17.30000 19.30000 20.20000  7.00000000  7.000000
## 20010614  90.00000 17.60000 20.30000 17.40000  6.90039512  6.000000
## 20010615  72.00000 18.50617 22.87722 24.33947  7.00000000  5.000000
## 20010616  70.00000 17.10000 18.20000 18.00000  7.66908852  7.000000
## 20010617  83.00000 15.40000 15.47086 16.60000  8.00000000  7.000000
## 20010618  88.00000 14.67214 19.10000 22.80961  6.00000000  5.000000
## 20010620 102.05407 21.00000 24.60000 26.90000  4.67074646  3.638624
## 20010621  76.27161 15.62673 16.41409 20.18014  7.56592461  7.561540
## 20010622 121.00000 19.70000 24.20000 26.90000  2.00000000  1.000000
## 20010623 146.00000 23.60000 28.60000 28.40000  4.09947838  3.000814
## 20010624 121.00000 20.40000 25.20000 27.70000  1.00000000  0.000000
## 20010625 146.00000 24.24771 28.97041 34.43874  0.02657389  0.000000
## 20010626 108.00000 24.00000 23.50000 29.57853  4.00000000  4.000000
## 20010627  83.00000 19.70000 22.90000 24.80000  1.29058615  2.347589
## 20010628  43.25290 18.27653 18.41883 20.22242  8.20210445  7.351997
## 20010629  81.00000 18.46771 23.26503 27.31425  3.00000000  4.000000
## 20010630  67.00000 20.38816 23.40000 23.70000  4.88190828  5.288584
## 20010701  70.00000 14.53692 20.80710 21.20756  5.00000000  2.000000
## 20010702 106.00000 16.12429 21.65661 22.03842  1.99238071  0.000000
## 20010703 139.00000 23.78721 30.10000 31.90000  0.57203008  1.000000
## 20010704  79.00000 21.14207 21.74640 22.27660  9.64036395  6.881575
## 20010705  68.84075 16.80000 18.20000 22.00000  8.00000000  8.000000
## 20010706 111.27184 20.80000 25.49492 27.30797  0.42124726  3.000000
## 20010707 113.00000 15.31762 18.20000 22.70000  4.28430841  4.438908
## 20010708  72.00000 20.93771 21.20000 23.90000  7.00000000  5.851472
## 20010709  88.00000 19.20000 22.00000 21.40646  8.30933460  6.845498
## 20010710  77.00000 19.40000 20.70000 22.50000  7.00000000  8.000000
## 20010711  71.00000 19.20000 21.00000 22.40000  6.00000000  4.000000
## 20010712  56.00000 13.80000 17.13849 18.50000  8.00000000  8.000000
## 20010713  45.00000 10.53372 14.50000 15.20000  8.00000000  6.515825
## 20010714  67.00000 15.60000 18.60000 20.05710  5.00000000  4.595977
## 20010715  69.68141 16.90000 19.10000 21.07672  5.00000000  7.755903
## 20010716  84.00000 17.40000 20.40000 20.47158  3.00000000  5.435097
## 20010717  63.00000 16.86205 20.50000 20.60000  8.00000000  6.000000
## 20010718  63.37600 15.45559 15.60000 17.45757  6.76160353  8.000000
## 20010719  92.00000 16.70000 19.10000 19.30000  7.00000000  6.000000
## 20010720  88.00000 16.03904 20.30000 21.24744  5.54484092  4.016574
## 20010721  66.00000 18.00000 21.38622 21.12882  8.00000000  6.000000
## 20010722  72.00000 18.60000 21.90000 23.60000  4.00000000  7.000000
## 20010723  81.00000 18.80000 22.50000 23.90000  6.00000000  3.000000
## 20010724  85.95220 19.00000 22.50000 24.10000  2.59768165  6.037746
## 20010725 149.00000 19.90000 26.90000 29.00000  3.00000000  4.000000
## 20010726 153.00000 22.51325 25.26532 26.47447  1.00000000  1.815343
## 20010727 159.00000 24.00000 28.30000 26.50000  2.00000000  2.925899
## 20010728 149.00000 23.30000 27.60000 28.80000  4.00000000  1.597219
## 20010729 160.00000 28.76397 33.08261 35.80712  3.08808415  2.834814
## 20010730 156.00000 24.90000 30.50000 32.20000  0.00000000  1.000000
## 20010731  84.00000 24.77295 26.30000 27.80000  6.85197315  5.124353
## 20010801 126.00000 25.30000 29.50000 31.20000  1.49419806  4.000000
## 20010802 116.00000 21.30000 23.80000 22.10000  7.00000000  7.000000
## 20010803  77.00000 20.00000 18.20000 23.60000  5.00000000  7.000000
## 20010804  63.00000 18.70000 20.60000 20.30000  6.00000000  7.776010
## 20010805  64.65715 18.60000 18.70000 17.80000  8.00000000  8.000000
## 20010806  65.00000 19.20000 23.00000 22.70000  8.00000000  7.000000
## 20010807  72.00000 19.90000 23.43528 20.40000  7.00000000  7.000000
## 20010808  60.00000 18.70000 21.40000 21.70000  7.00000000  7.000000
## 20010809  70.00000 18.40000 17.10000 19.33899  3.00000000  6.000000
## 20010810  77.00000 19.60962 22.83395 25.67891  4.00000000  5.000000
## 20010811  98.00000 18.00214 22.41430 23.53235  1.00000000  1.000000
## 20010812 111.00000 19.46136 24.15144 26.15194  1.00000000  5.000000
## 20010813  75.00000 22.86183 22.27799 24.34126  8.00000000  7.000000
## 20010814 116.00000 23.50000 29.80000 31.70000  1.00000000  3.000000
## 20010815 109.00000 20.80000 23.70000 26.60000  8.00000000  5.000000
## 20010819  67.00000 18.80000 19.25737 18.90000  6.96123992  7.715002
## 20010820  76.00000 17.68070 23.48558 24.00000  6.01175727  5.000000
## 20010821 113.00000 20.60000 24.80000 27.00000  3.82667378  1.090034
## 20010822 117.00000 21.60000 26.90000 28.60000  6.00000000  4.102541
## 20010823 131.00000 23.58056 28.40000 30.10000  5.00000000  3.000000
## 20010824 166.00000 19.80000 27.20000 30.80000  4.00000000  0.000000
## 20010825 159.00000 25.00000 33.50000 35.50000  1.00000000 -1.773299
## 20010826  88.82022 20.10000 22.90000 27.60000  8.00000000  8.000000
## 20010827 114.00000 21.66340 27.15018 27.94839  7.00000000  4.000000
## 20010828 114.65149 21.00000 24.40000 28.55569  1.00000000  6.000000
## 20010829 103.36613 16.90000 17.80000 20.60000  5.12095447  8.987622
## 20010830  76.00000 17.66969 18.60000 18.70000  7.00000000  7.000000
## 20010831  59.00000 16.50000 20.30000 20.30000  5.00000000  7.000000
## 20010901  78.00000 17.70000 20.20000 21.50000  6.32120758  5.870106
## 20010902  76.00000 17.30000 22.70000 24.60000  4.00000000  4.776455
## 20010903  55.00000 15.30000 16.80000 19.20000  8.00000000  7.000000
## 20010904  71.00000 15.90000 19.20000 19.50000  7.00000000  5.000000
## 20010905  90.89311 16.20000 18.90000 19.30000  2.00000000  5.000000
## 20010906  59.00000 17.65789 19.60300 20.71286  7.00000000  7.000000
## 20010907  90.38218 17.60736 23.50447 22.82799  6.00000000  5.000000
## 20010908  63.00000 17.30000 19.80000 19.40000  9.81269155  8.822484
## 20010912  96.38751 14.20000 22.20000 23.00920  5.00000000  5.220775
## 20010913  74.00000 15.80000 18.70000 19.10000 10.59818517  7.000000
## 20010914  71.00000 15.20000 17.90000 18.60000  7.50959222  6.917151
## 20010915  69.00000 17.10000 17.70000 17.50000  6.00000000  7.000000
## 20010916  71.00000 15.40000 16.64581 16.60000  4.00000000  5.000000
## 20010917  60.00000 13.63314 14.26533 17.31172  4.00000000  5.000000
## 20010918  42.00000 13.70624 14.30000 14.90000  8.00000000  7.000000
## 20010919  65.00000 14.80000 16.22858 15.90000  7.00000000  6.981274
## 20010920  71.00000 15.50000 18.00000 17.40000  7.00000000  7.000000
## 20010921  96.00000 11.30000 15.14872 20.20000  3.00000000  3.000000
## 20010922  98.00000 15.20000 19.70000 20.30000  2.00000000  2.000000
## 20010923  92.00000 15.18837 17.60000 18.20000  1.00000000  4.000000
## 20010924  76.00000 13.30000 17.70000 17.70000  7.69230130  4.529095
## 20010925  86.29810 13.30000 17.57597 17.80000  3.00000000  5.000000
## 20010927  77.00000 16.20000 20.80000 18.91661  7.46391854  6.002445
## 20010928  99.00000 17.77853 23.50070 24.94055  4.03945072  2.721671
## 20010929  83.00000 15.14978 18.85833 21.92508  6.40737641  5.000000
## 20010930  70.00000 15.70000 18.60000 20.70000  7.00000000  6.642732
##                Ne15        Vx9       Vx12       Vx15    maxO3v
## 20010601  8.0000000  0.6946000 -1.7101000 -0.6946000  84.00000
## 20010602  7.0000000 -4.3301000 -4.0000000 -3.0000000  87.00000
## 20010603  5.9940177  2.9544000 -0.0511080  0.5209000  82.00000
## 20010604  0.0000000 -0.7322283  0.3473000 -0.1736000  92.00000
## 20010605  3.4741995 -0.5000000 -2.9544000 -4.3301000 114.00000
## 20010606  7.0000000 -5.6382000 -5.0000000 -6.0000000  94.00000
## 20010607  8.4519716 -4.3301000 -1.8794000 -3.7588000  80.00000
## 20010610  3.9770461  0.0000000 -1.0419000 -1.3892000  99.00000
## 20010611  4.0000000 -0.7660000 -1.0261000 -2.2981000  79.00000
## 20010612  7.3763326  1.2856000 -2.2981000 -3.9392000 101.00000
## 20010613  3.0000000 -1.5000000 -1.5000000 -0.8682000 106.00000
## 20010614  8.0000000 -1.3082139 -1.0419000 -0.6946000 101.00000
## 20010615  6.0000000 -0.8682000 -2.7362000 -6.8944000  90.00000
## 20010616  9.1141444 -5.1115852 -7.8785000 -5.1962000  72.00000
## 20010617 10.4692452 -4.3301000 -2.0521000 -3.0000000  70.00000
## 20010618  4.0000000  0.5209000 -2.9544000 -1.0261000  83.00000
## 20010620  1.0000000 -0.3420000 -0.5727690 -0.6840000 121.00000
## 20010621  5.9009644  0.0000000  0.3473000 -2.5712000 100.78747
## 20010622  0.0000000  1.4741327  3.4407665  2.0000000  81.00000
## 20010623  5.5447773  1.0000000 -1.9284000 -1.2155000 121.00000
## 20010624  0.0000000  1.0694308 -0.5209000  1.0261000 146.00000
## 20010625  0.0000000  2.9544000  6.5778000  5.3384016 121.00000
## 20010626  0.0000000 -2.5712000 -3.8567000 -4.6985000 146.00000
## 20010627  1.0977518 -2.5981000 -2.0189830 -1.1554228  70.18961
## 20010628  6.9279163 -5.6382000 -3.8302000 -4.5963000  83.00000
## 20010629  4.0000000 -1.9284000 -2.5712000 -4.3301000  57.00000
## 20010630  1.9710613 -1.5321000 -3.0642000 -0.8682000  81.00000
## 20010701  1.0000000  0.6840000  0.0000000  1.3681000  67.00000
## 20010702  1.0000000  2.8191000  3.9392000  3.4641000  70.00000
## 20010703  4.0000000  1.8794000  2.0000000  1.3681000 106.00000
## 20010704  6.7149261  0.6946000 -0.8660000 -1.0261000 139.00000
## 20010705  6.0000000  0.0000000  0.0000000  1.2856000  79.00000
## 20010706  4.0000000  0.0000000  1.7101000  2.3209603  93.00000
## 20010707  4.4082495 -3.7588000 -3.9392000 -4.6985000  97.00000
## 20010708  4.0000000 -2.5981000 -3.9392000 -3.7588000 113.00000
## 20010709  8.4782681 -1.9696000 -3.0642000 -4.0000000  72.00000
## 20010710  7.2089767 -4.8665625 -5.6382000 -9.0000000  88.00000
## 20010711  6.0000000 -7.8785000 -6.8937000 -6.8937000  77.00000
## 20010712  6.0000000  1.5000000 -3.8302000 -2.0521000  71.00000
## 20010713  8.0000000  0.6840000  4.0000000 -1.2976149  35.26838
## 20010714  5.0000000 -3.2139000 -2.7446181 -2.4644075  45.00000
## 20010715  6.0000000 -2.2981000 -3.7588000  0.0000000  67.00000
## 20010716  6.0000000  0.0000000  1.2665053 -2.5981000  67.00000
## 20010717  6.0000000  2.0000000 -5.3623000 -6.1284000  84.00000
## 20010718  7.3114577 -5.8826176 -3.8302000 -4.3301000  63.00000
## 20010719  4.0000000 -2.0521000 -4.4995000 -2.7362000  69.00000
## 20010720  5.7737498 -2.2642118 -3.4641000 -4.1657740  92.00000
## 20010721  5.0000000 -3.0000000 -3.5000000 -2.7690666  88.00000
## 20010722  6.0000000 -1.1425583 -1.9696000 -1.9559833  66.00000
## 20010723  2.0000000  0.5209000 -1.0000000 -2.0000000  90.00593
## 20010724  5.0645718  0.1253692 -1.0261000  0.5209000  81.00000
## 20010725  5.6050701  1.9229266 -0.9397000 -0.6428000  83.00000
## 20010726  4.0000000  0.9397000  1.5000000  1.0148141 149.00000
## 20010727  7.0000000 -0.3420000  1.2856000 -2.0000000 108.09037
## 20010728  3.0000000  0.8660000 -1.5321000 -0.1736000 159.00000
## 20010729  1.5130171  1.5321000  1.1324969 -0.3081405 149.00000
## 20010730  4.0000000 -0.5000000 -1.8794000 -1.2856000 160.00000
## 20010731  2.0000000 -1.3681000 -2.0369999  0.0000000 156.00000
## 20010801  4.0000000  3.0000000  3.7588000  4.6410530  84.00000
## 20010802  8.0000000  0.0000000 -2.3941000 -1.3892000 126.00000
## 20010803  6.0000000 -3.4641000 -2.5981000 -3.7588000 116.00000
## 20010804  7.0000000 -5.0000000 -4.9240000 -5.6382000  70.81975
## 20010805  8.0000000 -4.6985000 -2.5000000 -0.8682000  63.00000
## 20010806  7.0000000 -3.8302000 -4.9240000 -5.6382000  54.00000
## 20010807  8.0000000 -3.0000000 -4.5963000 -3.8197081  65.00000
## 20010808  7.0000000 -5.6382000 -6.0622000 -6.8937000  72.00000
## 20010809  3.0000000 -5.9088000 -3.2139000 -4.4995000  60.00000
## 20010810  4.7351076 -1.9284000 -1.0261000  0.5209000  70.00000
## 20010811  0.0000000 -1.1799417 -1.5321000 -1.0000000  85.22632
## 20010812  2.0000000 -1.0261000 -3.0000000 -2.2981000  98.00000
## 20010813  1.0000000 -0.8660000  0.0000000  0.0000000 161.61367
## 20010814  5.0000000  1.8794000  1.3681000  0.6946000  75.00000
## 20010815  4.0000000 -1.0261000 -1.7101000 -3.2139000 116.00000
## 20010819  4.6378441 -5.2768880 -5.3623000 -2.5000000  86.00000
## 20010820  5.0000000 -3.0642000 -2.2981000 -3.0578974  67.00000
## 20010821  2.0373952  1.3681000  0.8682000 -2.2981000  76.00000
## 20010822  4.0000000  1.5321000  1.9284000  1.9284000 113.00000
## 20010823  3.0000000  0.1736000 -1.9696000 -1.9284000 117.00000
## 20010824  1.0000000  0.6428000 -0.8660000  0.6840000 131.00000
## 20010825  1.0000000  1.0000000  0.6946000 -1.7101000 166.00000
## 20010826  6.0000000  1.2856000 -1.7321000 -0.6840000 123.01379
## 20010827  5.0000000  3.0642000  2.8191000  1.3681000 100.00000
## 20010828  3.0000000  4.0000000  4.0000000  3.7588000 114.00000
## 20010829  7.0000000 -2.0000000 -0.5209000  1.8794000 112.00000
## 20010830  7.0000000 -3.4641000 -4.0000000 -1.7321000 101.00000
## 20010831  6.0000000 -4.3301000 -5.3623000 -4.6656495  76.00000
## 20010901  7.4679767 -1.1578017  0.5209000  0.0000000  59.00000
## 20010902  6.0000000 -2.9544000 -2.9544000 -2.0000000  55.62733
## 20010903  5.0000000 -1.8794000 -0.4165336 -2.3941000  76.00000
## 20010904  3.0000000 -0.6829562 -1.7344263 -1.3892000  55.00000
## 20010905  6.0000000 -1.3681000 -0.8682000  0.5318193  71.00000
## 20010906  7.0000000 -4.1655460 -1.9284000 -1.7101000  66.00000
## 20010907  5.5929749 -1.5000000 -3.4641000 -3.0642000  59.00000
## 20010908  9.2735133 -4.5963000 -6.0622000 -4.3301000  68.00000
## 20010912  6.0000000 -0.8660000 -5.0000000 -2.6767895  62.00000
## 20010913  7.0000000 -4.5963000 -6.8937000 -7.7798305  78.00000
## 20010914  5.5423889 -1.0419000 -1.3681000  0.5504088  74.00000
## 20010915  8.0000000 -5.1962000 -2.7362000 -1.0419000  71.00000
## 20010916  5.0000000 -3.8302000  0.0000000  1.3892000  69.00000
## 20010917  4.0000000  0.0000000  3.2139000  0.0000000  71.00000
## 20010918  7.0000000 -2.5000000 -3.2139000 -2.5000000  60.00000
## 20010919  7.0000000 -5.9596316 -6.0622000 -5.1962000  42.00000
## 20010920  6.0000000 -3.9392000 -3.0642000  0.0000000  65.00000
## 20010921  3.0000000 -0.1736000  3.7588000  3.8302000  71.00000
## 20010922  2.0000000  4.0000000  5.0000000  3.1254313  96.00000
## 20010923  6.0000000  5.1962000  5.1423000  2.5462259  98.00000
## 20010924  3.6531104 -0.9397000 -0.7660000 -0.5000000  77.65559
## 20010925  4.8993079  0.0000000 -1.0000000 -1.2856000  76.00000
## 20010927  4.0647822 -0.6946000 -2.0000000 -3.3268079  71.00000
## 20010928  0.9105977  1.5000000  0.8682000  0.8682000  80.72372
## 20010929  3.0000000 -4.0000000 -3.7588000 -4.0000000  99.00000
## 20010930  7.0000000 -0.1253549 -1.0419000 -4.0000000  83.00000
```

__(R5)__ Now generate 100 imputed data sets with the MIPCA method and 2 components. Store the result in a variable called res.MIPCA.


```r
?MIPCA
?plot.MIPCA
```



We will **inspect the imputed values created** to know if the imputation method should require more investigation or if we can continue and analyze the data. A common practice consists in comparing the distribution of the imputed values and of the observed values. Check the **compare.density** function and apply it to compare the distributions of the T12 variable.


```r
?compare.density
```


```r
compare.density(res.amelia, var = 8)
```

![](full-analysis_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


__(Q5)__ Do both distributions need to be close? Could the missing values differ from the observed ones both in spread and in location? 

*Not necessarily, they should only be close if the missing values are missing at random (MCAR)*

The quality of imputation can also be assessed with cross-validation using the **overimpute** function. Each observed value is deleted and for each one 100 values are predicted (using the same MI method) and the mean and 90% confidence intervals are computed for these 100 values. Then, we inspect whether the observed value falls within the obtained interval. On the graph, the y=x line is plotted (where the imputations should fall if they were perfect), as well as the mean (dots) and intervals (lines) for each value. Around ninety percent of these confidence intervals should contain the y = x line, which means that the true observed value falls
within this range. The color of the line (as coded in the legend) represents the
fraction of missing observations in the pattern of missingness for that observation (ex: blue=0-2 missing entries). 


```r
?overimpute
```


```r
overimpute(res.amelia,3)
```

![](full-analysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


__(Q6)__ Comment the quality of the imputation.

*The data is rather well imputed: there are only a few values where the confidence interval does not coincide with the line *

We can also examine the variability by projecting as supplementary tables the imputed data sets on the PCA configuration (plot the results of MI with PCA).


```r
plot(res.MIPCA,choice= "ind.supp")
plot(res.MIPCA,choice= "var")
```

__(R6)__ Apply a regression model on each imputed data set of the amelia method. Hint: a regression with several variables can be performed as follows 'lm(formula="maxO3 ~ T9+T12", data =don)'. You can also use the function
**with**.

__(R7)__ Now do the same with the imputed datasets of the MIPCA method.


The package **mice** (Multiple Imputation by Chained Equations) allows to aggregate the results from some simple models.


```r
library(mice)
```

```
## Loading required package: lattice
```

```r
# ?mice
# pool is a function from mice to aggregate the results according to Rubin's rule
# ?pool
```


__(R8)__ Aggregate the results of Regression with Multiple Imputation according to Rubin's rule (slide "Multiple imputation") for MI with amelia with the 
**pool** function from the mice package.


```r
poolamelia<-pool(as.mira(fitamelia)) 
summary(poolamelia)
```

__(R9)__ Now do the same with the MIPCA results.

__(R10)__ Write a function that removes the variables with the largest pvalues step by step (each time a variable is removed the regression model is performed again) until all variables are significant.


#### EM algorithm

In this simple setting, we have an explicit expression of the maximum likelihood estimator despite missing values. However, this is not always the case but it is possible to use an EM algorithm which allows to get the maximum likelihood estimators in the cases where data are missing.

The EM algorithm consists in maximizing the "observed likelihood" 
$$l(\mu, \Sigma|y_1,y_2)=-\frac{n}{2}\log(\sigma_{11}^2)-\frac{1}{2}\sum_{i=1}^n\frac{(y_{i1}-\mu_1)^2}{\sigma_{11}^2}-\frac{r}{2}\log((\sigma_{22}-\frac{\sigma_{12}^2}{\sigma_{11}})^2)$$
$$-\frac{1}{2}\sum_{i=1}^r\frac{(y_{i2}-\mu_2-\frac{\sigma_{12}}{\sigma_{11}}(y_{i1}-\mu_1))^2}{(\sigma_{22}-\frac{\sigma_{12}^2}{\sigma_{11}})^2},$$
through successive maximization of the "complete likelihood" (if we had observed all $n$ realizations of $y_1$ and $y_2$). Maximizing the complete likelihood 
$$l_c(\mu, \Sigma|y_1,y_2)=-\frac{n}{2}\log(\det(\Sigma))-\frac{1}{2}\sum_{i=1}^n(y_{i1}-\mu_1)^T\Sigma^{-1}(y_{i1}-\mu_1)$$

would be straightforward if we had all the observations. However elements of this likelihood are not available. Consequently, we replace them by the conditional expectation given observed data and the parameters of the current iteration. These two steps of computation of the conditional expectation (E-step) and maximization of the completed likelihood (M step) are repeated until convergence. 

The update formulas for the E and M steps are the following  

__E step__:

The sufficient statistics of the likelihood are:  

$$s_1=\sum_{i=1}^ny_{i1},\quad s_2=\sum_{i=1}^ny_{i2},\quad s_{11}=\sum_{i=1}^ny_{i1}^2,\quad s_{22}=\sum_{i=1}^ny_{i2}^2,\quad s_{12}=\sum_{i=1}^ny_{i1}y_{i2}.$$

Since some values of $y_2$ are not available, we fill in the sufficient statistics with:

$$E[y_{i2}|y_{i1},\mu,\Sigma]=\beta_{20.1}+\beta_{21.1}y_{i1}$$
$$E[y_{i2}^2|y_{i1},\mu,\Sigma]=(\beta_{20.1}+\beta_{21.1}y_{i1})^2+\sigma_{22.1}$$
$$E[y_{i2}y_{i2}|y_{i1},\mu,\Sigma]=(\beta_{20.1}+\beta_{21.1}y_{i1})y_{i1}.$$

__M step__: 

The M step consists in computing the maximum likelihood estimates as usual. 
Given $s_1, s_2, s_{11}, s_{22}, \text{and } s_{12},$ update $\hat{\mu}$ and $\hat{\sigma}$ with
$$\hat{\mu}_1=s_1/n\text{, }\hat{\mu}_2=s_2/n,$$
$$\hat{\sigma}_1=s_{11}/n-\hat{\mu}_1^2\text{, }\hat{\sigma}_2=s_{22}/n-\hat{\mu}_2^2\text{, }\hat{\sigma}_{12}=s_{12}/n-\hat{\mu}_1\hat{\mu}_2$$

Note that $s_1$, $s_{11}$, $\hat{\mu}_1$ and $\hat{\sigma}_1$ are constant accross iterations since we do not have missing values on $y_1$.

__(R11)__ Write two functions called Estep and Mstep that respectively perform the E step and the M step. 
The Estep function can take as an input $\mu$ and $\Sigma$. Then, you can compute 
$\beta_{21.1}=\sigma_{12}/\sigma_{11}$, $\beta_{20.1}=\mu_2-\beta_{21.1}\mu_1$, and $\sigma_{22.1}=\sigma_{22}-\sigma^2_{12}/\sigma_{11}$  and update the sufficient statistics $s_{ij}$.
The Mstep function consists in updating the update the $\mu$ and $\Sigma$ given the $s_{ij}$.


__(Q7)__ How could we initialize the algorithm ?

__(R12)__ Implement a function called initEM that returns initial values for $\hat{\mu}$ and $\hat{\Sigma}$.

__(R13)__ Implement the EM algorithm over 15 iterations and plot the value of $\left\|\mu-\hat{\mu}\right\|^2$ over iterations. Comment your results briefly.

__(R14)__ Check that the EM estimator $\mu$ is equal to the maximum likelihood estimator.

