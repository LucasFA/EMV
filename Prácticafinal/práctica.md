# Abstract
Diabetes, in Latin diabetes mellitus, is, worldwide, one of the largest public health problem and it seems to increase year after year. This is also why the National Institute of Diabetes and Digestive and Kidney Diseases every 2 years since 1965 analyses the evolution of diabetes in the Pima Indian population. In this paper we start with an univariate exploratory analysis and conclude with a multivariate analysis of the data, since the main goal is to understand which factors are the most relevant to rise the risks of diabetes.

# Introduction
In $2021$, more than $10.5\%$ of the world population apparently suffered from diabetes. The exact cause of this chronic illness is unknown, but several factors such as ethnicity, genetic inheritance, age, physical inactivity and excess of weight increase the risk of developing it. Diabetes results in high level of blood sugar as a consequence of insufficient pancreatic activity. In fact, it develops if the pancreas is not able to generate the appropriate amount of insulin or when the insulin produced is not used effectively by the organism. That is why, among other paramenters, in the study on the Pima population, the level of insulin 2 hours after glucose administration has been considered. The Pima Indians model of this illness is largely studied, because of the wide amount of data, which have been collected since the 1960s. The lifestyle of this population has changed and this is for sure one of the main purposes of the development of diabetes, but scientists suspect that as well a genetic factor played a great role. \
In this analysis we will now discuss using both a univariate and a multivariate explorative analysis, which factors most influence the onset of diabetes considering a sample of $768$ females of Pima Indian heritage, who are at least $21$ years old.

# Read the data

```r
diabetes_full <- read.csv("diabetes.csv", stringsAsFactors = TRUE)
head(diabetes_full)
```

```
##   preg plas pres skin insu mass  pedi age           class
## 1    6  148   72   35    0 33.6 0.627  50 tested_positive
## 2    1   85   66   29    0 26.6 0.351  31 tested_negative
## 3    8  183   64    0    0 23.3 0.672  32 tested_positive
## 4    1   89   66   23   94 28.1 0.167  21 tested_negative
## 5    0  137   40   35  168 43.1 2.288  33 tested_positive
## 6    5  116   74    0    0 25.6 0.201  30 tested_negative
```

# Univariate explorative analysis
The variables we are dealing with are described as below: 

+ number of times pregnant;
+ plasma glucose concentration a 2 hours in an oral glucose tolerance test;
+ diastolic blood pressure (mmHg);
+ triceps skin fold thickness (mm);
+ 2-Hour serum insulin ($\mu$U/mL);
+ body mass index (weight in kg/(height in m)^2);
+ diabetes pedigree function;
+ age (years);
+ class variable (0 or 1 for tested_positive or tested_negative).

We are not interesed in the number of pregnancies the patient has had, so we will omit this variable. The response variable will be `class`, which tells us if a patient shows signs of diabetes according to World Health Organization criteria (i.e., if the 2 hour post-load plasma glucose was at least $200$ mg/dl at any survey examination or if found during routine medical care). A sample of $768$ females of Pima Indian heritage, a population that lives near Phoenix, Arizona, USA, have been considered, where the girls are at least $21$ years old.\


```r
# Remove the first column from the dataset
diabetes <- diabetes_full[, -1]
head(diabetes)
```

```
##   plas pres skin insu mass  pedi age           class
## 1  148   72   35    0 33.6 0.627  50 tested_positive
## 2   85   66   29    0 26.6 0.351  31 tested_negative
## 3  183   64    0    0 23.3 0.672  32 tested_positive
## 4   89   66   23   94 28.1 0.167  21 tested_negative
## 5  137   40   35  168 43.1 2.288  33 tested_positive
## 6  116   74    0    0 25.6 0.201  30 tested_negative
```

In this dataset, all the missing values have been substituted by the number $0$. Since, for example, it doesn't make any sense to consider a $0$ mm thickness of the triceps skin fold of a girl or a zero diastolic blood pressure, we detect this ``kind of" missing values and we check if it would be appropriate to substitute them with the mean of the considered variable. 

```r
# Replace zeros of all columns with NA, so that we can use the functions to
# detect missing values showed in class
diabetes[diabetes == 0] <- NA
head(diabetes)
```

```
##   plas pres skin insu mass  pedi age           class
## 1  148   72   35   NA 33.6 0.627  50 tested_positive
## 2   85   66   29   NA 26.6 0.351  31 tested_negative
## 3  183   64   NA   NA 23.3 0.672  32 tested_positive
## 4   89   66   23   94 28.1 0.167  21 tested_negative
## 5  137   40   35  168 43.1 2.288  33 tested_positive
## 6  116   74   NA   NA 25.6 0.201  30 tested_negative
```

```r
### ----- i) -----
# Percentage of missing values in each column
percNA <- (colMeans(is.na(diabetes))) * 100
percNA
```

```
##       plas       pres       skin       insu       mass       pedi        age 
##  0.6510417  4.5572917 29.5572917 48.6979167  1.4322917  0.0000000  0.0000000 
##      class 
##  0.0000000
```

```r
# Columns skin and insu have more then 5%
```
The percentage of missing values for the variables `skin` and `insu` is greater then $5\%$, so let's check if it makes sense to substitute that values by the mean as described above.\
To prove this, we test the homogenity of the columns with a Student t-test and with the chi-squared test of independece, when we deal with the last colum (since it is a discrete variable).


```r
### ------- ii) ----------
# Student t-test to check homogenity between columns/variables
t.test(diabetes$plas, diabetes$skin)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  diabetes$plas and diabetes$skin
## t = 77.517, df = 997.24, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  90.19087 94.87582
## sample estimates:
## mean of x mean of y 
## 121.68676  29.15342
```

```r
t.test(diabetes$pres, diabetes$skin)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  diabetes$pres and diabetes$skin
## t = 67.379, df = 1248.5, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  41.99240 44.51113
## sample estimates:
## mean of x mean of y 
##  72.40518  29.15342
```

```r
t.test(diabetes$insu, diabetes$skin)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  diabetes$insu and diabetes$skin
## t = 21.063, df = 397.46, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  114.5976 138.1920
## sample estimates:
## mean of x mean of y 
## 155.54822  29.15342
```

```r
t.test(diabetes$mass, diabetes$skin)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  diabetes$mass and diabetes$skin
## t = 6.4033, df = 869.31, p-value = 2.486e-10
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  2.291311 4.316777
## sample estimates:
## mean of x mean of y 
##  32.45746  29.15342
```

```r
t.test(diabetes$pedi, diabetes$skin)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  diabetes$pedi and diabetes$skin
## t = -63.652, df = 540.76, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -29.56668 -27.79640
## sample estimates:
##  mean of x  mean of y 
##  0.4718763 29.1534196
```

```r
t.test(diabetes$age, diabetes$skin)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  diabetes$age and diabetes$skin
## t = 6.6049, df = 1237.6, p-value = 5.891e-11
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  2.873349 5.301583
## sample estimates:
## mean of x mean of y 
##  33.24089  29.15342
```

```r
# Chi-squared test to check independence between the discrete column class and the column skin.
chisq.test(diabetes$class, diabetes$skin)
```

```
## Warning in chisq.test(diabetes$class, diabetes$skin): Chi-squared approximation
## may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  diabetes$class and diabetes$skin
## X-squared = 73.101, df = 49, p-value = 0.0144
```

The p-value obtained in each test is smaller than the significance level $\alpha= 0.05$, therefore we would statistically reject the homogenity between the variables and we would not substitute the missing values with the mean. Anyway, after a discussion with the lead researcher on the project, we replace the missing values by the mean of the respective column and we obtain a ``clean" dataset.


```r
# Function to detect missing values and replace them with the mean of that
# variable
not_available <- function(data) {
  data[is.na(data)] <- mean(data, na.rm = T)
  data
}

# Replace NA with the mean of that variable
# Only the first 7 columns have been considered, since they contain numerical values.
# The last one is a 'character'-column, so we cannot apply the function
# to dectect missing values, because there aren't any there.
df <- data.frame(lapply(diabetes[1:7], not_available))
head(df)
```

```
##   plas pres     skin     insu mass  pedi age
## 1  148   72 35.00000 155.5482 33.6 0.627  50
## 2   85   66 29.00000 155.5482 26.6 0.351  31
## 3  183   64 29.15342 155.5482 23.3 0.672  32
## 4   89   66 23.00000  94.0000 28.1 0.167  21
## 5  137   40 35.00000 168.0000 43.1 2.288  33
## 6  116   74 29.15342 155.5482 25.6 0.201  30
```

```r
# Data without NA. The NA are replaced by the mean of each column.
new_diab <- cbind(df, diabetes[8])
head(new_diab)
```

```
##   plas pres     skin     insu mass  pedi age           class
## 1  148   72 35.00000 155.5482 33.6 0.627  50 tested_positive
## 2   85   66 29.00000 155.5482 26.6 0.351  31 tested_negative
## 3  183   64 29.15342 155.5482 23.3 0.672  32 tested_positive
## 4   89   66 23.00000  94.0000 28.1 0.167  21 tested_negative
## 5  137   40 35.00000 168.0000 43.1 2.288  33 tested_positive
## 6  116   74 29.15342 155.5482 25.6 0.201  30 tested_negative
```

In the following table, the main statistical characteristics of the dataset have been summarized.


```
## Warning: package 'kableExtra' was built under R version 4.2.2
```

```
## Warning in !is.null(rmarkdown::metadata$output) && rmarkdown::metadata$output
## %in% : 'length(x) = 2 > 1' in coercion to 'logical(1)'
```

```
## Warning: package 'latex2exp' was built under R version 4.2.2
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Main statistical characteristics</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Plasma glucose concentration </th>
   <th style="text-align:center;"> Diastolic blood pressure </th>
   <th style="text-align:center;"> Triceps thickness </th>
   <th style="text-align:center;"> Insulin </th>
   <th style="text-align:center;"> BMI </th>
   <th style="text-align:center;"> Pedigree function </th>
   <th style="text-align:center;"> Age </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Mean </td>
   <td style="text-align:center;"> 121.690 </td>
   <td style="text-align:center;"> 72.410 </td>
   <td style="text-align:center;"> 29.150 </td>
   <td style="text-align:center;"> 155.500 </td>
   <td style="text-align:center;"> 32.460 </td>
   <td style="text-align:center;"> 0.472 </td>
   <td style="text-align:center;"> 33.240 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Standard deviation </td>
   <td style="text-align:center;"> 30.436 </td>
   <td style="text-align:center;"> 12.096 </td>
   <td style="text-align:center;"> 8.791 </td>
   <td style="text-align:center;"> 85.021 </td>
   <td style="text-align:center;"> 6.875 </td>
   <td style="text-align:center;"> 0.331 </td>
   <td style="text-align:center;"> 11.760 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25%-Quantile </td>
   <td style="text-align:center;"> 99.750 </td>
   <td style="text-align:center;"> 64.000 </td>
   <td style="text-align:center;"> 25.000 </td>
   <td style="text-align:center;"> 121.500 </td>
   <td style="text-align:center;"> 27.500 </td>
   <td style="text-align:center;"> 0.244 </td>
   <td style="text-align:center;"> 24.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Median </td>
   <td style="text-align:center;"> 117.000 </td>
   <td style="text-align:center;"> 72.200 </td>
   <td style="text-align:center;"> 29.150 </td>
   <td style="text-align:center;"> 155.500 </td>
   <td style="text-align:center;"> 32.400 </td>
   <td style="text-align:center;"> 0.372 </td>
   <td style="text-align:center;"> 29.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 75%-Quantile </td>
   <td style="text-align:center;"> 140.250 </td>
   <td style="text-align:center;"> 80.000 </td>
   <td style="text-align:center;"> 32.000 </td>
   <td style="text-align:center;"> 155.500 </td>
   <td style="text-align:center;"> 36.600 </td>
   <td style="text-align:center;"> 0.626 </td>
   <td style="text-align:center;"> 41.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Skewness </td>
   <td style="text-align:center;"> 0.532 </td>
   <td style="text-align:center;"> 0.137 </td>
   <td style="text-align:center;"> 0.821 </td>
   <td style="text-align:center;"> 3.013 </td>
   <td style="text-align:center;"> 0.597 </td>
   <td style="text-align:center;"> 1.916 </td>
   <td style="text-align:center;"> 1.127 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kurtosis </td>
   <td style="text-align:center;"> 2.735 </td>
   <td style="text-align:center;"> 4.083 </td>
   <td style="text-align:center;"> 8.372 </td>
   <td style="text-align:center;"> 18.079 </td>
   <td style="text-align:center;"> 3.906 </td>
   <td style="text-align:center;"> 8.551 </td>
   <td style="text-align:center;"> 3.631 </td>
  </tr>
</tbody>
</table>
The data are collected in the appropriate, but of course different, measuring scales, so in order to be able to compare them with boxplots, we need to standardize the data.

```r
# Standardize the variables
v_plas <- (new_diab[1] - s[4, 1]) / sd$plas
v_pres <- (new_diab[2] - s[4, 2]) / sd$pres
v_skin <- (new_diab[3] - s[4, 3]) / sd$skin
v_insu <- (new_diab[4] - s[4, 4]) / sd$insu
v_mass <- (new_diab[5] - s[4, 5]) / sd$mass
v_pedi <- (new_diab[6] - s[4, 6]) / sd$pedi
v_age <- (new_diab[7] - s[4, 7]) / sd$age

# Boxplot of the data
standardData <- cbind(v_plas, v_pres, v_skin, v_insu, v_mass, v_pedi, v_age)
boxplot(standardData, col = cm.colors(7))
```

![](práctica_files/figure-html/BoxplotStand-1.png)<!-- -->

All variables present outliers, except the one that measures the plasma glucose concentracion. We try to eliminate the outliers and substitute this values by the mean of the column.


```r
# Function for finding the outliers
outlier <- function(data) {
  H <- 1.5 * IQR(data)
  data[data < quantile(data, 0.25, na.rm = T) - H] <- NA
  data[data > quantile(data, 0.75, na.rm = T) + H] <- NA
  data[is.na(data)] <- mean(data, na.rm = T)
  H <- 1.5 * IQR(data)
  if (TRUE %in% (data < quantile(data, 0.25, na.rm = T) - H) |
    TRUE %in% (data > quantile(data, 0.75, na.rm = T) + H)) {
    outlier(data)
  } else {
    return(data)
  }
}

# Find the outliers in the dataset and replace them with the appropriate mean
no_out_data <- as.data.frame(apply(new_diab[1:7], 2, outlier))

# Standardize now the data without outliers
no_out_standard <- lapply(no_out_data, function(column) (column - mean(column)) / sd(column))

# Boxplots without outliers
boxplot(no_out_standard,
  main = "Boxplots without outliers",
  names = c("plas", "pres", "skin", "insu", "mass", "pedi", "age"),
  col = cm.colors(7)
)
```

![](práctica_files/figure-html/outliers-1.png)<!-- -->

Even if we replaced the outliers by the mean and we standardized the values, we see that we couldn't get rid of all of them for the blood pressure. One of the main purposes, according to the lead researcher, is the genetic inheritance: women with a hight blood pressure probabily had parents with hight blood pressure as well. We note also that this variable takes the smalles and the highest value between all columns. The variable `insu` is the one with the larger variability, since the quantity of serum insulin depends on how the organism absorbes the glucose, while for all the others parameters the variability is almost the same.\

We analyse if the data are normally distributed. For a perfect normal distribution, the skewness and the kurtosis should be zero, but, in our case, non of corresponding value for each column is zero. So let's see if the normality assumption could be appropriate by looking at the following QQ-plots.

![](práctica_files/figure-html/qq plot-1.png)<!-- -->![](práctica_files/figure-html/qq plot-2.png)<!-- -->

By looking at the QQ-plots, it seems that only the diastolic blood pressure and the body mass index follow a normal distribution. This is as well confirmed by the histogram of the corresponding variables: the bars of the histogram follow roughly a bell-shaped curve and are symmetric around zero. Also the histogram of the plasma glucose concentration approximate to a normal distribution, but the QQ-plot exibits an ``S-shape" and the points are not following a stright line (which is what we need in case of normality).\
To conclude this first part, we test for normality with a Shapiro-Wilk test.

```r
# The null hypothesis of these tests is that “sample distribution is normal”. If the test is significant, the distribution is non-normal.
# All tests reject, so all the variables are non-normal

shapiro.test(no_out_standard$plas)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  no_out_standard$plas
## W = 0.9699, p-value = 1.777e-11
```

```r
shapiro.test(no_out_standard$pres)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  no_out_standard$pres
## W = 0.9949, p-value = 0.01154
```

```r
shapiro.test(no_out_standard$skin)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  no_out_standard$skin
## W = 0.79534, p-value < 2.2e-16
```

```r
shapiro.test(no_out_standard$insu)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  no_out_standard$insu
## W = 0.73597, p-value < 2.2e-16
```

```r
shapiro.test(no_out_standard$mass)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  no_out_standard$mass
## W = 0.99097, p-value = 0.0001202
```

```r
shapiro.test(no_out_standard$pedi)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  no_out_standard$pedi
## W = 0.93796, p-value < 2.2e-16
```

```r
shapiro.test(no_out_standard$age)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  no_out_standard$age
## W = 0.89375, p-value < 2.2e-16
```

As expected, all tests reject the null hypothesis of normality in favor of the alternative. With a significance level of $\alpha = 0.05$ we can assume that none of the variable is normally distributed. Interesting is to note that, when testing the normality of the blood pressure the p-value obtained is 0.0115. This suggest that if we made a test at a significance level of $0.01$, we would not have rejected the null hypotesis so we would have assumed that the variable `pres` is normally distributed. For all the other cases, the p-values are smaller then the $0.001$ so we would have rejected normality in every case. \
Finally, with an histogram we visualized that $500$ females out of $768$ have been tested negative, while the remaining $268$ present signs of diabetes.


```r
hist(as.numeric(unlist(new_diab[8])),
  col = "coral1",
  xlab = "tested positive - tested negative",
  ylab = "Frequency",
  main = "Histogram tested positive - tested negative",
  cex.lab = 1.2
)
```

![](práctica_files/figure-html/HistResponse-1.png)<!-- -->


# Multivariate exploratory analysis

<!-- a) -->
We will first use the Bartlett test to check whether the variables are correlated.


```r
diab_no_output <- subset(new_diab, select = -class)
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following object is masked _by_ '.GlobalEnv':
## 
##     outlier
```

```r
data_normalised <- scale(diab_no_output)
cortest.bartlett(cor(data_normalised), n = nrow(data_normalised))
```

```
## $chisq
## [1] 727.8411
## 
## $p.value
## [1] 1.72976e-140
## 
## $df
## [1] 21
```

We reject the null hypothesis of uncorrelatedness between the variables, so applying PCA is justified.

<!-- b hecho? TODO -->
<!-- c Done in the previous  -->

<!-- d) -->
## PCA
We will use the function `prcomp` from the `stats` package, which performs PCA.

```r
PCA <- prcomp(diab_no_output, scale = TRUE, center = TRUE)
```
Then the coefficients are given by:

```r
PCA$rotation
```

```
##             PC1        PC2        PC3         PC4         PC5         PC6
## plas -0.4480411  0.3191698 -0.3028767  0.14139991 -0.15628167  0.74098732
## pres -0.3699291  0.1901707  0.5355249 -0.17366948 -0.62215823 -0.22484572
## skin -0.4413442 -0.4560805  0.1555021  0.13492789  0.47408989 -0.01811531
## insu -0.3569437  0.2490175 -0.5130184  0.40365452 -0.04791350 -0.61806685
## mass -0.4593964 -0.5043107  0.1191386  0.09371944 -0.12588391  0.04831860
## pedi -0.1869537 -0.1977758 -0.4868578 -0.81742070 -0.07092809 -0.09753710
## age  -0.3069550  0.5463534  0.2878982 -0.30289030  0.58357578 -0.07873515
##              PC7
## plas -0.11034199
## pres -0.26907828
## skin -0.57422123
## insu  0.01242205
## mass  0.70247196
## pedi -0.07862296
## age   0.29306786
```
We can visualize the proportion of variance explained by each component:

```r
plot(cumsum(PCA$sdev^2) / (sum(PCA$sdev^2)), type = "l",
      col = "lightblue",
      xlab = "Number of components", ylab = "Proportion of variance explained",
      xlim = c(1, 7), ylim = c(0, 1)
)
lines(PCA$sdev^2 / (sum(PCA$sdev^2)), type = "l", col = "red")
```

![](práctica_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
The red line is the proportion of variance explained by each component, while the blue line represents the cumulative proportion of variance explained by the first $i$ components.

As a table, this is given by:

```r
summary(PCA)
```

```
## Importance of components:
##                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
## Standard deviation     1.4771 1.0902 1.0501 0.9514 0.81587 0.73977 0.63956
## Proportion of Variance 0.3117 0.1698 0.1575 0.1293 0.09509 0.07818 0.05843
## Cumulative Proportion  0.3117 0.4815 0.6390 0.7683 0.86339 0.94157 1.00000
```

### Number of components
We need the function `fviz_screeplot` from the `factoextra` package.


```r
library(factoextra)
```

There's a few methods to determine the number of components to keep. We will use the elbow method and the mean variance method.

#### Elbow method

```r
fviz_screeplot(PCA, addlabels = TRUE)
```

![](práctica_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Does't suggest any particular number of components.

#### Mean variace method

```r
PCA$sdev
```

```
## [1] 1.4770648 1.0902104 1.0500504 0.9514090 0.8158670 0.7397696 0.6395606
```

```r
mean(PCA$sdev^2)
```

```
## [1] 1
```
Suggests 3 using components.


### Visualization of components
Notice that the first variable has a negative coefficient with all variables (which can be seen in the `PCA$rotation` table above), which leads to both plots to stay in the left side of the graph.


```r
fviz_pca_var(PCA,
  repel = TRUE, col.var = "cos2",
  legend.title = "Distance"
) + theme_bw()
```

![](práctica_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
fviz_pca_var(PCA,
  axes = c(1, 3),
  repel = TRUE, col.var = "cos2",
  legend.title = "Distance"
) + theme_bw()
```

![](práctica_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

<!-- e) -->
## Factor analysis
### Preconditions
We saw in our previous analysis of correlations for PCA that the data was not uncorrelated according to the Bartlett test. 

We can visualize the correlation matrix two ways:


```r
poly_cor <- polycor::hetcor(diab_no_output)$correlations
ggcorrplot::ggcorrplot(poly_cor, type = "lower", hc.order = T)
```

![](práctica_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
corrplot::corrplot(cor(diab_no_output), order = "hclust", tl.col = "black", tl.cex = 1)
```

![](práctica_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

In either of these, groupings are not very clear. Though there's a strong correlation between _skin_ and _mass_, and between _plas_ and _insu_.

### Models

MLE model:

```r
model1 <- fa(poly_cor,
  nfactors = 3,
  rotate = "none",
  fm = "mle"
)
```
Minimum residual model:

```r
model2 <- fa(poly_cor,
  nfactors = 3,
  rotate = "none",
  fm = "minres"
)
```

```
## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
## ultra-Heywood case was detected. Examine the results carefully
```

We can compare the communalities, that is, the amount of variance accounted by factors in the model:

```r
c1 <- sort(model1$communality, decreasing = TRUE)
c2 <- sort(model2$communality, decreasing = TRUE)
cbind(c1, c2)
```

```
##              c1         c2
## mass 0.99500053 1.00098587
## plas 0.50751848 0.55458040
## age  0.38723903 0.43272900
## insu 0.38015466 0.33600145
## pres 0.36405794 0.31663149
## skin 0.31663757 0.30963194
## pedi 0.05042393 0.05132043
```

And the uniquenesses, the variance not accounted for by the factors:

```r
u1 <- sort(model1$uniquenesses, decreasing = TRUE)
u2 <- sort(model2$uniquenesses, decreasing = TRUE)
cbind(u1, u2)
```

```
##               u1            u2
## pedi 0.949576072  0.9486795665
## skin 0.683362428  0.6903680634
## pres 0.635942061  0.6833685110
## insu 0.619845340  0.6639985493
## age  0.612760967  0.5672710044
## plas 0.492481520  0.4454195982
## mass 0.004999471 -0.0009858746
```

Notice that the communalities go beyond 1 in the minimum residual model. This is an ultra-Heywood case, which means that it is not a reliable model. We will therefore continue with the MLE model.

### Determine the number of factors
We have a few methods to determine the number of factors we should use. We will use the scree plot and parallel analysis.


```r
scree(poly_cor)
```

![](práctica_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
fa.parallel(poly_cor, n.obs = length(diab_no_output[, 1]), fa = "fa", fm = "mle")
```

![](práctica_files/figure-html/unnamed-chunk-18-2.png)<!-- -->

```
## Parallel analysis suggests that the number of factors =  3  and the number of components =  NA
```

In both cases, using three factors is suggested.

### Model interpretation
We can visualize the model by presenting how each latent factor affects each obsevable variable:

```r
fa.diagram(model1)
```

![](práctica_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

### Note
Notice that we received a warning about this being an Ultra-Heywood case. According to the literature, this means the model is not reliable.

<!-- f -->
## Multivariate normality testing

We previously saw the data did not fit a normal distribution:
### Marginal distributions

```r
par(mfrow = c(2, 4))
for (k in seq_len(length(new_diab) - 1)) {
  variable_name <- names(new_diab)[k]
  for (i in 1:2) {
    output_result <- levels(new_diab$class)[i]
    x <- new_diab[new_diab$class == output_result, variable_name]
    hist(x,
      breaks = 20,
      main = paste("Histogram", variable_name, "for", output_result),
      xlab = variable_name,
      col = i + 1
    )
  }
}
```

![](práctica_files/figure-html/unnamed-chunk-20-1.png)<!-- -->![](práctica_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

### QQ-plots

```r
par(mfrow = c(2, 4))
for (k in seq_len(length(new_diab) - 1)) {
  name_variable <- names(new_diab)[k]
  some_seq <- seq(min(new_diab[, k]), max(new_diab[, k]), le = 50)
  for (i in 1:2) {
    output_result <- levels(new_diab$class)[i]
    x <- new_diab[new_diab$class == output_result, name_variable]
    qqnorm(x,
      main = paste("QQ-plot", name_variable, "for", output_result),
      pch = 19,
      col = i + 1
    )
    qqline(x)
  }
}
```

![](práctica_files/figure-html/unnamed-chunk-21-1.png)<!-- -->![](práctica_files/figure-html/unnamed-chunk-21-2.png)<!-- -->

In addition to this, we showed that the data was not normally distributed according to the Shapiro-Wilk test.

#### Multivariate

We first check for multivariate outliers:

```r
outliers <- MVN::mvn(data = diab_no_output, mvnTest = "hz", multivariateOutlierMethod = "quan")
```

![](práctica_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

There is a large number of outliers. According to the Royston test, the data is not multivariate normal:


```r
royston_test <- MVN::mvn(data = diab_no_output, mvnTest = "royston", multivariatePlot = "qq")
```

![](práctica_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
royston_test$multivariateNormality
```

```
##      Test        H       p value MVN
## 1 Royston 558.6337 2.123319e-116  NO
```

Likewise with the Henze-Zirkler test:

```r
hz_test <- MVN::mvn(data = diab_no_output, mvnTest = "hz")
hz_test$multivariateNormality
```

```
##            Test       HZ p value MVN
## 1 Henze-Zirkler 3.437203       0  NO
```

Summing up, the data is not multivariate normal, with significant deviation from it.

<!-- g -->
## Classifier

Visually, there is no clear variable that separates the two classes.

```r
pairs(
  x = diab_no_output[sample(nrow(diab_no_output), 100), ],
  col = c("green", "red")[new_diab$class],
  pch = 19
)
```

![](práctica_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

#### Homogeneity of variance

In the Bartlett test we saw that the data is not spherically distributed, but it is also the case that the variances are not homogeneous as can be determined form the boxM test:


```r
biotools::boxM(
  data = new_diab[1:7],
  grouping = new_diab[, 8]
)
```

```
## 
## 	Box's M-test for Homogeneity of Covariance Matrices
## 
## data:  new_diab[1:7]
## Chi-Sq (approx.) = 112.57, df = 28, p-value = 4.248e-12
```


#### Pipeline

In order to simplify model-building, we create a pipeline function that takes the data, the model name, and the formula as input. The function then splits the data into training and test data, and fits the model. The function returns the model and writes out the results of cross-validation.


```r
pipeline_model <- function(data, model_name, formula = class ~ .) {
  set.seed(42)
  training_rows <- sample(
    nrow(data),
    size = 0.75 * nrow(data),
    replace = FALSE
  )
  training_data <- data[training_rows, ]
  test_data <- data[-training_rows, ]

  myformula <- formula

  model <-
    if (model_name == "lda") {
      MASS::lda(formula = myformula, data = training_data)
    } else if (model_name == "qda") {
      MASS::qda(formula = myformula, data = training_data)
    } else {
      stop("Model not supported")
    }

  # In sample error
  in_sample_pred <- predict(model, training_data)
  print(biotools::confusionmatrix(training_data$class, in_sample_pred$class))
  training_error <- mean(training_data$class != in_sample_pred$class) * 100
  print(paste("In sample error = ", training_error, "%"))

  # Out of sample error
  pred <- predict(model, test_data)
  print(biotools::confusionmatrix(test_data$class, pred$class))
  test_error <- mean(test_data$class != pred$class) * 100
  print(paste("Out of sample error = ", test_error, "%"))

  model
}
```

### Linear Discriminant


```r
lda_model <- pipeline_model(new_diab, "lda")
```

```
##                 new tested_negative new tested_positive
## tested_negative                 328                  46
## tested_positive                  83                 119
## [1] "In sample error =  22.3958333333333 %"
##                 new tested_negative new tested_positive
## tested_negative                 111                  15
## tested_positive                  27                  39
## [1] "Out of sample error =  21.875 %"
```

```r
lda_model
```

```
## Call:
## lda(myformula, data = training_data)
## 
## Prior probabilities of groups:
## tested_negative tested_positive 
##       0.6493056       0.3506944 
## 
## Group means:
##                     plas     pres     skin     insu     mass      pedi      age
## tested_negative 110.6267 71.00129 27.73826 142.4578 31.04038 0.4389626 30.77540
## tested_positive 141.8648 75.48340 32.17960 179.1404 35.37949 0.5510644 36.15842
## 
## Coefficients of linear discriminants:
##                LD1
## plas  0.0299652140
## pres -0.0008924685
## skin  0.0093083447
## insu -0.0008551254
## mass  0.0592302492
## pedi  0.3981185948
## age   0.0222664108
```

Due to the high number of dimensions and the high error rate in classification, visualizing the data is not very insightful in this case.

### Quadratic Discriminant


```r
qda_model <- pipeline_model(new_diab, "qda")
```

```
##                 new tested_negative new tested_positive
## tested_negative                 328                  46
## tested_positive                  92                 110
## [1] "In sample error =  23.9583333333333 %"
##                 new tested_negative new tested_positive
## tested_negative                 107                  19
## tested_positive                  28                  38
## [1] "Out of sample error =  24.4791666666667 %"
```

```r
qda_model
```

```
## Call:
## qda(myformula, data = training_data)
## 
## Prior probabilities of groups:
## tested_negative tested_positive 
##       0.6493056       0.3506944 
## 
## Group means:
##                     plas     pres     skin     insu     mass      pedi      age
## tested_negative 110.6267 71.00129 27.73826 142.4578 31.04038 0.4389626 30.77540
## tested_positive 141.8648 75.48340 32.17960 179.1404 35.37949 0.5510644 36.15842
```

### Conclusion

Despite the fact that the data is not normally distributed, both the LDA and QDA models show a possible discriminant, though it does have a high error rate.

# Feature engineering

First off, we are going to resample the dataset for NA's, instead of replacing the value with the mean. To see why this might be a good idea, let's plot the distribution of the data.


```r
col_hist <- function(column, main, xlab) {
  hist(column,
    breaks = 20,
    probability = TRUE,
    col = "lightblue",
    main = main,
    xlab = xlab
  )
  lines(density(column), col = "blue")

  x <- seq(min(column), max(column), length.out = 100)
  normal <- dnorm(
    x = x,
    mean = mean(column),
    sd = sd(column)
  )

  lines(
    x = x,
    y = normal,
    col = "red"
  )
}

par(mfrow = c(2, 2))
for (k in seq_len(length(new_diab) - 1)) {
  column <- new_diab[, k]
  column_name <- names(new_diab)[k]
  hist_title <- paste("Histogram of", column_name)
  col_hist(column, hist_title, column_name)
}
```

![](práctica_files/figure-html/unnamed-chunk-30-1.png)<!-- -->![](práctica_files/figure-html/unnamed-chunk-30-2.png)<!-- -->

Notice that some variables have a very pronounced mode in exactly the mean, because the mean is being used to replace the NA's. This may affect the performance of the classifier, that relies on normality of variables. Instead, we will resample the data, and replace the NA's with the resampled values.


```r
diabetes_resampled <-
  diabetes |>
  lapply(function(column) {
    column[column == 0] <- NA
    replace(
      column,
      is.na(column),
      sample(
        column[!is.na(column)],
        sum(is.na(column)),
        replace = TRUE
      )
    )
  }) |>
  data.frame()
```

Visualizing this new data:


```r
par(mfrow = c(2, 2))
for (k in seq_len(length(diabetes_resampled) - 1)) {
  column <- diabetes_resampled[, k]
  column_name <- names(diabetes_resampled)[k]
  hist_title <- paste("Histogram of", column_name)
  col_hist(column, hist_title, column_name)
}
```

![](práctica_files/figure-html/unnamed-chunk-32-1.png)<!-- -->![](práctica_files/figure-html/unnamed-chunk-32-2.png)<!-- -->
And we test the normality of the data using the Shapiro-Wilk test:

```r
lapply(diabetes_resampled[1:7], shapiro.test)
```

```
## $plas
## 
## 	Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.96996, p-value = 1.832e-11
## 
## 
## $pres
## 
## 	Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.99114, p-value = 0.0001441
## 
## 
## $skin
## 
## 	Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.95591, p-value = 2.071e-14
## 
## 
## $insu
## 
## 	Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.80698, p-value < 2.2e-16
## 
## 
## $mass
## 
## 	Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.98002, p-value = 9.635e-09
## 
## 
## $pedi
## 
## 	Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.83652, p-value < 2.2e-16
## 
## 
## $age
## 
## 	Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.87477, p-value < 2.2e-16
```

Still very far from being normal distributions.

The QQ-plots show some variables have very high deviations from the normal specially in the extremes, for example, `pedi`.


```r
par(mfrow = c(2, 4))
for (k in seq_len(length(diabetes_resampled) - 1)) {
  name_variable <- names(diabetes_resampled)[k]
  some_seq <- seq(min(diabetes_resampled[, k]), max(diabetes_resampled[, k]), le = 50)
  for (i in 1:2) {
    output_result <- levels(diabetes_resampled$class)[i]
    x <- diabetes_resampled[diabetes_resampled$class == output_result, name_variable]
    qqnorm(x,
      main = paste("QQ-plot", name_variable, "for", output_result),
      pch = 19,
      col = i + 1
    )
    qqline(x)
  }
}
```

![](práctica_files/figure-html/unnamed-chunk-34-1.png)<!-- -->![](práctica_files/figure-html/unnamed-chunk-34-2.png)<!-- -->

These distributions resemble the normal distribution more closely, so we can try to use the LDA and QDA models again, but the performance has not improved dramatically.

### Linear Discriminant

```r
lda_model <- pipeline_model(diabetes_resampled, "lda")
```

```
##                 new tested_negative new tested_positive
## tested_negative                 326                  48
## tested_positive                  85                 117
## [1] "In sample error =  23.0902777777778 %"
##                 new tested_negative new tested_positive
## tested_negative                 111                  15
## tested_positive                  27                  39
## [1] "Out of sample error =  21.875 %"
```

```r
lda_model
```

```
## Call:
## lda(myformula, data = training_data)
## 
## Prior probabilities of groups:
## tested_negative tested_positive 
##       0.6493056       0.3506944 
## 
## Group means:
##                     plas     pres     skin     insu     mass      pedi      age
## tested_negative 110.6631 71.21390 27.81818 140.0936 31.07594 0.4389626 30.77540
## tested_positive 141.8317 75.74257 32.20792 186.3168 35.43119 0.5510644 36.15842
## 
## Coefficients of linear discriminants:
##               LD1
## plas 0.0287465805
## pres 0.0019401358
## skin 0.0011160034
## insu 0.0001943832
## mass 0.0626003427
## pedi 0.4157108012
## age  0.0218069011
```

### Quadratic Discriminant


```r
qda_model <- pipeline_model(diabetes_resampled, "qda")
```

```
##                 new tested_negative new tested_positive
## tested_negative                 325                  49
## tested_positive                  85                 117
## [1] "In sample error =  23.2638888888889 %"
##                 new tested_negative new tested_positive
## tested_negative                 108                  18
## tested_positive                  25                  41
## [1] "Out of sample error =  22.3958333333333 %"
```

```r
qda_model
```

```
## Call:
## qda(myformula, data = training_data)
## 
## Prior probabilities of groups:
## tested_negative tested_positive 
##       0.6493056       0.3506944 
## 
## Group means:
##                     plas     pres     skin     insu     mass      pedi      age
## tested_negative 110.6631 71.21390 27.81818 140.0936 31.07594 0.4389626 30.77540
## tested_positive 141.8317 75.74257 32.20792 186.3168 35.43119 0.5510644 36.15842
```

## Feature transformations

We can make some transformations to the data in order to have it approach the normal further.


```r
diabetes_transformed <- diabetes_resampled |>
  transform(
    plas_log = log(plas),
    pres = pres,
    skin = skin,
    insu = insu,
    mass = mass,
    pedi_log = log(pedi),
    age = age
  )
# remove plas, pedi
diabetes_transformed <- diabetes_transformed[, -c(1, 6)]

temp <- diabetes_transformed$class
diabetes_transformed$class <- NULL
diabetes_transformed$class <- temp
```

Notice the $p$ values of the transformed variables, which have increased dramatically in value:


```r
shapiro.test(diabetes_transformed$plas_log)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  diabetes_transformed$plas_log
## W = 0.99154, p-value = 0.0002222
```

```r
shapiro.test(diabetes_transformed$pedi_log)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  diabetes_transformed$pedi_log
## W = 0.99317, p-value = 0.0014
```

We are finally succesful in transforming the data to a "more normal" distribution, to some degree.

Let's compare the original and transformed data for the variables `plas` and `pedi` in both terms of the QQ-plot and the histogram.

The QQ-plot for `plas`:

```r
par(mfrow = c(2, 2))
for (i in 1:2) {
  output_result <- levels(diabetes_transformed$class)[i]
  x <- diabetes_transformed[diabetes_transformed$class == output_result, "plas_log"]
  qqnorm(x,
    main = paste("QQ-plot", "plas_log", "for", output_result),
    xlab = "Transformed data",
    pch = 19,
    col = i + 1
  )
  qqline(x)
}

for (i in 1:2) {
  output_result <- levels(diabetes_resampled$class)[i]
  x <- diabetes_resampled[diabetes_resampled$class == output_result, "plas"]
  qqnorm(x,
    main = paste("QQ-plot", "plas", "for", output_result),
    xlab = "Original data",
    pch = 19,
    col = i + 1
  )
  qqline(x)
}
```

![](práctica_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

And for `pedi`

```r
par(mfrow = c(2, 2))
for (i in 1:2) {
  output_result <- levels(diabetes_transformed$class)[i]
  x <- diabetes_transformed[diabetes_transformed$class == output_result, "pedi_log"]
  qqnorm(x,
    main = paste("QQ-plot", "pedi_log", "for", output_result),
    xlab = "Transformed data",
    pch = 19,
    col = i + 1
  )
  qqline(x)
}

for (i in 1:2) {
  output_result <- levels(diabetes_resampled$class)[i]
  x <- diabetes_resampled[diabetes_resampled$class == output_result, "pedi"]
  qqnorm(x,
    main = paste("QQ-plot", "pedi", "for", output_result),
    xlab = "Original data",
    pch = 19,
    col = i + 1
  )
  qqline(x)
}
```

![](práctica_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

In both cases, the transformed data fits the normal slightly better.
Plotting the histogram:


```r
par(mfrow = c(2, 2))
for (k in 6:7) {
  column <- diabetes_transformed[, k]
  column_name <- names(diabetes_transformed)[k]
  hist_title <- paste("Histogram of", column_name)
  col_hist(column, hist_title, column_name)
}

for (k in 1:2) {
  column <- diabetes_resampled[, k]
  column_name <- names(diabetes_resampled)[k]
  hist_title <- paste("Histogram of", column_name)
  col_hist(column, hist_title, column_name)
}
```

![](práctica_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

Again, clearly matches the normal distribution better.

### Linear Discriminant

```r
lda_model <- pipeline_model(
  diabetes_transformed,
  formula = class ~ pres + skin + insu + mass + age + plas_log + pedi_log,
 "lda")
```

```
##                 new tested_negative new tested_positive
## tested_negative                 323                  51
## tested_positive                  83                 119
## [1] "In sample error =  23.2638888888889 %"
##                 new tested_negative new tested_positive
## tested_negative                 109                  17
## tested_positive                  25                  41
## [1] "Out of sample error =  21.875 %"
```

```r
lda_model
```

```
## Call:
## lda(myformula, data = training_data)
## 
## Prior probabilities of groups:
## tested_negative tested_positive 
##       0.6493056       0.3506944 
## 
## Group means:
##                     pres     skin     insu     mass      age plas_log
## tested_negative 71.21390 27.81818 140.0936 31.07594 30.77540 4.681506
## tested_positive 75.74257 32.20792 186.3168 35.43119 36.15842 4.932334
##                   pedi_log
## tested_negative -1.0303849
## tested_positive -0.7866429
## 
## Coefficients of linear discriminants:
##                   LD1
## pres     0.0021946735
## skin     0.0023088120
## insu     0.0003251751
## mass     0.0615333005
## age      0.0223054469
## plas_log 3.3867227889
## pedi_log 0.3013253426
```

### Quadratic Discriminant

```r
qda_model <- pipeline_model(
  diabetes_transformed,
  formula = class ~ pres + skin + insu + mass + age + plas_log + pedi_log,
 "qda")
```

```
##                 new tested_negative new tested_positive
## tested_negative                 327                  47
## tested_positive                  84                 118
## [1] "In sample error =  22.7430555555556 %"
##                 new tested_negative new tested_positive
## tested_negative                 108                  18
## tested_positive                  25                  41
## [1] "Out of sample error =  22.3958333333333 %"
```

```r
qda_model
```

```
## Call:
## qda(myformula, data = training_data)
## 
## Prior probabilities of groups:
## tested_negative tested_positive 
##       0.6493056       0.3506944 
## 
## Group means:
##                     pres     skin     insu     mass      age plas_log
## tested_negative 71.21390 27.81818 140.0936 31.07594 30.77540 4.681506
## tested_positive 75.74257 32.20792 186.3168 35.43119 36.15842 4.932334
##                   pedi_log
## tested_negative -1.0303849
## tested_positive -0.7866429
```

Unfortunately, these transformations do not seem to improve the models significantly.
In any case, cross-validation shows that both models have similar performance, with an out of sample error rate of approximately $22%$ for both. An interesting caveat is that the in-sample error rate is higher in both cases.

### Thoughts on the classifier

The non-normality may contribute to the low performance of our classifier. Even when transformed, the data does not follow the normal distribution.
The prior probabilities of belonging to `tested_negative` and `tested_positive` are  $0.6493$ and $0.3506$, respectively. Therefore the rate of positives, $0.3506$, would be the expected error rate of the naive classifier that always returns `tested_negative`.

Having that in mind, our estimated error rate $22%$ of our classifier is almost a $13%$ improvement over the mentioned trivial classifier.

# Results and conclusions

Our objective was to understand the relationship between the variables in the dataset and to build a classifier that can predict whether a patient has diabetes or not. 

First, we found that the data is not normally distributed, and that the variables `plas` and `pedi`, seem to be log-normal. We then transformed these two variables in order to approach the normal, and found that this transformation did not significantly  improve the performance of the classifier according to the cross-validation results.


We have built a classifier that can predict whether a patient has diabetes or not, with an estimated error rate of $22%$. This is a $10%$ improvement over the naive classifier that always predicts `tested_positive`.

# Possible improvements

- We have not used the `preg` variable in out analysis, which is the number of times pregnant. This variable may or may not be useful in predicting whether a patient has diabetes or not, in which case it could be included in the model.

- The cross-validation results may change significantly depending on the training - test split. This affects our confidence in our results. We could use a more robust method to estimate the error rate, such as k-fold cross-validation.

- Regarding the classifier itself, other models could be used, such as logistic regression.

- Other transformations could be applied to the data. In this case, domain knowledge may be useful.
