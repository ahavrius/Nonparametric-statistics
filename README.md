# Nonparametric statistics

![Project Image](http://qnimate.com/wp-content/uploads/2014/03/images2.jpg)

---
## Table of Contents
- [Description](#description)
- [How to use](#how-to-use)
- [Theoretical basis](#theoretical-basis)
- [Author Info](#author-info)
---

## Description

Here you can find application and discription of widespread nonparametric statistics' methods. They used to estimate distibution function and density in a simple case and cases of censored and biased samples, classify data, estimate nonparametric regression function.

## How to use

Download and install packages

 - [triangle](https://cran.r-project.org/web/packages/triangle/index.html)

 ```install.packages("~/Downloads/triangle_0.12.tar", repos = NULL, type="source")```

## Theoretical basis

### Estimation of distribution function (usual sample)

Let sample X = (X1, …, Xn) be independent, identically distributed real random variables with the common cumulative distribution function F(t). For estimation F of the sample can be used the Empirical Distribution function defined as

<p align="center">
 <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/aacca85bf28da15cbba66ea7c456cf7ad9784047">
</p>

Then
![empirical-distribution](https://wikimedia.org/api/rest_v1/media/math/render/svg/e3af321cf7c2a77134157f2ba3f8c1391e0d2f5d) is an unbiased estimator of F(x) with variance
<p align="center">
 <img src="https://latex.codecogs.com/svg.latex?\sigma^2_{\widehat{F}}(x)=\frac{1}{n}F(x)(1-F(x))">
</p>

More ditails on wiki [Empirical distribution function](https://en.wikipedia.org/wiki/Empirical_distribution_function)

- #### `empirical distribution function.R` content:
I build Empirical distibution estimators for samples  of length n = 10, 50, 100, 500, 1000 and Confidence intervals for same samples with m = 1000 - number of repetitions, check the quality of those statistics. 

### Estimation of distribution function (censored sample)

Let X = (X1, …, Xn) be a censored sample in term of Xj = min(Zj, Yj), where Zj - Observed Variable, Yj - censor, j = 1..n.

In this case 


## Author Info
Made by Havriushenko Anastasiia
- mail - havriushenko.nasty@gmail.com
- facebook - https://www.facebook.com/gastad601

Tasks provided by [Maiboroda, R.E.](http://probability.univ.kiev.ua/index.php?page=userinfo&person=mre&lan=ru)
- mail - mre@univ.kiev.ua


[Back to the top](#nonparametric-statistics)
