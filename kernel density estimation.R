library(LaplacesDemon)

#Statistical parameters
alpha = 0.05   #significance level
n = 500
location_ = 0
scale_ = 1
#qfunc = function(p, location, scale) qalaplace(p, location, scale)  #quantile function
dfunc = function(x, location, scale) dalaplace(x, location, scale)  #density function
pfunc = function(t, location, scale) palaplace(t, location, scale)  #distribution function
#rfunc = function(n, location, scale) ralaplace(n, location, scale)  #generates random deviates
kernel = function(x) dnorm(x, mean = 0, sd = 1)                     #kernel density function

t0 = -2
t1 = 2
tt = seq(t0, t1, 0.01)

kernel_tilda = function(u) {
  func = function(x) kernel(x) * kernel(x + u)
  integrate(func, -Inf, Inf)$value
}
d_2_calculate = function(kernel) {   #calculation of d^2 = integral of K^2(x)dx
  func = function(x) kernel(x) * kernel(x)
  integrate(func, -Inf, Inf)$value
}
D_caclulate = function(kernel) { #D = integral of x^2 K(x)dx
  func = function(x) kernel(x)*x^2
  integrate(func, -Inf, Inf)$value
}

kernel_tilda_0 = kernel_tilda(0)
d_2 =  d_2_calculate(kernel)
D = D_caclulate(kernel)

kernel_density_gener = function(sample, kernel, smooth) {
  n = length(sample)
  function(x) sum(kernel((x - sample)/smooth)) / (n * smooth)
}
smooth_silverman_simple = function(d_2, D, sample) {
  s = sd(sample)
  s * (d_2 * 8 * sqrt(pi) / (3 * length(sample) * D^2)) ^ (1/5)
}
smooth_silverman_advanced = function(d_2, D, sample) {
  s = min(sd(sample), IQR(sample) / 1.34)
  s * (d_2 * 8 * sqrt(pi) / (3 * length(sample) * D^2)) ^ (1/5)
}
#smooth_nonparam = function(d_2, D, sample, func, x) {
#  N = length(sample)
#  func = numericDeriv(numericDeriv(func(x)))
#  phi = integrate()
#}
cv_gener = function(kernel, kernel_tilda, k_0, sample) {   #cross-validation functional
  n = length(sample)
  two_sums = function(h) {
    sum_k = 0
    sum_k_tilda = 0
    i = 1
    j = 1
    while (i < n) {
      while (j < i) {
        sum_k = sum_k + kernel((sample[i] - sample[j]) / h)
        sum_k_tilda = sum_k_tilda + kernel_tilda((sample[i] - sample[j]) / h)
        j = j + 1
      }
      i = i + 1
    }
    c(sum_k_tilda, sum_k)
  }
  function(h) {
    sum_prepared = two_sums(h)
    k_0 / (h*n) + 2*sum_prepared[1] / (h*n^2) - 4*sum_prepared[2] / (n*(n-1)*h)
  }
}
smooth_cv = function(func, value) nlm(func, value)$estimate

X = sapply(tt, pfunc, location = location_, scale = scale_)
density_silverman_simple = kernel_density_gener(X, kernel, smooth_silverman_simple(d_2, D, X))
density_silverman_advanced = kernel_density_gener(X, kernel, smooth_silverman_advanced(d_2, D, X))
cv = cv_gener(kernel, kernel_tilda, kernel_tilda_0, X)
smooth_cv_here = smooth_cv(cv, smooth_silverman_simple(d_2, D, X))
density_cv = kernel_density_gener(X, kernel, smooth_cv_here)
matplot(tt, dfunc(tt, location_, scale_), ylim = c(0, 3), col = 1, type = "l")                      #draw the real density function
lines(tt, sapply(tt, density_silverman_simple), col = 2)                                            #draw Silverman simple density estimation
lines(tt, sapply(tt, density_silverman_advanced), col = 3)                                          #draw Silverman advanced density estimation
lines(tt, sapply(tt, density_cv), col = 4)                                                          #draw Cross-validation density estimation

