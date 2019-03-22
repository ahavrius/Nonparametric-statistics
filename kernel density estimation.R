library(triangle)

#Statistical parameters
alpha = 0.05   #significance level
location = 0
scale = 1
qfunc = function(p, location, scale) qalaplace(p, location, scale)  #quantile function
dfunc = function(x, location, scale) dalaplace(x, location, scale)  #density function
pfunc = function(t, location, scale) palaplace(t, location, scale)  #distribution function
rfunc = function(n, location, scale) ralaplace(n, location, scale)  #generates random deviates
kernel = function(x) dnorm(x, mean = 0, sd = 1)                     #kernel density function
kernel_0 = function(u) {
  func = function(x)
}
t0 = -2
t1 = 2
tt = seq(t0, t1, 0.01)

kernel_density_gener = function(sample, kernel, smooth) {
  n = length(sample)
  function(x) sum(kernel((x - sample)/smooth)) / (n * smooth)
}

d_2_calculate = function(kernel) {   #calculation of d^2 = integral of K^2(x)dx
  func = function(x) kernel(x) * kernel(x)
  integrate(func, -Inf, Inf)
}

D_caclulate = function(kernel) { #D = integral of x^2 K(x)dx
  func = function(x) kernel(x)*x^2
  integrate(func, -Inf, Inf)
}

smooth_silverman_simple = function(d_2, D, sample) {
  s = sd(sample)
  s * (d_2 * 8 * sqrt(pi) / (3 * length(sample) * D^2)) ^ (1/5)
}
  
smooth_silverman_simple_advanced = function(d_2, D, sample) {
  s = min(sd(sample), IQR(sample) / 1.34)
  s * (d_2 * 8 * sqrt(pi) / (3 * length(sample) * D^2)) ^ (1/5)
}
smooth_nonparam = function(d_2, D, sample, func, x) {
  N = length(sample)
  func = numericDeriv(numericDeriv(func(x)))
  phi = integrate()
  
}

cv_func = function(kernal, k_0, sample) {   #cross-validation functional
  n = lenght(sample)
  
}



d_2 =  d_2_calculate(kernel)
D = D_caclulate(kernel)

