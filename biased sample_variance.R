library(triangle)

t0 = 0                                                      #start of interval
t1 = 2                                                      #end of interval
n1 = c(50, 100, 500, 1000)                                  #number of unbiased sample
n2 = c(50, 100, 500, 1000)                                  #number of biased sample
omega = function(x) 1 - x/2                                 #bias function
qfunc = function(p, min, max) qtriangle(p, min, max)        #quantile function
pfunc = function(t, min, max) ptriangle(t, min, max)        #distribution function
rfunc = function(n, min, max) rtriangle(n, min, max)        #generates random deviates

Femp_general = function(sample) {                           #generator of Empirical distribution function
  n = length(sample)
  sample = c(-Inf, sort(sample))
  f = (0 : n) / n
  function(t) f[findInterval(t, sample)]
}
F_mean_general = function(f1, f2) {                         #generator of mean function (of Empirical distribution and Horwitz-Thompson estimation)
  function(x) 1/2*(f1(x) + f2(x))
}
F_HT_general = function(sample, max) {                      #generator of Horwitz-Thompson estimation
  n = length(sample)
  sum_up = 0
  sum_down = sum(1 / omega(sample))
  sample = c(-Inf, sort(sample), max)
  f = (0 : n + 1)
  for (i in 1:n) {
    sum_up = sum_up + 1 / omega(sample[i])
    f[i] = sum_up
  }
  f[n + 1] = sum_down
  function(t) f[findInterval(t, sample)] / sum_down
}
F_Vardi_general = function(sample1, sample2) {              #generator of Vardi estimation
  sample = c(sample1, sample2)
  n2 = length(sample2)
  Lamda = function(l) abs(sum(omega(sample) / (n2*sample + l)) - 1)
  l = nlm(Lamda, 100)$estimate
  f = function(y) 1/(n2 * omega(y) + l)
  w = 1 / sum(f(sample))
  p = w * f(sample)
  function(x)  sum(p * (sample < x))
}
biased_sample_gen = function(n) {                           #generator of biased sample
  k = 1
  X = c()
  while (k <= n) {
    templ_x = rfunc(1, t0, t1)
    temp_uni = runif(1, 0, 1)
    if (temp_uni < omega(templ_x)) {
      X = c(X, templ_x)
      k = k + 1
    }
  }
  X
}
Variance = function(sample, F_prob) {                       #calcucation of Variance 
  prob = sapply(sample, F_prob)
  average = sum(sample * prob)
  sum(prob * (sample - average)^2)  
}

tt = seq(t0, t1, 0.01)                                      #interval
M_variance = matrix(nrow = length(n1), ncol = 4)            #matrix of variances
for (k in 1:length(n1)) {
  X_real = pfunc(tt, t0, t1)                                #real distributed sample
  X_abs = rfunc(n1[k], t0, t1)                              #unbiased sample
  X_shift = biased_sample_gen(n2[k])                        #biased sample
  Femp = Femp_general(X_abs)                                #Empirical distribution based on X_abs sample 
  F_HT = F_HT_general(X_shift, t1)                          #Horwitz-Thompson estimation based on X_shift sample
  F_mean = F_mean_general(Femp, F_HT)                       #estimation = mean of Empirical and Horwitz-Thompson estimations
  F_Vardi = F_Vardi_general(X_abs, X_shift)                 #Vardi estimation based on  X_abs + X_shift samples
  Func = c(Femp, F_HT, F_mean, F_Vardi)                     #vector of Estimate distribution functions
  for (j in 1:length(Func))    M_variance[k, j] = Variance(X_real, Func[[j]])  
}
colnames(M_variance) = c("Femp", "F_HT", "F_mean", "F_Vardi")
rownames(M_variance) = c(50, 100, 500, 1000)
M_variance