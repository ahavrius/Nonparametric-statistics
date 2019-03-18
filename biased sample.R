library(triangle)

t0 = 0                                                #start of interval
t1 = 2                                                #end of interval
n1 = 1000                                             #number of unbiased sample
n2 = 1000                                             #number of biased sample
omega = function(x) 1 - x/2                           #bias function
qfunc = function(p, min, max) qtriangle(p, min, max)  #quantile function
pfunc = function(t, min, max) ptriangle(t, min, max)  #distribution function
rfunc = function(n, min, max) rtriangle(n, min, max)  #generates random deviates

Femp = function(x, sample) mean(sample < x)           #Empirical distribution function

Femp_general = function(sample) {                     #generator of Empirical distribution function
  n = length(sample)
  sample = c(-Inf, sort(sample))
  f = (0 : n) / n
  function(t){f[findInterval(t, sample)]}
}
F_mean = function(x, f1, f2) 1/2*(f1(x) + f2(x))

#F_HT = function(x, sample){                            #simple Horwitz-Thompson estimation - works slower
#  sample_up = (sample < x) / omega(sample)
#  sample_down = 1 / omega(sample)
#  sum(sample_up) / sum(sample_down)
#}
F_HT_general = function(sample, max) {
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
  function(t){f[findInterval(t, sample)] / sum_down}
}
F_Vardi_general = function(sample1, sample2) {         #generator of Vardi estimation
  sample = c(sample1, sample2)
  n2 = length(sample2)
  Lamda = function(l) abs(sum(omega(sample) / (n2*sample + l)) - 1)
  l = nlm(Lamda, 100)$estimate
  f = function(y) 1/(n2 * omega(y) + l)
  w = 1 / sum(f(sample))
  p = w * f(sample)
  function(x)  {sum(p * (sample < x))}
}

biased_sample_gen = function(n) {
  X_test = rfunc(n, t0, t1)
  k = 1
  X = c()
  while (k <= n) {
    temp = runif(1, 0, 1)
    if (temp < omega(X_test[k])) {
      X = c(X, X_test[k])
      k = k + 1
    }
  }
  X
}
biased_sample_gen = function(n) {
  k = 1
  X = c()
  while (k <= n) {
    templ_x = rfunc(1, t0, t1)
    temp_uni = runif(1, 0, 1)
    if (temp < omega(templ_x)) {
      X = c(X, templ_x)
      k = k + 1
    }
  }
  X
}

color = 1
tt = seq(t0, t1, 0.01)              #interval
X_abs = rfunc(n1, t0, t1)           #unbiased sample
X_shift = biased_sample_gen(n2)     #biased sample

Femp = Femp_general(X_abs)
F_HT = F_HT_general(X_shift, t1)
F_Vardi = F_Vardi_general(X_abs, X_shift)

matplot(tt, ptriangle(tt, 0, 2), col = 1, type = "l")
lines(tt, sapply(tt, Femp), col = 2)
lines(tt, sapply(tt, F_HT), col = 3)
lines(tt, sapply(tt, F_mean, f1 = Femp, f2 = F_HT), col = 4)
lines(tt, sapply(tt, F_Vardi), col = 5)
legend("bottomright",col=1:5, legend=c("real","Femp", "F_HT", "1/2(Femp + F_HT)", "F_Vardi"), lty=c(1,1,1,1,1))
