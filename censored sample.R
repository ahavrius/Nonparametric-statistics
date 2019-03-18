library(triangle)

#Statistical parameters
alpha = 0.05   #significance level
t0 = 0         #start of interval
t1 = 2         #end of interval
qfunc = function(p, min, max) qtriangle(p, min, max)  #quantile function
pfunc = function(t, min, max) ptriangle(t, min, max)  #distribution function
rfunc = function(n, min, max) rtriangle(n, min, max)  #generates random deviates

pcensor = function(t, min, max) punif(t, min, max)    #distribution function of censor
rcensor = function(n, min, max) runif(n, min, max)    #generates random deviates with censor distribution

g = function(x) log(1 - x)                            #any monotonic differentiable function
g_deriv = function(x) -1/(1 - x)                      #derivation of g
id = function(x) x
id_deriv = function(x) 1

F_KM = function(x, sample, delta) {                   #Kaplan-Meier estimator of distribution function
  dot = 1
  n = length(sample)
  index = order(sample)
  sample = sample[index]
  delta = delta[index]
  i = 1
  while (i <= n && sample[i] <= x) {
    dot = dot * (1 - delta[i]/(n - i + 1))
    i = i + 1  
  }
  1 - dot
}
SFsqrt = function(x, sample,  delta, f_KM) {          #calculation of Standard deviation
  n = length(sample)
  index = order(sample)
  sample = sample[index]
  delta = delta[index]
  s = 0
  i = 1
  while (i <= n && sample[i] <= x) {
    if ((n + 1 - i - delta[i]) != 0)
      s = s + delta[i] / ((n + 1 - i)*(n + 1 - i - delta[i]))
    i = i + 1
  }
  (1 - f_KM)*sqrt(s)
}
Fconf_censored = function(x, sample, delta, alpha, func, deriv) {    #calculation of Confidence interval of g(F)
  lambda = qnorm(1 - alpha/2)
  f_KM = F_KM(x, sample, delta)
  stand = SFsqrt(x, sample, delta, f_KM) * lambda * abs(deriv(f_KM))
  c(-stand + func(f_KM), stand + func(f_KM))
}

#Assignment of variables
color = 1
tt = seq(t0, t1, 0.01)                  #interval
x_value = qfunc(1/2, t0, t1)            #x of quantile level = 1/2
f_x = pfunc(x_value, t0, t1)            # = 1/2
A_freq = matrix(ncol = 2, nrow = 0)     #matrix of frequency

matplot(tt, pfunc(tt, t0, t1), col = color, type = "l")             #draw the real distribution function

for (n in c(5, 10, 100, 500, 1000))
{
  color = color + 1
  F = rfunc(n, t0, t1)                  #real sample
  G = rcensor(n, t0, t1)                #censoring sample
  X = pmin(F, G)                        #censored sample
  D = (F < G)                           #right censoring delta
  lines(tt, sapply(tt, F_KM, sample = X, delta = D), col = color)    #draw Kaplan-Meier estimation
  amount = c(0, 0)
  for (m in 1:1000) {                   #frequency of hitting confidence interval
    F_m = rfunc(n, t0, t1)              #real sample
    G_m = rcensor(n, t0, t1)            #censoring sample
    X_m = pmin(F_m, G_m)                #censored sample
    D_m = (F_m < G_m)                   #right censoring delta
    interval = Fconf_censored(x_value, X_m, D_m, alpha, id, id_deriv)
    interval_g = Fconf_censored(x_value, X_m, D_m, alpha, g, g_deriv)
    amount[1] = amount[1] + ((interval[1] <= id(f_x)) & (id(f_x) <= interval[2]))
    amount[2] = amount[2] + ((interval_g[1] <= g(f_x)) & (g(f_x) <= interval_g[2]))
  }
  A_freq = rbind(A_freq, amount / 1000)
}
legend("bottomright",col=1:6, legend=c("real", 5, 10, 100, 500, 1000), lty=c(1, 1, 1, 1, 1))
colnames(A_freq) = c("F", "ln(1-F)")
rownames(A_freq) = c(5, 10, 100, 500, 1000)
print(A_freq)
