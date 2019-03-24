library(triangle)

#Statistical parameters
alpha = 0.05   #significance level
t0 = 0         #start of interval
t1 = 2         #end of interval
qfunc = function(p, min, max) qtriangle(p, min, max)  #quantile function
pfunc = function(t, min, max) ptriangle(t, min, max)  #distribution function
rfunc = function(n, min, max) rtriangle(n, min, max)  #generates random deviates

Femp = function(x, sample) mean(sample < x)           #simple empirical function = use if you need it one time

Femp_generator = function(x) {                        #generator of Empirical distribution function
  n = length(x)
  x = c(-Inf, sort(x))
  f = (0 : n) / n
  function(t){f[findInterval(t, x)]}
}
Fconf = function(x, sample, alpha, func){             #calculation of Confidence interval
  lambda = qnorm(1 - alpha/2)
  Fnx = func(x, sample)
  stand = sqrt(Fnx * (1 - Fnx) / length(sample)) * lambda
  c(-stand + Fnx, stand + Fnx)
}
#Assignment of variables
color = 1
tt = seq(qfunc(0.01, t0, t1), qfunc(0.99, t0, t1), 0.01)                    #interval
x_value =c(qfunc(1/3, t0, t1), qfunc(1,t0, t1), qfunc(2/3, t0, t1))         #x of quantile level = 1/3, 1, 2/3
A_freq = matrix(ncol = length(x_value), nrow = 0)                           #matrix of frequency

matplot(tt, sapply(tt, pfunc, min = t0, max = t1),                          #ploting the real distribution function
col = color, type = "l", main = "Density functions", xlab = "", ylab = "")  #title

for (n in c(5, 10, 100, 500, 1000))
{
  color = color + 1
  X = rfunc(n, t0, t1)                                                #generate sample of n elems
  lines(tt, Femp_generator(X)(tt), col = color, type = "s")           #draw empirical estimation of distribution function
  amount = c(0, 0, 0)
  for (m in 1:1000) {
    X_m = rfunc(n, t0, t1)
    for (k in 1:3) {
      interval = Fconf(x_value[k], X_m, alpha, Femp)
      f_x = pfunc(x_value[k], t0, t1)
      amount[k] = amount[k] + ((interval[1] <= f_x) & (f_x <= interval[2]))
    }
  }
  A_freq = rbind(A_freq, amount / 1000)
}
legend("bottomright",col=1:6, legend=c("real", 5, 10, 100, 500, 1000), lty = c(1, 1, 1, 1, 1))

colnames(A_freq) = c("1/3", "1", "2/3")
rownames(A_freq) = c(5, 10, 100, 500, 1000)
print(A_freq)
