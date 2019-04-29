library(triangle)

#Statistical parameters
alpha = 0.05   #significance level
t0 = 0         #start of interval
t1 = 2         #end of interval
min = 0        #parameters
max = 2        # of distribution
qfunc = function(p) qtriangle(p, min, max)                   #quantile function
pfunc = function(t) ptriangle(t, min, max)                   #distribution function
rfunc = function(n) rtriangle(n, min, max)                   #generates random deviates

Femp = function(x, sample) mean(sample < x)                  #simple empirical function = use if you need it one time

Femp_generator = function(x) {                               #generator of Empirical distribution function
  n = length(x)
  x = c(-Inf, sort(x))
  f = (0 : n) / n
  function(t){f[findInterval(t, x)]}
}
Fconf = function(x, sample, alpha, func){                    #calculation of Confidence interval
  lambda = qnorm(1 - alpha/2)
  Fnx = func(x, sample)
  stand = sqrt(Fnx * (1 - Fnx) / length(sample)) * lambda
  c(-stand + Fnx, stand + Fnx)
}
#Assignment of variables
color = 1
tt = seq(qfunc(0.01), qfunc(0.99), 0.01)                     #interval
x_value =c(qfunc(1/3), qfunc(1), qfunc(2/3))                 #x of quantile level = 1/3, 1, 2/3
A_freq = matrix(ncol = length(x_value), nrow = 0)            #matrix of frequency

matplot(tt, sapply(tt, pfunc), col = color, type = "l",      #ploting the real distribution function
        main = "Distribution functions", xlab = "", ylab = "")    #title
legend("bottomright",col=1:6, legend=c("real",paste("n =", strtoi(c(10, 50, 100, 500, 1000)))), lty = c(1, 1, 1, 1, 1))

for (n in c(10, 50, 100, 500, 1000))
{
  color = color + 1
  X = rfunc(n)                                               #generate sample of n elems
  lines(tt, Femp_generator(X)(tt), col = color, type = "s")  #draw empirical estimation of distribution function
  amount = c(0, 0, 0)
  for (m in 1:1000) {
    X_m = rfunc(n)
    for (k in 1:3) {
      interval = Fconf(x_value[k], X_m, alpha, Femp)
      f_x = pfunc(x_value[k])
      amount[k] = amount[k] + ((interval[1] <= f_x) & (f_x <= interval[2]))
    }
  }
  A_freq = rbind(A_freq, amount / 1000)
}

colnames(A_freq) = c("1/3", "1", "2/3")
rownames(A_freq) = c(5, 10, 100, 500, 1000)
print(A_freq)
