library(triangle)

alpha = 0.05
color = 1
tt = seq(qtriangle(0.01, 0, 2), qtriangle(0.99, 0, 2), 0.01)
Femp = function(x, sample) mean(sample < x)

#dfgen = function(x){
#  n = length(x)
#  x = c(-Inf, sort(x))
#  f = (0 : n) / n
#  function(t){f[findInterval(t, x)]}
# }

Fconf = function(x, sample, alpha)
{
  lambda = qnorm(1 - alpha/2)
  Fnx = Femp(x, sample)
  stand = sqrt(Fnx * (1 - Fnx) / length(sample)) * lambda
  c(-stand + Fnx, stand + Fnx)
}

x_value =c(qtriangle(1/3, 0, 2), qtriangle(1, 0, 2), qtriangle(2/3, 0, 2))
A_freq = matrix(ncol = length(x_value), nrow = 0)
matplot(tt, ptriangle(tt, 0, 2), col = color, type = "s")
for (n in c(5, 10, 100, 500, 1000))
{
  color = color + 1
  X = rtriangle(n, 0, 2)
  lines(tt, sapply(tt, Femp, sample = X), col = color)
  legend("bottomright",col=1:6,legend=c("real", 5, 10, 100, 500, 1000), lty=1:6)
  amount = c(0, 0, 0)
  for (m in 1:1000)
  {
    X_m = rtriangle(n, 0, 2)
    for (k in 1:3)
    {
      interval = Fconf(x_value[k], X_m, alpha)
      f_x = ptriangle(x_value[k], 0, 2)
      amount[k] = amount[k] + ((interval[1] <= f_x) & (f_x <= interval[2]))
    }
  }
  A_freq = rbind(A_freq, amount / 1000)
}
colnames(A_freq) = c("1/3", "1", "2/3")
rownames(A_freq) = c(5, 10, 100, 500, 1000)
A_freq 

