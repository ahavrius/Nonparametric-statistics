library(triangle)

alpha = 0.05
color = 1
tt = seq(0, 2, 0.01)


#part 2
FKM = function(x, sample, delta){
  dot = 1
  n = length(sample)
  index = order(sample)
  sample = sample[index]
  delta = delta[index]
  i = 1
  while (i <= n && sample[i] <= x)
  {
    dot = dot * (1 - delta[i]/(n - i + 1))
    i = i + 1  
  }
  1 - dot
}

DFsqrt = function(x, sample,  delta)
{
  n = length(sample)
  index = order(sample)
  sample = sample[index]
  delta = delta[index]
  s = 0
  for (i in 1:n)
    if ((n + 1 - i - delta[i]) != 0)
      s = s + (delta[i] * (sample[i] <= x)) / ((n + 1 - i)*(n + 1 - i - delta[i]))
    (1 - FKM(x, sample, delta))*sqrt(s)
}
  
Fconf = function(x, sample, delta, alpha)
{
  lambda = qnorm(1 - alpha/2)
  Fnx = FKM(x, sample, delta)
  stand = sqrt(DFsqrt(x, sample, delta) / length(sample)) * lambda
  stand
  c(-stand + Fnx, stand + Fnx)
}


x_value =qtriangle(1/2, 0, 2)
A_freq = matrix(ncol = 2, nrow = 0)
matplot(tt, pmin(ptriangle(tt, 0, 2), punif(tt, 0, 2)), col = color, type = "s")
for (n in c(5, 10, 100, 500, 1000))
{
  color = color + 1
  F = rtriangle(n, 0, 2)
  G = runif(n, 0, 2)
  X = pmin(F, G)
  D = (F < G)
  lines(tt, sapply(tt, FKM, sample = X, delta = D), col = color)
  legend("bottomright",col=1:6,legend=c("real", 5, 10, 100, 500, 1000), lty=1:6)
  amount = c(0, 0)
  for (m in 1:1000)
  {
    F_m = rtriangle(n, 0, 2)
    G_m = runif(n, 0, 2)
    X_m = pmin(F, G)
    D_m = (F < G)
    interval = Fconf(x_value, X_m, D_m, alpha)
    f_x = pmin(ptriangle(x_value, 0, 2), punif(x_value, 0, 2))
    amount[1] = amount[1] + ((interval[1] <= f_x) & (f_x <= interval[2]))
  
  }
  A_freq = rbind(A_freq, amount / 1000)
}
colnames(A_freq) = c("1/2 for F", "1/2 for ln(1-F)")
rownames(A_freq) = c(5, 10, 100, 500, 1000)
A_freq 
