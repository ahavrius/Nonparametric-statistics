library(triangle)

omega = function(x) 1 - x/2
#omega = function(x) 1/(1+x)

Femp = function(x, sample) mean(sample < x)

F_HT = function(x, sample){
  sample_up = (sample < x) / omega(sample)
  sample_down = 1 / omega(sample)
  sum(sample_up) / sum(sample_down)
}

F_HT_general = function(x, sample)
{
  
  
}


F_Vardi_general = function(sample1, sample2)
{
  sample = c(sample1, sample2)
  n2 = length(sample2)
  Lamda = function(l) abs(sum(omega(sample) / (n2*sample + l)) - 1)
  l = nlm(Lamda, 100)$estimate
  f = function(y) 1/(n2 * omega(y) + l)
  w = 1 / sum(f(sample))
  p = w * f(sample)
  function(x)  {sum(p * (sample < x))}
}

Femp_general = function(x){
  n = length(x)
  x = c(-Inf, sort(x))
  f = (0 : n) / n
  function(t){f[findInterval(t, x)]}
}

F_mean = function(x, f_emp1, sample2) 1/2*(f_emp1(x) + F_HT(x, sample2))

n1 = 1000
n2 = 1000
times = c(10, 50, 100, 500, 1000)

t1 = 2
tt = seq(0, t1, 0.01)
for (i in times) {
  
}

X_abs = rtriangle(n1, 0, 2)
X_test = rtriangle(n2, 0, 2)
X_shift = c()
for (k in 1:n2){
  temp = runif(1, 0, 1)
  if (temp < omega(X_test[k]))
    X_shift = c(X_shift, X_test[k])
}
Femp = Femp_general(X_abs)
F_Vardi = F_Vardi_general(X_abs, X_shift)

matplot(tt, ptriangle(tt, 0, 2), col = 1, type = "l")
#matplot(tt, pchisq(tt, df = 3), col = 1, type = "l")
lines(tt, sapply(tt, Femp), col = 2)
lines(tt, sapply(tt, F_HT, sample = X_shift), col = 3)
lines(tt, sapply(tt, F_mean, f_emp1 = Femp, sample2 = X_shift), col = 4)
lines(tt, sapply(tt, F_Vardi), col = 5)
legend("bottomright",col=1:5, legend=c("real","Femp", "F_HT", "1/2(Femp + F_HT)", "F_Vardi"), lty=c(1,1,1,1,1))
