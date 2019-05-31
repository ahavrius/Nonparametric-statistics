library(MASS)

n = 300
r_func = function(n) runif(n, 0, 3)               
r_epsilon = function(n) runif(n, -1, -1)
g = function(x)
    -x*(-242 + 805*x - 742*x^2 + 200*x^3) / 7
K = function(value){
  func = function(x) (3/4 * abs(1 - x^2) * (abs(x) <= 1))
  sapply(value, func)
}
    
basis = function(x)
    c(1, x)

sliding_window = function(X,Y)
    function(x_0, h) Y[abs(X - x_0) < h/2]

sliding_mean = function(X, Y, h){
    sliding_window_local = sliding_window(X, Y)
    function(x_0)
      mean(sliding_window_local(x_0, h))  
  }

sliding_median = function(X, Y, h){
    sliding_window_local = sliding_window(X, Y)
    function(x_0)
      median(sliding_window_local(x_0, h))  
  }

loc_lin_regression = function(X, Y, h){
  X_basis = t(sapply(X, basis))
  function(x_0){
    W = diag(K((x_0 - X)/h))
    b = ginv(t(X_basis) %*% W %*% X_basis) %*% t(X_basis) %*% W %*% Y
    sum(b * func_basis(x_0))
  }
}

X = r_func(n)
Y = g(X) + r_epsilon(n)

g_sl_mean = sliding_mean(X, Y, 0.15)
g_sl_median = sliding_median(X, Y, 0.35)
g_loc_lin = loc_lin_regression(X, Y, 0.1)
tt = seq(0, 3, 0.05)

plot(tt, g(tt), type = "l", col = 1)
lines(tt, sapply(tt, g_sl_mean), col = 2)
lines(tt, sapply(tt, g_sl_median), col = 3)
lines(tt, sapply(tt, g_loc_lin), col = 4)
legend("bottomright",col=1:4, legend=c("real g", "sliding_mean", "sliding_median", "loc_lin_regression"), lty = c(1, 1, 1, 1))
