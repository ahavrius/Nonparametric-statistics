
n = 300
r_func = function(n) runif(n, 0, 3)               
r_epsilon = function(n) runif(n, -1, -1)
g = function(x)
    -x*(-242 + 805*x - 742*x^2 + 200*x^3) / 7

K = function(x)
    3/4 * abs(1 - x^2) * (abs(x) <= 1)
func_basis = function(x)
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

loc_lin_regression = function(X, Y)
{
}



X = r_func(n)
Y = g(X) + r_epsilon(n)

#print(median(sliding_window(X, Y)(1, 0.5)))

g_sl_mean = sliding_mean(X, Y, 0.15)
g_sl_median = sliding_median(X, Y, 0.35)
tt = seq(0, 3, 0.05)

plot(tt, g(tt))
plot(tt, sapply(tt, g_sl_mean))
plot(tt, sapply(tt, g_sl_median))