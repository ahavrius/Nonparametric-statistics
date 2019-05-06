library(readxl)
library(plot3D)

TotalData <- read_excel("Nonparametric-statistics/wine2.xlsx")
my_data =  data.frame(TotalData$Site, TotalData$Alcogol, TotalData$Magnesium)  # problems with magnesium in IQR()
names(my_data) = c("Site", "Alcogol", "Magnesium");

n = nrow(my_data)
n_i = c(sum(my_data$Site == 1), sum(my_data$Site == 2), sum(my_data$Site == 3))
M = 3
d = 2

kernelIponechnikova = function(x) (abs(x)<1) * (1 - x^2) *3/4
kernel_vector = function(x) prod(sapply(x, kernelIponechnikova))

kernel_density_gener_m = function(sample, kernel, smooth){
  function(x, m){
    sample = sample[sample[,1] == m, ]
    dim = ncol(sample) - 1
    value_for_kernel = as.matrix(sample[,-1])
    for (i in 1:dim) value_for_kernel[, i] = (x[i] - sample[, i+1]) /smooth[i]
    sum(apply(value_for_kernel, 1, kernel)) / (n_i[m] * prod(smooth))
  }
}

smooth_silverman_simple_vector = function(sample) {
  dim = ncol(sample) - 1
  s = rep(1, dim)
  for (i in 1:dim) s[i] = sd(sample[,i + 1])
  s * (4/(dim+2))^(1/(dim+4)) * length(sample[,1])^(-1/(dim + 4))
}

empirical_bayes_classifier = function(func){
  p_i = n_i / n
  for_max = rep(1, M)
  function(x){
    for (i in 1:M) for_max[i] = p_i[i] * func(x, i)
    which.max(c(0, for_max))-1
  }
}

empirical_classifier_bias = function(func){
  p_i = n_i / n
  for_max = rep(1, M)
  function(x){
    for (i in 1:M) for_max[i] = p_i[i] * func(x, i)
    max(for_max)
  }
}


build_classifier_from_sample = function(sample){
  smooth = smooth_silverman_simple_vector(sample)
  kernel_density = kernel_density_gener_m(sample, kernel_vector, smooth)
  empirical_bayes_classifier(kernel_density)
}

caclulate_bias_empirical_class  = function(sample){
  smooth = smooth_silverman_simple_vector(sample)
  kernel_density = kernel_density_gener_m(sample, kernel_vector, smooth)
  empirical_classifier_bias(kernel_density)
}

empirical_classifier = build_classifier_from_sample(my_data)

#part2
integrate_classifier =  function(func, min, max, delta){
  tt = seq(min-1, max+1, delta)
  sum(sapply(tt, func)*delta)
}

projected_classifier = function(sample){
  tt = seq(0, 2*pi, 0.1)
  proj_vector = cbind(sin(tt), cos(tt))
  success_prob = rep(0, length(tt))
  proj_sample = cbind(sample[, 1], sample[, 1])
  for (j in 1:length(tt)) {
    for (i in 1:nrow(proj_sample)) proj_sample[i, 2] = sum(proj_vector[j,] * sample[i,-1])
    proj_classifier = caclulate_bias_empirical_class(proj_sample)
    success_prob[j] = integrate_classifier(proj_classifier, min(proj_sample[,2]), max(proj_sample[,2]), 0.5)
    print(success_prob[j])
  }
  points3D(proj_vector[,1], proj_vector[,2], success_prob, pch = 19, cex = 0.5)
  arg_max = which.max(success_prob)
  print(tt[arg_max])
  for (i in 1:nrow(proj_sample)) proj_sample[i, 2] = sum(proj_vector[arg_max,] * sample[i,-1])
  best_proj_class = build_classifier_from_sample(proj_sample)
  function(x) best_proj_class(sum(proj_vector[arg_max,]*x))
}

draw_line = function(angle, x0, y0)
{
  tt = seq(0, 100, 0.1)
  lines(x0 + tt * sin(angle), y0 + tt*cos(angle), col = 1, type = "l")
}

draw_area = function(func){
  size = 75
  transparency = 0.3
  colors = c(rgb(1, 1, 1),
             rgb(1, 0, 0, alpha = transparency),
             rgb(0, 1, 0, alpha = transparency),
             rgb(0, 0, 1, alpha = transparency))
  xx = seq(min(my_data[,2]), max(my_data[,2]), length.out = size)
  xx = rep(xx, each=size)
  yy = seq(min(my_data[,3]), max(my_data[,3]), length.out = size)
  yy = rep(yy, size)
  plot(xx, yy, pch = 16, col = colors[apply(data.frame(xx, yy), 1, func) + 1], cex = 2)
}

proj_classifier = projected_classifier(my_data)

errors1 = sum(apply(my_data[, -1], 1, empirical_classifier) != my_data[,1]) / n
errors2 = sum(apply(my_data[, -1], 1, proj_classifier) != my_data[,1]) / n

plot(my_data[,2], my_data[,3], pch = 16, col = my_data[,1] + 1)
draw_line(4.7, 16, 100)
legend("topright",col=2:4, legend=1:3, lty = rep(1,3))
plot(my_data[,2], my_data[,3], pch = 21, bg = my_data[,1] + 1, lwd=3, cex = 1.5,
     col = apply(my_data[, -1], 1, empirical_classifier) + 1)
legend("topright",col=2:4, legend=1:3, lty = rep(1,3))

plot(my_data[,2], my_data[,3], pch = 21, bg = my_data[,1] + 1, lwd=3, cex = 1.5,
     col = apply(my_data[, -1], 1, proj_classifier) + 1)
draw_line(4.7 - pi/2, 12, 165)
legend("topright",col=2:4, legend=1:3, lty = rep(1,3))

draw_area(empirical_classifier)
draw_area(proj_classifier)

