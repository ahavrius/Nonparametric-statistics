#install.packages("plot3D")
library(plot3D)

#TotalData = read.csv("/home/asta/Nonparametric-statistics/wine.csv", header = TRUE)
TotalData = read.csv("/Users/ahavrius/Nonparametric-statistics/wine.csv", header = TRUE)
my_data =  data.frame(TotalData$Site, TotalData$Alcogol, TotalData$Malic_acid)  # problems with magnesium in IQR()
names(my_data) = c("Site", "Alcogol", "Magnesium");

n = nrow(my_data)
n_i = c(sum(my_data$Site == 1), sum(my_data$Site == 2), sum(my_data$Site == 3))
M = 3
d = 2

kernelIponechnikova = function(x) (abs(x)<1) * (1 - x^2) *3/4
kernel_vector = function(x1, x2) kernelIponechnikova(x1) * kernelIponechnikova(x2)#sapply(x1, kernelIponechnikova)*sapply(x2, kernelIponechnikova)

kernel_density_gener_vector = function(sample, kernel, smooth, n) {
  function(x) sum(kernel((x[1] - sample[,1])/smooth[1], (x[2] - sample[,2])/smooth[2])) / (n * prod(smooth))
}

kernel_density_m(sample, m, smooth){
  filtered_sample = sample[sample$Site == m]
  kernel_density_gener_vector(filtered_vector, kernel_vector, smooth, n_i[m])
  filtered_vector = cbind(filtered_sample$Alcogol, filtered_sample$Magnesium)
}

smooth_silverman_advanced_vector = function(sample) {
  s = c(1, 1)
  s[1] = min(sd(sample[,1]), IQR(sample[,1]) / 1.34)
  s[2] = min(sd(sample[,2]), IQR(sample[,2]) / 1.34)
  s * (4/(d+2))^(1/(d+4)) * length(sample[,1])^(-1/(d + 4))
}

bayes_classifier(func){
  for_max = rep(1, length(n_i))
  function(x){
    for (i in 1:length(n_i)) for_max[i] = n_i[i] * func(x, i) / n
    which.max(for_max)
  }
}



smooth = smooth_silverman_advanced_vector(SampleVector)
density_estimated = kernel_density_m(my_data, 1, smooth)