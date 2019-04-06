#install.packages("plot3D")
library(plot3D)

#TotalData = read.csv("/home/asta/Nonparametric-statistics/wine.csv", header = TRUE)
TotalData = read.csv("/Users/ahavrius/Nonparametric-statistics/wine.csv", header = TRUE)
my_data =  data.frame(TotalData$Site, TotalData$Alcogol, TotalData$Malic_acid)  # problems with magnesium in IQR()
names(my_data) = c("Site", "Alcogol", "Magnesium");

M = 3
d = 2

kernelIponechnikova = function(x) (abs(x)<1) * (1 - x^2) *3/4
kernel_vector = function(x1, x2) kernelIponechnikova(x1) * kernelIponechnikova(x2)#sapply(x1, kernelIponechnikova)*sapply(x2, kernelIponechnikova)

kernel_density_gener_vector = function(sample, kernel, smooth) {
  n = length(sample[,1])
  function(x) sum(kernel((x[1] - sample[,1])/smooth[1], (x[2] - sample[,2])/smooth[2])) / (n * prod(smooth))
}

smooth_silverman_advanced_vector = function(sample) {
  s = c(1, 1)
  s[1] = min(sd(sample[,1]), IQR(sample[,1]) / 1.34)
  s[2] = min(sd(sample[,2]), IQR(sample[,2]) / 1.34)
  s * (4/(d+2))^(1/(d+4)) * length(sample[,1])^(-1/(d + 4))
}

SampleVector = cbind(my_data$Alcogol, my_data$Magnesium)
smooth = smooth_silverman_advanced_vector(SampleVector)
density_estimated = kernel_density_gener_vector(SampleVector, kernel_vector, smooth)

step = 100
x  = seq(5, 15, length.out= step)
y  = seq(0, 100, length.out= step)
z = seq(0, 0, length.out= step)
for (i in 1:step) {
  z[i] = density_estimated(c(x[i], y[i]))
}

