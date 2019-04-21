N_houses  = 60 * 8
N_test = N_houses/2

col_residents = 3
col_subsidies = 9
col_income = 11
col_rent = 19
DATA = read.table("/home/asta/Nonparametric-statistics/database.csv", header = FALSE, sep = ",")

get_data = function(amount){
  index_selected  = sample(1:N_houses, amount, replace=F)
  data_selected = DATA[index_selected, c(1, 2, col_residents, col_subsidies)]
  names(data_selected) = c("block", "unit", "residents", "subsidies")
  data_selected
}
data_test = get_data(N_test)

#estimation by ratio
data_real = DATA[, c(1, 2, col_residents)]
ratio = sum(data_test[,4])/sum(data_test[,3])
estim_ratio = ratio * sum(data_real[,3])

