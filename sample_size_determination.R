N_houses  = 60 * 8
N_test = 10

col_residents = 3
col_subsidies = 9
col_income = 11
col_rent = 19
DATA = read.table("/home/asta/Nonparametric-statistics/database.csv", header = FALSE, sep = ",")
#DATA = read.table("/Users/ahavrius/sampling/database.csv", header = FALSE, sep = ",")

alpha = 0.05
e = 0.1
x = qnorm(1 - alpha / 2)

get_data = function(amount){
  index_selected  = sample(1:N_houses, amount, replace=F)
  data_selected = DATA[index_selected, c(1, 2, col_residents, col_income, col_subsidies, col_rent)]
  names(data_selected) = c("block", "unit", "residents", "income", "subsidies", "rent")
  data_selected
}

Value = function(data_sample){
  N_sample = nrow(data_sample)
  #Estimation of total
  t_subsidies = N_houses * sum(data_test_selected$subsidies) / N_sample
  #Estimation of Mean
  m_income = mean(data_test_selected$income)
  #Estimation of Proportion
  p_rent = sum(data_test_selected$rent != '.') / N_sample
  c(t_subsidies, m_income, p_rent)
}

Variance = function(data_sample){
  N_sample = nrow(data_sample)
  # Estimation of total subsidies variance
  var_t_subsidies = N_houses^2 * (1 - N_sample/N_houses) * sd(data_test_selected$subsidies)^2 / N_sample
  # Estimation of mean variance  
  var_m_income = (1 - N_sample/N_houses) * sd(data_test_selected$income)^2 / N_sample
  # Estimation of proportion variance
  p_rent = sum(data_test_selected$rent != '.') / N_sample
  var_p_rent = p_rent * (1 - p_rent) * (1 - N_sample/N_houses) / (N_sample - 1)
  c(var_t_subsidies, var_m_income, var_p_rent)
}

data_test_selected = get_data(N_test)
values_test = Value(data_test_selected)
variances_test = Variance(data_test_selected)
#Confidence N of samples

cv = sqrt(variances_test) / values_test
N_conf = ceiling(x^2 * cv^2 / (e^2 + cv^2 * x^2 / N_houses))
N_conf = max(N_conf)

data_conf_selected = get_data(N_conf)
values_conf = Value(data_conf_selected)
variances_conf = Variance(data_conf_selected)
val1 = values_conf - x * sqrt(variances_conf * (1/N_conf - 1/N_houses))
val2 = values_conf + x * sqrt(variances_conf * (1/N_conf - 1/N_houses))
