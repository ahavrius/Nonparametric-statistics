X = c(13, -5, 10, 5, -3, -1, 0, -2, 20, 2)
Y = c(-2, 16, 6, 0, 7, 0, 20, 17, 13, 20)

amount_bs = 1000
n_sample = length(X)
conf.level = 0.95
c_value = qt(1 - conf.level/2, df = n_sample - 2)

rank_unique = function(sample){
  index = order(sample, decreasing = TRUE)
  result = c(1:length(sample))
  result[index]  = result
  result
}

rank = function(sample){
  result = rank_unique(sample)
  for (i in 1:length(sample))
    result[i] = sum(result[sample == sample[i]]) / sum(sample == sample[i])
  result
}

#bootstrap
bootstrap = function(x, y, amount, func){
  n = length(x)
  vector_bs = c()
  for (i in 1:amount) {
    x_bs = sample(x, n, replace=T)
    y_bs = sample(y, n, replace=T)
    vector_bs = c(vector_bs, func(x_bs, y_bs))
  }
  vector_bs
}

cor_spearman = function(x, y)
{
  x_rank = rank(x)
  y_rank = rank(y)
  x_rank_mean = mean(x_rank)
  y_rank_mean = mean(y_rank)
  sum((x_rank - x_rank_mean)*(y_rank - y_rank_mean)) /
    sqrt(sum((x_rank - x_rank_mean)^2) * sum((y_rank - y_rank_mean)^2))
}
  
print(cor.test(X, Y, method = 'spearman'))
print(cor_spearman(X,Y))

spearman_bs = bootstrap(X, Y, amount_bs, cor_spearman)
#t_stat = sqrt((n_sample-2)*spearman_bs^2 / (1 - spearman_bs^2))
#p_value_bs = sum(t_stat <= c_value) / amount_bs

hist(spearman_bs, breaks=10, col="blue", xlab="Coeficient of Spearman", main="Bootstrap")
