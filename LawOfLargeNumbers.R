experiment_visuazilation = function(distribution,samples,population_mean){
  n=length(samples)
  cumulative_means <- cumsum(samples)/(1:n)
  library(ggplot2)
  require(gridExtra)
  j <- ggplot(data.frame(x = distribution[,1], y = distribution[,2]), aes(x = x, y = y))
  j <- j + geom_hline(yintercept = population_mean) + geom_point()
  j <- j + labs(x = "value", y = "probality")
  
  h <- ggplot(data.frame(x = 1:n, y = samples), aes(x = x, y = y))
  h <- h + geom_hline(yintercept = population_mean) + geom_point()
  h <- h + labs(x = "Number of obs", y = "observation")

  
  g <- ggplot(data.frame(x = 1:n, y = cumulative_means), aes(x = x, y = y))
  g <- g + geom_hline(yintercept = population_mean) + geom_line(size = 2)
  g <- g + labs(x = "Number of obs", y = "Cumulative mean")
  grid.arrange(j,h, g, ncol=3)
}
distribution = data.frame(seq(from =-5,to = 5,by=0.01),dnorm(seq(from =-5,to = 5,by=0.01)))
experiment_visuazilation(distribution,rnorm(1000),0)
distribution = data.frame(c(0,1),c(0.5,0.5))
experiment_visuazilation(distribution,sample(0:1, 10000, replace = TRUE),0.5)
