dir1 <- "C:/Users/pvdbe/PycharmProjects/first_project"
setwd(dir1)

d <- read.table("outfile.txt", header=TRUE)

plot(0,0, type="n", xlim = c(-1,1), ylim=c(0,1), xlab="payoff for type B ", ylab="frequency type A", main="payoff type A = -0.1")

cols = grey.colors(101)

for(i in unique(d$payoff_advantage_A))
{
  for(j in unique(d$frequency_A))
  {
    val = subset(d$optimal_delta_A, d$payoff_advantage_A > i-0.04 & d$payoff_advantage_A < i+0.04 & d$frequency_A > j - 0.04 & d$frequency_A < j + 0.04)
    rect(i - 0.05, j-0.05, i + 0.05, j + 0.05, col = cols[round(100*((1-val)+0.01))], border = FALSE)
  }
}