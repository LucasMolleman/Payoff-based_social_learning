dir1 <- "C:/Users/pvdbe/Google Drive/PROJECT with Lucas/SLOP/SLOP"
setwd(dir1)

d <- read.table("output.txt", header=TRUE)

cols = rainbow(150)
par(mfrow=c(1,length(unique(d$frequency_A))))

step_payB = 0.2
step_sd_payB = 0.1

for(i in unique(d$frequency_A))
{
  plot(0,0, type="n", xlim = c(-1,3), ylim=c(1,3), xlab="payoff for type B", ylab="standard deviation payoff for type B", main=paste("payoff type A = 1 (sd 1)\nfrequency type A = ", i, sep=""))
  
  for(j in unique(d$payoff_B))
  {
    for(k in unique(d$sd_payoff_B))
    {
      val = subset(d$optimal_delta_A, d$frequency_A == i & d$payoff_B == j & d$sd_payoff_B == k)
      rect(j - step_payB/2, k - step_sd_payB/2, j + step_payB/2, k + step_sd_payB/2, col = cols[round(100*((1-val)+0.01))], border = FALSE)
    }
  }
}
