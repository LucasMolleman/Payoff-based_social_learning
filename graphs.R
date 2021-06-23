library(scales)
library(lme4)
library(splines)
library(effects)
library(car)
library(lmerTest)
library(cAIC4)
library(export)
library(emmeans)

dir1 <- "C:/Users/pvdbe/Google Drive/PROJECT with Lucas/SLDEV/SLDEV/"
setwd(dir1)

d <- read.table("output.txt", header = TRUE)
b_scale <- 0.5
myal = 0.1

par(mfrow=c(1,3))
cols <- rainbow(100)

### GRAPHSET 1

plot(0:99*10, subset(d$av_initdelta, d$rep == 0), type = "l", ylim=c(0,1), lwd=3, col=alpha("black", myal), ylab="initial weighing of social info (init_delta)", xlab="generations")

for(i in 1:99)
{
  lines(0:99*10, subset(d$av_initdelta, d$rep == i), lwd=3, col=alpha("black", myal)) # , col=cols[i])
}

plot(0:99*10, subset(d$av_b, d$rep == 0), type = "l", ylim=c(-b_scale,b_scale), lwd=3, col=alpha("black", myal), ylab="reinforcement of social learning from experience (b)", xlab="generations")

for(i in 1:99)
{
  lines(0:99*10, subset(d$av_b, d$rep == i), lwd=3, col=alpha("black", myal)) #, col=cols[i])
}

lines(0:99*10, rep(0,100), col="red")

plot(subset(d$av_b, d$rep == 0), subset(d$av_initdelta, d$rep == 0), type = "l", xlim = c(-b_scale, b_scale), ylim=c(0,1), lwd=3, col=alpha("black", myal), ylab="initial weighing of social info (init_delta)", xlab="reinforcement of social learning from experience (b)")

for(i in 1:99)
{
  lines(subset(d$av_b, d$rep == i), subset(d$av_initdelta, d$rep == i), lwd=3, col=alpha("black", myal)) #, col=cols[i])
}

lines(rep(0,20), 0:19*10,  col="red")



### GRAPHSET 2

par(mfrow = c(1,2))
ylim = 1

#the following graphs are for the developmental trajectories averaged over all individuals
#of the rare type (left) and the common type (right) at the end of the simulation (generation 990)

plot(0:9, subset(d[,10:19]-d$av_initdelta, d$rep == 0 & d$gen == 990), type = "l", ylim=c(-ylim,ylim), lwd=3, col=alpha("firebrick1", myal), xlab="developmental time", ylab="change in delta through development (initial = 0), rare type")

for(i in 1:99)
{
  lines(0:9, subset(d[,10:19]-d$av_initdelta, d$rep == i & d$gen == 990), type = "l", ylim=c(0,1), lwd=3, col=alpha("firebrick1", myal))
}
lines(0:9, rep(0,10))

lines(0:9, colMeans(subset(d[,10:19], d$gen == 990))-mean(subset(d$av_initdelta, d$gen == 990)), type = "l", ylim=c(0,1), lwd=3, col="black")


plot(0:9, subset(d[,20:29]-d$av_initdelta, d$rep == 0 & d$gen == 990), type = "l", ylim=c(-ylim,ylim), lwd=3, col=alpha("dodgerblue", myal), , xlab="developmental time", ylab="change in delta through development (initial = 0), common type"))

for(i in 1:99)
{
  lines(0:9, subset(d[,20:29]-d$av_initdelta, d$rep == i & d$gen == 990), type = "l", ylim=c(0,1), lwd=3, col=alpha("dodgerblue", myal))
}
lines(0:9, rep(0,10))

lines(0:9, colMeans(subset(d[,20:29], d$gen == 990))-mean(subset(d$av_initdelta, d$gen == 990)), type = "l", ylim=c(0,1), lwd=3, col="black")
