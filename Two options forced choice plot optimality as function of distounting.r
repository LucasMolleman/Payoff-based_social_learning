# base payoffs 
basePayX<-0;
basePayY<-1;

basePayoffs<-c(basePayX, basePayY)

# noise associated with realised payoffs
errorSD<-1;

# simulation parameters
numberOfRounds<-10;
numberOfIndividuals<-1000;

# proportion of population with 'type A' (for type B, the base payoffs of X and Y are reversed)
proportionTypeA<-0.9;

# choice sensitivity (for high values, small differences in attraction scores of options already strongly impact the chance of choosing the option with the higher value)
temp<- 3;
# learning rate (for updating the attraction scores upon experiencing (or observing) a payoff)
lambda<-0.2;

# discounting factor for social information (1=no discounting; 0=ignore social information)
delta<-1;

# initial attraction scores of both options
Ax<-0.5;
Ay<-0.5;


outcomeMatrix<-matrix(nrow=0, ncol=4)


### global plotting parameters
par(mfrow=c(3,3), mar=c(4,4,2,2))

# explore 3 relative proportions of type A
for (proportionTypeA in c(0.5, 0.51, 0.75, 0.9, 0.99)){
	# explore 3 levels of discounting
	for (delta in 0:10/10){

		# define a list of 'types' defining each individual
		typeList<-rep(2, numberOfIndividuals)
		typeList[1:round(numberOfIndividuals * proportionTypeA)]<-1

		### BOOKKEEPING ###
		# matrices of attraction scores (1 column per individual)
		Ax_m<-matrix(NA, nrow=(1+numberOfRounds), ncol=numberOfIndividuals)
		Ay_m<-matrix(NA, nrow=(1+numberOfRounds), ncol=numberOfIndividuals)

		choice_m<-matrix(NA, nrow=(1+numberOfRounds), ncol=numberOfIndividuals)
		payoff_m<-matrix(NA, nrow=(1+numberOfRounds), ncol=numberOfIndividuals)

		totalPay<-rep(0, numberOfIndividuals);

		# initial weights
		Ax_m[1,]<-Ax
		Ay_m[1,]<-Ay


		# loop over rounds
		for (i in 1:numberOfRounds){
			
			### ENVIRONMENT SWITCHES
	#		if (i%%25==0) basePayoffs<-rev(basePayoffs)

			### STEP 1: all individuals make a choice and update attraction scores for X and Y
			for (ind in 1:numberOfIndividuals){
			
				# extract the weights for the two options
				w<-c(Ax_m[i, ind], Ay_m[i, ind])
			
				# make choice
				Z<- temp * (w[1] - w[2])
				p<- 1/(1+exp(-Z))
				
				choice<-ifelse(runif(1) < p,1,2)
				
				# record choice
				choice_m[i, ind]<-choice;
				
				# determine payoffs, based on individual's type
				pays<-basePayoffs
				if (typeList[ind]==2) pays<-rev(basePayoffs)
				realisedPayoff<-rnorm(1, mean=pays[choice], sd=errorSD)
				
				# record payoffs
				payoff_m[i, ind]<-realisedPayoff
				
				totalPay[ind]<-totalPay[ind] + realisedPayoff;
				
				#### UPDATE ATTRACTION SCORES
				
				# learn individually
				PE<- realisedPayoff - w[choice];
				w[choice]<-w[choice] + lambda * PE;

				# store weights in vectors
				Ax_m[i+1, ind]<-w[1]
				Ay_m[i+1, ind]<-w[2]
			}
			
			### STEP 2: all individuals sample a peer and learn from their most recent behaviour + payoffs	
				# learn socially
				
			for (ind in 1:numberOfIndividuals){	
				IDother<-ind;
				while (IDother==ind) IDother<-sample(1:numberOfIndividuals,1)
				otherChoice<-choice_m[i, IDother]
				otherPayoff<-payoff_m[i, IDother] #+ rnorm(1, mean=0, sd=1)  # add more noise to social payoffs

				#### UPDATE ATTRACTION SCORES
				# extract the weights for the two options again (already updated, so stored in row i+1; see above in individual learning)
				w<-c(Ax_m[i+1, ind], Ay_m[i+1, ind])
			
				# learn socially - DISCOUNTED by factor 'delta'
				PE<- otherPayoff - w[otherChoice];
				w[otherChoice]<-w[otherChoice] + delta * lambda * PE;

				# store weights in vectors
				Ax_m[i+1, ind]<-w[1]
				Ay_m[i+1, ind]<-w[2]		
				
			}
		}

		### PLOTTING ###
		# plot simulation outcome
#		par(las=1, lend=1, cex.lab=1.5, cex.axis=1.5)
#		plot(0, type='n', ylim=c(-0.5, 1.5), xlim=c(0,numberOfRounds), xlab='Round number', ylab='Attraction scores')

		for (ind in 1:numberOfIndividuals){
			col1<-'grey90'; col2<-'grey90'; lwd1<-1;

	#		lines(0:numberOfRounds, Ax_m[,ind], col=col1, lwd=lwd1)	
	#		lines(0:numberOfRounds, Ay_m[,ind], col=col2, lwd=lwd1)
		}


		proportionOptimals<-c(NA,NA)
		for (types in 1:2){
		
			Ax_m_thisType<-matrix(nrow=nrow(Ax_m), ncol=0)
			Ay_m_thisType<-matrix(nrow=nrow(Ay_m), ncol=0)
			choice_m_thisType<-matrix(nrow=nrow(choice_m), ncol=0)
			payoff_m_thisType<-matrix(nrow=nrow(payoff_m), ncol=0)
			for (k in 1:numberOfIndividuals){
				if (typeList[k]==types) {
					Ax_m_thisType<-cbind(Ax_m_thisType, Ax_m[,k])	
					Ay_m_thisType<-cbind(Ay_m_thisType, Ay_m[,k])
					choice_m_thisType<-cbind(choice_m_thisType, choice_m[,k])
					payoff_m_thisType<-cbind(payoff_m_thisType, payoff_m[,k])
				}
			}
			
		
			mAx<-rep(NA, (numberOfRounds+1))
			mAy<-rep(NA, (numberOfRounds+1))
			proportionY<-rep(NA, numberOfRounds+1)

			for (i in 0:numberOfRounds){
				mAx[i+1]<-mean(Ax_m_thisType[i+1,])
				mAy[i+1]<-mean(Ay_m_thisType[i+1,])
				proportionY[i+1]<-mean(choice_m_thisType[i+1,]-1)
			}
			
			meanProportionY<-mean(proportionY, na.rm=TRUE)
			proportionOptimal<-ifelse(types==1, meanProportionY, 1-meanProportionY)
			
			proportionOptimals[types]<-proportionOptimal;
			
			
#			lines(0:numberOfRounds, mAx, col='green', lwd=3, lty=types)
#			lines(0:numberOfRounds, mAy, col='red', lwd=3, lty=types)
#			lines(0:numberOfRounds, proportionY, col='black', lwd=3, lty=types)
			
			rndOpt<-sprintf("%.2f", proportionOptimal)
			if (types==1) text(numberOfRounds*0.25,1.2, bquote(Pi['A']==.(rndOpt)), cex=1.5)
			if (types==2) text(numberOfRounds*0.75,1.2, bquote(Pi['B']==.(rndOpt)), cex=1.5)
			
			
		}
		arrows(-1,0,100,0, lty=2, code=0)
		arrows(-1,1,100,1, lty=2, code=0)

		text(numberOfRounds/2,1.4, paste('Discount factor delta=', delta, sep=''))
		text(numberOfRounds/2,1.5, paste('Proportion type A=', proportionTypeA, sep=''))
		
		
		### save outcomes in the big matrix
		outcome<-c(proportionTypeA, delta, proportionOptimals)
		outcomeMatrix<-rbind(outcomeMatrix, outcome)
	}
}

outcomeMatrix<-data.frame(outcomeMatrix)
names(outcomeMatrix)<-c('majority', 'delta', 'payMajority', 'payMinority')
row.names(outcomeMatrix)<-c()
outcomeMatrix
#dev.off()
par(mfrow=c(1,5), las=1, cex.lab=1.5, cex.axis=1.5)
for (m in unique(outcomeMatrix$majority)){
	plot(0, type='n', xlim=c(0,1), ylim=c(0.4, 1), xlab='Weight of social info (Delta)', ylab='Optimality', main=paste('Frac type A:', m))
	
	b<-subset(outcomeMatrix, outcomeMatrix$majority==m)
	for (i in 1:nrow(b)){
		points(b$delta[i], b$payMajority[i], pch=15, cex=1.5, col='green')
		points(b$delta[i], b$payMinority[i], pch=16, cex=1.5, col='red')
	}
	arrows(-1, 0.5, 2, 0.5, code=0, lty=2)
	arrows(-1, 1, 2, 1, code=0, lty=2)
}