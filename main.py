import numpy as np
import math
import statistics as stat
import sys

# This is a simulation that finds the optimal degree of payoff-based learning dependent on a few parameters.
# There are two types of individuals in the population: type A and type B. We focus on an individual of type A that is
# deciding whether to adopt a certain behaviour. The behaviour may have different payoff consequences for the different
# types. In this simulation I systematically check which is the optimal weighing of social information, for different
# payoffs for type B (pay_B_range - the payoff for type A is held constant) and for different frequencies of both types
# in the population (freq_A_range).
#
# The individual makes a number of observations of their own payoff and the payoffs of others in the population that
# are doing the behaviour, and then integrates these observations through a weighing to get to an estimate of the
# overall payoff of the behaviour (this occurs through the weighing with delta). Based on this, they then adopt the
# behaviour with a probability that is given by a sigmoidal learning function that takes this payoff as an input and
# that has a fixed slope.
#
# We make a few basic assumptions:
# * individuals have a number of experiences with the behaviour themselves, and also do a number of observations of
# others' experiences. For now, I assume there are more observations (five) than personal experiences (one).
# * for now, the assumption is that the standard deviation around the payoffs of both types is the same (0.1). Also,
# I have not yet implemented the possibility that observations of others are more uncertain than own observations
#
# What I quantify is the probability of adopting the behaviour for all possible values of delta. If pay_A < 0,
# then the lower the probability of adoption, the better. Hence I search for the delta that has the lowest adoption
# probability. If pay_A > 0, it is the other way around.


reps = 1000  # individual learning instances run per parameter combo

pay_A = 0.1  # payoff of the behaviour for a type A individual
sd_pay_A = 0.1  # standard deviation in the payoff for a type A individual
sd_pay_B = 0.1  # standard deviation in the payoff for a type B individual

slope = 10  # slope of the sigmoidal learning function
sample_SELF = 1  # number of samples of own payoff
sample_OTHER = 5  # number of samples of others' payoff

pay_B_range = np.linspace(-0.25, 0.25, 21)  # range of payoffs for type B to consider
freq_A_range = np.linspace(0, 1, 21)  # range of frequencies of type A in the population to consider
delta_range = np.linspace(0, 1, 101)  # range of values of delta to consider

mat = np.empty((len(pay_B_range), len(freq_A_range)))  # output matrix

for i in range(0, len(pay_B_range)):  # check a range of payoffs of type B
    for j in range(0, len(freq_A_range)):  # frequency of type A
        delta_A_prob = np.empty(len(delta_range)) # vector that will save average probs of adopting for different deltas
        for k in range(0, len(delta_range)):  # weighting of social information
            A_prob = np.empty((len(range(0, reps))))  # vector that will contain the probs of adopting for reps of this delta
            for rep in range(0, reps):
                own_pay = np.random.normal(pay_A, sd_pay_A, sample_SELF)  # draw own experiences
                oth_A = np.random.binomial(sample_OTHER, freq_A_range[j])  # determine how many A's in observations
                oth_pay = np.concatenate((np.random.normal(pay_A, sd_pay_A, oth_A), np.random.normal(pay_B_range[i], sd_pay_B, sample_OTHER-oth_A))) # draw others' experiences

                A_prob[rep] = 1/(1 + math.e**(-slope*(stat.mean(own_pay) + delta_range[k] * stat.mean(oth_pay))))
            delta_A_prob[k] = stat.mean(A_prob)
        if pay_A > 0:  # in this case A is the better behaviour, so the highest prob to adopt is the best
            maxval = max(delta_A_prob) # get the highest prob of adoption from the vector
            mat[i][j] = delta_range[np.where(delta_A_prob == maxval)] # find (and save) the corresponding delta
        else:
            minval = min(delta_A_prob)
            mat[i][j] = delta_range[np.where(delta_A_prob == minval)]
    print(i, "\n")

with open('outfile.txt', 'w') as f:
    sys.stdout = f
    print("payoff_B\tfrequency_A\toptimal_delta_A\n")
    for i in range(0, len(pay_B_range)):
        for j in range(0, len(freq_A_range)):
            print(pay_B_range[i], "\t", freq_A_range[j], "\t", mat[i][j], "\n")










