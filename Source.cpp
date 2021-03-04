#include <iostream>
#include <fstream>
#include <random>
#include <vector>
#include <ctime>
#include <chrono>

using namespace std;

//random
auto seed = chrono::high_resolution_clock::now().time_since_epoch().count();
mt19937 mt(seed);
uniform_real_distribution<double> Uniform(0, 1);
normal_distribution<double> Normal(0, 1);
uniform_int_distribution<int> Random(0, 1);

/////////////
// GLOBALS //
/////////////

const int reps = 100000;  // individual learning instances run per parameter combo

double payA = 1;  // payoff of the behaviour for a type A individual
double sd_payA = 1; // standard deviation in the payoff for a type A individual

const int sample_SELF = 1;  // number of samples of own payoff
const int sample_OTHER = 1;  // number of samples of others' payoff
double slope = 1; // learning rate

double min_payB = -1; // range of average payoffs for type B to consider
double max_payB = 3;
const int num_payB = 21;
double payB[num_payB];

double min_sd_payB = 1; // range of standard deviations of the payoffs of type B to consider
double max_sd_payB = 3;
const int num_sd_payB = 21;
double sd_payB[num_sd_payB];

double min_freqA = 0.0; // range of frequencies for type A to consider
double max_freqA = 1.0;
const int num_freqA = 5;
double freqA[num_freqA];

double min_delta = 0.0; // range of delta to consider
double max_delta = 1.0;
const int num_delta = 51;
double delta[num_delta];

ofstream output; // output stream

int main()
{
	output.open("output.txt");
	output << "payoff_B\tsd_payoff_B\tfrequency_A\toptimal_delta_A\n";

	for (int i = 0; i < num_payB; ++i)
	{
		payB[i] = min_payB + ((max_payB - min_payB) / (num_payB - 1)) * i;
	}

	for (int i = 0; i < num_sd_payB; ++i)
	{
		sd_payB[i] = min_sd_payB + ((max_sd_payB - min_sd_payB) / (num_sd_payB - 1)) * i;
	}

	for (int i = 0; i < num_freqA; ++i)
	{
		freqA[i] = min_freqA + ((max_freqA - min_freqA) / (num_freqA - 1)) * i;
	}

	for (int i = 0; i < num_delta; ++i)
	{
		delta[i] = min_delta + ((max_delta - min_delta) / (num_delta - 1)) * i;
	}

	for (int i = 0; i < num_payB; ++i)
	{
		cout << "payB: " << payB[i] << "\n";

		for (int j = 0; j < num_sd_payB; ++j)
		{
			for (int k = 0; k < num_freqA; ++k)
			{
				double delta_probA[num_delta];
				for (int d = 0; d < num_delta; ++d)
				{
					double probA = 0;
					for (int rep = 0; rep < reps; rep++)
					{
						double own_pay = 0;
						double oth_pay = 0;

						/* the code below is for if you want to sample more than one time for either self or other. I simplify this now to save time.
						
						for (int own = 0; own < sample_SELF; own++)
						{
							own_pay += Normal(mt) * sd_payA + payA;  // draw own experiences
						}
						own_pay /= sample_SELF; // average own experiences

						for (int oth = 0; oth < sample_OTHER; oth++)
						{
							if (Uniform(mt) < freqA[k])
							{
								oth_pay += Normal(mt) * sd_payA + payA;  // observe a type A individual
							}
							else
							{
								oth_pay += Normal(mt) * sd_payB[j] + payB[i];  // observe a type B individual
							}
						}
						oth_pay /= sample_OTHER; // average observation of others*/

						own_pay += Normal(mt) * sd_payA + payA;

						if (Uniform(mt) < freqA[k])
						{
							oth_pay += Normal(mt) * sd_payA + payA;  // observe a type A individual
						}
						else
						{
							oth_pay += Normal(mt) * sd_payB[j] + payB[i];  // observe a type B individual
						}

						probA += 1 / (1 + exp(-slope * ((1 - delta[d]) * own_pay + delta[d] * oth_pay))); //the probability of adapting payoff A
					}
					delta_probA[d] = probA / reps; //average intergrated payoff over all replicated for this parameter combination
				}

				//now find the best delta
				double max_probA = -1000;
				double min_probA = 1000;
				double best_delta = -1;

				for (int d = 0; d < num_delta; ++d)
				{
					if (payA > 0) // in this case find the beta that gives the highest probability of adopting A
					{
						if (delta_probA[d] > max_probA)
						{
							max_probA = delta_probA[d];
							best_delta = delta[d];
						}
					}
					else // otherwise find the beta that gives the lowest probability of adopting A
					{
						if (delta_probA[d] < min_probA)
						{
							min_probA = delta_probA[d];
							best_delta = delta[d];
						}
					}
				}
				output << payB[i] << "\t" << sd_payB[j] << "\t" << freqA[k] << "\t" << best_delta << "\n"; // write data to file
			}
		}
	}

	output.close();
}