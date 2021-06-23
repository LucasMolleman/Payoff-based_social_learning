#include <iostream>
#include <fstream>
#include <random>
#include <vector>
#include <ctime>
#include <chrono>
#include <numeric>
#include <algorithm>

using namespace std;

//random
auto seed = chrono::high_resolution_clock::now().time_since_epoch().count();
mt19937 mt(seed);
uniform_real_distribution<double> Uniform(0, 1);
normal_distribution<double> Normal(0, 1);
uniform_int_distribution<int> Random(0, 1);

//ISSUES
// perhaps implement two traits, 0 and 1? with possibly opposing payoffs? or does this lead to the same thing

/////////////
// GLOBALS //
/////////////

const int numrep = 20;					// number of replications
const int numgen = 1000;				// number of generations
const int outputgen = 10;				// output every so many generations
const int popsize = 1000;				// population size
const double subpopfreq = 0.25;			// frequency of pop belonging to the smaller subpopulation
const int cultvars = 15;				// number of cultural variants
const double cultcorr = 1.0;			// correlation between payoff consequences of variants

const double init_initdelta = 0.5;		// initial average of the initial delta
const double sd_init_initdelta = 0.2;	// initial sd of above
const double init_b = 0.0;				// initial average of b
const double sd_init_b = 0.2;			// initial sd of above

const double baseline_w = 5;			// baseline fitness
const double mutprob = 0.01;			// mutation probability
const double mutsize_initdelta = 0.05;	// stdev mutation stepsize initdelta
const double mutsize_b = 0.01;			// stdev mutation stepsize b

const int sample_SELF = 1;				// number of samples of own payoff (not currently used - always equal to 1)
const int sample_OTHER = 10;			// number of samples of others' payoff 
const double other_noise = 1.0;			// noise in the observation of payoffs of others (stdev)
const double other_bias = 0;			// bias in the the observation of payoffs of others
const double self_noise = 1.0;			// noise in the observation of own payoffs (stdev)
const double self_bias = 0;				// bias in the the observation of own payoffs
double slope = 10;						// learning rate

ofstream output;						// output streams
ofstream output2;

//individual & population
struct indiv
{
	double initdelta;			
	double b;		
	int type;						//two different types, now used only for bookkeeping purposes
	int pay[cultvars];				//payoff consequences for adopting each cultural variant
	int pheno[cultvars];			//are the individuals exhibiting the different variants or not
									//not exhibiting the variant gives payoff 0, whereas exhibiting it can give
									//either 1 or 0
	int phenocopy[cultvars];		//copy of the pheno array so that we can update all individual learning
									//simultaneously and do not have undesired effects of sequential updating
	double delta;				
	//fitness
	double w;
}

pop[popsize];
indiv newpop[popsize];


///////////////
// FUNCTIONS //
///////////////

///////////////////////////////////////////////////////////////////////////////////////////////////initialize the population
void init()
{
	for (int i = 0; i < popsize; ++i)
	{
		(pop + i)->initdelta = init_initdelta + Normal(mt) * sd_init_initdelta;
		if ((pop + i)->initdelta > 1) (pop + i)->initdelta = 1;
		if ((pop + i)->initdelta < 0) (pop + i)->initdelta = 0;
		(pop + i)->delta = (pop + i)->initdelta;
		(pop + i)->b = init_b + Normal(mt) * sd_init_b;
		(pop + i)->w = baseline_w;

		vector<vector<double>> probvec;

		for (int j = 0; j < cultvars / 2; ++j)
		{
			(pop + i)->pheno[j] = Random(mt);
			
			vector<double> myvec;
			if (i < subpopfreq * popsize)
			{
				myvec.push_back(Uniform(mt));
				(pop + i)->type = 0;
			}
			else
			{
				myvec.push_back(cultcorr + Uniform(mt));
				(pop + i)->type = 1;
			}
			myvec.push_back(j);
			probvec.push_back(myvec);
		}
		for (int j = cultvars / 2; j < cultvars; ++j)
		{
			(pop + i)->pheno[j] = Random(mt);

			vector<double> myvec;
			if (i < subpopfreq * popsize)
			{
				myvec.push_back(cultcorr + Uniform(mt));
			}
			else
			{
				myvec.push_back(Uniform(mt));
			}
			myvec.push_back(j);
			probvec.push_back(myvec);
		}
		
		//This sorts the vector by the probabilities in column 1 but then the original indices are stored in col 2
		sort(probvec.begin(), probvec.end(), [](const vector<double>& a, const vector<double>& b)
				{
					return a[0] < b[0];
				});

		for (int j = 0; j < cultvars/2; ++j)
		{
			(pop + i)->pay[int(probvec[j][1])] = -1;
		}
		for (int j = cultvars / 2; j < cultvars; ++j)
		{
			(pop + i)->pay[int(probvec[j][1])] = 1;
		}
	}
}

void learn(int gen)
{
	double sumdelta_1[cultvars];
	double sumdelta_2[cultvars];

	for (int i = 0; i < cultvars; ++i)
	{
		sumdelta_1[i] = 0;
		sumdelta_2[i] = 0;
	}

	for (int i = 0; i < popsize; ++i)
	{
		//current assumption: learn all variants from the same model
		int model[sample_OTHER];
		
		for (int j = 0; j < sample_OTHER; ++j)
		{
			model[j] = i;
		}

		for (int j = 0; j < sample_OTHER; ++j)
		{
			int keepgoing = 1;

			while (keepgoing == 1)
			{
				keepgoing = 0;
				model[j] = floor(Uniform(mt) * popsize);
				if (model[j] == i) keepgoing = 1;

				for (int k = 0; k < j; ++k)
				{
					if (model[j] == model[k]) keepgoing = 1;
				}
			}
		}

		
		for (int cultvar = 0; cultvar < cultvars; ++cultvar)
		{
			double sum_pay_OTHER = 0;

			for (int j = 0; j < sample_OTHER; ++j)
			{
				sum_pay_OTHER += ((pop + model[j])->pay[cultvar] + other_bias + Normal(mt) * other_noise);
			}
			double avg_pay_OTHER = sum_pay_OTHER / sample_OTHER;
			double avg_pay_SELF = ((pop + i)->pay[cultvar] + self_bias + Normal(mt) * self_noise);

			double learnpick = Uniform(mt);
			double adoptprob = 0;
			int soclearn = 0;
			//delta determines prob that the indiv learns from own vs other payoff
			if (learnpick < (pop + i)->delta)
			{
				adoptprob = 1 / (1 + exp(-slope * (avg_pay_OTHER)));
				soclearn = 1;
			}
			else
			{
				adoptprob = 1 / (1 + exp(-slope * (avg_pay_SELF)));
				soclearn = -1;
			}
			//OLD LEARNING RULE
			//double estpay = avg_pay_OTHER * (pop + i)->delta
			//	+ ((pop + i)->pay[cultvar] + self_bias + Normal(mt) * self_noise) * (1 - ((pop + i)->delta));

			double pick = Uniform(mt);

			if (pick < adoptprob)
			{
				(pop + i)->phenocopy[cultvar] = 1;
			}
			else
			{
				(pop + i)->phenocopy[cultvar] = 0;
			}

			//update delta
			if ((pop + i)->pheno[cultvar] != (pop + i)->phenocopy[cultvar])
			{
				//this is multiplied by soclearn so that the individual starts relying MORE on social learning
				//if they socially learned and it gave a good outcome, but LESS if they learned individually
				//and it gave a good outcome (and vice versa for bad outcomes).
				(pop + i)->delta += soclearn * (pop + i)->b * (((pop + i)->phenocopy[cultvar] - (pop + i)->pheno[cultvar]) * (pop + i)->pay[cultvar]);
				if ((pop + i)->delta > 1) (pop + i)->delta = 1;
				if ((pop + i)->delta < 0) (pop + i)->delta = 0;
			}

			if (i < popsize * subpopfreq) sumdelta_1[cultvar] += (pop + i)->delta;
			else sumdelta_2[cultvar] += (pop + i)->delta;
		}
	}
	if ((gen-1) % outputgen == 0)
	{
		for (int k = 0; k < cultvars; ++k)
		{
			output << sumdelta_1[k] / (popsize * subpopfreq) << "\t";
		}
		for (int k = 0; k < (cultvars - 1); ++k)
		{
			output << sumdelta_2[k] / (popsize * (1 - subpopfreq)) << "\t";
		}
		output << sumdelta_2[cultvars - 1] / (popsize * (1 - subpopfreq)) << "\n";
	}
}
 
void reproduce()
{
	//first update phenotypes
	for (int i = 0; i < popsize; ++i)
	{
		for (int j = 0; j < cultvars; ++j)
		{
			(pop + i)->pheno[j] = (pop + i)->phenocopy[j];
		}
	}
	
	double cumfit[popsize];
	pop->w = baseline_w;
	cumfit[0] = pop->w;

	for (int j = 0; j < cultvars; ++j)
	{
		pop->w += pop->pay[j] * pop->pheno[j];
		cumfit[0] += pop->pay[j] * pop->pheno[j];
	}
	
	//try to increase fitness differences by setting mimimal fitness obtained to zero
	double minw = 1000;

	for (int i = 1; i < popsize; ++i)
	{
		(pop + i)->w = baseline_w;
		cumfit[i] = cumfit[i - 1] + (pop + i)->w;


		for (int j = 0; j < cultvars; ++j)
		{
			(pop + i)->w += (pop + i)->pay[j] * (pop + i)->pheno[j];
			cumfit[i] += (pop + i)->pay[j] * (pop + i)->pheno[j];
		}

		if ((pop + i)->w < minw) minw = (pop + i)->w;
	}

	//switch this on if you want to have maximal fitness differences (downside is that the baseline fitness differs
	//by generation
	for (int i = 1; i < popsize; ++i)
	{
		cumfit[i] -= minw * (i + 1);
	}
	
	
	for (int i = 0; i < popsize; ++i)
	{
		double pick = Uniform(mt) * cumfit[popsize-1];

		int min = 0;
		int max = popsize - 1;
		int parent = -1;
		int mid = (int)((max + min) * 0.5);

		while ((max - min) > 1)
		{
			if (cumfit[mid] >= pick) max = mid;
			else min = mid;
			mid = (int)((max + min) * 0.5);
		}
		parent = max;

		//inherit 'initdelta', also make sure it is bound between 0 and 1
		if (Uniform(mt) < mutprob) (newpop + i)->initdelta = (pop + parent)->initdelta + Normal(mt) * mutsize_initdelta;
		else (newpop + i)->initdelta = (pop + parent)->initdelta;
		if ((newpop + i)->initdelta > 1) (newpop + i)->initdelta = 1;
		if ((newpop + i)->initdelta < 0) (newpop + i)->initdelta = 0;
		(newpop + i)->delta = (newpop + i)->initdelta;

		//inherit 'b'
		if (Uniform(mt) < mutprob) (newpop + i)->b = (pop + parent)->b + Normal(mt) * mutsize_b;
		else (newpop + i)->b = (pop + parent)->b;

		(newpop + i)->w = 0.0;
	}

	//current assumption: individuals do not inherit cultural traits or payoffs but they are again randomly generated
	//however this takes uberlong to run every gen. A fast workaround solution is to just copy the traits/payoffs from the previous
	//generation (but not inherit them, so simply individual 1 just gets the traits of individual 1 in the prev gen and so on).
	//I know implement this below.
	/*
	vector<vector<double>> probvec;
	for (int i = 0; i < popsize; ++i)
	{
		for (int j = 0; j < cultvars / 2; ++j)
		{
			(pop + i)->pheno[j] = Random(mt);

			vector<double> myvec;
			if (i < subpopfreq * popsize)
			{
				myvec.push_back(Uniform(mt));
			}
			else
			{
				myvec.push_back(cultcorr + Uniform(mt));
			}
			myvec.push_back(j);
			probvec.push_back(myvec);
		}
		for (int j = cultvars / 2; j < cultvars; ++j)
		{
			(pop + i)->pheno[j] = Random(mt);

			vector<double> myvec;
			if (i < subpopfreq * popsize)
			{
				myvec.push_back(cultcorr + Uniform(mt));
			}
			else
			{
				myvec.push_back(Uniform(mt));
			}
			myvec.push_back(j);
			probvec.push_back(myvec);
		}

		//This sorts the vector by the probabilities in column 1 but then the original indices are stored in col 2
		sort(probvec.begin(), probvec.end(), [](const vector<double>& a, const vector<double>& b)
		{
			return a[0] < b[0];
		});

		for (int j = 0; j < cultvars / 2; ++j)
		{
			(pop + i)->pay[int(probvec[j][1])] = -1;
		}
		for (int j = cultvars / 2; j < cultvars; ++j)
		{
			(pop + i)->pay[int(probvec[j][1])] = 1;
		}
	}*/

	for (int i = 0; i < popsize; ++i)
	{
		for (int j = 0; j < cultvars; ++j)
		{
			(newpop + i)->pheno[j] = Random(mt);
			(newpop + i)->pay[j] = (pop + i)->pay[j];
		}
		if (i < subpopfreq * popsize) (newpop + i)->type = 0;
		else (newpop + i)->type = 1;

	}
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////statistics
void write()
{
	double sum_initdelta = 0;
	double av_initdelta = 0;
	double ss_initdelta = 0;
	double sd_initdelta = 0;
	double sum_b = 0;
	double av_b = 0;
	double ss_b = 0;
	double sd_b = 0;

	double sumtype0_w = 0;
	double avgtype0_w = 0;
	double sumtype1_w = 0;
	double avgtype1_w = 0;

	double cultsum[cultvars] = { 0,0,0,0,0,0,0,0,0,0 };
	double cultavg[cultvars] = { 0,0,0,0,0,0,0,0,0,0 };
	double cultss[cultvars] = { 0,0,0,0,0,0,0,0,0,0 };
	double cultvar[cultvars] = { 0,0,0,0,0,0,0,0,0,0 };
	double meancultvar = 0;


	for (int i = 0; i < popsize; ++i)
	{
		sum_initdelta += (pop + i)->initdelta;
		sum_b += (pop + i)->b;
		if((pop + i)->type == 0) sumtype0_w += (pop + i)->w;
		if((pop + i)->type == 1) sumtype1_w += (pop + i)->w;
	}

	av_initdelta += sum_initdelta / popsize;
	av_b += sum_b / popsize;
	avgtype0_w += sumtype0_w / (popsize * subpopfreq);
	avgtype1_w += sumtype1_w / (popsize * (1-subpopfreq));

	for (int i = 0; i < popsize; ++i)
	{
		ss_initdelta += ((pop + i)->initdelta - av_initdelta) * ((pop + i)->initdelta - av_initdelta);
		ss_b += ((pop + i)->b - av_b) * ((pop + i)->b - av_b);
	}

	sd_initdelta = sqrt(ss_initdelta / popsize);
	sd_b = sqrt(ss_b / popsize);

	for (int i = 0; i < cultvars; ++i)
	{
		for (int j = 0; j < popsize; ++j)
		{
			cultsum[i] += (pop + j)->pheno[i];
		}
		cultavg[i] = cultsum[i] / popsize;

		for (int j = 0; j < popsize; ++j)
		{
			cultss[i] += ((pop + j)->pheno[i] - cultavg[i]) * ((pop + j)->pheno[i] - cultavg[i]);
		}
		cultvar[i] = sqrt(cultss[i] / popsize);
		meancultvar += cultvar[i];
	}

	//output stats
	output << av_initdelta << "\t" << sd_initdelta << "\t" << av_b << "\t" << sd_b << "\t" << avgtype0_w << "\t" << avgtype1_w << "\t" << meancultvar << "\t";
}

void writeheaders()
{
	output << "rep\t" << "gen\t" << "av_initdelta\t" << "sd_initdelta\t" << "av_b\t" << "sd_b\t" << "avgtype0_w\t" << "avgtype1_w\t" << "cultvar\t" 
		<< "delta_0_1\t" << "delta_0_2\t" << "delta_0_3\t" << "delta_0_4\t" << "delta_0_5\t" << "delta_0_6\t" << "delta_0_7\t" << "delta_0_8\t" << "delta_0_9\t" << "delta_0_10\t"
		<< "delta_1_1\t" << "delta_1_2\t" << "delta_1_3\t" << "delta_1_4\t" << "delta_1_5\t" << "delta_1_6\t" << "delta_1_7\t" << "delta_1_8\t" << "delta_1_9\t" << "delta_1_10\n";
}

int main()
{
	output.open("output.txt");
	writeheaders();

	for (int i = 0; i < numrep; ++i)
	{
		init();

		for (int j = 0; j < numgen; ++j)
		{
			learn(j);
			reproduce();
			
			if (j % outputgen == 0)
			{
				cout << "rep " << i << "\tgen " << j << "\n";
				output << i << "\t" << j << "\t";
				write();
			}

			for (int i = 0; i < popsize; ++i)
			{
				pop[i] = newpop[i];
			}
		}
	}
	output.close();
	output2.close();
}