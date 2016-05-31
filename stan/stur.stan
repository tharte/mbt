// Stochastic Unit Root ("STUR")

data {
	int<lower=1> T;
	vector[T]    y;
}

parameters {
    real sigma_1;
    real sigma_2;

    vector[T] omega;
    real mu_omega;
    real phi;
}

transformed parameters {
    vector[T] rho;
	for (t in 1:T) {
		rho[t]<- exp(omega[t]);
	}
}

model {
    //
	//  priors
    //

	mu_omega ~ normal(0, 1);
	phi ~ normal(0,1);
	sigma_1 ~ inv_gamma(0.001, 0.001);
	sigma_2 ~ inv_gamma(0.001, 0.001);

    // initial conditions as latent data: diffuse prior
	omega[1] ~ normal(0, 1);

	for (t in 2:T) {
		y[t] ~ normal(rho[t]*y[t-1], sigma_1);
		omega[t] ~ normal(mu_omega + phi*(omega[t-1]-mu_omega), sigma_2);
	}
}
