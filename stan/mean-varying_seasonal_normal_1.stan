functions {
    real sum_constraint(vector x, int t, int Lambda) {
		int t_start;
		int t_end;
        real s;
		s<- 0;

		// wind back from: (t-1)  to:  t-(Lambda-1)
		t_start<- t-1;
		t_end<-   t-(Lambda-1);

		for (i in t_start:t_end) {
		    s<- s - x[i];
		}

        return s;
	}
}

data {
    int<lower=12> T;
    vector[T] y;
    int<lower=12,upper=12> Lambda;
}

parameters {
    vector[T] theta_1;
    vector[T] s;
    real<lower=0> sigma_1;
    real<lower=0> sigma_2;
    real<lower=0> sigma_3;
}

/*
transformed parameters {
    vector[T] ss;

    for (t in 1:(Lambda-1)) {
        ss[t]<- 0;
    }

	for (t in Lambda:T) {
	    ss[t]<- 0;
		for (lambda in 1:(Lambda-1)) {
			ss[t]<- ss[t] - s[t-lambda];
		}
	}
}
*/

model {
    for (t in 1:T) {
        y[t] ~ normal(theta_1[t] + s[t], sigma_1);
    }

    for (t in 2:T) {
        theta_1[t] ~ normal(theta_1[t-1], sigma_2);
    }

    for (t in Lambda:T) {
        // s[t] ~ normal(ss[t], sigma_3);
	    // the following explicit version (ugly as sin!) beats the above in terms of speed:
        s[t] ~ normal(-s[t-1]-s[t-2]-s[t-3]-s[t-4]-s[t-5]-s[t-6]-s[t-7]-s[t-8]-s[t-9]-s[t-10]-s[t-11], sigma_3);
        // s[t] ~ normal(sum_constraint(s, t, Lambda), sigma_3);
    }

    // priors on initial values
	theta_1[1] ~ normal(y[1], sigma_2);

    // hyperparameters
	sigma_1 ~ inv_gamma(0.001, 0.001);
	sigma_2 ~ inv_gamma(0.001, 0.001);
	sigma_3 ~ inv_gamma(0.001, 0.001);
}
