// ---------------------------------------------------------------------
// model name: mean-only, normal [predicts T_new values in y_tilde]

data {
    int<lower=1> T;
    real y[T];
    int<lower=0> T_new;
}

parameters {
    real theta;
    real<lower=0> sigma;
}

model {
    for (t in 1:T) {
        y[t] ~ normal(theta, sigma);
    }
}

generated quantities {
    vector[T_new] y_tilde;
    for (t in 1:T_new) {
        y_tilde[t]<- normal_rng(theta, sigma);
    }
}
