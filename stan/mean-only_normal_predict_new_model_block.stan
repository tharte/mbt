// ---------------------------------------------------------------------
// model name: mean-only, normal [predict T_new in model block]

data {
    int<lower=1> T;
    real y[T];

    int<lower=0> T_new;
}

parameters {
    real theta;
    real<lower=0> sigma;
    vector[T_new] y_tilde;
}

model {
    for (t in 1:T) {
        y[t] ~ normal(theta, sigma);
    }
    for (t in 1:T_new) {
        y_tilde[t] ~ normal(theta, sigma);
    }
}
