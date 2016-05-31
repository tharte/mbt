// ---------------------------------------------------------------------
// model name: mean-only, normal [uses for loop over index t]

data {
    int<lower=1> T;
    real y[T];
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
