// ---------------------------------------------------------------------
// model name: mean-only, normal [vectorized: no for loop over index t]

data {
    int<lower=1> T;
    real y[T];
}

parameters {
    real theta;
    real<lower=0> sigma;
}

model {
    y ~ normal(theta, sigma);
}
