data {
    int<lower=1> T;
    vector[T] y;
}

parameters {
    vector[T] theta_1;
    real<lower=0> sigma_1;
    real<lower=0> sigma_2;
}

model {
    for (t in 1:T) {
        y[t] ~ normal(theta_1[t], sigma_1);
    }

    for (t in 2:T) {
        theta_1[t] ~ normal(theta_1[t-1], sigma_2);
    }
}
