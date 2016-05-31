data {
    int<lower=1> T;
    int<lower=1> T_new;
    vector[T] y;
}

transformed data {
    int<lower=T> T_start;
    int<lower=T> T_end;

    T_start<- T+1;
    T_end<-   T+T_new;
}

parameters {
    real<lower=0> sigma_1;
    real<lower=0> sigma_2;
    real<lower=0> sigma_3;
    vector[T+T_new] theta_1;
    vector[T+T_new] theta_2;
}

model {
    for(t in 1:T) {
        y[t] ~ normal(theta_1[t], sigma_1);
    }

    for(t in 2:T_end) {
        theta_1[t] ~ normal(theta_1[t-1] + theta_2[t], sigma_2);
        theta_2[t] ~ normal(theta_2[t-1], sigma_3);
    }
}

generated quantities {
    vector[T_new] y_tilde;

    for (t in T_start:T_end) {
        y_tilde[t-T]<- normal_rng(theta_1[t] + theta_2[t], sigma_1);
    }
}
