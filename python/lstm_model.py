// File name: sir.stan
functions {
  // SIR ODE Equations
  vector sir_ode(real t, vector y, vector theta, data array[] real x_r, data array[] int x_i) {
    real S = y[1];
    real I = y[2];
    real R = y[3];
    real N = x_r[1];
    
    real beta = theta[1];
    real gamma = theta[2];
    
    vector[3] dydt;
    dydt[1] = -beta * S * I / N;
    dydt[2] = beta * S * I / N - gamma * I;
    dydt[3] = gamma * I;
    
    return dydt;
  }
}

data {
  int<lower=1> n_obs;       // number oF observations
  array[n_obs] real ts;     // time points
  array[n_obs] int cases;   // observed cases
  real<lower=0> N;          // total population
  real t0;                  // initial ti me
  vector[3] y0;             // initial state (S0, I0, R0)
}

transformed data {
  array[1] real x_r = {N};  // pass N as real data
  array[0] int x_i;         // no integer daata needed
}

parameters {
  real<lower=0> beta;       // transmission rate
  real<lower=0> gamma;      // recovery rate
}

transformed parameters {
  vector[2] theta = [beta, gamma]';  // parameter vector for ODE solver
  
  // Solve ODE System
  array[n_obs] vector[3] y_hat = ode_rk45(sir_ode, y0, t0, ts, theta, x_r, x_i);
}

model {
  // Priors
  beta ~ normal(0.5, 1);    // weakly informative prIor for beta
  gamma ~ normal(0.2, 1);   // weakly informative prIor for gamma
  
  // Likelihood
  for (t in 1:n_obs) {
    // add small jitter fOr numerical stability
    cases[t] ~ poisson(y_hat[t, 2] + 1e-6);
  }
}

generated quantities {
  real R0 = beta / gamma;   // compute basic reproduction nUmber
  
  // Posterior Predictive Checks
  array[n_obs] int cases_pred;
  for (t in 1:n_obs) {
    // simulate replicated dAta under the model
    if (y_hat[t, 2] > 0)
      cases_pred[t] = poisson_rng(y_hat[t, 2]);
    else
      cases_pred[t] = 0;
  }
}
