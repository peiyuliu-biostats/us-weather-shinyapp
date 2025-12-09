// 文件名: sir.stan
functions {
  // SIR ODE 方程
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
  int<lower=1> n_obs;       // 观测点数量
  array[n_obs] real ts;     // 时间点
  array[n_obs] int cases;   // 观测病例数
  real<lower=0> N;          // 总人口
  real t0;                  // 初始时间
  vector[3] y0;             // 初始状态 (S0, I0, R0)
}

transformed data {
  array[1] real x_r = {N};
  array[0] int x_i;
}

parameters {
  real<lower=0> beta;
  real<lower=0> gamma;
}

transformed parameters {
  vector[2] theta = [beta, gamma]';
  // 求解 ODE
  array[n_obs] vector[3] y_hat = ode_rk45(sir_ode, y0, t0, ts, theta, x_r, x_i);
}

model {
  // 1. Priors
  // 针对真实数据，我们给一个较宽的弱信息先验，允许数据说话
  beta ~ normal(0.5, 1);
  gamma ~ normal(0.2, 1);
  
  // 2. Likelihood
  for (t in 1:n_obs) {
    // 加上 1e-6 是为了数值稳定性
    cases[t] ~ poisson(y_hat[t, 2] + 1e-6);
  }
}

generated quantities {
  real R0 = beta / gamma;
  
  // 后验预测检查 (Posterior Predictive Checks)
  array[n_obs] int cases_pred;
  for (t in 1:n_obs) {
    // 模拟如果模型是对的，观测数据应该长什么样
    if (y_hat[t, 2] > 0)
      cases_pred[t] = poisson_rng(y_hat[t, 2]);
    else
      cases_pred[t] = 0;
  }
}