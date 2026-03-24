var C m b I T M_var P v_tau v_m;

varexo eps_tau eps_m;

parameters thetap mup betap taup alphap rho_taup rho_m Y G P_ss B_ss sigma_eps_tau sigma_eps_m;

% Parameters
thetap = 1.5;
mup = 4.0;
betap = 0.995;
taup = 0.1;
alphap = -0.5;
rho_taup = 0.8;
rho_m = 0.8;
Y = 100;
G = 25;
P_ss = 0.2;
B_ss = (G/(taup + 1 - 1/betap)) * P_ss;
sigma_eps_tau = 0.01;
sigma_eps_m = 0.01;

model;

exp(C)^(-thetap) = betap*exp(I)*(exp(C(+1))^(-thetap)/exp(P(+1))*exp(P));
exp(m) = (exp(I)/(exp(I)-1)*exp(C)^thetap)^(1/mup);
exp(m) + exp(b) + exp(T) = exp(I(-1))*exp(b(-1))/(exp(P)/exp(P(-1)))+exp(m(-1))/(exp(P)/exp(P(-1))) + G;
exp(T) = taup*B_ss/P_ss + Y*v_tau;
exp(I) = (1/betap)*(exp(P)/P_ss)^alphap/exp(v_m);
exp(m) = exp(M_var)/exp(P);
exp(C) = Y - G;
v_tau = rho_taup*v_tau(-1) + sigma_eps_tau*eps_tau;
v_m = rho_m*v_m(-1) + sigma_eps_m*eps_m;

end;

shocks;
var eps_tau; stderr 1;
var eps_m;   stderr 1;
end;

steady_state_file = 'steadystate_part_e.m';

steady;
check;

disp(exp(oo_.steady_state'));

model_info;

stoch_simul(order=1, irf=30);