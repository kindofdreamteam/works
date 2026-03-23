var C m b I T M_var P v_tau v_m;

varexo eps_tau eps_m;

parameters thetap mup betap taup rho_taup rho_m Y G M sigma_eps_tau sigma_eps_m;

thetap = 1.5;
mup = 4.0;
betap = 0.995;
taup = 0.1;
rho_taup = 0.8;
rho_m = 0.8;
Y = 100;
G = 25;
M = 10;
sigma_eps_tau = 0.01;
sigma_eps_m = 0.01;

model;

exp(C)^(-thetap) = betap*exp(I)*(exp(C(+1))^(-thetap)/exp(P(+1))*exp(P));
exp(m) = (exp(I)/(exp(I)-1)*exp(C)^thetap)^(1/mup);
exp(m) + exp(b) + exp(T) = exp(I(-1))*exp(b(-1))/(exp(P)/exp(P(-1))) + exp(m(-1))/(exp(P)/exp(P(-1))) + G;
exp(T) = taup*exp(b(-1)) + Y*v_tau;
exp(M_var) = M*exp(v_m);
exp(m) = exp(M_var)/exp(P);
exp(C) = Y - G;
v_tau = rho_taup*v_tau(-1) + sigma_eps_tau*eps_tau;
v_m = rho_m*v_m(-1) + sigma_eps_m*eps_m;

end;

shocks;
var eps_tau; stderr 1;
var eps_m;   stderr 1;
end;

steady_state_file = 'steadystate_part_d.m';

steady;
check;

disp(exp(oo_.steady_state'));

model_info;

stoch_simul(order=1, irf=50);
