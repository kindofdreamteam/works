function [ys,params,check] = steadystate(ys,exe,M_,options)

%% Steady state: part d

% Pick up the parameters defined in param_core_part_d.m
param_core_part_d

% Calculate steady state in terms of parameters in levels
C_level = Y - G;
I_level = 1/betap;
b_level = G/(taup + 1 - 1/betap);
T_level = taup*b_level;
m_level = ((1/(1-betap))*(C_level^thetap))^(1/mup);
P_level = M/m_level;
M_var_level = M;
v_tau_level = 0;
v_m_level = 0;

% Convert steady state in levels to logs as varibles in var block of dynare are in logs
C = log(C_level);
I = log(I_level);
b = log(b_level);
T = log(T_level);
m = log(m_level);
P = log(P_level);
M_var = log(M_var_level);
v_tau = v_tau_level;      
v_m = v_m_level;   

%% Leave unchanged

check = 0;
options = optimset();

% Export parameters to Dynare
nparams = size(M_.param_names,1);

params = NaN(nparams,1);
for icount = 1:nparams
    eval(['params(' num2str(icount) ') = ',M_.param_names{icount},';'])
end

nvars = M_.endo_nbr;

ys = zeros(nvars,1);
for i_indx = 1:nvars
    eval(['ys(i_indx) = ',M_.endo_names{i_indx},';'])
end

end