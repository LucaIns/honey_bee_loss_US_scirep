tic();
n = N;
Vic=length(k_sinit:maxk_s);
# initial k_s value	
if years_up_21 == 1
	k_sinit=7;
else
	k_sinit = 1;
end

#-------------------------------------------------------------------------
# function to calculate rAIC and rBIC
#-------------------------------------------------------------------------
function Verror(xn,yn,bn,id,k_s)
	n = length(yn);
	hh = n - sum(id.== 1);
	res = (yn - xn*bn) .* (id.==0);	
	if hh<n   
		vt = quantile.(Normal(), 0.5 * (1 + hh/n));
		factor = 1/(1 - 2 * (n/hh) * vt * pdf.(Normal(),vt));
	else
		factor = 1;
	end
	RSS = factor * res' * res;
	BIC = n*log.(RSS/hh) + (k_s+n-hh)*log.(n);
	AIC = n*log.(RSS/hh) + (k_s+n-hh)*2;
	return BIC[1], AIC[1]
end

#-------------------------------------------------------------------------
# initialize results
#-------------------------------------------------------------------------
ic = zeros(Vic,4);
bIC = zeros(s,Vic);
bzIC = zeros(s,Vic);
pIC = zeros(n,Vic);
pzIC = zeros(n,Vic);
#-------------------------------------------------------------------------
# MIP with IC
#-------------------------------------------------------------------------
m = Model(solver = GurobiSolver(TimeLimit=maxtime, MIPGap = MIPg, Threads=threads, OutputFlag=out, InfUnbdInfo=1)); 

println(maxtime)

X = X[:,1:s];
p = s+N;
n=N;
# Define variables
@variable(m, Z[1:s],Bin);
@variable(m, Zp[1:n],Bin);	
@variable(m, B[1:s]);
@variable(m, Phi[1:n]);
@variable(m, G[1:3],Bin);
groupind = [fill(1, 4); fill(2, 8); fill(3, 3)];
# Objective
@objective(m, Min, (1/n) * (dot(Y,Y)*.5 + dot(B,(X'*X)*B)*.5 + dot(Phi, X*B) - dot(Y[:,1],(X*B)) + dot(Phi, Phi)*.5  - dot(Phi, Y[:,1])) );
# Constraints
if addSOS == 0
	# big-M 
	@constraint(m, cons2[i = 1:s], -(M[i]*Z[i]) <= B[i]);
	@constraint(m, cons3[i = 1:s], (M[i]*Z[i]) >= B[i]);
	@constraint(m, cons4[i = 1:n], -(M[s+i]*Zp[i]) <= Phi[i]);
	@constraint(m, cons5[i = 1:n], (M[s+i]*Zp[i]) >= Phi[i]);
else
	# SOS
	@variable(m, T[1:s],Bin);
	@constraint(m, cons2[i=1:s], T[i] == 1-Z[i]);
	for i=1:s
		addSOS1(m, [T[i],B[i]]);
	end
	@variable(m, Tp[1:n],Bin);
	@constraint(m, cons3[i=1:n], Tp[i] == 1-Zp[i]);
	for i=1:n
		addSOS1(m, [Tp[i],Phi[i]]);
	end
end
# L_0-constraints
@constraint(m, cons6, sum(Zp[i] for i in 1:n) <= Int(round(N * k_n))); # sum across all n outlier dummy
@constraint(m, cons7, sum(Z[i] for i in 1:s) == k_sinit);
if intercept == 1
	@constraint(m, cons8, Z[1] == 1);	    
end
# group contraints
for i = 1:3
	idx = find(x->x == i,groupind)+intercept;
	@constraint(m, Z[idx] .== G[i]);
end
# force years in (only for 2015-2021)
if years_up_21 == 1
	@constraint(m, consGrp, G[1] == 1);
end
#-------------------------------------------------------------------------
# warm-starts
#-------------------------------------------------------------------------
for kcv= k_sinit:maxk_s
	println(join(("On k =", kcv)," "))
	v = Int(kcv-k_sinit+1);
	if (kcv!=k_sinit)
		JuMP.setRHS(cons7,Int(kcv)); # update k_s
	end
	solve(m)
    #-------------------------------------------------------------------------
    # get information criteria
    #-------------------------------------------------------------------------
    bIC[:,v] = getvalue(B);
    bzIC[:,v] = getvalue(Z);
	pIC[:,v] = getvalue(Phi);
	pzIC[:,v] = getvalue(Zp);

	tmpSol= Verror(X, Y, bIC[:,v], pzIC[:,v], kcv);
	ic[v,1] = kcv;
	ic[v,2] = tmpSol[1][1];
	ic[v,3] = tmpSol[2][1];
	ic[v,4] = getsolvetime(m);
end

# min IC
BICmin = indmin(ic[:,2]);
AICmin = indmin(ic[:,3]);

if ICtype == "BIC"
	ICmin = BICmin;
elseif ICtype == "AIC"
	ICmin = AICmin;
end

# results 
sol[rep_i, 8, j_iter] = toq();
B_est[rep_i, :, j_iter] = bIC[:,ICmin];
MIPtemp = ones(N, 1);
MIPtemp[pzIC[:,ICmin] .== 1] = 0;
Phi_est[rep_i, :, j_iter] = MIPtemp;

if saveResIC == 1
	cd(string(user_path, "/output/tuning"));
	CSV.write(join((opt_str, rep_i, Int(ic[ICmin,1]), "MIPIC.csv"),"-"),DataFrame(ic));
end

j_iter += 1;