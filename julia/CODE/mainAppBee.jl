println(versioninfo())
# model data for years 2015-2021
# years_up_21 = 1
# model data for years 2015-2019
years_up_21 = 0
#------------------------------------------------------------------------
#necessary packages
#------------------------------------------------------------------------
user_path = string("YOUR_PATH");
# install R libraries (to be used only once)
R_lib_path = string("YOUR_R_LIB_PATH")
inst_R_lib = 0;
# install julia libraries (to be used only once)
inst_jl_lib = 0;
if inst_jl_lib==1
	Pkg.add("Pkg");
	using Pkg
	Pkg.add("RCall")
	Pkg.add("Gurobi")
	Pkg.add("JuMP")
	Pkg.add("DataFrames")
	Pkg.add("CSV")
end
using RCall
using Gurobi
using JuMP
using DataFrames
using CSV
#-------------------------------------------------------------------------
# Setting
#-------------------------------------------------------------------------
typeDat = ARGS[1];
typeDat = parse(Int,typeDat);
k_n = ARGS[2];
k_n = parse(Float64,k_n);
seed = ARGS[3];
seed = parse(Int,seed);
# if typeDat == 1
typeDat = "bee";
intercept = 1;
# fraction of contamination
R = k_n;
est_nam = ["sparseLTS","mipIC"];
n_est 		= length(est_nam);
# MIP method
MIPtype 	= 1;
# type of warm-starting for MIP - see: est_prel.jl file
prel 		= 2;
# multiplicative constant for M-bounds (based on preliminary esnsamble estimators)
const_mult 	= 2;
# save generated data as CSV file
saveCSV 	= 0;
# save results for each iteration
saveResEach = 1;
# num. of folds
V 			= 5; #Int(round(min(N/10, 5)));
# save overall CV results
saveResCV 	= 1;
# save CV results for each fold
saveResCVeach = 0;
# replications
rep_tot = 1;
rep_i = 1;
if years_up_21 == 0
	# where categorical features end in X (including intercept)
	gscale = 16;
	# groupwise constraints for categorical variables
	groupind = [fill(1, 4); fill(2, 8); fill(3, 3)];
	# max number of active features for IC and CV
	maxk_s = 30;
else
	# where categorical features end in X (including intercept)
	gscale = 18;
	# # groupwise constraints for categorical variables
	groupind = [fill(1, 6); fill(2, 8); fill(3, 3)];
	# max number of active features for IC and CV
	maxk_s = 35;
end
# IC type (BIC or AIC)
ICtype = "AIC";
# min BIC or elbow in IC
ICelbow = 0;
# save IC path
saveResIC = 1;

@rput intercept;
@rput user_path;
@rput scaleX
@rput R;
@rput seed;
@rput years_up_21;

#-------------------------------------------------------------------------
# Gurobi options
#-------------------------------------------------------------------------
out 	= 1;
threads = 24;
solver 	= "gurobi";
maxtime = 5000;
MIPg 	= 0.01;
#-------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------
if typeDat == "bee"
	R"""
		setwd(user_path)
		set.seed(seed)
		load("CODE/beeLM.RData") 
		ZZ = Zlog
		Y = ZZ$y
		X = ZZ[,2:ncol(ZZ)]
		if (years_up_21 == 0){
			X = X[, -c(5,6,18)]
		} else {
			X = X[, -c(7,8,20)]	
		}
		print(names(X))		
		if (intercept == 1){
			X = cbind(rep(1, length(Y)), X)  
		}
		if (scaleX == 3){
			X[, (gscale+1):ncol(X)] = scale(X[, (gscale+1):ncol(X)])
		}
		d = dim(X)[2]
		nall = dim(X)[1]
		if (seed == 1){
			N = floor(1*nall)
		} else {
			N = floor(0.80*nall)	
		}
		idx = sample(1:nall,N,replace=FALSE)
		Xtest = as.matrix(X[-idx,])
		X = as.matrix(X[idx,])
		Ytest = as.matrix(Y[-idx])
		Y = as.matrix(Y[idx])
	"""	
end
X 			= rcopy(R"X");
Y 			= rcopy(R"Y");
intercept	= Int(rcopy(R"intercept"));
N 			= Int(rcopy(R"N")); # sample size in training set
d 			= rcopy(R"d");
s 			= d;
Xtest 		= rcopy(R"Xtest");
Ytest 		= rcopy(R"Ytest"); 
Ntest 		= size(Xtest)[1];
# file names
opt_str = join((N, d, k_n, seed, typeDat), "-");
# num. trimmed units
k_m 		= round(N * k_n); 
m_out 		= k_m;
#-------------------------------------------------------------------------
# R libraries
#------------------------------------------------------------------------
# path
user_pathOLD = string("/storage/home/lfi5013/ROBUSTnew/")
R_lib_path = string(user_pathOLD, "Rpack", "")
@rput R_lib_path
R"""
	.libPaths(c(.libPaths(), R_lib_path))
"""	
# install (needed only once)
if inst_R_lib == 1
	@rput prel;
	include(string(user_path, "/CODE/inst_R_lib.jl", ""));
end
#-------------------------------------------------------------------------
# Initialize results
#-------------------------------------------------------------------------
B_est 	= 	zeros(rep_tot, d, n_est);
Phi_est = 	zeros(rep_tot, N, n_est);  
res_fit = 	zeros(rep_tot, N, n_est); 
sol 	= 	zeros(rep_tot, 8, n_est);
#-------------------------------------------------------------------------
# Preliminary robust estimators (used also for warm starting MIP and M bounds)
#-------------------------------------------------------------------------
include(string(user_path, "CODE/est_prel.jl", ""));
#-------------------------------------------------------------------------
# robustly normalize design matrix
#-------------------------------------------------------------------------
X=convert(Matrix,X);
Xtest=convert(Matrix,Xtest);
Y=convert(Matrix,Y);
Ytest=convert(Matrix,Ytest);
#-------------------------------------------------------------------------
# MIP
#-------------------------------------------------------------------------
if "mipIC" in est_nam 
	# using information criteria
	include(string(user_path, "CODE/MIPbee.jl", ""));
end
#-------------------------------------------------------------------------
# store results for each estimator
#-------------------------------------------------------------------------
for est_j = 1:n_est
	# RMSPE
	respred = (Ytest - Xtest*B_est[rep_i, :, est_j]).^2;
	sol[rep_i, 1, est_j] = sqrt(1/Ntest * sum(respred));

	respredTrim = mean(sort(respred[:,1])[1:Int(round(Ntest-k_n*Ntest))]);
	sol[rep_i, 2, est_j] = sqrt(respredTrim);
end
# save beta and phi
if saveResEach == 1	
	cd(string(user_path, "/output"));
	B_est_nam = join((opt_str,"B_est.csv"),"-");
	BestF = DataFrame(B_est[1, :, :]);
	names!(BestF, [Symbol(est_nam[i]) for i in 1:length(est_nam)])
	CSV.write(B_est_nam, BestF);
	Phi_est_nam = join((opt_str,"Phi_est.csv"),"-");
	PHIestF = DataFrame(Phi_est[1, :, :]);
	names!(PHIestF, [Symbol(est_nam[i]) for i in 1:length(est_nam)])
	CSV.write(Phi_est_nam, PHIestF);
end
# save overall results
sol = sol[:, [1, 2, 8], :];
sol = reshape(sol, rep_tot, 3*n_est);
cd(string(user_path, "output"));	
sav_nam = join((opt_str,"OUT.csv"),"-")
varNam = ["RMSPE", "TRMSPE", "time"];
colnam = []
for i = 1:length(est_nam)
    colnam = vcat(colnam, est_nam[i] .* "_" .* varNam);
end
CSV.write(sav_nam, DataFrame(sol, Symbol.(colnam[:])));