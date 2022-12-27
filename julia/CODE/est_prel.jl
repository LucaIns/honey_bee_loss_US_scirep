if intercept == 1
	Xr = X[:,2:d];
else	
	Xr = X[:,1:d];
end
@rput Xr;
@rput Y;
@rput threads;	
@rput intercept;

if rep_i  ==1
	if "sparseLTS" in est_nam
		@rlibrary robustHD
		@rlibrary parallel
	end
end

j_iter = 1;
est_i = nothing;
# Loop in order to respect the specified order in est_nam
for j in 1:length(est_nam)

	est_i = est_nam[j];
	 
	if "sparseLTS" == est_i
		# Lasso with LTS		
		trSPlts = 1-R;
		@rput trSPlts;
		tic();
		R"""
			  maxtri = 10
			  tri = 1
			  indErr = 1
			  while (indErr == 1 && tri <= maxtri){   
			     a <- 
			          tryCatch(
			            {
        					clus <- parallel:::makeForkCluster(nnodes = getOption("mc.cores", threads))
							sol_sparseLTS = robustHD:::sparseLTS(Xr, Y, mode = "lambda", alpha = trSPlts, normalize = T, intercept=(intercept==1), nsamp=c(1000, 20), 
											model=F, crit = "BIC", # crit = "PE", splits = foldControl(10, 5), 
											cost = rtRMSPE, ncores = threads, cl=clus)  
							parallel:::stopCluster(clus)
				            indErr = 0
			            },
			              error = function(cond) {
			                indErr = 1
			                Sys.sleep(0.1)
			                cat("-----------------------------------------------------------------------------")
			                cat("\n")
			                print("error sparseLTS")
			                cat("\n")
			                cat("-----------------------------------------------------------------------------")
			              }
			  )
			     tri=tri+1
			  }
		 """;
		sol[rep_i, 8, j_iter] = toq();
		B_est[rep_i, :, j_iter] = rcopy(R"sol_sparseLTS$raw.coefficients");
		Phi_est[rep_i, :, j_iter] = rcopy(R"sol_sparseLTS$raw.wt");
		res_fit[rep_i, :, j_iter] = rcopy(R"sol_sparseLTS$raw.residuals");
		j_iter += 1;

	end
end				