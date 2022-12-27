
############
# To install R packages (only once)
############
function install_packages(pkg, repos = "https://cran.rstudio.com")
    run(`R -e "install.packages('$pkg', dependencies=TRUE, repos = '$repos', lib = R_lib_path)"`)
end
install_packages("robustHD")
install_packages("doParallel")