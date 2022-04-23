# Set these variables to the right paths - these are paths on my system
# Keep all the codes in directory you soecify below
current_working_dir <- '.'

# Get user defined codes in system to use later
source (paste(current_working_dir, 'Call Functions.R', sep="\\"), print.eval=T, echo=T)

# Read in all Qtrs files and save appended data
source (paste(current_working_dir, 'Contact Driver.R', sep="\\"))

# CLUSTERING solution achieved via investigation in BIC for Expectation-Maximization, k-means and spherical k-means
# this also involves data preparation suited for clustering
source (paste(current_working_dir, 'Clustering v02.R', sep="\\"))

# Get title information
source (paste(current_working_dir, 'Get Industry.R', sep="\\"), print.eval=T, echo=T)

# Further detailing on k-means
source (paste(current_working_dir, 'K-means Cluster solution.R', sep="\\"))
