# Perform PCA, leaving out rows with NAs

# Column 14, the HAI column, is excluded because it is dependent with the other 
# columns

Alameda_PCA <- prcomp(na.omit(Alameda_new[, -14]))

# Looking at the explained variance per PC, choose the first 3

Alameda_comps <- Alameda_PCA$rotation[, 1:3]
Alameda_SDs <- Alameda_PCA$sdev[1:3]

# produce final weights for the index as a weighted sum of the PCs

weights <- t(Alameda_SDs %*% t(Alameda_comps))

#############################
#       Alternatively       #
#############################

# 2008-2009 marks a pretty weird point in economic history, and the fundamentals
# at play in the economy are different across that inflection point. The idea 
# here would be to run PCA twice, once before and once after, produce two sets 
# of weights using the procedure above, and combine those in some way.