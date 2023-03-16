# Updating one of GRYM's base-case setup configuration files used for Krill
# simulations, as available in GRYM's repository and used in Maschette et al (2021)
# (https://github.com/ccamlr/Grym_Base_Case/tree/Simulations). Adding some input
# parameters that are missing from the original files. This configuration
# file will be used as the reference of GRYM input parameters in the MSE analysis.


grym_setup_ref <- read_rds("part1_openMSE_GRYM_approx/part1_shared_files/data/Setup_pars_48.1_Initial_values.rds")

#-----------------------------------------------------------------------------------------------------
# Specification of `ms` -  the matrix representing the intra-annual component of natural mortality
#
# Assuming the impact of natural mortality is constant throughout the year
grym_setup_ref$ms <- matrix(1, grym_setup_ref$nsteps+1, length(grym_setup_ref$Ages))



# write-out updated setup file
write_rds(grym_setup_ref, file = "part1_openMSE_GRYM_approx/part1_shared_files/data/grym_setup_ref.rds")











