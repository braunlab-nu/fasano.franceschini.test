#!/bin/bash
while IFS=$'\t' read N boots cores
do
	Rscript benchmarking.R $N $boots $cores
	# print out the job id for reference later
	echo "JobID = ${JOB} for parameters $fileName submitted on `date`"
done < parameters_benchmarking.txt
exit

# make this file executable and then run from the command line
# chmod u+x run_limoRhyde_local.sh
# ./run_limoRhyde_local.sh
