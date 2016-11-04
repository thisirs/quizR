all:
	Rscript -e "library(devtools); load_all('.'); document('.')"
	R CMD build .
	-R CMD check quizR_1.0.tar.gz
	R CMD INSTALL quizR_1.0.tar.gz
