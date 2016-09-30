all:
	Rscript -e "library(devtools); load_all('.'); document('.')"
	R CMD build .
	R CMD INSTALL quizR_1.0.tar.gz
