.PHONY: all activity performance evaluation

all: activity performance evaluation

activity: scripts/01_practice_activity.Rmd
	Rscript -e "rmarkdown::render('scripts/01_practice_activity.Rmd', output_format = 'all', output_dir = 'output')"

performance: scripts/02_test_performance.Rmd
	Rscript -e "rmarkdown::render('scripts/02_test_performance.Rmd', output_format = 'all', output_dir = 'output')"

evaluation: scripts/03_evaluation.Rmd
	Rscript -e "rmarkdown::render('scripts/03_evaluation.Rmd', output_format = 'all', output_dir = 'output')"
