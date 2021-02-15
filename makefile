.PHONY:
	dependencies \
	pipeline \
	clean

dependencies:
	Rscript -e "renv::restore()"

pipeline:
	Rscript -e "targets::tar_make()"

clean:
	Rscript -e "targets::tar_destroy()"
