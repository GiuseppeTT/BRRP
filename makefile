.PHONY:
	dependencies \
	pipeline \
	clean \
	container_local \
	container_rstudio \
	container_github

dependencies:
	Rscript -e "renv::restore()"

pipeline:
	Rscript -e "targets::tar_make()"

clean:
	rm -rf _targets/

container_local:
	docker run \
		-ti \
		--name BRRP \
		--rm \
		--volume $$PWD:/home/rstudio/BRRP \
		--workdir /home/rstudio/BRRP \
		rocker/verse bash

container_rstudio:
	@echo "Open http://localhost:8787/ on your browser"
	@echo "Press Ctrl+C to close"
	@docker run \
		--env DISABLE_AUTH=true \
		--name BRRP \
		--publish 8787:8787 \
		--rm \
		--volume $$PWD:/home/rstudio/BRRP \
		--workdir /home/rstudio/BRRP \
		rocker/verse > /dev/null

container_github:
	docker run \
		-ti \
		--detach \
		--name BRRP \
		--rm \
		--workdir /home/rstudio/BRRP \
		rocker/verse bash
	docker exec BRRP git clone https://github.com/GiuseppeTT/BRRP.git .
	docker attach BRRP
