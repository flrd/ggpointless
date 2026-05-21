.PHONY: site site-clean docs test check install help

help:
	@echo "Targets:"
	@echo "  site        Build pkgdown site and remove leaked private pages"
	@echo "  site-clean  Remove leaked CLAUDE.html from docs/"
	@echo "  docs        devtools::document()"
	@echo "  test        devtools::test()"
	@echo "  check       devtools::check()"
	@echo "  install     devtools::install()"

site:
	Rscript -e 'pkgdown::build_site(preview = FALSE)'
	$(MAKE) site-clean

site-clean:
	rm -f docs/CLAUDE.html

docs:
	Rscript -e 'devtools::document()'

test:
	Rscript -e 'devtools::test()'

check:
	Rscript -e 'devtools::check()'

install:
	Rscript -e 'devtools::install()'
