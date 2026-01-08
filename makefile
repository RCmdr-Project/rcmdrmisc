package := RcmdrMisc
version := 2.10.1
R := $(wildcard pkg/R/*.R)
Rd := $(wildcard pkg/man/*.Rd)

# User targets
default: $(package)_$(version).tar.gz
	@echo $(package)_$(version).tar.gz ready

check: $(package).Rcheck
	@echo Checking $(package)_$(version).tar.gz

# Other targets
$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz

$(package)_$(version).tar.gz: $(R) $(Rd) pkg/DESCRIPTION pkg/NAMESPACE
	R CMD build pkg

pkg/NAMESPACE: $(R)
	R -e "roxygen2::roxygenize('pkg')"
	# Temporary patch until all Rd were migrated
	touch pkg/DESCRIPTION

