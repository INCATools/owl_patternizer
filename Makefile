# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

#PACKNAME=sparkle
#include ../Makefile.inc

SWIPL = swipl  -L0 -G0 -T0  -p library=prolog
all: test

check:
install:
clean:

test: coretest t-def t-envo_mkdef
coretest: t-def t-envo_mkdef
	$(SWIPL) -l tests/tests.pl -g run_tests,halt

coverage:
	$(SWIPL) -l tests/bigtests.pl -l tests/tests.pl -g "show_coverage(run_tests),halt"

t-%:
	$(SWIPL) -l tests/$*_test.pl -g run_tests,halt

# E.g. do-envo
do-%:
	swipl --stack_limit=12g --table_space=12g -l examples/conf.pl -g "do_for($*),halt"

doall:
	swipl -l examples/conf.pl -g do_all,halt

#examples/%/inf.owl: examples/%/merged.owl
#	robot reason -r elk -i $< -a true -o $@
examples/%/inf.obo: 
	robot reason -r elk -i examples/$*/_induced_axioms_merged.ttl -a true -o $@

examples/%/merged.owl:
	robot merge -i examples/$*/_induced_axioms.ttl -i examples/$*/_src.ttl -a true -o $@

# --------------------
# Docker
# --------------------

# Get version from pack
VERSION = v$(shell swipl -l pack.pl -g "version(V),writeln(V),halt.")

show-version:
	echo $(VERSION)

IM = cmungall/owl_patternizer

docker-all: docker-clean docker-build docker-run

docker-clean:
	docker kill $(IM) || echo not running ;
	docker rm $(IM) || echo not made 

docker-build:
	@docker build -t $(IM):$(VERSION) . \
	&& docker tag $(IM):$(VERSION) $(IM):latest


docker-run:
	docker run --name rdf_matcher $(IM)

docker-publish: docker-build
	@docker push $(IM):$(VERSION) \
	&& docker push $(IM):latest
