# ---------------- configuration ----------------------

# if you have multiple SWI Prolog installations or an installation
# in a non-standard place, set PLLD to the appropriate plld invokation, eg
# PLLD=/usr/local/bin/plld -p /usr/local/bin/swipl

#PACKNAME=sparkle
#include ../Makefile.inc

OBO = http://purl.obolibrary.org/obo

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
	swipl --stack_limit=14g --table_space=14g -l examples/conf.pl -g "do_for($*),halt"

doall:
	swipl -l examples/conf.pl -g doall,halt


EXDIR = examples
ONTS = $(patsubst $(EXDIR)/%/, %, $(sort $(dir $(wildcard $(EXDIR)/*/*.yaml))))

doall2: $(patsubst %, do-%, $(ONTS))
x:
	echo yy $(patsubst %, do-%, $(ONTS))
	echo xx $(ONTS)

#examples/%/inf.owl: examples/%/merged.owl
#	robot reason -r elk -i $< -a true -o $@
# temp switch to owltools, see https://github.com/ontodev/robot/issues/518
examples/%/inf.obo:
	owltools  examples/$*/_induced_axioms_merged.ttl --assert-inferred-subclass-axioms --allowEquivalencies --markIsInferred -o -f obo --no-check $@.tmp && obo-grep.pl -r is_inferred $@.tmp | obo-filter-tags.pl -t id -t name -t is_a - > $@

examples/%/merged.owl:
	robot merge -i examples/$*/_induced_axioms.ttl -i examples/$*/_src.ttl -a true -o $@

#list: $(foreach file, $(wildcard examples/chebi/*), foo-$(filename $(file));)
list: $(foreach file, $(wildcard examples/chebi/*.yaml), $(basename $(file)).tsv)

# e.g. make tsv ONT=pato
tsv: $(foreach file, $(wildcard examples/$(ONT)/*.yaml), $(basename $(file)).tsv)

examples/%.tsv:
	dosdp-tools query --obo-prefixes --template=examples/$*.yaml --ontology=examples/$(dir $*)_input.ttl --outfile=$@
#	dosdp-tools query --obo-prefixes --template=examples/$*/X_has_part_X.yaml --ontology=examples/chebi/_input.ttl --outfile=examples/$*/X_has_part_X.tsv

examples/%/pattern.owl:
	dosdp-tools prototype --obo-prefixes --template=examples/$* --outfile=$@

examples/%/pattern-terms.txt: examples/%/pattern.owl
	robot query -i $< -q sparql/seed.rq $@
examples/%/pattern-imports.owl: examples/%/pattern-terms.txt
	robot extract -m BOT -i examples/$*/_input.ttl -T $< -o $@
examples/%/pattern-inf.owl: examples/%/pattern-imports.owl
	robot merge -i examples/$*/pattern.owl -i $< reason -r hermit annotate -O $(OBO)/$@ -o $@

#examples/chebi/seed.txt:
#	pq-ontobee -f tsv --distinct 'owl_some(_,_,C),str_starts(str(C),"http://purl.obolibrary.org/obo/CHEBI_")' C > $@

tests/data/chebi.owl:
	curl -L -s http://purl.obolibrary.org/obo/chebi.owl > $@.tmp && mv $@.tmp $@

# Doesn't work see https://github.com/OntoZoo/ontofox/issues/4
#examples/chebi/chebi-subset.owl:
#	curl -s -F file=@examples/chebi/ontofox.txt -o $@ http://ontofox.hegroup.org/service.php

examples/chebi/chebi-subset.owl: tests/data/chebi.owl examples/chebi/seed.txt
	robot extract -m BOT -i $< -T examples/chebi/seed.txt -o $@

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
