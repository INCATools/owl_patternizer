# Note: all examples are OBO
# in theory should work for other ontologies

ONTS = pizza so envo pato obi uberon mp hp doid foodon efo cl ncit  go
OPTS=

all: $(patsubst %, gen-%, $(ONTS))
#	pl2sparql -e -c conf.pl -A void.ttl doall

clean:
	rm gen-*

realclean: clean
	rm */*.yaml

gen-%:
	touch init-$* && pl2sparql $(OPTS) -e --consult conf.pl -A void.ttl  "do_for('$*')" && touch $@


prep-chebi:
	owltools ../tests/data/chebi.owl ../target/chebi.ttl --merge-support-ontologies -o -f ttl ../tests/data/chebi-with-logdefs.ttl
