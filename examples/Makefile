# Note: all examples are OBO
# in theory should work for other ontologies

ONTS = pizza so envo pato obi go uberon efo cl ncit mp hp doid foodon

all: $(patsubst %, gen-%, $(ONTS))
#	pl2sparql -e -c conf.pl -A void.ttl doall

clean:
	rm */*.yaml

gen-%:
	pl2sparql -e -c conf.pl -A void.ttl  "do_for('$*')"
