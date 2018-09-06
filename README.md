# owl patternizer

Automatically extract common patterns from an OWL Ontology

Currently this only looks for equivalence axioms between a named class
and a class expression (aka logical definitions), where the class
expression uses the following constructs:

 - some
 - and
 - named classes
 - named object properties

It will generate out descriptions of the pattern in YAML following [DOSDP Schema](https://github.com/INCATools/dead_simple_owl_design_patterns/)

The approach is intended to be knowledge-free. Minimal underlying assumptions about the ontology. As such, the generated yaml will be more generic than hand-crafted. The intent is that it is used to bootstrap and analyze an ontology.

## Example

Given as input PATO, an ontology which contains implicit patterns of the form `{increased, descreased} X`, DPs such as the following are generated:

```
pattern_name: X increased_in_magnitude_relative_to normal
pattern_iri: http://purl.obolibrary.org/obo/pato/X_increased_in_magnitude_relative_to_normal

description: >-
  This is auto-generated. Add your description here

  Examples: [increased quality](http://purl.obolibrary.org/obo/PATO_0002300), [increased speed](http://purl.obolibrary.org/obo/PATO_0000303), [increased age](http://purl.obolibrary.org/obo/PATO_0001764) (105 total)

classes: 
  quality: "PATO:0000001"
  normal: "PATO:0000461"

relations: 
  increased_in_magnitude_relative_to: "http://purl.obolibrary.org/obo/pato#increased_in_magnitude_relative_to"

vars:
  v0: "'quality'"

name:
  # Induced, frequency=0.5619047619047619, p=http://www.w3.org/2000/01/rdf-schema#label 
  text: "increased %s"
  vars:
    - v0

def:
  # Induced, frequency=0.24761904761904763, p=http://purl.obolibrary.org/obo/IAO_0000115 
  text: "A %s which is relatively high."
  vars:
    - v0

annotationProperties:
  exact_synonym: "oio:hasExactSynonym"
  related_synonym: "oio:hasRelatedSynonym"

annotations:
  - annotationProperty: exact_synonym
    # Induced p=exact_synonym 
    text: high %s
    vars:
      - v0

equivalentTo:
  text: "%s and ('increased_in_magnitude_relative_to' some 'normal')"
  vars:
    - v0
```

## Installation

There are two ways to do this

 1. via Docker
 2. direct via swipl

For 1, no additional installation required. TODO: document this

For 2, you will need to install [sparqlprog](http://www.swi-prolog.org/pack/list?p=sparqlprog)

## Command Line Usage

`pl2sparql -e -c conf/disorder_conf.pl -i tests/data/neoplasm.owl doall`

See [conf/disorder_conf.pl](blob/master/conf/disorder_conf.pl)

With docker:

First check out this repo, then:

```
docker run -v $PWD:/work -w /work --rm -ti cmungall/sparqlprog swipl -p library=/home/myuser/prolog:prolog /home/myuser/bin/pl2sparql  -e -c conf/disorder_conf.pl -i tests/data/neoplasm.owl doall,halt
```

(this is admittedly awkward - working on better wrapping script)

## Examples

For purely auto-generated examples, see:

 * [examples/](blob/master/examples/)

## TODO

 * range validation gets thrown off if there are dangling classes