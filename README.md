# owl patternizer

Automatically extract common patterns from an OWL Ontology

## Background

Creating large ontologies or terminologies is a resource-intensive task, with the possibility to introduce human error. Use of description logic languages such as OWL-DL accompanied by reasoning can automate some parts of ontology construction and validation, but adding the appropriate OWL axioms can itself be a time consuming process requiring expert ontologists.

the OWL patternizer looks for stereotypical patterns in ontologies and vocabularies (both logical and lexical), and generalizes these to [DOSDPs](https://github.com/INCATools/dead_simple_owl_design_patterns/commits)

For example, if an ontology has lots of terms like:

 * abnormal limb morphology
 * abnormal head morphology
 * abnormal epiphysis morphology
 * increased limb size
 * decreased limb size
 * increased head size
 * decreased head size

With logical definitions, then it will detect the OWL patterns for these. If there are no logical definitions present, it will do a best effort at inducing logical definitions from lexical patterns.

The approach is intended to be knowledge-free. Minimal underlying assumptions about the ontology. As such, the generated yaml will be more generic than hand-crafted. The intent is that it is used to bootstrap and analyze an ontology.

Additionally, if your starting point is a thesaurus-like ontology (no logical defs), there is a step for generating candidates.

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

The patterns are annotated with comments indicating provenance and confidence. 

Currently this only looks for equivalence axioms between a named class
and a class expression (aka logical definitions), where the class
expression uses the following constructs (arbitrarily nested):

 - some
 - only
 - and
 - or
 - named classes
 - named object properties



## Installation

There are two ways to do this

 1. via Docker
 2. direct via swipl

For 1, no additional installation required. TODO: document this

For 2, you will need to install [sparqlprog](http://www.swi-prolog.org/pack/list?p=sparqlprog)

## Command Line Usage

TODO

## Examples

For purely auto-generated examples, see:

 * [examples/](https://github.com/INCATools/owl_patternizer/tree/master/examples)

(may be moved to other repo)

## TODO

 * range validation gets thrown off if there are dangling classes

## Draft paper

 * [google doc](https://docs.google.com/document/d/177cASJWn8QnxCSu05cw3HoJiweBGJgPmpHeua3ORKh0/edit#)

