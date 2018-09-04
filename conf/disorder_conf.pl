:- use_module(library(owl_patternizer)).

:- rdf_register_prefix('UBERON','http://purl.obolibrary.org/obo/UBERON_').
:- rdf_register_prefix('MONDO','http://purl.obolibrary.org/obo/MONDO_').
:- rdf_register_prefix('RO','http://purl.obolibrary.org/obo/RO_').

doall :-
        generate_patterns([min(5),
                           dir(tmp),
                           trim(true),
                           base('http://purl.obolibrary.org/obo/myest'),
                           exclude_prefixes(['UBERON'])
                          ]).
