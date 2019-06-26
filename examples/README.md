# Auto-generated examples

All YAML in this folder was generated automatically from owl_patternizer. See [conf.pl](conf.pl) for configurations

## Highlighted

 * Pizza
    * [topping](pizza/Pizza_hasTopping_X.yaml)
 * Uberon
    * [skeleton of](uberon/X_skeleton_of_X.yaml)
 * SO
    * [has quality](so/X_has_quality_X.yaml)
 * SNOMED
    * [Left X](sctid/X_Laterality__attribute__Left__qualifier_value_.yaml)
    * [generic laterality](ctid/X_Laterality__attribute__X.yaml)
 * OBI
    * [T-cell assay](obi/biological_activity_assay_measuring_epitope_specific_cytokine_production_by_T_cells_has_specified_output_information_content_entity_is_about_X.yaml)
    * [assay about process](obi/X_has_specified_output_information_content_entity_is_about_X.yaml)
 * CHEBI
    * see [chebi/README.md](chebi/README.md)

## Induced Logical Definitions

The patternizer algorithm expects logical definitions for classes. Not all ontologies provide these. If the conf.pl file includes `infer_axioms(true)` then logical defs are induced. These are stored in a file `_induced_axioms.ttl`

These can then be merged in with the main ontology file.

Sometimes we also induce additional axioms if we suspect an ontologies axioms to be incomplete

Examples:

 * [apo](examples/apo/_induced_axioms.ttl)



