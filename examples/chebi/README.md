# CHEBI results

The contents of this directory are the results of running OWL patternizer on a subset of CHEBI

This is executed with the following pre-processing steps:

 * extract a subset of chebi using GO as seed, using BOT and all axioms
 * `mutate_chebi` (axiomatizes charges)
 * `infer_axioms` (infer logical definitions from lexical patterns and structure)

See the directory for details. This README provides a hand-written summary of the results.

Note patternizer will produce some mix of generic patterns (with `X`s
indicating variables in the pattern name) and specific patterns (where
`X` is replaced by a hard-wired class). These should classify under
one another. The ontology manager can choose whether to go with morre
generic patterns or more specific ones.

An advantage of more specific patterns is that auto-generated text
definitions and synonyms look less generic.

For more on the approach, see the [parent dir](https://github.com/cmungall/owl_patternizer).

Note this is a work in progress, there are bugs to fix.

## Conjugate bases and charges

### Generic anions and cations

Example inferred logical def:

```
'L-cystine anion' EquivalentTo
 anion
  and ('is conjugate base of' some L-cystine)
```

See:

 * [anion_is_conjugate_base_of_X.yaml](anion_is_conjugate_base_of_X.yaml) - DP
 * [anion_is_conjugate_base_of_X.tsv](anion_is_conjugate_base_of_X.tsv) - derived tuples

Note that we infer both a text definition pattern:

```
def:
  # Induced, frequency=0.05921052631578947, p=http://purl.obolibrary.org/obo/IAO_0000115 
  text: "An alpha-amino-acid anion that is the conjugate base of %s, arising from deprotonation of the carboxy group."
  vars:
    - v0
```

as well as a synonym pattern:

```
annotations:
  - annotationProperty: exact_synonym
    # Induced p=exact_synonym 
    text: "%sate"
    vars:
      - v0
  - annotationProperty: related_synonym
    # Induced p=related_synonym 
    text: "%s anion"
    vars:
      - v0
```

this is in addition to the logical definition pattern:

```
equivalentTo:
  text: "'anion' and ('is conjugate base of' some %s)"
  vars:
    - v0
```

Examples in derived tuple form:

|defined_class|defined_class_label|v0|v0_label^M|
|---|---|---|---|
|http://purl.obolibrary.org/obo/CHEBI_32650|L-asparaginate|http://purl.obolibrary.org/obo/CHEBI_17196|L-asparagine^M|
|http://purl.obolibrary.org/obo/CHEBI_63163|L-cystine anion|http://purl.obolibrary.org/obo/CHEBI_16283|L-cystine^M|
|http://purl.obolibrary.org/obo/CHEBI_32665|L-glutaminate|http://purl.obolibrary.org/obo/CHEBI_18050|L-glutamine^M|
|http://purl.obolibrary.org/obo/CHEBI_58319|coenzyme M(1-)|http://purl.obolibrary.org/obo/CHEBI_17905|coenzyme M^M|
|http://purl.obolibrary.org/obo/CHEBI_62074|3-chloroacrylate|http://purl.obolibrary.org/obo/CHEBI_19982|3-chloroacrylic acid^M|



### Ions with a specific charge

The infer_axioms step will infer axioms such as:

```
'citrate(1-)' EquivalentTo
  'citrate anion'
     and (charge_state some '-1 ion')
```

From classes like this, we infer the following dosdp yamls:

 * [X_charge_state_X.yaml](X_charge_state_X.yaml)
    * [X_charge_state__1_ion.yaml](X_charge_state__1_ion.yaml)
    * [X_charge_state__2_ion.yaml](X_charge_state__2_ion.yaml)
    * [X_charge_state__3_ion.yaml](X_charge_state__3_ion.yaml)
    * [X_charge_state__4_ion.yaml](X_charge_state__4_ion.yaml)

Note that when turning these into blessed DPs, we have the option of either choosing

There seems to be a bug in dosdp-tools that prevents the tsv from being populated...

### Zwitterions

E.g.

```
'cysteine zwitterion' EquivalentTo
zwitterion
 and ('is tautomer of' some cysteine)
```

|defined_class|defined_class_label|v0|v0_label|v1|v1_label^M|
|---|---|---|---|---|---|
|http://purl.obolibrary.org/obo/CHEBI_58359|L-glutamine zwitterion|http://purl.obolibrary.org/obo/CHEBI_27369|zwitterion|http://purl.obolibrary.org/obo/CHEBI_18050|L-glutamine^M|
|http://purl.obolibrary.org/obo/CHEBI_57595|L-histidine zwitterion|http://purl.obolibrary.org/obo/CHEBI_27369|zwitterion|http://purl.obolibrary.org/obo/CHEBI_15971|L-histidine^M|
|http://purl.obolibrary.org/obo/CHEBI_58199|L-homocysteine zwitterion|http://purl.obolibrary.org/obo/CHEBI_27369|zwitterion|http://purl.obolibrary.org/obo/CHEBI_17588|L-homocysteine^M|
|http://purl.obolibrary.org/obo/CHEBI_57476|L-homoserine zwitterion|http://purl.obolibrary.org/obo/CHEBI_27369|zwitterion|http://purl.obolibrary.org/obo/CHEBI_15699|L-homoserine^M|
|http://purl.obolibrary.org/obo/CHEBI_58045|L-isoleucine zwitterion|http://purl.obolibrary.org/obo/CHEBI_27369|zwitterion|http://purl.obolibrary.org/obo/CHEBI_17191|L-isoleucine^M|
|http://purl.obolibrary.org/obo/CHEBI_57959|L-kynurenine zwitterion|http://purl.obolibrary.org/obo/CHEBI_27369|zwitterion|http://purl.obolibrary.org/obo/CHEBI_16946|L-kynurenine^M|

 * [X_is_tautomer_of_X.yaml](X_is_tautomer_of_X.yaml)
 * [X_is_tautomer_of_X.tsv](X_is_tautomer_of_X.tsv)


## Roles

Example inferred logical def:

```
'corticosteroid hormone' EquivalentTo
 corticosteroid
  and ('has role' some hormone)
```

 * X_has_role_X [yaml](X_has_role_X.yaml)
 * X_has_role_X [tsv](X_has_role_X.tsv)

|defined_class|defined_class_label|v0|v0_label|v1|v1_label^M|
|---|---|---|---|---|---|
|http://purl.obolibrary.org/obo/CHEBI_64600|C21-steroid hormone|http://purl.obolibrary.org/obo/CHEBI_61313|C21-steroid|http://purl.obolibrary.org/obo/CHEBI_24621|hormone^M|
|http://purl.obolibrary.org/obo/CHEBI_36699|corticosteroid hormone|http://purl.obolibrary.org/obo/CHEBI_50858|corticosteroid|http://purl.obolibrary.org/obo/CHEBI_24621|hormone^M|
|http://purl.obolibrary.org/obo/CHEBI_87200|dicarboximide antifungal agent|http://purl.obolibrary.org/obo/CHEBI_35356|dicarboximide|http://purl.obolibrary.org/obo/CHEBI_35718|antifungal agent^M|
|http://purl.obolibrary.org/obo/CHEBI_87195|dicarboximide fungicide|http://purl.obolibrary.org/obo/CHEBI_35356|dicarboximide|http://purl.obolibrary.org/obo/CHEBI_24127|fungicide^M|
|http://purl.obolibrary.org/obo/CHEBI_60951|flavonoid phytoalexin|http://purl.obolibrary.org/obo/CHEBI_72544|flavonoids|http://purl.obolibrary.org/obo/CHEBI_26115|phytoalexin^M|


## Functional Parent

|defined_class|defined_class_label|v0|v0_label|v1|v1_label^M|
|---|---|---|---|---|---|
|http://purl.obolibrary.org/obo/CHEBI_84135|L-serine derivative|http://purl.obolibrary.org/obo/CHEBI_26649|serine derivative|http://purl.obolibrary.org/obo/CHEBI_17115|L-serine^M|
|http://purl.obolibrary.org/obo/CHEBI_84189|L-threonine derivative|http://purl.obolibrary.org/obo/CHEBI_26987|threonine derivative|http://purl.obolibrary.org/obo/CHEBI_16857|L-threonine^M|
|http://purl.obolibrary.org/obo/CHEBI_27177|L-tyrosine derivative|http://purl.obolibrary.org/obo/CHEBI_84144|L-phenylalanine derivative|http://purl.obolibrary.org/obo/CHEBI_17895|L-tyrosine^M|
|http://purl.obolibrary.org/obo/CHEBI_22480|amino disaccharide|http://purl.obolibrary.org/obo/CHEBI_22483|amino oligosaccharide|http://purl.obolibrary.org/obo/CHEBI_36233|disaccharide^M|
|http://purl.obolibrary.org/obo/CHEBI_22529|amino sugar phosphate|http://purl.obolibrary.org/obo/CHEBI_28963|amino sugar|http://purl.obolibrary.org/obo/CHEBI_26078|phosphoric acid^M|
|http://purl.obolibrary.org/obo/CHEBI_59412|amino tetrasaccharide|http://purl.obolibrary.org/obo/CHEBI_22483|amino oligosaccharide|http://purl.obolibrary.org/obo/CHEBI_50126|tetrasaccharide^M|
|http://purl.obolibrary.org/obo/CHEBI_59266|amino trisaccharide|http://purl.obolibrary.org/obo/CHEBI_22483|amino oligosaccharide|http://purl.obolibrary.org/obo/CHEBI_27150|trisaccharide^M|

 * [X_has_functional_parent_X.yaml](X_has_functional_parent_X.yaml)
 * [X_has_functional_parent_X.tsv](X_has_functional_parent_X.tsv)

## Has part

|defined_class|defined_class_label|v0|v0_label|v1|v1_label^M|
|---|---|---|---|---|---|
|http://purl.obolibrary.org/obo/CHEBI_33296|alkali metal molecular entity|http://purl.obolibrary.org/obo/CHEBI_23367|molecular entity|http://purl.obolibrary.org/obo/CHEBI_22314|alkali metal atom^M|
|http://purl.obolibrary.org/obo/CHEBI_35479|alkali metal salt|http://purl.obolibrary.org/obo/CHEBI_24866|salt|http://purl.obolibrary.org/obo/CHEBI_22314|alkali metal atom^M|
|http://purl.obolibrary.org/obo/CHEBI_36902|chalcogen hydride|http://purl.obolibrary.org/obo/CHEBI_33692|hydrides|http://purl.obolibrary.org/obo/CHEBI_33303|chalcogen^M|
|http://purl.obolibrary.org/obo/CHEBI_33304|chalcogen molecular entity|http://purl.obolibrary.org/obo/CHEBI_23367|molecular entity|http://purl.obolibrary.org/obo/CHEBI_33303|chalcogen^M|
|http://purl.obolibrary.org/obo/CHEBI_33484|chalcogen oxoacid|http://purl.obolibrary.org/obo/CHEBI_24833|oxoacid|http://purl.obolibrary.org/obo/CHEBI_33303|chalcogen^M|
|http://purl.obolibrary.org/obo/CHEBI_33485|chalcogen oxoanion|http://purl.obolibrary.org/obo/CHEBI_35406|oxoanion|http://purl.obolibrary.org/obo/CHEBI_33303|chalcogen^M|
|http://purl.obolibrary.org/obo/CHEBI_23114|chloride salt|http://purl.obolibrary.org/obo/CHEBI_24866|salt|http://purl.obolibrary.org/obo/CHEBI_17996|chloride^M|


 * [X_has_part_X.yaml](X_has_part_X.yaml)
 * [X_has_part_X.tsv](X_has_part_X.tsv)
