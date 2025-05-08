# Ants on flowers: a meta-analysis exploring the ecological cost of protective ants to pollination

Authors: Amanda Vieira da Silva¹,², Anselmo Nogueira¹, Judith Bronstein³, Pedro Rey Zamora4, Laura Carolina Leal5

Affiliation: ¹Centro de Ciências Naturais e Humanas, Universidade Federal do ABC, São Paulo, Brazil. ²Ecology and Evolutionary Biology Department, University of Michigan, Michigan, USA. ³Ecology and Evolutionary Biology Departament, University of Arizona, Arizona, USA. 4Departamento de Biología Animal, Biología Vegetal y Ecología, Universidad de Jaén, Jaén, Spain. 5Departamento de Ecologia e Biologia Evolutiva, Universidade Federal de São Paulo, São Paulo, Brazil.

E-mail: amanda.vieira@ufabc.edu.br


----------------------------------------------------------------------------------------------
# GENERAL INFORMATION

### Type of the study: meta-analysis

### Date of data collection: last time updated on September 2022

### Information about geographic location of data collection: global

### Language information: English

### Information about funding sources that supported the collection of the data: Amanda Vieira da Silva thank São Paulo Research Foundation for the scholarship (FAPESP/grant number 2020/11171-4). Anselmo Nogueira thank São Paulo Research Foundation (FAPESP/grant number 2019/19544-7) 

### Data on Dryad: 10.5061/dryad.6wwpzgn9f

### Published: Journal of Ecology

----------------------------------------------------------------------------------------------
# DATA AND FILE OVERVIEW

meta.xlsx

### Description
Sheet1 (data): data used to run all analysis
Sheet2 (metadata): description of the data used to run the analysis

### Date that the file was created: 2021

### Date(s) that the file(s) was updated (versioned) and the nature of the update(s): September 2022 (update search and inclusion of three studies)

map.xlsx

### Description
Sheet1 (data): data used to run the map
Sheet2 (metadata): description of the data used to run the map

### Date that the file was created: 2021

### Date(s) that the file(s) was updated (versioned) and the nature of the update(s): September 2022 (update search and inclusion of three studies)

----------------------------------------------------------------------------------------------
# SHARING AND ACCESS INFORMATION

### Links to publications

----------------------------------------------------------------------------------------------
# METHODOLOGICAL INFORMATION

### Description of methods for data collection: we used keywords (desribed above) to find papers about the effect of ants on pollination on Web of Science and Scopus.

### Keywords used to search studies: (“(ant-plant OR myrmecoph*) AND (pollina* OR robbe* OR thie* OR larcen*)”. Based on this query, we used litsearchR R package to find more queries: (1) “(extrafloral nectar*) AND (pollina* OR robbe* OR thie* OR larcen*)”, (2) “(ant-pollinator conflict*) AND (pollina* OR robbe* OR thie* OR larcen*)”, and (3) “(ant-plant mutualis* OR ant-plant interact*) AND (pollina* OR robbe* OR thie* OR larcen*)”.

### Articles included in the meta-analysis: 28

### Description of methods used for data processing: our searches resulted in approximately 1200 papers. We used litsearchR v. 0.4.1 to remove duplicates (N = 635) and we removed manually more 32 duplicated not detected by litsearchR. To be included in the meta-analysis, the paper must satisfy six eligibility criteria: (1) have treatments with and without ants, (2) there must be a spatial and temporal overlap between ant and flower visitors foraging, (3) experimental or observational study (no theoretical modelling), (4) if the paper evaluated density or abundance of ants, it must report the effect of ant regardless of density or abundance, (5) have no insects secreting honeydew in addition to extrafloral nectaries, (6) plants should bear only extrafloral nectaries as a source of attraction for ants (no housing).

### Author contributions: Amanda Vieira da Silva designed the study, collected the data, performed statistical analysis, and wrote the first draft of the manuscript. Laura C. Leal and Anselmo Nogueira designed the study, interpreted the data and revised drafts of the manuscript. Judith Bronstein and Pedro Rey Zamora interpreted the data and revised drafts of the manuscript. All authors contributed equally to the final version of the manuscript.


----------------------------------------------------------------------------------------------
# DATA INFORMATION

EXCEL SHEETS 

meta.xlsx

### Count of number of variables, and number of cases or rows
Number of variables: 21
Number of observations: 140

### Variable list, including full names and definitions (spell out abbreviated words) of column headings for tabular data
* ID: Unique identification for each study	
* author: Authors of the original study	
* year: Year when the study was published	
* plant_sp: Plant species used in the study	
* plant_genus: Plant genera used in the study	
* plant_fam: Plant family used in the study	
* ant_presence: If the authors applied tanglefoot or if ants were naturally absent (tanglefoot or naturally_absent)
* EFN_position: Exact location of the EFNs within plants	
* resource_location: Categorical description of EFN location within plants( reproductive or non-reproductive)
* functional_group: Functional group of flower visitors	(bee, lepidoptera, diptera, wasp or NA)
* measure_study: The metric used as described in the study	
* measure_type: Categorical description of the measure used in the study (plant fitness or visitation)
* mean_with_ants: Mean value from the treatment with ants	
* mean_without_ants: Mean value from the treatment without ants	
* sd_with_ants: Standard deviation value from the treatment with ants	
* sd_without_ants: Standard deviation value from the treatment without ants	
* n_with_ants: Sample size from the treatment with ants	
* n_without_ants: Sample size from the treatment with ants	
* source: In which part of the study the effects were reported	
* yi: Hedge's g effect size	
* vi: Variation of the Hedge's g effect size	


### Definitions for codes used to record missing data
NA

map.xlsx

### Count of number of variables, and number of cases or rows
Number of variables: 3
Number of observations: 31

### Variable list, including full names and definitions (spell out abbreviated words) of column headings for tabular data
* ID: Unique identification for each study
* latitude: Latitude of the place where the study was conducted
* longitude: Longituded of the place where the study was conducted


### Definitions for codes used to record missing data
NA

R SCRIPTS

1effectsizes.R: calculate Hedges' g effect size used in the meta-analysis
2descriptive.R: calculate all descriptive data presented in the results section, such as number of studies included in each analysis
2map.R: map including all locations where original studies were conducted
4AntPolConflict.R: meta-analysis
5Sup_analysis.R: meta-analysis including outlier
6heterogeneity.R: calculate heterogeneity (I2)
7bias.R: calculate temporal and publication biases
8sensitivity.R: sensibility analysis



