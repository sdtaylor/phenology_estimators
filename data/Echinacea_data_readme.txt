This data under public domain license, published at this data repo

Waananen A, Kiefer G, Ison JL, Wagenius S (2018) Data from: Mating opportunity increases with synchrony of flowering among years more than synchrony within years in a nonmasting perennial. Dryad Digital Repository. https://doi.org/10.5061/dryad.487db24

for this paper

Waananen A, Kiefer G, Ison JL, Wagenius S (2018) Mating opportunity increases with synchrony of flowering among years more than synchrony within years in a nonmasting perennial. The American Naturalist 192(3): 379-388. https://doi.org/10.1086/698657

Dataset metadata below
--------------------------------------------------------

This folder contains contains the data and computer code files for the article:

Waananen A, Kiefer G, Ison JL, Wagenius S. Mating opportunity increases with synchrony of flowering among years more than synchrony within years in a non-masting perennial. Am. Nat.

This README file describes the contents of the 3 files, listed below, related to the data and analysis of reproductive timing of Echinacea angustifolia plants in an experimental plot in Douglas County, Minnesota, USA during the years 2005 - 2015:

1. 2005_2015_Echinacea_1996cohort_phenology_dataset.csv: This file contains information on all plants that flowered in the Echinacea Project main common garden experimental plot (C1) during the summers of 2005- 2015. Each row corresponds to one flowering plant. Columns are separated by commas. Descriptions of variables in this file are listed below.

2. Waananen_etAl_Synchrony_Analysis_and_Figures.R: This file includes all the R code and analysis and creating figures in Waananen, Kiefer, Ison, and Wagenius 2018. This file is organized into the following sections:
1. Read in Phenology Data
2. Calculate Individual Within and Among Year Synchrony and Long-Term Mating Potential
3. Regression Analysis of Within and Among Year Synchrony and Long-Term Mating Potential
4. Make Main Publication Figures
5. Calculate Population Synchrony Using Overlap (Among-Year) and MDE (Within-Year) models
6. Individual Synchrony Bootstrap Analysis
7. Fire Effects on Flowering

Sections 1-5 correspond to analyses included in the main publication and sections 6-7 contributed to the supplemental information (Appendix B).

3. Mateable_functions.R: This file is an R script that contains three functions that are required for the analysis in Waananen_etAl_Synchrony_Analysis_and_figures.R. These functions are adapted from functions in the R package ‘mateable’ (Wagenius et al. 2016).

Field data were collected by the authors and other members of Team Echinacea during each summer.

Associated datasets and further information about this common garden experiment are available on the Echinacea Project website: http://echinaceaproject.org/.

Contact Amy Waananen or Stuart Wagenius with questions about this dataset: amy.waananen@gmail.com, stuart.wagenius@gmail.com or, echinaceaproject@gmail.com

Below are descriptions of the variables in 2005_2015_Echinacea_1996cohort_phenology_dataset.csv, which accompanies Waananen, Kiefer, Ison, Wagenius (2018) "Mating opportunity increases with synchrony of flowering among years more than synchrony within years in a non-masting perennial.”


cgHdId	=	Unique identifier for each flowering head in the experimental plot. Factor.
startDtEarly	=	Earliest possible first day the plant shed pollen (as date). Missing data is represented by the date ‘1911-01-01’ or January 1st for the corresponding phenYear. Datetime.
startDtLate	=	Latest possible first day the plant shed pollen (as date). Missing data is represented by the date ‘1911-01-01’ or January 1st for the corresponding phenYear. Date.
endDtEarly	=	Earliest possible last day the plant shed pollen (as date). Missing data is represented by the date ‘1911-01-01’ or January 1st for the corresponding phenYear. Date.
endDtLate	=	Latest possible last day the plant shed pollen (as date). Missing data is represented by the date ‘1911-01-01’ or January 1st for the corresponding phenYear. Date.
phenYear	=	The year the plant was observed. (Integer)
cgPlaId		=	Unique identifier for each plant in the experimental plot. Factor.
phenNote	=	Any notes recorded in the field or data management.
Start, start date of flowering used for analysis (startDtEarly). Date.
End, end date of flowering used for analysis (endDtLate). Date
hdCt	=	Count of flowering heads per plant. Integer.
start.numeric, Start date as number of days since 1970-01-01. Integer.
Duration	=	Number of days an individual flowered. Integer.
Midpoint	=	Midpoint date of an individuals’ flowering period within a year (rounded to nearest date). Integer.
mid.yday	=	yday (i.e. Julian date) of individuals midpoint (see above)
DAM	=	number of days after median that an individual began flowering. Integer.
Z, the z-score of an individuals’ midpoint date of flowering. Numeric.
z.start	=	the z-score of an individuals’ start date of flowering. Numeric.
year.pop	=	Number of flowering individuals in the year the plant was observed. Integer.
year.pop.prop	=	Proportion of cohort that flowered in a given year. Numeric.


