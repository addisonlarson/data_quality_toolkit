# Notes

## Potential Study Areas

### DVRPC

- PA Counties: Bucks, Chester, Delaware, Montgomery, Philadelphia
- NJ Counties: Burlington, Camden, Gloucester, Mercer

### NCTCOG

- TX Counties: Erath, Palo Pinto, Wise, Parker, Hood, Somervell, Denton, Tarrant, Johnson, Collin, Dallas, Ellis, Navarro, Hunt, Rockwall, Kaufman

### TPB: DC Metro

MD

- Counties: Frederick, Montgomery, Prince George, Charles
- Cities: Frederick, Gaithersburg, Rockville, College Park, Takoma Park, Laurel, Greenbelt, Bowie

DC

VA

- Counties: Loudoun, Prince William, Fairfax, Arlington
- Cities: Manassas, Manassas Park, Fairfax, Falls Church, Alexandria
- Urbanized Areas: Fauquier County

What would be the advantages or downsides of a comparative study? If there's a difference in the data quality, that's important to know -- after a couple of analyses, it'll be clear whether to stick with this or not.

## General ideas

- Frame presentation in terms of a planning scenario.
- Provide access to systematic findings of all (?) sample tables for the DVRPC region and others, if applicable.
- Would be interesting to create some sort of index to rank relative data quality.
- Consider highlighting outliers (abnormally high CVs), either the the same geographies over time or each geography compared to its neighbors or the region. What happens to map accuracy if outliers are removed?
- Is it feasible to compute the median CV? To remove outliers and recalculate the median CV?

## Caveats and cautions

- What about calibration and resolution? (Philip Tetlock)
- One could consider the issue of data quality to be relative to the use case. If you're comparing tracts to determine who gets funding for a trails project, then you want that data to be suitable for *comparative purposes*; you don't care about the ground truth of the values provided that the relative order is correct.
- What does data quality mean in the context of the ACS? The question is not one of completeness but (perhaps) one of usefulness.
- A data quality metric would have to dynamically adjust to collapsing subgroups, aggregating fields, aggregating geometries, and the like.
- Does one need to consider anything other than the CV? Why fix it if it ain't broke?

## Existing ACS-relevant reliability references

- Data-driven regionalization (Seth Spielman)
- Static mapping of uncertainty (Joe Francis)
- Quantifying the expected error of maps (Joseph Salvo and Joel Alvarez)
- NYC FactFinder provides a "heads-up display" of data quality by greying out unreliable values.

## First sweep: Googling for "rank data quality"

*What's out there?*

[The Data Quality Index](https://www.worldeconomics.com/pages/Data-Quality-Index.aspx)

You can't trust the GDP data countries give you. Factor in the "shadow economy" and "corruption" and your GDP values can differ significantly. This site allows you to set your priorities and the data quality will change accordingly. Similar approach as affordable housing prioritization project with CZ.

[ECB supervisory data quality framework, tools and products](https://www.bankingsupervision.europa.eu/press/conferences/sup_rep_conf/shared/pdf/2017/Data_quality_framework_tools_and_products.pdf)

On the subject of the European Central Bank and DQI, a good example of framing in terms of concepts and the metrics that measure each concept.

[Even more on the DQI](https://www.reg.tech/en/knowledge-hub/regulatory-topics/data-quality-index-dqi/)

Presentation buzzwords: Punctuality, accuracy, consistency, completeness, stability, plausibility.

[A review of data quality assessment methods for public health information systems](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4053886/)

Unlike most other data sources, we don't have to worry about the "completeness" of ACS data per se -- but does a value really exist if it's plus or minus 199%?

[Annoying industry white paper](https://siliconangle.com/files/2016/01/Blazent_State_of_Data_Quality_Management_2016.pdf)

- 8.5% of respondents say, "We don't manage quality, we hope for the best."
- Data attributes (aka presentation buzzwords): integrity, accuracy, consistency, validity, timeliness, completeness, accessibility, duplication.

[A data quality practical approach](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.675.5480&rep=rep1&type=pdf)

Ways to track the data through the ETL process (not relevant). But definitions of data quality are useful food for thought: "fitness for use?" "The distance between data views presented by an Information System and the same data in the Real World?"

[The Aggregate Data Quality score (ADQ)](https://academic.oup.com/ejcts/article/49/5/1470/2570882)

- "*Completeness.* This parameter measures the extent to which data are not missed and is of sufficient breadth and depth to describe the corresponding set of the real world. In the present analysis, it reflects the rate of present values on the 16 expected variables for all the records uploaded by each Unit. The formula used to calculate the completeness was: COM = 1 - ('null values'/total expected values for the Unit) x 100. The concept of 'null value' (that represents a missing or an out-of-scale value) was defined for each variable as reported in Table 1."
- "*Reliability.* This parameter measures the extent to which data are coherent with each other within the same record (elsewhere this was also indicated as cross-record consistency). In the present analysis, it reflects the rate of valid checks on the 9 checks defined and tested for all the records uploaded by each Unit. The formula used to calculate the completeness was: 1 - (valid controls/total possible controls for the Unit) x 100."
- ADQ = Rescaled COM + Rescaled REL, where the rescale is the *z* score of each observation.

[Sallie Keller talk: Building resilient communities](http://ww2.amstat.org/meetings/sdss/2018/onlineprogram/ViewPresentation.cfm?file=304291.pdf)

Sample use cases:

- Locating and describing a population
- Forecasting future needs
- Evaluating a program, policy, or SOP

Otherwise, "SDAL Synthetic Technology" allows for cool areal interpolation through population/housing unit imputation but is way too complicated for these purposes

## For later
- [An approach for managing data quality](http://www.canberra.edu.au/researchrepository/file/9474198c-c848-102a-3745-e1671cd29881/1/full_text.pdf)
- [`dlookr`, an R package for EDA, data summary, outlier removal, missing data imputation](https://cran.r-project.org/web/packages/dlookr/vignettes/diagonosis.html)

## Les autres

- [x] Defining an "outlier" observation temporally. *Findings*: in a test run with ACS B08101 (Means of Transpo to Work: Transit), only the most stupendous census tracts don't have a problem with wildly fluctuating CVs. It would take several years of data to determine what tracts are least reliable over time.

![alt text](https://github.com/addisonlarson/data_quality_toolkit/raw/master/figs/temporal.png "CV changes, 2011-2017")

- [x] Defining an "outlier" observation spatially. *Findings*: so far, it looks simple but effective to construct a spatial link matrix and identify those census tracts that have a large percentage difference from their neighbors. Attempts to use local spatial autocorrelation are a flop so far, but it's still worth thinking about.

Finally figured out how to plot a spatial link matrix!

![alt text](https://github.com/addisonlarson/data_quality_toolkit/raw/master/figs/first_order.png "First order spatial link matrix, queen contiguity")

The CV followed by three attempts to identify outliers.

![alt text](https://github.com/addisonlarson/data_quality_toolkit/raw/master/figs/cv.png "Percentage difference from neighbors") <!-- .element height="50%" width="50%" -->

![alt text](https://github.com/addisonlarson/data_quality_toolkit/raw/master/figs/sp_outlier.png "Percentage difference from neighbors") <!-- .element height="50%" width="50%" -->

![alt text](https://github.com/addisonlarson/data_quality_toolkit/raw/master/figs/first_moran.png "Local Moran's i p-values, first-order") <!-- .element height="50%" width="50%" -->

![alt text](https://github.com/addisonlarson/data_quality_toolkit/raw/master/figs/second_moran.png "Local Moran's i p-values, second-order") <!-- .element height="50%" width="50%" -->

One might ask why we don't simply set a maximum acceptable CV. Certainly it's worth looking into maximum acceptable ceilings, and for larger geos and less detailed variables every CV could theoretically be acceptable. These refinements can be included later. I am trying to create a way to benchmark *relative* data quality as one moves into smaller geographies and more detailed crosstabs, and eliminate the worst observations while allowing one to preserve most of the study area.

- [ ] Mean and median CVs; changes with geography, variable detail, and dropped outliers.
- [ ] Changes in map accuracy with dropped outliers (we know the story for geography and variable detail).
- [ ] CV Viewer
