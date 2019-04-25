# Notes

## General ideas

- Frame presentation in terms of a planning scenario.
- Provide access to systematic findings of all (?) sample tables for the DVRPC region and others, if applicable.
- Would be interesting to create some sort of index to rank relative data quality.
- Consider highlighting outliers (abnormally high CVs), either the the same geographies over time or each geography compared to its neighbors or the region. What happens to map accuracy if outliers are removed?
- Is it feasible to compute the median CV? To remove outliers and recalculate the median CV?

[Sallie Keller talk: Building resilient communities](http://ww2.amstat.org/meetings/sdss/2018/onlineprogram/ViewPresentation.cfm?file=304291.pdf)

Sample use cases of ACS data:

- Locating and describing a population
- Forecasting future needs
- Evaluating a program, policy, or SOP

## Caveats and cautions

- **Edge effects.** Any applications of spatial link matrices must include spatial units adjacent to the study area and clip to the study area later.
- A data quality metric would have to dynamically adjust to collapsing subgroups, aggregating fields, aggregating geometries, and the like.
- Does one need to consider anything other than the CV? Why fix it if it ain't broke?

## Existing ACS-relevant reliability references

- Data-driven regionalization (Seth Spielman)
- Static mapping of uncertainty (Joe Francis)
- Quantifying the expected error of maps (Joseph Salvo and Joel Alvarez)
- "Touch method" (uses a first-order queen contiguity matrix) (Ken Hodges)
- NYC FactFinder provides a "heads-up display" of data quality by greying out unreliable values.

## Tests and progress

- [x] Defining an "outlier" observation temporally. *Findings*: in a test run with ACS B08101 (Means of Transpo to Work: Transit), only the most stupendous census tracts don't have a problem with wildly fluctuating CVs. It would take several years of data to determine what tracts are least reliable over time.
- [x] Defining an "outlier" observation spatially. *Findings*: It is simple but effective to construct a spatial link matrix and identify those census tracts that have a large percentage difference from their neighbors. Attempts to use local spatial autocorrelation were a flop.
- [x] Automated reports on data reliability created with R Markdown. Outliers are flagged spatially if they're considerably higher than their neighbors; there's also a set of arbitrary geography-specific CV thresholds. These refinements can be included later. The relative CV cutoffs are useful as one moves into smaller geographies and more detailed crosstabs because they eliminate the biggest CV culprits while allowing one to preserve most of the study area.
- [x] Mean and median CVs; changes with table type, geography, and variable detail.
- [x] Updates to Map Reliability Calculator to increase user-friendliness.

## Next steps

- [ ] Changes in map accuracy with dropped outliers (we know the story for geography and variable detail).
- [ ] CV Viewer
