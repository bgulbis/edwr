% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_morph_eq.R
\name{calc_morph_eq}
\alias{calc_morph_eq}
\title{Calculate morphine equivalents}
\usage{
calc_morph_eq(x)
}
\arguments{
\item{x}{data_frame}
}
\value{
data_frame
}
\description{
This takes a data frame with columns: med, med.dose, med.dose.units, and
route and will convert the med.dose to the morphine equivalent.
}
\references{
Lexicomp - Opiod Agonist Conversion (from drug, to Morphine IM);
https://www.cms.gov/Medicare/Prescription-Drug-Coverage/PrescriptionDrugCovContra/Downloads/Opioid-Morphine-EQ-Conversion-Factors-March-2015.pdf (note, doses listed in oral mme, converted to iv mme by * 0.3);
http://clincalc.com/opioids/
}
