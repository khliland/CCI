#' Protein signaling dataset
#'
#' A widely used dataset from flow cytometry experiments on human T-cells,
#' originally published by \insertCite{Sachs2005;textual}{CCI}.
#' The data records the expression levels of multiple proteins and phospholipids
#' in single cells, under different experimental interventions. It has been
#' frequently used as a benchmark dataset in the causal discovery literature.
#'
#' @format A data frame with 7466 rows and 11 variables:
#' \describe{
#'   \item{PKC}{Protein kinase C}
#'   \item{PKA}{Protein kinase A}
#'   \item{p38}{Mitogen-activated protein kinase p38}
#'   \item{JNK}{c-Jun N-terminal kinase}
#'   \item{ERK}{Extracellular signal-regulated kinase}
#'   \item{Akt}{Protein kinase B (Akt)}
#'   \item{Raf}{Raf proto-oncogene serine/threonine-protein kinase}
#'   \item{Mek}{Mitogen-activated protein kinase kinase}
#'   \item{PIP2}{Phosphatidylinositol 4,5-bisphosphate}
#'   \item{Plcg}{Phospholipase C gamma}
#'   \item{PIP3}{Phosphatidylinositol (3,4,5)-trisphosphate}
#' }
#'
#' @details
#' - Each row corresponds to a single cell measurement.  
#' - Values are continuous protein expression levels (fluorescence intensities).  
#' - The dataset has been used extensively to test causal discovery algorithms.  
#'
#' @source \doi{10.1126/science.1105809}
"protein_data"


#' Example dataset: HardCase
#'
#' A dataset containing simulated conditional independence test results.
#'
#' @format A data frame with 500 rows and 3 variables:
#' \describe{
#'   \item{X}{Numeric vector}
#'   \item{Y}{Numeric vector}
#'   \item{Z1}{Conditioning variable}
#'   \item{Z2}{Conditioning variable}
#' }
#' @source Simulated data.
"HardCase"


#' Example dataset: NormalData
#' 
#' A dataset containing simulated data from a multivariate normal distribution.
#' 
#' @format A data frame with 400 rows and 4 variables:
#' \describe{
#'  \item{X}{Numeric vector}
#'  \item{Y}{Numeric vector}
#'  \item{Z1}{Conditioning variable}
#'  \item{Z2}{Conditioning variable}
#'  }
#'  @source Simulated data.
"NormalData"

#' Example dataset: PolyData
#' 
#' A dataset containing simulated data from a polynomial relationship.
#' 
#' @format A data frame with 600 rows and 4 variables:
#' \describe{
#' \item{X}{Numeric vector}
#' \item{Y}{Numeric vector}
#' \item{Z1}{Conditioning variable}
#' \item{Z2}{Conditioning variable}
#' }
#' @source Simulated data.
"PolyData"

#' Example dataset: PoissonNoise
#' 
#' A dataset containing simulated data from a Poisson distribution.
#' 
#' @format A data frame with 1000 rows and 4 variables:
#' \describe{
#' \item{X}{Numeric vector}
#' \item{Y}{Numeric vector}
#' \item{Z1}{Conditioning variable}
#' \item{Z2}{Conditioning variable}
#' }
#' @source Simulated data.
"PoissonNoise"

#' Example dataset: NonLinNormal
#' 
#' A dataset containing simulated data from a non-linear transformation of a multivariate normal distribution.
#' 
#' @format A data frame with 500 rows and 4 variables:
#' \describe{
#' \item{X}{Numeric vector}
#' \item{Y}{Numeric vector}
#' \item{Z1}{Conditioning variable}
#' \item{Z2}{Conditioning variable}
#' }
#' @source Simulated data.
"NonLinNormal"

#' Example dataset: NonLinearCategorization
#' 
#' A dataset containing simulated data from a non-linear transformation followed by categorization.
#' 
#' @format A data frame with 600 rows and 3 variables:
#' \describe{
#' \item{X}{Numeric vector}
#' \item{Y}{Numeric vector}
#' \item{Z}{Conditioning variable}
#' }
#' @source Simulated data.
"NonLinearCategorization"












