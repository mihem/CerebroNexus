# Attribution — demo_hla_tcr_dextramer.crb

The single cells, paired αβ TCR sequences, transcriptomes and pMHC dextramer
calls bundled in `demo_hla_tcr_dextramer.crb` are derived from the public 10x Genomics
data sets *CD8+ T cells of Healthy Donor 1–4* (2019):

- <https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-1-1-standard-3-0-2>
- <https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-2-1-standard-3-0-2>
- <https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-3-1-standard-3-0-2>
- <https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-4-1-standard-3-0-2>

Published as:

- Zhang W, Hawkins PG, He J, Gupta NT, Liu J, Choonoo G, Jeong SW, Chen CR,
  Dhanik A, Dillon M, Deering R, Macdonald LE, Thurston G, Atwal GS.
  *A framework for highly multiplexed dextramer mapping and prediction of T cell
  receptor sequences to antigen specificity.*
  **Science Advances** 7(20):eabf5835 (2021).
  <https://doi.org/10.1126/sciadv.abf5835>

Distributed by 10x Genomics under the **Creative Commons Attribution 4.0
International (CC-BY 4.0)** license
(<https://creativecommons.org/licenses/by/4.0/>).

**Note on the HLA typing in this file.** The donor genotypes are the ones
published in table S1 of the supplementary material of the paper cited above,
stored with `source_type = "genotyped"`. The repertoire itself is
antigen-selected (cells were sorted for dextramer binding), which the object
declares in `technical_info$tcr_selection`.

This file accompanies the installed demo so the CC-BY 4.0 attribution travels
with the data (the full provenance and build steps live in `data-raw/DATASETS.md`
and `vignettes/hla_tcr_antigen_selected.Rmd` in the source repository, which
are not part of the installed package).
