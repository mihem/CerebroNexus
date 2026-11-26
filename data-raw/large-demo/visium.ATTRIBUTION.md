# Attribution — multi-section Visium demos for the Builder

`run-demo.R --data=dlpfc,mouse_brain,breast,prostate` prepares four public 10x Visium data sets, each with several tissue sections and the H&E photograph of every section, as input files for the Builder. **No data file is part of this repository or of the installed package**: the script downloads the sources into the user cache and writes one SingleCellExperiment file (`visium_<id>_sce.rds`) per data set there.

## Sources

| Id | Sections | Source | Licence |
|---|---|---|---|
| `dlpfc` | 12 (151507–151510, 151669–151676; donors Br5292, Br5595, Br8100) | Maynard KR, Collado-Torres L, et al. *Transcriptome-scale spatial gene expression in the human dorsolateral prefrontal cortex.* **Nature Neuroscience** 24:425–436 (2021). <https://doi.org/10.1038/s41593-020-00787-0>. Matrices and high-resolution pictures from `https://spatial-dlpfc.s3.us-east-2.amazonaws.com/` (`h5/`, `images/`); spot positions, scale factors and the manual layer labels from [LieberInstitute/HumanPilot](https://github.com/LieberInstitute/HumanPilot) at commit `044446d6`, folder `10X/` | No separate data licence is stated; the HumanPilot repository is Artistic-2.0. The authors ask to cite the article and `spatialLIBD`. |
| `mouse_brain` | 4 (sagittal anterior and posterior, serial sections 1 and 2) | 10x Genomics, Space Ranger 1.1.0 data sets `V1_Mouse_Brain_Sagittal_{Anterior,Posterior}[_Section_2]` | CC BY 4.0 (stated on the 10x data set pages) |
| `breast` | 2 (block A, sections 1 and 2) | 10x Genomics, Space Ranger 1.1.0 data sets `V1_Breast_Cancer_Block_A_Section_{1,2}` | CC BY 4.0 |
| `prostate` | 2 (adenocarcinoma with invasive carcinoma; normal) | 10x Genomics, Space Ranger 1.3.0 FFPE data sets `Visium_FFPE_Human_Prostate_Cancer`, `Visium_FFPE_Human_Normal_Prostate` | CC BY 4.0 |

10x files come from `https://cf.10xgenomics.com/samples/spatial-exp/<release>/<sample>/`: the filtered feature-barcode matrix (`.h5`) and the `spatial` folder archive (positions, scale factors, pictures). The script checks each file's size and MD5 (those of the files downloaded on 2026-11-20).

## What the Builder input contains, and what was changed

- **Spots.** The spots under tissue of every section, named `<section>_<barcode>`; `library_id` and `section` name the section. Coordinates are the full-resolution pixel positions (x = column, y = row), as in an AnnData file, so the Builder places each picture with the same vertical flip it uses for Visium data.
- **Pictures.** The high-resolution H&E picture of each section, unchanged, kept in `metadata(sce)$spatial_images` with its extent from `tissue_hires_scalef`. The Builder attaches every picture to its own section.
- **Expression.** The raw counts of the genes all sections share, and `logcounts`: `log1p` of the counts per 10,000 per spot.
- **Groups.** `section`, `cluster` (computed here) and, per data set, `donor` and `layer` (DLPFC: the published manual layer of each spot, L1–L6 and WM; spots without one are `unlabelled`), `region` (mouse brain) and `tissue` (prostate). `sample` repeats the donor (DLPFC), the region (mouse brain) or the section, so the Builder preselects it and the Spatial page can show one sample's sections; `orig.ident` holds the data set id. The Builder preselects `sample` and `cluster`; tick `layer`, `section`, `donor`, `region` or `tissue` there to colour by them.
- **Embedding.** None is published with the files. `PCA` and `UMAP` are **computed here** from 2,000 variable genes of the log-normalised counts of all sections together (30 principal components), and `cluster` is a shared-nearest-neighbour clustering of the same components (resolution 0.6). Sections are not batch-corrected.
