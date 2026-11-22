# Attribution — Kukanja et al. EAE and MS demos

`run-demo.R --data=eae` and `--data=ms` build two Viewer data sets from the data of Kukanja et al. 2024: mouse spinal cord in experimental autoimmune encephalomyelitis (EAE), measured by in situ sequencing (ISS), and human multiple sclerosis (MS) spinal cord, measured by Xenium. **No data file is part of this repository or of the installed package**: the script downloads the sources into the user cache and builds the `.crb` and `.h5` files there.

## Source

- Paper: Kukanja P, Langseth CM, Rubio Rodríguez-Kirby LA, et al. *Cellular architecture of evolving neuroinflammatory lesions and multiple sclerosis pathology.* **Cell** 187:1990–2009 (2024). <https://doi.org/10.1016/j.cell.2024.02.030>
- Data: Zenodo record 8037425, <https://zenodo.org/records/8037425>. Code: <https://github.com/Moldia/cellular-architecture-of-evolving-neuroinflammatory-lesions-and-multiple-sclerosis-pathology>.
- Files: one AnnData archive per data set. The script checks size and MD5 (as Zenodo lists them) after the download.

| Data set | File | Bytes | MD5 |
|---|---|---|---|
| `eae` | `EAE_rev_ingest_only_new_v4_with_images_tmap.h5ad.zip` | 567,853,835 | `2df533e6be0347839ffc074039242311` |
| `ms` | `MS_xenium_data_v5_with_images_tmap.h5ad.zip` | 511,633,139 | `da79033e5a4113184d471debe143e09f` |

## Licence and redistribution

The Zenodo record is published under **CC BY 4.0**. The data are downloaded by each user from Zenodo and are not committed to this repository. Credit the authors and cite the paper when showing views of these data sets.

## What the Viewer data sets contain, and what was changed

- **Cells.** All cells of the AnnData files: 592,421 cells of 52 mouse sections (`eae`) and 660,801 cells of 6 human sections (`ms`). Each section is one spatial entry, named by the authors' `sample_id` and in their order. The cells keep the authors' aligned coordinates (`obsm["xy_loc"]`), in which every section is turned the same way; the y axis is mirrored so the sections stand as in the paper's figures. The section images inside the files are not used.
- **Expression.** All genes of the panels (239 for `eae`, 266 for `ms`). The authors' counts (`X`) are stored as `log1p` of the counts per 10,000 per cell, so the Viewer's "log-normalised expression" pages apply; `nUMI` and `nGene` come from the counts.
- **Groups.** The authors' annotations under shorter names, each with the colours the file keeps for it where there are any (`uns["<column>_colors"]`, without the alpha channel). `eae`: `section` (`sample_id`), `condition`, `time`, `region`, `sex` (`SEX`), `compartment`, `cell_type_broad` (`Annotation 1 (broad)`), `cell_type` (`Annotation 3 (medium with DA)`), `cell_type_fine` (`Annotation 4 (high)`) and `disease_associated` (`DA`). `ms`: `section` (`sample_id`), `condition` (`type_spec`), `sex`, `compartment` (`compartment_2`), `cell_type_broad` (`Level1`), `cell_type` (`Level2`) and `cell_type_fine` (`Level3`). A cell without a label is `N/A`. Numeric columns kept: `area` and `distance_to_boundary_um` (`eae`), `cell_area` and `nucleus_area` (`ms`).
- **Embedding.** The authors' UMAP (`obsm["X_umap"]`), not changed.
