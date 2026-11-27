# Attribution — Allen whole mouse brain MERFISH demo

`run-demo.R --data=allen` builds a Viewer data set from the Allen Brain Cell Atlas's MERFISH data of one whole adult mouse brain. **No data file is part of this repository or of the installed package**: the script downloads the sources into the user cache and builds the `.crb` and `.h5` files there.

## Source

- Atlas: Allen Brain Cell Atlas, data set `MERFISH-C57BL6J-638850`, release 20231215. Access notes: <https://alleninstitute.github.io/abc_atlas_access/>.
- Paper: Yao Z, van Velthoven CTJ, Kunst M, et al. *A high-resolution transcriptomic and spatial atlas of cell types in the whole mouse brain.* **Nature** 624:317–332 (2023). <https://doi.org/10.1038/s41586-023-06812-z>
- Files, from the public bucket `https://allen-brain-cell-atlas.s3.us-west-2.amazonaws.com/`. The script checks size and MD5 (the `size` and `file_hash` of the release manifest `releases/20231215/manifest.json`) after the download.

| File | Bytes | MD5 |
|---|---|---|
| `metadata/MERFISH-C57BL6J-638850/20231215/views/cell_metadata_with_cluster_annotation.csv` | 1,018,613,800 | `b4cddf29d092d5cb29f73af1cc156f7a` |
| `metadata/MERFISH-C57BL6J-638850/20231215/gene.csv` | 48,416 | `d9cee1242bc13374b5e2beeea827de3e` |
| `expression_matrices/MERFISH-C57BL6J-638850/20230830/C57BL6J-638850-log2.h5ad` | 7,627,589,574 | `0630f7a36d36ebd336cd8d0564b07b14` |

## Licence and redistribution

The atlas's data access notes give the licence of the data as **CC BY-NC 4.0** (attribution, non-commercial use); the Allen Institute's terms of use apply as well. The data are downloaded by each user from the Allen Institute and are not committed to this repository. Credit the Allen Institute and cite Yao et al. 2023 when showing views of this data set.

## What the Viewer data set contains, and what was changed

- **Cells.** The 3,938,808 cells of the cell table, the cells of the 59 coronal sections that passed the atlas's quality control and were mapped to its taxonomy. Each section is one spatial entry, named `Section <number>` after its number in the section series and ordered front to back by its position along the brain (`z`). The cells keep the section coordinates of the cell table, in millimetres; the y axis is mirrored so the section stands upright.
- **Expression.** The 550 genes of the panel, as the atlas's log2-normalised values (`C57BL6J-638850-log2.h5ad`), not changed. Genes are named by symbol; a symbol used twice keeps its Ensembl identifier. `nGene` counts the genes with a value above zero.
- **Groups.** `section`, and four levels of the taxonomy: `class`, `subclass`, `supertype` and `neurotransmitter`, each with the colours the atlas gives its terms; the data set opens coloured by `class`. Cells without a neurotransmitter are `N/A`. The 5,322 clusters are not kept as a group. `mapping_score` is the atlas's `average_correlation_score`; `z` is the section's position.
- **Embedding.** The atlas publishes no embedding of the MERFISH cells. The `umap` projection is **computed here**: 30 principal components of the scaled genes, then UMAP.
