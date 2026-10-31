# Builder and Viewer integration

Builder uses the package export functions to produce CRB datasets and standalone
Viewer apps. The Viewer consumes the resulting content and coordinate contracts;
it does not require a running Builder session. See the
[Builder guide](../vignettes/build_a_data_set_by_pointing.Rmd) for the user workflow
and [project persistence](builder-projects.md) for snapshot and recovery behavior.

## Runtime boundaries

Seurat objects are opened and processed in isolated `callr` workers. The parent
Shiny session holds configuration and lightweight previews. This keeps object
loading and export work off the UI process. Viewer background work continues to
use its own `mirai` runtime.

Generated apps contain the Viewer runtime and dataset resources they need, so
they can run without the CerebroNexus package installed. Shared runtime contracts
are copied into the app; contract drift tests check the package and bundled
copies together.

## Identity and coordinate contracts

- Dataset identities are separate from display labels and source paths. Two
  dataset identities may refer to the same CRB without being collapsed.
- Projection coordinates must follow canonical cell order. Only explicitly
  verified canonical projections bypass barcode alignment; equal row counts
  alone are insufficient. Aligned coordinates are reused by the Viewer.
- Builder's verification cache includes the dataset catalog as well as backend
  and spatial metadata, so reuse preserves the Viewer's startup information.
- Spatial content carries ROI scope, image alignment, point appearance and
  initial projection settings through the shared export contracts. Embedded
  images already calibrated into coordinate space retain `geometryBaked`, which
  prevents applying their transform twice.

## Worker and publication ownership

Worker cleanup verifies the owned process through `processx` before and after
obtaining a fresh `ps` handle. Start-time estimates from the two libraries are
not compared for exact equality. Descendant ownership is checked before
termination.

Project and release identities use raw-input MD5 digests. File publication uses
buffered copying followed by destination verification, avoiding whole-file
materialization in memory. These digests identify content; authentication keys
instead use operating-system entropy and fail if secure randomness is unavailable.

## Verification entry points

- `tests/testthat/test-builder-integration.R` covers shared export and Viewer
  contracts, including verification-cache reuse and spatial coordinates.
- `tests/testthat/test-bundle-path-contract-drift.R` checks bundled contract
  consistency.
- `tests/testthat/test-builder-end-to-end.R` exercises export, relocation and
  generated-app behavior across supported storage and content combinations.
- `scripts/check-installed-builder.R` checks installed example discovery.
- `tests/testthat/test-viewer-linux-launcher.R` checks Linux deployment
  permissions and resolves resources in both source and installed layouts.

Run the complete source tests and installed-package checks using the commands in
[tests/README.md](../tests/README.md). Focused test results do not replace the
full Nix CI checks or establish million-cell performance guarantees.
