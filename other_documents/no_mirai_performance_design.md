# Viewer performance without new dependencies

## Goal

Improve Viewer startup and interactive latency from PR #153 without adding
packages or changing asynchronous execution semantics.

## Constraints

- Keep `DESCRIPTION`, `NAMESPACE`, environment files, and lock files unchanged.
- Do not use `mirai`, `promises`, background workers, or process-wide CRB/R6
  caches.
- Preserve current errors, progress reporting, and per-session isolation.
- Keep generated Viewer runtime code under `inst/`.

## Design

1. Keep hidden outputs suspended until their owning sidebar tab is visited,
   then restore their existing `suspendWhenHidden = FALSE` behavior.
2. Use `debounceEventAfterFirst()`: the first event is delivered immediately;
   later events debounce before evaluating the expensive reactive.
3. Fetch all requested RGB genes with one `getExpressionMatrix()` call in Gene
   Expression, Spatial, and Linked Views, then split the returned rows locally.
4. Cache rotated full Spatial extents within a session. The key includes the
   selected data set, Spatial section, and rotation.

## Phase 2 design

1. Dedicated Overview, Gene Expression, and Spatial pages request only their
   specialist payload. They no longer mark Linked Views as visible or build its
   full bundle. A specialist-only cell index is invalidated by the dataset
   fingerprint before it is reused.
2. Continuous-colour paint order and clipping caches keep one entry per field
   instead of one global slot, so a multi-gene redraw does not evict the
   previous panel's calculation.
3. Pointer-driven pan, orbit, and lasso redraws are coalesced to one browser
   animation frame. Final mouse-up state is still rendered synchronously.
4. Projection hover HTML is built only for cells that the specialist view can
   display. Overview, Gene Expression, and Spatial sampling use integer row
   indices and perform one sample operation without copying the metadata frame.
5. Gene-set means use the existing block-expression API and backend-aware
   matrix operations instead of materialising a dense gene-by-cell matrix.

## Non-goals

- No cross-session data sharing.
- No asynchronous cancellation or worker lifecycle.
- No speculative preload of hidden Linked Views.
- No WebGL renderer, binary transport, level-of-detail system, or expression
  backend format migration in this phase.
- No new public API.

## Error handling

Existing validation and `tryCatch()` behavior remains authoritative. Batched
expression reads must preserve missing-gene and missing-cell handling and return
the same values and ordering as the current per-gene calls.

## Verification

- Add focused tests that fail before each optimization.
- Run the focused Viewer tests first, then the full test suite.
- Confirm dependency and environment files have no diff.
- Compare the number of expression backend reads and first-render debounce
  behavior against PR #153.
- Compare the specialist first-open journey, 100k-cell hover construction,
  multi-gene redraw preparation, and sparse gene-set aggregation before and
  after the phase 2 changes.
