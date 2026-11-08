# Builder projects

CerebroNexus Builder can keep a multi-dataset build across sessions. A project
stores the source datasets, the choices made for each dataset, spatial image
settings, and CRB files that are safe to reuse.

The aim is simple: users should be able to stop after any dataset, return later,
and rebuild only the parts that changed.

## Project folder

After an import batch has completely settled, the first successful upload
offers to create a project folder. The automatic offer waits for browser,
server, and worker import queues to become idle; the toolbar action remains
available earlier when the user wants to choose a folder manually. Browser
uploads are copied out of the Shiny session's temporary directory so they
remain available after the app closes.

The native folder picker is synchronous on the host operating system. While it
is open, the background worker may continue processing, but Builder cannot
render new progress until the picker returns. This is a display pause rather
than a pause or restart of the import itself.

```text
my-project/
├── builder-project.json
├── sources/
│   └── <dataset-id>/...
├── attachments/
│   └── <dataset-id>/...
├── spatial-assets/
│   └── <dataset-id>/<fov>/...
├── artifacts/
│   └── <dataset-id>/...crb
└── checkpoints/
    └── <build-attempt>/...
```

`builder-project.json` is the working manifest. It records stable dataset IDs,
source locations, inspection summaries, user settings, checked state, and CRB
metadata. Supplementary table files live under `attachments/`; the manifest
stores only their managed path and selected sheet names. Managed paths are
relative to the project folder. Authentication secrets are not stored.

Schema version 3 also records the last safe workflow location and non-secret
Viewer/Build preferences (output mode, welcome text, initial page and dataset,
host/port, display and launch options, and whether login was requested). Login
accounts, usernames, passwords, tokens, active builds, worker snapshots, and
preview caches remain session-only. Older manifests are migrated in memory and
are written as version 3 on the next save.

The last UI location includes the active Spatial FOV and image label for the
selected dataset. These values are navigation state rather than dataset
configuration: reopening selects them when they still exist and otherwise
falls back to the first available FOV or image without changing saved image
alignment parameters.

During version-1 migration, each readable typed payload receives a new canonical
configuration digest. A reusable artifact that explicitly referenced the old
record digest is carried to the same new digest. The legacy digest is not used
as an integrity proof; source fingerprints and managed Spatial asset
fingerprints remain the integrity checks for restoring Checked state.

Project source restoration is isolated per dataset. If one saved payload cannot
be hydrated, Builder removes that incomplete live entry, retains its original
manifest record, releases the temporary worker snapshot, and continues restoring
the remaining datasets.

Spatial images are stored as verified project assets instead of large base64
strings inside the JSON manifest. Each image descriptor records its relative
path and fingerprint. Older manifests with inline image data are migrated on
their next save. Missing or changed image assets make the affected dataset
non-restorable instead of silently dropping its image.

## Independent states

A dataset does not have one linear status. Its configuration, source, runtime,
artifact, and release membership can change independently. For example, a
dataset can have a missing source but still have a valid CRB for publication.

```mermaid
stateDiagram-v2
    state "Configuration" as config {
        [*] --> Draft
        Draft --> Checked: user confirms settings
        Checked --> Changed: persistent setting changes
        Changed --> Checked: user checks again
    }

    state "CRB artifact" as artifact {
        [*] --> None
        None --> Building
        Building --> Ready
        Building --> Failed
        Building --> Interrupted
        Ready --> Stale: build identity changes
        Stale --> Building
        Failed --> Building: retry
        Interrupted --> Building: retry
    }

    state "Current session" as runtime {
        [*] --> Unloaded
        Unloaded --> Loading
        Loading --> Loaded
        Loading --> Error
        Loaded --> Unloaded
    }
```

`Checked` means that the user reviewed a configuration. `Ready` means that a CRB
was built from that configuration. A CRB is reusable only when its saved build
identity still matches the current dataset configuration and its files remain
available.

## Saving

Saving the project is deliberately separate from building CRBs. The manifest is
written before optional CRB preparation, so a long build cannot prevent the
user's work from being saved. Builder locks editing while it retains sources and
writes the manifest, and tells the user to keep the page open.

```mermaid
flowchart TD
    A[Save project] --> L[Lock editing and show Saving]
    L --> B[Materialize committed spatial settings]
    B --> C[Retain dataset sources, attachments, and Spatial image assets]
    C --> D[Write a new manifest atomically]
    D --> E{Checked datasets without a reusable CRB?}
    E -- No --> F[Project is safe]
    E -- Yes --> G[Offer Prepare checked CRBs]
    G --> H[Build datasets one at a time]
    H --> I[Register each successful CRB]
    I --> F
```

The manifest has a monotonic project revision. A save is rejected when another
Builder window has already written a newer revision. The previous manifest is
kept as `builder-project.json.bak` during replacement.

## Safe interaction

Builder derives every action from one activity state. The same capabilities
disable controls in the browser and reject stale requests on the server.

```mermaid
stateDiagram-v2
    [*] --> Clean
    Clean --> Dirty: settings or checked state changes
    Dirty --> Saving: save project
    Saving --> Clean: manifest committed
    Saving --> SaveFailed: copy or write fails
    Saving --> Conflict: newer manifest found
    SaveFailed --> Saving: retry
    Conflict --> Restoring: reopen project
    Clean --> Restoring: open saved project
    Restoring --> Clean: selected sources loaded
    Clean --> Registering: CRB build finishes
    Registering --> Clean: artifacts committed
    Registering --> SaveFailed: manifest write fails
```

Uploads may continue while the user edits a different loaded dataset. Save,
Checked, Review, and Build wait until both browser and server import queues are
empty. Restoring blocks project mutations but keeps failed-import retry and
remove actions available. Saving and artifact registration make the workspace
temporarily inert.

Unsaved changes, imports, builds, restores, and project writes request a browser
warning before the page closes. A disconnected browser fails closed until the
server state is synchronized again.

## Opening a project

Opening a project starts with metadata only. The recovery dialog lets the user
decide how much data should enter memory.

```mermaid
flowchart LR
    A[Open builder-project.json] --> B[Read lightweight dataset records]
    B --> C{Dataset choice}
    C -- Reuse CRB --> D[Add a lightweight CRB entry]
    C -- Continue editing --> E[Load the managed source]
    C -- Skip for now --> F[Keep it only in the project]
    E --> G[Restore saved settings]
    D --> H[Next release]
    G --> H
    F --> H
```

A reusable CRB appears in the dataset rail without reopening its Seurat object.
The source can still be loaded later when the user wants to edit that dataset.
Loading the source restores the saved choices onto a fresh inspection of the
managed file. Coordinate and image controls enter the dataset configuration
automatically; there is no separate Spatial alignment save action.

For a Spatial FOV without an attached histology image, Builder also retains the
point opacity and size used by its alignment preview. Those points-only display
values are project-editor state and are not exported into a CRB. When an image
is added, Builder transfers the values into that image's alignment record; when
the last image is removed, it transfers them back to the FOV's project state.
Additional images inherit the active image's point appearance at creation, then
retain independent settings; removing one image never rewrites the survivors.

Project checkpoint CRBs always receive a private embedded-image build
projection so every FOV image travels inside the reusable CRB. This projection
does not change the dataset's normal Viewer App storage preference.

## End-to-end restore and release flow

Opening, restoring, building, and publishing use one production state machine.
The native picker supplies only the initial project or output path; after that
boundary, interactive and automated validation runs execute the same functions
and safety checks.

```mermaid
flowchart TD
    Open[Open Project] --> Start[builder_project_start_open]
    Start --> Recover{Read manifest}
    Recover -->|primary valid| Preview[Build lightweight preview]
    Recover -->|primary invalid| Backup[Validate .bak and migration recovery]
    Backup --> Preview
    Preview --> Choice{Per-dataset choice}
    Choice -->|Saved CRB| Fresh[Verify freshness, fingerprint, and source identity]
    Choice -->|Source| Restore[Restore managed dataset and saved settings]
    Choice -->|Skip| Keep[Keep manifest record unloaded]
    Fresh --> Snapshot[Commit full project snapshot]
    Restore --> Snapshot
    Keep --> Snapshot

    Snapshot --> Review[Review release membership and blocking issues]
    Review --> Plan[Create immutable BuildPlan]
    Plan --> Coordinator[Parent coordinator prepares owned private stage]
    Coordinator --> Worker[Worker builds changed CRBs and Viewer bundle]
    Fresh -->|Reusable artifact selected| Copy[Copy verified CRB and sidecars into stage]
    Copy --> Worker
    Worker --> Settlement[Worker settlement and result contract]
    Settlement --> Parent[Parent verifies files, identities, digests, and topology]
    Parent --> Owner[Write ownership record]
    Owner --> Publish[Atomically publish unique release]
    Publish --> Register[Register successful CRB artifacts in Project manifest]
    Register --> Complete[Release complete and reusable on reopen]

    Parent -->|any mismatch| Reject[Reject publication and retain diagnostics]
    Recover -->|no valid recovery| Reject
    Fresh -->|stale or changed| Restore
```

The stage is private and release-scoped. The worker cannot publish directly:
the parent independently verifies the worker result, staged membership, CRB and
sidecar identities, Viewer bundle, and authentication inputs before it writes
the ownership record and renames the stage into place. A failed verification
therefore cannot partially replace or silently shrink an existing release.

These boundaries are platform-neutral. Paths are normalized for comparison,
while atomic rename, link handling, ownership records, and permission checks use
the guarantees available on Windows, Linux, and macOS without treating one
platform's textual path form or permission bits as another platform's contract.

## Incremental releases

The project is the complete workspace; a release is only a selected set of
datasets. Excluding a dataset from one release does not delete its settings or
CRB.

```mermaid
flowchart TD
    P[Builder project] --> A[Dataset A · reusable CRB]
    P --> B[Dataset B · excluded this time]
    P --> C[Dataset C · changed]
    P --> D[Dataset D · new]
    A --> R[Assemble release]
    C --> X[Build changed CRB]
    D --> Y[Build new CRB]
    X --> R
    Y --> R
```

The normal build pipeline still verifies every reused CRB after copying it into
the private build stage. New or changed datasets continue through the existing
snapshot, analysis, export, and verification path.

## Practical constraints

- Managed uploads use additional disk space, but they are the reliable default
  for browser-based uploads whose original local path is unavailable.
- Supplementary workbooks are copied into the project once. Their sheet names
  are saved immediately, but sheet data is read only for the build that uses it.
- External spatial assets and expression sidecars must travel with their CRB;
  a missing companion file makes the artifact unavailable for reuse.
- A changed or re-linked source must be inspected again. It must not silently
  inherit a previous checked state.
- Closing the Shiny session can interrupt a running checkpoint build. Builder
  warns before closing, but cannot guarantee an automatic save after the browser
  or process has already terminated.
- Project JSON may contain local external paths, but generated Viewer packages
  must never expose them.
