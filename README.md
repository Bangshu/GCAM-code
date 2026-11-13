# GCAM PAC BS Source Evidence

This repository is a source-evidence mirror for local GCAM PAC BS Archify diagrams.

It contains only:

- `input/gcamdata/R/`: R source files used to identify `DECLARE_INPUTS`, `MAKE`, and XML-writing code.
- `LICENSE`: the upstream Educational Community License 2.0 text retained with the source.
- `GCAM BS/`: current progress, validation boundaries, and data-integration notes.

It intentionally excludes all raw CSV inputs, CEDS emissions data, XML outputs, executable files, databases, model results, and user-specific regional datasets. The local model workspace remains the authoritative execution workspace.

The R snapshot was refreshed from the active 43-region PAC project on 2026-11-05 and again on 2026-11-13 (emissions R aligned with Global v9.1: L152, L252 and constants.R restored; L102 and L114 simplified; Employment and GTAP updates from 2026-11-12). See [GCAM BS](GCAM%20BS/README.md) for the scope and remaining gaps. A source snapshot is not evidence that the full model solves successfully.
