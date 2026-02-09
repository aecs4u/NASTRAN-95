# Phase 3: Source Code Reorganization - Progress Report

**Status:** Ready for Migration
**Date:** 2026-02-09
**Phase:** 3.2 Complete - Automated Categorization

---

## Overview

Successfully analyzed and categorized 1,847 FORTRAN source files from flat directory structure into logical, modular architecture.

## File Categorization Results

### Automated Categorization Statistics

| Status | Files | Percentage | Action |
|--------|-------|------------|--------|
| **Auto-categorized** | 1,112 | 60.2% | Ready for migration |
| **Manual review needed** | 735 | 39.8% | Requires human review |
| **Total** | 1,847 | 100% | - |

### Categorization Method Performance

1. **Pattern Matching** (Round 1)
   - Files categorized: 413 (22.4%)
   - Method: Filename patterns (alg*.f, mma*.f, element names)

2. **Content Analysis** (Round 2)
   - Additional files categorized: 699 (37.8%)
   - Method: Subroutine names, keywords, COMMON blocks, CALL analysis
   - Improvement: Reduced review-needed from 77.6% to 39.8%

### Files by Target Category

Successfully categorized 1,112 files across 17 categories:

| Category | Files | Percentage | Description |
|----------|-------|------------|-------------|
| `utilities/output` | 167 | 15.0% | Output formatting, printing, reports |
| `elements/shell` | 129 | 11.6% | QUAD, TRIA shell elements |
| `system/io` | 122 | 11.0% | GINO I/O layer, file operations |
| `utilities/helpers` | 108 | 9.7% | Sorting, packing, MCB utilities |
| `analysis/static` | 89 | 8.0% | Static analysis (GP, TA, SMA modules) |
| `analysis/modal` | 76 | 6.8% | Eigenvalue, modal analysis |
| `system/database` | 73 | 6.6% | DBM database manager |
| `elements/beam` | 58 | 5.2% | ROD, BAR, BEAM elements |
| `elements/aero` | 57 | 5.1% | AMG, APD, AMP aeroelastic modules |
| `core/matrices` | 49 | 4.4% | Matrix operations (MMA family) |
| `core/solvers` | 41 | 3.7% | Equation solvers, factorization |
| `blockdata/init` | 40 | 3.6% | Block data initialization (from bd/) |
| `core/algorithms` | 39 | 3.5% | Algorithm library (ALG family) |
| `utilities/parser` | 35 | 3.1% | Input parsing, NASTRAN data blocks |
| `elements/solid` | 18 | 1.6% | IHEX, hexahedral solid elements |
| `analysis/dynamic` | 8 | 0.7% | Frequency, transient analysis |
| `system/platform` | 3 | 0.3% | Platform abstractions, bootstrap |

## Source Directory Distribution

Original flat structure breakdown:

| Source Dir | Total Files | Auto-Categorized | Review Needed | Success Rate |
|------------|-------------|------------------|---------------|--------------|
| `mis/` | 1,674 | 1,033 | 641 | 61.7% |
| `mds/` | 130 | 127 | 3 | 97.7% |
| `bd/` | 40 | 40 | 0 | 100% |
| `um/` | 1 | 1 | 0 | 100% |
| `utility/` | 2 | 2 | 0 | 100% |

**Key Insight:** System directories (mds/, bd/, um/, utility/) achieved near-perfect categorization (99.2%). Most ambiguous files are in the large `mis/` directory.

## Categorization Techniques

### Pattern Matching Rules

```
Algorithm modules:    alg*.f       → core/algorithms/
Matrix operations:    mma*.f       → core/matrices/
Beam elements:        rod*.f, bar*.f, beam*.f → elements/beam/
Shell elements:       quad*.f, tria*.f, squd*.f → elements/shell/
Solid elements:       ihex*.f, hex*.f, wedge*.f → elements/solid/
Aeroelastic:          amg*.f, apd*.f, amp*.f → elements/aero/
```

### Content Analysis Scoring

For files without clear patterns, analyzed:
- **Subroutine/function names** - Matched against category-specific patterns
- **Keywords in comments** - Searched for domain-specific terms
- **CALL statements** - Identified dependencies and relationships
- **Scoring threshold** - Minimum score of 20 for auto-categorization

Example keywords by category:
- `elements/shell`: QUAD, TRIA, SHELL, PLATE, MEMBRANE
- `analysis/static`: STIFFNESS, LOAD VECTOR, DISPLACEMENT, GP1, GP4
- `core/solvers`: DECOMPOSITION, FACTORIZATION, LU FACTOR, FBS, DCMP

## Files Needing Manual Review (735)

### Common Patterns in Uncategorized Files

1. **Very short names** (a*.f, ai.f, af.f) - Cryptic single/double letter names
2. **Legacy utility functions** - Purpose unclear without deep analysis
3. **Cross-cutting concerns** - Files that span multiple categories
4. **Specialized modules** - Unique functionality not fitting standard patterns

### Top Uncategorized Files (Sample)

```
a82int.f      - Data conversion utilities
ascm*.f       - Substructure command DMAP data (12 files)
akapm.f       - Matrix parameter calculations
alamda.f      - Algorithm parameter functions
area.f        - Geometry calculations
```

### Recommended Next Steps for Review Files

1. **Group analysis** - Many uncategorized files are families (ascm01-ascm12)
2. **Dependency analysis** - Use CALL graph to infer relationships
3. **Documentation review** - Check NASTRAN manuals for module descriptions
4. **Incremental categorization** - Can be done post-migration

## Migration Readiness

### Automated Migration Script

Created: `scripts/migrate_files.py`

**Features:**
- CSV-based file mapping
- `git mv` preserves history
- Dry-run mode for verification
- Batch processing support
- Category filtering
- Comprehensive error handling

**Dry-Run Results:**
```
Files to migrate:   1,112 (60.2%)
Files to skip:        735 (39.8% - REVIEW_NEEDED)
Errors:                 0 (100% success in dry-run)
```

### Migration Command Examples

```bash
# Full dry-run preview
python3 scripts/migrate_files.py --dry-run

# Migrate specific category
python3 scripts/migrate_files.py --category core/algorithms

# Migrate in batches of 100
python3 scripts/migrate_files.py --batch 100

# LIVE migration (all auto-categorized files)
python3 scripts/migrate_files.py
```

## Directory Structure Created

Phase 3.1 complete - all 22 directories created:

```
src/
├── core/
│   ├── algorithms/    (39 files ready)
│   ├── matrices/      (49 files ready)
│   ├── solvers/       (41 files ready)
│   └── utilities/     (ready for manual categorization)
├── elements/
│   ├── beam/          (58 files ready)
│   ├── shell/         (129 files ready)
│   ├── solid/         (18 files ready)
│   └── aero/          (57 files ready)
├── system/
│   ├── io/            (122 files ready)
│   ├── database/      (73 files ready)
│   └── platform/      (3 files ready)
├── analysis/
│   ├── static/        (89 files ready)
│   ├── dynamic/       (8 files ready)
│   ├── modal/         (76 files ready)
│   └── nonlinear/     (ready for manual categorization)
└── blockdata/
    └── init/          (40 files ready)
```

## Scripts Created

| Script | Purpose | Status |
|--------|---------|--------|
| `scripts/categorize_files.py` | Initial pattern-based categorization | ✓ Complete |
| `scripts/deep_categorize.py` | Enhanced content analysis | ✓ Complete |
| `scripts/migrate_files.py` | Git-aware file migration | ✓ Complete |
| `scripts/migrate_files.sh` | Bash migration (deprecated) | Alternative |

## Output Files

| File | Description | Records |
|------|-------------|---------|
| `file_categories.csv` | Complete categorization mapping | 1,847 rows |
| `files_needing_review.txt` | List of uncategorized files | 735 files |

## Next Steps

### Phase 3.3: Execute Migration (Ready)

1. **Recommended approach:** Migrate by category to enable incremental testing

   ```bash
   # Start with core modules (smallest impact)
   python3 scripts/migrate_files.py --category core/algorithms
   python3 scripts/migrate_files.py --category core/matrices

   # Then system services
   python3 scripts/migrate_files.py --category system/io
   python3 scripts/migrate_files.py --category system/database

   # Finally elements (largest category)
   python3 scripts/migrate_files.py --category elements/shell
   python3 scripts/migrate_files.py --category elements/beam
   ```

2. **After each migration batch:**
   - Run `git status` to review changes
   - Test that files are in correct locations
   - Commit: `git commit -m "Phase 3: Move [category] files to new structure"`

3. **Full migration:**
   ```bash
   python3 scripts/migrate_files.py  # Migrate all 1,112 auto-categorized files
   git status
   git commit -m "Phase 3: Complete source reorganization (1,112 files)"
   ```

### Phase 3.4: Manual Review (735 files)

Can be done incrementally after automated migration:
- Review files_needing_review.txt
- Categorize in batches of 50-100 files
- Update file_categories.csv
- Run migration script for newly categorized files

### Phase 3.5: Update Build System

After file migration:
- Update root CMakeLists.txt with new src/ structure
- Create module-level CMakeLists.txt in each subdirectory
- Update include paths
- Test build: `cmake . && make`

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Breaking git history | Low | High | Using `git mv` preserves history |
| Build system breakage | Medium | High | Incremental migration + testing |
| Incorrect categorization | Low | Medium | Dry-run verification, easy to re-categorize |
| Missing dependencies | Low | Low | Build system will catch missing files |

## Success Metrics

- ✓ Directory structure created (22 directories)
- ✓ 60.2% of files auto-categorized
- ✓ Migration scripts tested (0 errors in dry-run)
- ⧖ Files migrated (pending execution)
- ⧖ Build system updated (Phase 3.5)
- ⧖ Manual review complete (Phase 3.4)

## Timeline

- **Phase 3.1** (Directory creation): ✓ Complete
- **Phase 3.2** (Categorization): ✓ Complete
- **Phase 3.3** (Migration): Ready to execute (~1 hour)
- **Phase 3.4** (Manual review): 2-4 days (incremental)
- **Phase 3.5** (Build system): 1-2 days

**Total Phase 3 elapsed:** Planning + automation complete, execution ready

---

**Recommendation:** Proceed with Phase 3.3 automated migration of 1,112 files. Manual review of remaining 735 files can be done incrementally post-migration without blocking progress.
