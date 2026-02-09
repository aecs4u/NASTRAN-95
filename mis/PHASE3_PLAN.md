# Phase 3: Source Code Reorganization Plan

**Goal:** Transform flat 1,847-file structure into logical, modular architecture

**Status:** Planning → Implementation

## Current Structure (Flat)

```
NASTRAN-95/
├── mis/     (1,674 files) - Main implementation, everything mixed
├── mds/     (130 files)   - System/database/IO
├── bd/      (40 files)    - Block data initialization
├── um/      (1 file)      - Utility
└── utility/ (2 files)     - Utilities
```

## Proposed Structure (Modular)

```
NASTRAN-95/
├── src/
│   ├── core/              # Core algorithms and utilities
│   │   ├── algorithms/    # alg*.f → algorithm modules
│   │   ├── matrices/      # mma*.f → matrix operations
│   │   ├── solvers/       # Equation solvers
│   │   └── utilities/     # General utilities
│   │
│   ├── elements/          # Finite element implementations
│   │   ├── beam/          # rod*.f, bar*.f
│   │   ├── shell/         # quad*.f, tria*.f, squd*.f
│   │   ├── solid/         # ihex*.f, hex*.f, solid*.f
│   │   └── aero/          # amg*.f, apd*.f, amp*.f
│   │
│   ├── system/            # System services (from mds/)
│   │   ├── io/            # GINO I/O layer
│   │   ├── database/      # DBM manager
│   │   └── platform/      # Platform abstractions
│   │
│   ├── analysis/          # Analysis modules
│   │   ├── static/        # Static analysis
│   │   ├── dynamic/       # Dynamic analysis
│   │   ├── modal/         # Eigenvalue/modal
│   │   └── nonlinear/     # Nonlinear analysis
│   │
│   └── blockdata/         # Initialization (from bd/)
│       └── init/          # Block data files
│
├── templates/             # Phase 2 templates (already done)
└── [existing dirs]        # bin, docs, etc.
```

## File Categorization Strategy

### 1. Core Algorithms (core/algorithms/)
**Pattern:** `alg*.f` (39 files)
- alg01.f through alg30.f
- Algorithm library for interpolation, iteration, etc.

### 2. Matrix Operations (core/matrices/)
**Pattern:** `mma*.f` (18 files)
- mma101.f through mma*.f
- Matrix manipulation and operations

### 3. Element Implementations

#### Beam Elements (elements/beam/)
**Patterns:** `rod*.f`, `bar*.f`, `beam*.f` (~15 files)
- rod.f, rodd.f, rods.f
- bar.f, bard.f, bars.f
- beam.f, etc.

#### Shell Elements (elements/shell/)
**Patterns:** `quad*.f`, `tria*.f`, `squd*.f` (~25 files)
- quad4.f, quad4d.f, quad4s.f
- tria3.f, tria3d.f, tria3s.f
- squd42.f, etc.

#### Solid Elements (elements/solid/)
**Patterns:** `ihex*.f`, `hex*.f`, `wedge*.f`, `tetra*.f` (~20 files)
- ihex.f, ihexd.f, ihexs.f, ihexsd.f, ihexss.f
- hex.f, wedge.f, tetra.f, etc.

#### Aeroelastic Elements (elements/aero/)
**Patterns:** `amg*.f`, `apd*.f`, `amp*.f` (~30 files)
- amg*.f - blade matrices
- apd*.f - panel matrices
- amp*.f - aeroelastic

### 4. System Services (system/)
**Source:** mds/ directory (130 files)
- io/ - open.f, close.f, read.f, write.f, gino*.f
- database/ - dbm*.f
- platform/ - btstrp.f, dummy.f

### 5. Block Data (blockdata/)
**Source:** bd/ directory (40 files)
- init/ - All bd*.f files

### 6. Solvers (core/solvers/)
**Patterns:** Files with solver algorithms
- Direct solvers
- Iterative solvers
- Eigenvalue solvers

## Implementation Phases

### Phase 3.1: Create Directory Structure ✓
```bash
mkdir -p src/{core/{algorithms,matrices,solvers,utilities},elements/{beam,shell,solid,aero},system/{io,database,platform},analysis/{static,dynamic,modal,nonlinear},blockdata/init}
```

### Phase 3.2: Categorize All Files
- Create file_categories.csv mapping each file to target directory
- Use automated analysis + manual review

### Phase 3.3: Move Files (Git-Aware)
```bash
git mv mis/alg*.f src/core/algorithms/
git mv mis/mma*.f src/core/matrices/
# etc.
```

### Phase 3.4: Update Build System
- Update CMakeLists.txt with new structure
- Create module-level CMakeLists.txt files
- Update include paths

### Phase 3.5: Create Module Interfaces
- Add Fortran MODULE wrappers where appropriate
- Define public APIs for each subsystem

### Phase 3.6: Update Documentation
- Update file references
- Create architecture documentation
- Module dependency diagrams

## File Analysis Strategy

### Automated Categorization
```bash
# Elements: Look for SUBROUTINE names matching element types
grep -l "SUBROUTINE.*\(ROD\|BAR\|QUAD\|TRIA\|IHEX\)" *.f

# Algorithms: alg*.f pattern
ls alg*.f

# Matrices: mma*.f pattern
ls mma*.f

# System: Check for GINO, DBM references
grep -l "GINO\|DBM" *.f
```

### Manual Review Categories
- Files with ambiguous names
- Multi-purpose files
- Shared utilities
- Complex dependencies

## Success Metrics

- All 1,847 files categorized and moved
- Logical directory structure with <100 files per directory
- Build system updated and working
- No broken dependencies
- Git history preserved (using git mv)

## Risks & Mitigation

**Risk:** Breaking build dependencies
- **Mitigation:** Move in small batches, test after each batch

**Risk:** Losing git history
- **Mitigation:** Use git mv, not mv + git add

**Risk:** Incorrect categorization
- **Mitigation:** Start with obvious categories (alg*, mma*, elements)

## Next Steps

1. Create directory structure
2. Analyze and categorize 100 most obvious files
3. Create automated categorization script
4. Move files in batches of 50-100
5. Update build system incrementally
6. Test after each batch

---

**Plan Status:** Ready for implementation
**Estimated Time:** 2-4 weeks for complete reorganization
**Current Phase:** 3.0 - Planning Complete
