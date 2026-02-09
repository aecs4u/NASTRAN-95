# NASTRAN-95 Example Problems

This document catalogs the demonstration problems included in the `inp/` directory.

## Overview

The demonstration problems are organized by analysis type and numbered sequentially. Each problem includes:
- An input file (`.inp`) containing the NASTRAN model
- A description file (`.txt`) explaining the problem setup and objectives
- Expected output in the `demoout/` directory for verification

## Problem Naming Convention

Problems follow the pattern: `dXXYYZa`
- `d` = demonstration
- `XX` = Rigid format number (analysis type)
- `YY` = Problem series within that format
- `Z` = Sub-problem variant
- `a-z` = Additional variations

## Analysis Types by Rigid Format

### Rigid Format 1: Static Analysis (d01xxx)

Static structural analysis under various loading conditions.

| Problem | Description |
|---------|-------------|
| d01011a | Delta wing with biconvex cross-section using QDMEM1 and QDMEM2 elements |
| d01011b | Delta wing biconvex cross-section variant |
| d01011c | Delta wing using QDMEM1 elements only |
| d01012a | Delta wing using QDMEM2 elements |
| d01013a | Delta wing analysis variant |
| d01014a | Additional delta wing configuration |
| d01021a | Static analysis with different boundary conditions |
| d01021b | Static analysis variant |
| d01031a | Static analysis problem |
| d01032a | Static analysis problem variant |
| d01033a | Static analysis problem variant |
| d01041a | Static analysis problem |
| d01051a | Static analysis problem |
| d01061a | Static analysis problem |
| d01071a | Static analysis problem |
| d01081a | Static analysis problem |
| d01091a | Static analysis problem |
| d01092a | Static analysis problem variant |
| d01101a | Static analysis problem |
| d01111a | Static analysis problem |
| d01112a | Static analysis problem variant |
| d01121a | Static analysis problem |
| d01122a | Static analysis problem variant |
| d01131a | Static analysis problem |
| d01132a | Static analysis problem variant |
| d01133a | Static analysis problem variant |
| d01141a | Static analysis problem |
| d01151a | Static analysis problem |
| d01161a | Static analysis problem |
| d01171a | Static analysis problem |

### Rigid Format 2: Eigenvalue/Normal Modes Analysis (d02xxx)

Vibration and normal modes analysis for natural frequency determination.

| Problem | Description |
|---------|-------------|
| d02011a | Normal modes analysis |
| d02021a | Eigenvalue extraction problem |
| d02022a | Eigenvalue extraction variant |
| d02023a | Eigenvalue extraction variant |
| d02024a | Eigenvalue extraction variant |
| d02025a | Eigenvalue extraction variant |
| d02026a | Eigenvalue extraction variant |
| d02027a | Eigenvalue extraction variant |
| d02031a | Normal modes problem |
| d02032a | Normal modes problem variant |
| d02033a | Normal modes problem variant |
| d02034a | Normal modes problem variant |

### Rigid Format 3: Frequency Response (d03xxx)

Steady-state dynamic response to harmonic excitation.

| Problem | Description |
|---------|-------------|
| d03011a | Frequency response analysis |
| d03012a | Frequency response variant |
| d03013a | Frequency response variant |
| d03014a | Frequency response variant |
| d03021a | Frequency response problem |
| d03031a | Frequency response problem |
| d03041a | Frequency response problem |
| d03051a | Frequency response problem |
| d03061a | Frequency response problem |
| d03071a | Frequency response problem |
| d03081a | Frequency response problem |
| d03082a | Frequency response variant |
| d03083a | Frequency response variant |

### Rigid Format 4: Static Analysis with Differential Stiffness (d04xxx)

Static analysis including geometric nonlinearity effects.

| Problem | Description |
|---------|-------------|
| d04011a | Static analysis with differential stiffness |

### Rigid Format 5: Buckling Analysis (d05xxx)

Buckling and stability analysis for determining critical loads.

| Problem | Description |
|---------|-------------|
| d05011a | Buckling analysis problem |
| d05021a | Buckling analysis variant |

### Rigid Format 6: Preloaded Normal Modes (d06xxx)

Normal modes analysis with prestress effects.

| Problem | Description |
|---------|-------------|
| d06011a | Preloaded normal modes analysis |

### Rigid Format 7: Direct Transient Response (d07xxx)

Time-domain dynamic response to transient loads.

| Problem | Description |
|---------|-------------|
| d07011a | Direct transient response |
| d07012a | Direct transient response variant |
| d07021a | Direct transient response problem |
| d07022a | Direct transient response variant |

### Rigid Format 8: Direct Frequency Response (d08xxx)

Direct frequency response analysis.

| Problem | Description |
|---------|-------------|
| d08011a | Direct frequency response |
| d08012a | Direct frequency response variant |
| d08013a | Direct frequency response variant |
| d08014a | Direct frequency response variant |

### Rigid Format 9: Modal Transient Response (d09xxx)

Transient response using modal superposition.

| Problem | Description |
|---------|-------------|
| d09011a | Modal transient response |
| d09021a | Modal transient response variant |
| d09022a | Modal transient response variant |
| d09031a | Modal transient response problem |
| d09041a | Modal transient response problem |

### Rigid Format 10: Modal Complex Eigenvalue Analysis (d10xxx)

Complex eigenvalue analysis for systems with damping.

| Problem | Description |
|---------|-------------|
| d10011a | Modal complex eigenvalue analysis |
| d10021a | Modal complex eigenvalue variant |

### Rigid Format 11: Modal Frequency Response (d11xxx)

Frequency response using modal superposition.

| Problem | Description |
|---------|-------------|
| d11011a | Modal frequency response |
| d11021a | Modal frequency response variant |
| d11022a | Modal frequency response variant |
| d11031a | Modal frequency response problem |
| d11032a | Modal frequency response variant |

### Rigid Format 12: Shock Response Spectrum (d12xxx)

Response spectrum analysis for shock loads.

| Problem | Description |
|---------|-------------|
| d12011a | Shock response spectrum analysis |

### Rigid Format 13: Normal Modes Analysis (d13xxx)

Additional normal modes capabilities.

| Problem | Description |
|---------|-------------|
| d13011a | Normal modes analysis |

### Rigid Format 14: Cyclic Symmetry Analysis (d14xxx)

Analysis of cyclically symmetric structures.

| Problem | Description |
|---------|-------------|
| d14011a | Cyclic symmetry analysis |

### Rigid Format 15: Nonlinear Analysis (d15xxx)

Nonlinear static and dynamic analysis.

| Problem | Description |
|---------|-------------|
| d15011a | Nonlinear analysis problem |

## Running Examples

To run any example:

```bash
cd /path/to/NASTRAN-95
./bin/nastran <problem_name>
```

For example:
```bash
./bin/nastran d01011a
./bin/nastran d02021a
./bin/nastran d05011a
```

## Verifying Results

Compare your results with expected output:

```bash
diff <problem_name>.out demoout/<problem_name>.out
```

Small numerical differences are normal due to compiler and platform variations.

## Learning Path

Recommended progression for learning NASTRAN:

1. **Start with Static Analysis** (d01xxx)
   - Begin with d01011a (simple delta wing)
   - Understand basic element types and boundary conditions

2. **Move to Eigenvalue Analysis** (d02xxx)
   - Try d02021a
   - Learn about modal analysis and natural frequencies

3. **Explore Frequency Response** (d03xxx)
   - Run d03011a
   - Understand harmonic response

4. **Try Buckling Analysis** (d05xxx)
   - Study d05011a
   - Learn stability analysis concepts

5. **Advanced Topics**
   - Transient response (d07xxx, d09xxx)
   - Complex eigenvalues (d10xxx)
   - Nonlinear analysis (d15xxx)

## Problem Documentation

Each problem has a corresponding `.txt` file with detailed information:

```bash
cat inp/d01011a.txt
```

These files contain:
- Problem description
- Input data explanation
- Structural configuration
- Loading conditions
- Expected behavior
- Results interpretation

## Additional Resources

For detailed information about input formats, element types, and analysis procedures, consult:
- **NASTRAN Users Manual 2.pdf** - Complete reference for all input cards and analysis types
- **NASTRAN Programmers Manual.pdf** - Internal implementation details

---

**Note:** All demonstration problems are provided for educational purposes and illustrate various NASTRAN capabilities. They range from simple validation cases to complex structural analysis scenarios.
