# NASTRAN-95 Examples and Test Fixtures

This directory contains 80+ demonstration problems that serve dual purposes:
1. **Examples** for learning NASTRAN capabilities
2. **Test Fixtures** for validating modernization correctness

## Directory Structure

```
examples/
├── input/                 # Input files (.inp) and descriptions (.txt)
│   ├── d01011a.inp       # Delta wing static analysis
│   ├── d01011a.txt       # Problem description
│   └── ... (80+ problems)
├── reference_output/      # Expected output from original NASTRAN-95
│   ├── d01011a.out       # Reference results
│   └── ... (80+ outputs)
└── README.md             # This file
```

## Running Examples

### As Learning Resources

```bash
# From NASTRAN-95 root directory
./bin/nastran.sh d01011a
```

### As Test Fixtures

```bash
# Compare your result with reference output
diff d01011a.out examples/reference_output/d01011a.out
```

## Problem Categories

See [EXAMPLES.md](../EXAMPLES.md) for complete catalog of all 80+ examples organized by analysis type.

---

**80+ Examples** | **Dual-Purpose: Learning + Testing** | **Numerical Validation**
