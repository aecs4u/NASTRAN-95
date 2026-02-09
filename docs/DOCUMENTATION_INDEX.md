# NASTRAN-95 Documentation Index

Complete guide to all documentation resources for NASTRAN-95.

## Getting Started

Start here if you're new to NASTRAN-95:

1. **[README.md](README.md)** - Main overview and introduction
2. **[QUICKSTART.md](QUICKSTART.md)** - Run your first analysis in minutes
3. **[EXAMPLES.md](EXAMPLES.md)** - Explore demonstration problems

## User Guides

### Quick Reference
- **[QUICKSTART.md](QUICKSTART.md)** - Fast introduction with practical examples
  - First run walkthrough
  - Common operations
  - Quick reference table
  - File naming conventions

### Example Problems
- **[EXAMPLES.md](EXAMPLES.md)** - Complete catalog of 80+ demonstration problems
  - Organized by analysis type
  - Problem descriptions
  - Learning path recommendations
  - How to verify results

### Troubleshooting
- **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** - Solutions to common issues
  - Installation problems
  - Compilation errors
  - Runtime issues
  - Platform compatibility
  - Performance optimization

## Reference Documentation

### PDF Manuals

Located in the root directory:

- **NASTRAN Users Manual 2.pdf** (13.9 MB)
  - Complete input format reference
  - Element library descriptions
  - Analysis procedure guides
  - Output interpretation
  - Modeling guidelines

- **NASTRAN Programmers Manual.pdf** (63.7 MB)
  - System architecture
  - Module descriptions
  - Data flow and structures
  - Subroutine documentation
  - Development information

### Input Examples

The `inp/` directory contains:
- 80+ input files (`.inp`) - Ready-to-run NASTRAN models
- Description files (`.txt`) - Detailed problem explanations
- Expected outputs in `demoout/` directory

## Technical Documentation

### System Information

From **[README.md](README.md)**:
- Directory structure
- System requirements
- File formats
- Building from source
- Running NASTRAN

### Architecture

From **NASTRAN Programmers Manual.pdf**:
- Module organization
- Data management system
- Matrix operations
- Solution sequences
- Rigid format definitions

## Developer Resources

- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Contribution guidelines
  - Project status and purpose
  - How to contribute
  - Code style guidelines
  - Testing procedures
  - Community guidelines

## By Topic

### Installation and Setup
- [README.md](README.md) - System Requirements
- [QUICKSTART.md](QUICKSTART.md) - Prerequisites
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Installation Issues

### Running Analyses
- [QUICKSTART.md](QUICKSTART.md) - Basic Usage
- [EXAMPLES.md](EXAMPLES.md) - Example Problems
- [README.md](README.md) - Running NASTRAN section
- NASTRAN Users Manual 2.pdf - Analysis Procedures

### Input File Creation
- NASTRAN Users Manual 2.pdf - Complete Input Reference
- [EXAMPLES.md](EXAMPLES.md) - Example inputs as templates
- `inp/*.txt` files - Problem-specific documentation

### Understanding Results
- NASTRAN Users Manual 2.pdf - Output Descriptions
- `demoout/` directory - Reference outputs
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Output Issues

### Problem Solving
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - First stop for issues
- [QUICKSTART.md](QUICKSTART.md) - Common operations
- NASTRAN Users Manual 2.pdf - Error message reference

### Development
- [CONTRIBUTING.md](CONTRIBUTING.md) - How to contribute
- NASTRAN Programmers Manual.pdf - System internals
- [README.md](README.md) - Building from Source

## Analysis Type Quick Links

Quick access to examples by analysis type:

| Analysis Type | Examples | Manual Section |
|--------------|----------|----------------|
| **Static Analysis** | [d01xxx](EXAMPLES.md#rigid-format-1-static-analysis-d01xxx) | Users Manual Ch. 2 |
| **Normal Modes** | [d02xxx](EXAMPLES.md#rigid-format-2-eigenvaluenormal-modes-analysis-d02xxx) | Users Manual Ch. 3 |
| **Frequency Response** | [d03xxx](EXAMPLES.md#rigid-format-3-frequency-response-d03xxx) | Users Manual Ch. 4 |
| **Buckling** | [d05xxx](EXAMPLES.md#rigid-format-5-buckling-analysis-d05xxx) | Users Manual Ch. 5 |
| **Transient Response** | [d07xxx](EXAMPLES.md#rigid-format-7-direct-transient-response-d07xxx), [d09xxx](EXAMPLES.md#rigid-format-9-modal-transient-response-d09xxx) | Users Manual Ch. 6 |
| **Nonlinear** | [d15xxx](EXAMPLES.md#rigid-format-15-nonlinear-analysis-d15xxx) | Users Manual Ch. 8 |

## External Resources

- **NASA Technical Reports Server** - Historical NASTRAN documentation
- **Original NASA Repository** - https://github.com/nasa/NASTRAN-95
- **Commercial NASTRAN** - MSC Software, Siemens NX Nastran (modern versions)

## Documentation Quick Reference

| I want to... | Read this... |
|--------------|--------------|
| Get started quickly | [QUICKSTART.md](QUICKSTART.md) |
| Run an example | [EXAMPLES.md](EXAMPLES.md) |
| Fix an error | [TROUBLESHOOTING.md](TROUBLESHOOTING.md) |
| Understand a feature | NASTRAN Users Manual 2.pdf |
| Learn the architecture | NASTRAN Programmers Manual.pdf |
| Contribute improvements | [CONTRIBUTING.md](CONTRIBUTING.md) |
| Understand project structure | [README.md](README.md) |

## File Locations

```
NASTRAN-95/
├── README.md                           Main documentation
├── QUICKSTART.md                       Quick start guide
├── EXAMPLES.md                         Example catalog
├── TROUBLESHOOTING.md                  Problem solving
├── CONTRIBUTING.md                     Contribution guide
├── DOCUMENTATION_INDEX.md              This file
├── NASTRAN Users Manual 2.pdf          User reference
├── NASTRAN Programmers Manual.pdf      Developer reference
├── inp/*.txt                           Problem descriptions
└── NASA Open Source Agreement...       License document
```

## Recommended Reading Order

### For New Users
1. [README.md](README.md) - Understand what NASTRAN is
2. [QUICKSTART.md](QUICKSTART.md) - Run your first problem
3. [EXAMPLES.md](EXAMPLES.md) - Explore more examples
4. NASTRAN Users Manual 2.pdf - Learn input formats

### For Developers/Contributors
1. [README.md](README.md) - Project overview
2. [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution guidelines
3. NASTRAN Programmers Manual.pdf - System architecture
4. Source code in `mis/`, `bd/`, etc.

### For Troubleshooting
1. [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Common issues
2. [QUICKSTART.md](QUICKSTART.md) - Verify basic operation
3. Log files from failed runs
4. NASTRAN Users Manual 2.pdf - Error messages

## Updates and Maintenance

This is a historical archive. Documentation is maintained to:
- Preserve historical accuracy
- Improve accessibility for modern users
- Support educational use
- Enable research

See [CONTRIBUTING.md](CONTRIBUTING.md) for how to help improve documentation.

---

**Last Updated:** 2026-02-09
**Documentation Version:** Enhanced v1.0
