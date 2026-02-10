# NASTRAN-95

**NASA Structural Analysis System**

NASTRAN-95 is a historic finite element analysis (FEA) program developed by NASA in the early 1970s. It was the first major FEA software of its kind and revolutionized computer-aided engineering by enabling structural analysis through digital simulation.

## Overview

NASTRAN (NASA Structural Analysis System) can perform:
- Static response analysis to concentrated and distributed loads
- Elastic stability analysis
- Complex eigenvalue analysis for vibration and dynamic stability
- Dynamic response analysis for transient and steady-state loads
- Random excitation analysis
- Thermal expansion and enforced deformation analysis

This repository contains the NASTRAN-95 source code released under the [NASA Open Source Agreement version 1.3](https://github.com/nasa/NASTRAN-95/raw/master/NASA%20Open%20Source%20Agreement-NASTRAN%2095.doc).

**NOTE:** There is no technical support available for this software.

## 🌐 Web Interface

**NEW**: Interactive web dashboard for tracking modernization progress!

```bash
# Start the webapp
./scripts/start_webapp.sh --reload --open

# Access at http://localhost:9002
```

**Features:**
- 📊 **Project Dashboard** - Real-time KPIs and status overview
- 🔄 **Modernization Tracking** - File migration progress (3 files modernized)
- ✅ **Test Results** - 132 example test cases with filtering
- 🔨 **Build Monitor** - CMake configuration and library status
- 📚 **Documentation Browser** - Integrated markdown viewer
- 🌐 **JSON API** - RESTful endpoints for automation

**Quick Links:**
- Dashboard: http://localhost:9002
- API Status: http://localhost:9002/api/status
- Health Check: http://localhost:9002/health
- Full Documentation: [webapp/README.md](webapp/README.md)

## Documentation

**[Documentation Index](DOCUMENTATION_INDEX.md)** - Complete guide to all documentation

Quick Links:
- [Quick Start Guide](QUICKSTART.md) - Get started in minutes
- [Examples Guide](EXAMPLES.md) - Complete catalog of demonstration problems
- [Troubleshooting](TROUBLESHOOTING.md) - Solutions to common issues
- [Contributing](CONTRIBUTING.md) - How to contribute to this historical archive

## Directory Structure

```
NASTRAN-95/
├── mis/          Main source code (1,676 FORTRAN files)
├── bd/           Build-dependent modules
├── alt/          Alternative modules
├── mds/          Module definitions
├── rf/           Rigid format definitions
├── um/           Utility modules
├── utility/      Utility programs
├── inp/          Input examples and demonstration problems
├── demoout/      Expected output files for demonstration problems
├── bin/          Compiled executables and scripts
│   ├── nastran       Shell script to run NASTRAN interactively
│   ├── nastrn.exe    Main NASTRAN executable
│   ├── chkfil.exe    Input file checker
│   ├── nastlib.a     NASTRAN library archive
│   └── linknas       Linker script
└── docs/
    ├── NASTRAN Programmers Manual.pdf  (Detailed technical documentation)
    └── NASTRAN Users Manual 2.pdf      (User guide and reference)
```

## System Requirements

- **Platform:** Unix/Linux (originally developed for Unix systems)
- **Compiler:** FORTRAN 77 compiler (gfortran, f77, or compatible)
- **Shell:** C Shell (csh/tcsh) for running scripts
- **Memory:** Configurable (default: 12MB database memory, 2MB open core)
- **Disk Space:** Adequate space for temporary working files

## Running NASTRAN

The compiled executables are provided in the `bin/` directory. NASTRAN uses an interactive shell script interface.

### Basic Usage

```bash
cd /path/to/NASTRAN-95
./bin/nastran [problem_name]
```

Where `problem_name` is the base name of your input file (without the `.inp` extension).

### Interactive Menu

When you run the `nastran` script, it presents an interactive menu where you can configure:

- **Input file** (`.inp`) - Your NASTRAN input deck
- **Output file** (`.out`) - Analysis results
- **Log file** (`.log`) - Execution log
- **Plot file** (`.plt`) - Plotting data
- **Punch file** (`.pun`) - Punched output
- **Checkpoint files** - For restart capability
- **Memory allocation** - Database and open core memory
- **Work directory** - Temporary scratch space

### Running Demonstration Problems

The `inp/` directory contains numerous demonstration problems:

```bash
cd /path/to/NASTRAN-95
./bin/nastran d01011a
```

This will:
1. Read the input file `inp/d01011a.inp`
2. Run the analysis
3. Generate output in `d01011a.out`
4. Expected results are available in `demoout/d01011a.out` for comparison

### Example Problems

Demonstration problems are organized by analysis type:
- **d01xxx** - Static analysis examples
- **d02xxx** - Eigenvalue and vibration analysis
- **d03xxx** - Frequency response
- **d05xxx** - Buckling analysis
- **d07xxx** - Direct transient response
- **d08xxx** - Direct frequency response
- And many more...

Each `.txt` file in the `inp/` directory contains detailed descriptions of the corresponding problem.

## File Formats

### Input Files (`.inp`)

NASTRAN uses a card-image format (80-column fixed format) typical of 1970s computer systems. Input files contain:
- Executive Control Deck (NASTRAN operations)
- Case Control Deck (output requests and load case selection)
- Bulk Data Deck (model geometry, properties, loads, constraints)

### Output Files (`.out`)

Output files contain:
- Echo of input data
- Analysis results (displacements, stresses, forces)
- Error and warning messages
- Execution statistics

## Building from Source

The repository includes a pre-compiled library (`bin/nastlib.a`) and executables. To rebuild:

```bash
cd /path/to/NASTRAN-95/bin

# Extract and link (see linknas script for details)
./linknas
```

The `linknas` script extracts object files from the library archive and links them with the FORTRAN compiler.

## Documentation

Comprehensive documentation is provided:

1. **NASTRAN Users Manual 2.pdf** - User guide, input format, element descriptions, analysis procedures
2. **NASTRAN Programmers Manual.pdf** - Internal architecture, module descriptions, developer documentation

These manuals are essential for understanding NASTRAN's capabilities and input requirements.

## Historical Context

NASTRAN was developed in the late 1960s and early 1970s as part of NASA's effort to standardize structural analysis methods across the aerospace industry. It became one of the most successful NASA technology transfer programs and spawned numerous commercial derivatives (MSC Nastran, NX Nastran, etc.).

This NASTRAN-95 release represents the public domain version maintained by NASA through the mid-1990s. While superseded by modern commercial versions, it remains valuable for:
- Educational purposes
- Understanding the foundations of FEA
- Historical reference
- Research into legacy aerospace engineering methods

## License

Released under the [NASA Open Source Agreement version 1.3](https://github.com/nasa/NASTRAN-95/raw/master/NASA%20Open%20Source%20Agreement-NASTRAN%2095.doc).

## Contributing

This is an archival release with no active development or technical support. The code is provided as-is for historical and educational purposes.

## Additional Resources

- Original NASA repository: https://github.com/nasa/NASTRAN-95
- NASTRAN User Community Archives
- "The NASTRAN Theoretical Manual" (available from NASA technical reports)

## Known Limitations

- Designed for 1970s-era computing environments
- Requires C Shell (csh) for execution scripts
- Fixed-format FORTRAN 77 code
- Memory limitations based on 1990s-era systems
- No modern GUI or preprocessing tools included
- Limited to analyses defined in original rigid formats

---

*For questions about modern NASTRAN implementations, please refer to commercial vendors (MSC Software, Siemens, etc.). This archive is for historical reference only.*
