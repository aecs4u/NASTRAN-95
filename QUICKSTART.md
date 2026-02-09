# NASTRAN-95 Quick Start Guide

This guide will help you run your first NASTRAN analysis in minutes.

## Prerequisites

Ensure you have:
- Unix/Linux operating system
- C Shell (csh or tcsh) installed
- The NASTRAN-95 directory accessible

## Your First NASTRAN Run

### Step 1: Navigate to the NASTRAN directory

```bash
cd /path/to/NASTRAN-95
```

### Step 2: Run a demonstration problem

```bash
./bin/nastran d01011a
```

### Step 3: Follow the interactive prompts

The NASTRAN script will display a menu. The defaults are usually fine for demonstration problems:

```
                               NASTRAN

  (i)  Input file       ===> inp/d01011a.inp
  (o)  Output file      ===> d01011a.out
  (l)  Logfile          ===> d01011a.log
  ...

  (g)  To create shell script and execute NASTRAN
  (a)  Abort without building shell script

 Specify Option ===>
```

Type `g` and press Enter to proceed with the default settings.

### Step 4: Choose execution mode

When asked:
```
Do you want to execute this problem now? (y or n) ===>
```

Type `y` and press Enter.

Then choose foreground execution:
```
Do you want to run in foreground or background? (f or b) ===>
```

Type `f` and press Enter.

### Step 5: Review the results

After execution completes, check your output:

```bash
# View the output file
less d01011a.out

# Compare with expected results
diff d01011a.out demoout/d01011a.out
```

## Understanding the Example

The problem `d01011a` is a delta wing static analysis using membrane, shear panel, and rod elements. Check the description file:

```bash
cat inp/d01011a.txt
```

## Running Other Examples

Try different demonstration problems:

```bash
# Static analysis examples
./bin/nastran d01012a    # Another static analysis
./bin/nastran d01021a    # Different boundary conditions

# Vibration analysis
./bin/nastran d02021a    # Eigenvalue analysis

# Buckling analysis
./bin/nastran d05011a    # Buckling example
```

## Common Options

### Change Memory Allocation

In the interactive menu, type:
- `im` - Set in-memory database allocation (default: 12000000 words)
- `oc` - Set open core memory (default: 2000000 words)

### Specify Custom Files

In the interactive menu, type:
- `i` - Specify a different input file
- `o` - Specify a different output file
- `l` - Specify a different log file
- `w` - Specify a different work directory

### Background Execution

When prompted for execution mode, choose `b` for background:
```
Do you want to run in foreground or background? (f or b) ===> b
```

This allows you to continue working while NASTRAN runs.

## Next Steps

1. **Explore the examples** - Look through the `inp/` directory for various problem types
2. **Read the documentation** - See the User Manual in `NASTRAN Users Manual 2.pdf`
3. **Create your own models** - Use the examples as templates
4. **Study the results** - Learn to interpret NASTRAN output files

## Troubleshooting

### Problem: Script won't execute

```bash
chmod +x bin/nastran
```

### Problem: Input file not found

Ensure your input file is in the current directory or specify the full path in the interactive menu.

### Problem: "csh: command not found"

Install C Shell:
```bash
# Ubuntu/Debian
sudo apt-get install csh

# Fedora/RHEL
sudo dnf install tcsh
```

### Problem: FORTRAN runtime errors

Check that:
- You have enough disk space for temporary files
- The work directory has write permissions
- Memory allocation is sufficient for your problem size

## Quick Reference

| Menu Option | Description |
|-------------|-------------|
| `i` | Change input file |
| `o` | Change output file |
| `l` | Change log file |
| `pl` | Specify plot file |
| `pu` | Specify punch file |
| `c` | Specify checkpoint file |
| `im` | Set database memory |
| `oc` | Set open core memory |
| `w` | Set work directory |
| `g` | Generate script and run |
| `a` | Abort/quit |

## File Naming Convention

For a problem named `myanalysis`:
- Input: `myanalysis.inp`
- Output: `myanalysis.out`
- Log: `myanalysis.log`
- Plot: `myanalysis.plt`
- Dictionary: `myanalysis.dic`
- Checkpoint: `myanalysis.nptp`

---

**Ready to learn more?** Check out the full [README.md](README.md) and the NASTRAN Users Manual.
