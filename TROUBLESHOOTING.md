# NASTRAN-95 Troubleshooting Guide

Common issues and solutions when running NASTRAN-95 on modern systems.

## Installation and Setup Issues

### C Shell (csh) Not Found

**Symptom:** `csh: command not found` or script won't execute

**Solution:**

```bash
# Ubuntu/Debian
sudo apt-get install csh

# Fedora/RHEL/CentOS
sudo dnf install tcsh

# Arch Linux
sudo pacman -S tcsh
```

Alternatively, create a bash version of the scripts or use tcsh as a compatible alternative.

### Script Permission Denied

**Symptom:** Permission denied when trying to run `./bin/nastran`

**Solution:**

```bash
chmod +x bin/nastran
chmod +x bin/linknas
chmod +x bin/chkfil.exe
chmod +x bin/nastrn.exe
```

### Path Issues

**Symptom:** Scripts reference `/usr/users/bob/nast95/`

**Solution:** The `nastran` script contains hardcoded paths that need to be updated:

```bash
# Edit bin/nastran
# Change line 4:
set rfdir=/usr/users/bob/nast95/rf
# To:
set rfdir=/path/to/your/NASTRAN-95/rf

# Change line 5:
set nasexec=/usr/users/bob/nast95/bin/nastrn.exe
# To:
set nasexec=/path/to/your/NASTRAN-95/bin/nastrn.exe

# Change line 6:
set naschk=/usr/users/bob/nast95/bin/chkfil.exe
# To:
set naschk=/path/to/your/NASTRAN-95/bin/chkfil.exe
```

Or use absolute paths when running.

## Compilation Issues

### FORTRAN Compiler Not Found

**Symptom:** `f77: command not found` when running linknas

**Solution:**

Install a FORTRAN compiler:

```bash
# Ubuntu/Debian
sudo apt-get install gfortran

# Fedora/RHEL
sudo dnf install gcc-gfortran

# macOS (using Homebrew)
brew install gcc
```

Then modify `linknas` script to use `gfortran` instead of `f77`:

```bash
# Edit bin/linknas, change:
f77 -fast -dn -o ../bin/nastrn.exe
# To:
gfortran -O2 -o ../bin/nastrn.exe
```

### Compilation Warnings

**Symptom:** Numerous warnings about line length, obsolete features

**Solution:** These are normal for FORTRAN 77 code. Add compiler flags:

```bash
gfortran -std=legacy -w -o nastrn.exe ...
```

- `-std=legacy` - Accept legacy FORTRAN features
- `-w` - Suppress warnings
- `-O2` - Optimization level 2

### Line Terminator Issues

**Symptom:** Compiler errors about invalid characters or line endings

**Solution:** FORTRAN files have Windows-style (CRLF) line terminators. Convert if needed:

```bash
# Install dos2unix
sudo apt-get install dos2unix  # Ubuntu/Debian

# Convert files
find mis/ -name "*.f" -exec dos2unix {} \;
```

## Runtime Issues

### Input File Not Found

**Symptom:** `#### does not exist ####` next to input file

**Solution:**

```bash
# Ensure input file exists
ls inp/d01011a.inp

# If running from different directory, use full path
# or run from NASTRAN-95 root directory
cd /path/to/NASTRAN-95
./bin/nastran d01011a
```

### Memory Allocation Errors

**Symptom:** FORTRAN runtime error about array bounds or memory

**Solution:** Increase memory allocation in the interactive menu:

1. Run `./bin/nastran problem_name`
2. Type `im` for in-memory database
3. Enter larger value (e.g., `24000000` for 24MB)
4. Type `oc` for open core
5. Enter larger value (e.g., `4000000` for 4MB)
6. Continue with `g` to execute

Or edit the script to change defaults:
```csh
set dbmem=24000000  # line 16 in bin/nastran
set ocmem=4000000   # line 17 in bin/nastran
```

### Work Directory Already Exists

**Symptom:** Warning that work directory exists and will be recreated

**Solution:** This is normal. Choose `y` to continue or change work directory:

1. In the interactive menu, type `w`
2. Specify a different directory
3. Continue with `g`

Or manually clean up:
```bash
rm -rf temp*
```

### Output File Permission Denied

**Symptom:** Cannot write output files

**Solution:**

```bash
# Check directory permissions
ls -la

# Ensure write permission
chmod u+w .

# Check disk space
df -h .
```

### Temporary File Issues

**Symptom:** "Error opening temporary file" or similar

**Solution:**

```bash
# Check /tmp space
df -h /tmp

# Or specify different work directory with more space
# In the nastran menu, type 'w' and specify a directory with ample space
```

## Output and Results Issues

### Numerical Differences from Reference Output

**Symptom:** `diff` shows numerical differences from `demoout/` files

**Solution:** Small numerical differences are expected due to:
- Different compiler optimizations
- Different floating-point hardware
- Different math library implementations

Use numerical comparison tools:

```bash
# Instead of diff, use numerical comparison
# (requires custom script or tool)

# Or visually inspect key results
grep "DISPLACEMENT" problem.out
grep "STRESS" problem.out
```

Differences smaller than 0.01% are generally acceptable.

### Output File Too Large

**Symptom:** Output files are enormous, hard to navigate

**Solution:** NASTRAN output can be verbose. Use:

```bash
# View specific sections
grep -A 20 "DISPLACEMENT VECTOR" problem.out
grep -A 20 "STRESS" problem.out

# Page through output
less problem.out

# Extract summary
head -100 problem.out
tail -100 problem.out
```

### Plot Files Not Readable

**Symptom:** `.plt` files in binary format, not human-readable

**Solution:** NASTRAN plot files require specialized viewers. Options:
- Historical NASTRAN plot utilities (not included)
- Convert data using custom scripts
- Extract numerical data from `.out` files instead

## Performance Issues

### Analysis Runs Very Slowly

**Symptom:** Problem takes excessive time on modern hardware

**Solution:**

1. **Check problem size** - Large models may need hours even on modern systems
2. **Reduce output requests** - Excessive output requests slow execution
3. **Optimize memory** - Ensure adequate database and open core allocation
4. **Use compiled executables** - Ensure using `nastrn.exe`, not interpreted code

### Modern System Performance

NASTRAN-95 was designed for 1970s-1990s hardware and may not fully utilize modern multi-core processors. Performance will be limited by:
- Single-threaded execution
- Limited memory addressing
- I/O bound operations

## File Format Issues

### Invalid Card Format

**Symptom:** Errors about card format or column position

**Solution:** NASTRAN uses 80-column card format:
- Fixed field format: specific columns for specific data
- Free field format: comma or space delimited

Ensure proper formatting:
```
GRID    1       0       0.0     0.0     0.0     0
        ^       ^       ^       ^       ^       ^
Column: 1       9       17      25      33      41
```

Use fixed-width fonts when editing input files.

### Character Encoding Issues

**Symptom:** Strange characters or encoding errors

**Solution:**

```bash
# Convert to ASCII
iconv -f UTF-8 -t ASCII//TRANSLIT input.inp > output.inp

# Or ensure files are plain ASCII
file input.inp  # should report "ASCII text"
```

## Modern System Compatibility

### 64-bit vs 32-bit Issues

**Symptom:** Segmentation faults or address errors

**Solution:** NASTRAN-95 was designed for 32-bit systems. On 64-bit Linux:

```bash
# Check executable type
file bin/nastrn.exe

# May need to recompile for 64-bit
# or install 32-bit compatibility libraries
sudo apt-get install libc6-i386  # Ubuntu/Debian
```

### Missing Libraries

**Symptom:** "error while loading shared libraries"

**Solution:**

```bash
# Check dependencies
ldd bin/nastrn.exe

# Install missing libraries as indicated
```

## Getting More Help

### Enable Verbose Output

Check the log file for detailed execution information:

```bash
cat problem.log
```

### Check System Resources

```bash
# Available memory
free -h

# Disk space
df -h

# System limits
ulimit -a
```

### Create Minimal Test Case

If problems persist:

1. Try the simplest example: `d01000a`
2. If it works, gradually increase complexity
3. Identify which feature causes the problem

### Documentation Resources

- **NASTRAN Users Manual 2.pdf** - Complete input format reference
- **NASTRAN Programmers Manual.pdf** - Internal architecture details
- **Example .txt files** - Problem descriptions in `inp/` directory

### Report Issues

When reporting issues, include:
- Operating system and version
- FORTRAN compiler and version
- Complete error messages
- Input file (if custom problem)
- Steps to reproduce

---

## Quick Diagnostic Checklist

- [ ] C Shell (csh/tcsh) installed?
- [ ] Scripts have execute permission?
- [ ] Paths in scripts updated for your system?
- [ ] Input file exists and is readable?
- [ ] Adequate disk space in work directory?
- [ ] Memory allocation sufficient?
- [ ] FORTRAN runtime libraries installed?
- [ ] File permissions allow write access?

If all checks pass and problems persist, consult the detailed sections above.
