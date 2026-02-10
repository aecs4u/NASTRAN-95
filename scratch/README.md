# NASTRAN-95 Scratch Directory

This directory is for running NASTRAN examples and tests.

## Quick Start

```bash
# Run a quick test on a few examples
./run_quick_test.sh

# Run all examples (takes longer)
./run_all_examples.sh

# Clean up output files
./cleanup.sh
```

## Scripts

### `run_all_examples.sh`
Runs all example files from `../examples/input/` and generates a comprehensive test report.

**Features:**
- ✅ Runs all .inp files automatically
- ⏱️  300-second timeout per test
- 📊 Generates test_results.txt and test_summary.txt
- 🎨 Color-coded output (PASS/FAIL/WARNING)
- 📈 Summary statistics

**Output:**
- Creates .out, .log, .dic, .pun, .plt, .nptp files for each test
- `test_results.txt` - Detailed pass/fail for each file
- `test_summary.txt` - Overall summary with statistics

### `run_quick_test.sh`
Runs a quick test on a few representative examples for rapid feedback.

**Use cases:**
- Quick validation after code changes
- CI/CD pipeline smoke tests
- Development iteration

### `cleanup.sh`
Removes all generated output files to start fresh.

**Removes:**
- *.out, *.log, *.dic, *.pun, *.plt, *.nptp
- test_results.txt, test_summary.txt
- Temporary files

## Manual Testing

Run a specific example:

```bash
export DBMEM=12000000 OCMEM=2000000 RFDIR=/tmp/nrf DIRCTY=/tmp
../build/bin/nastran ../examples/input/d01000a.inp
```

Output files will be created with the input file's rootname:
- `d01000a.out` - Console output
- `d01000a.log` - Analysis log
- `d01000a.dic` - Dictionary
- `d01000a.pun` - Punch output
- `d01000a.plt` - Plot data
- `d01000a.nptp` - NPTP data

## Environment Variables

The scripts automatically set:
- `DBMEM=12000000` - Database memory (12M words)
- `OCMEM=2000000` - Open core memory (2M words)
- `RFDIR=/tmp/nrf` - Rigid format directory
- `DIRCTY=/tmp` - Scratch file directory

Override if needed:
```bash
DBMEM=20000000 OCMEM=4000000 ./run_all_examples.sh
```

## Interpreting Results

### PASS (Green ✓)
- Program completed successfully
- Output files created
- No fatal errors

### WARNING (Yellow ⚠)
- Program completed
- Output files created
- But contains FATAL error messages
- May still produce valid results

### FAIL (Red ✗)
- Program crashed or timed out
- Missing output files
- Non-zero exit code

## Tips

1. **First run**: Start with `run_quick_test.sh` to verify basic functionality
2. **Full validation**: Use `run_all_examples.sh` before committing code changes
3. **Debugging**: Check individual .out and .log files for detailed error messages
4. **Performance**: Tests may take 10-30 minutes for all examples
5. **Clean state**: Run `cleanup.sh` before starting a new test session

## Comparing with Reference

Reference outputs are in `../examples/reference_output/`:

```bash
# Compare specific output
diff scratch/d01000a.out examples/reference_output/d01000a.out

# Compare line counts
wc -l scratch/*.out examples/reference_output/*.out
```

Note: Reference outputs are from the original 1995 COSMIC NASTRAN-95 and may differ from the modernized version.
