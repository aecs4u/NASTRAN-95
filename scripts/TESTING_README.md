# NASTRAN-95 Testing Scripts

## Important: Run from scratch/ directory!

These test scripts are designed to be run from the `scratch/` directory:

```bash
cd scratch
../scripts/run_quick_test.sh      # Quick test
../scripts/run_parallel.sh        # Parallel execution
../scripts/run_all_examples.sh    # Sequential with reports
../scripts/cleanup.sh              # Clean up outputs
```

## Why run from scratch/?

- Output files (.out, .log, .dic, etc.) are created in the current directory
- Keeps the repository clean
- Easy to compare outputs and clean up

## Scripts Overview

### run_quick_test.sh (⚡ ~1 minute)
Tests 3 representative examples for quick validation.

### run_parallel.sh (🚀 ~5-10 minutes) - **UPDATED**
Runs all examples in parallel with **segfault protection**:
- **Reduced to 2 parallel jobs** for stability
- **Isolated execution**: Each job has separate temp directories
- **Better error detection**: Catches segfaults and reports them
- **Job isolation**: No interference between parallel tests

**Key improvements:**
- Each test runs in `parallel_results/<testname>/` directory
- Unique temp directories prevent conflicts
- Segfaults are detected and logged
- Failed tests show detailed logs

### run_all_examples.sh (📊 ~20-30 minutes)
Sequential execution with detailed timing and reports.

### cleanup.sh (🧹)
Removes all generated output files.

## Segfault Prevention

The updated `run_parallel.sh` prevents segmentation faults by:

1. **Job Isolation**: Each test gets its own directory
2. **Separate Temp Files**: `DIRCTY` set to unique path per job
3. **Reduced Parallelism**: MAX_JOBS=2 instead of 4
4. **Error Detection**: Catches exit code 139 (segfault)

If segfaults still occur, try:
```bash
# Run sequentially (no parallel conflicts)
../scripts/run_all_examples.sh

# Or run specific tests manually
export DBMEM=12000000 OCMEM=2000000 RFDIR=/tmp/nrf DIRCTY=/tmp
../build/bin/nastran ../examples/input/d01000a.inp
```

## Debugging Failed Tests

Check the parallel results:
```bash
# See what failed
ls parallel_results/*/status.txt

# Check a specific test log
cat parallel_results/d01000a/run.log

# View output
cat parallel_results/d01000a/d01000a.out
```

## Example Usage

```bash
# From project root
cd scratch

# Clean slate
../scripts/cleanup.sh

# Quick validation
../scripts/run_quick_test.sh

# Full test suite with segfault protection
../scripts/run_parallel.sh

# Check specific results
ls -lh parallel_results/
cat parallel_results/*/status.txt
```

## Manual Single Test

```bash
cd scratch
export DBMEM=12000000 OCMEM=2000000 RFDIR=/tmp/nrf DIRCTY=/tmp
../build/bin/nastran ../examples/input/d01000a.inp

# Check output
ls -lh d01000a.*
head d01000a.out
```

## Comparing with Reference

```bash
cd scratch
diff d01000a.out ../examples/reference_output/d01000a.out
```

## Notes

- The scripts set appropriate memory limits (DBMEM, OCMEM)
- RFDIR points to the rigid format directory
- Temporary files go to /tmp or isolated directories
- All output files stay in scratch/
