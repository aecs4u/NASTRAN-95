#!/bin/bash
# Clean up all generated NASTRAN output files

echo "Cleaning up NASTRAN output files in $(pwd) ..."

# Count files before cleanup
BEFORE=$(ls -1 *.{out,log,dic,pun,plt,nptp,txt} 2>/dev/null | wc -l)

# Remove output files
rm -f *.out *.log *.dic *.pun *.plt *.nptp 2>/dev/null
rm -f test_results.txt test_summary.txt 2>/dev/null
rm -f /tmp/nastran_input_tmp.inp 2>/dev/null

# Count files after cleanup
AFTER=$(ls -1 *.{out,log,dic,pun,plt,nptp,txt} 2>/dev/null | wc -l)

REMOVED=$((BEFORE - AFTER))

echo "Removed $REMOVED files"
echo "Remaining files:"
ls -lh 2>/dev/null || echo "  (directory is clean)"
