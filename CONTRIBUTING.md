# Contributing to NASTRAN-95

## Status

NASTRAN-95 is a **historical archive** released by NASA as open source. This repository contains the source code as it existed in the mid-1990s.

**Important:** There is no active development or official technical support for this software. The code is provided as-is for historical, educational, and research purposes.

## Purpose of This Repository

This repository serves to:
- Preserve historic NASA software engineering
- Provide educational resources for finite element analysis
- Enable research into legacy aerospace engineering methods
- Document the foundations of modern commercial FEA software

## How You Can Contribute

While there is no active development, contributions that enhance the historical and educational value are welcome:

### Documentation Improvements

- Clarify build instructions for modern systems
- Document compatibility issues and workarounds
- Add tutorials and learning guides
- Improve code comments explaining historical context
- Create modern visualization tools for NASTRAN output

### Historical Preservation

- Archive historical documentation and references
- Document the original development environment
- Preserve information about NASTRAN's evolution
- Link to related NASA technical reports

### Compatibility Updates

- Port to modern FORTRAN compilers
- Create build systems (CMake, Makefiles) for current platforms
- Fix compilation errors on modern systems
- Document platform-specific issues

### Educational Resources

- Create example problems with detailed explanations
- Develop teaching materials around the codebase
- Build visualization tools for results
- Write comparison studies with modern FEA software

## Contribution Guidelines

### Before Contributing

1. **Respect the historical nature** - Don't modernize unnecessarily; preserve the character of 1970s-era code
2. **Document changes thoroughly** - Explain why changes are needed and what original behavior is preserved/altered
3. **Test on multiple platforms** - Ensure compatibility across Unix/Linux variants if making portability changes
4. **Reference historical context** - Link to original documentation or NASA reports when available

### Making Changes

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/description`)
3. Make your changes with clear commit messages
4. Test thoroughly
5. Submit a pull request with detailed explanation

### Commit Message Format

```
[Category] Brief description

Detailed explanation of what changed and why.
Include references to original documentation if applicable.
Note any compatibility implications.
```

Categories:
- `[DOC]` - Documentation improvements
- `[BUILD]` - Build system changes
- `[PORT]` - Portability fixes
- `[EXAMPLE]` - New examples or tutorials
- `[ARCHIVE]` - Historical documentation

### Code Style

**Preserve the original style** for historical code:
- Keep FORTRAN 77 fixed-format structure
- Maintain original variable naming conventions
- Preserve comments in original form

**For new code** (build scripts, tools):
- Use clear, modern conventions
- Include comprehensive comments
- Follow language-specific best practices

## Areas Needing Help

### High Priority

1. **Modern Build System** - CMake or similar for easier compilation
2. **Platform Compatibility** - Testing and fixes for various Linux distributions
3. **Documentation** - More tutorials and learning materials
4. **Example Visualization** - Tools to visualize example problem results

### Medium Priority

1. **Compiler Compatibility** - Testing with gfortran, ifort, etc.
2. **Shell Script Modernization** - Bash alternatives to csh scripts
3. **Historical Research** - Documenting original development and usage
4. **Comparison Studies** - How NASTRAN-95 compares to modern software

### Low Priority

1. **Code Cleanup** - Fixing benign compiler warnings
2. **Performance Analysis** - Understanding bottlenecks in historical context
3. **Additional Examples** - Creating new demonstration problems

## What NOT to Contribute

Please avoid:
- **Extensive refactoring** - Preserve historical code structure
- **Major feature additions** - This is an archive, not active development
- **Modernization for its own sake** - Changes should serve educational/preservation goals
- **Removal of legacy features** - Even if unused, they're part of the historical record

## Testing

If you make changes that affect functionality:

1. Run existing demonstration problems
2. Compare output with reference results in `demoout/`
3. Document any numerical differences
4. Test on multiple platforms if possible
5. Verify original test cases still pass

## Documentation Standards

All documentation should:
- Be clear and accessible to newcomers
- Respect the historical context
- Include references to original materials
- Use proper markdown formatting
- Be technically accurate

## License

All contributions must be compatible with the [NASA Open Source Agreement version 1.3](https://github.com/nasa/NASTRAN-95/raw/master/NASA%20Open%20Source%20Agreement-NASTRAN%2095.doc).

By contributing, you agree that your contributions will be licensed under the same terms.

## Community Guidelines

- Be respectful and professional
- Help preserve an important piece of software engineering history
- Support learners and researchers using this resource
- Share knowledge about historical computing and aerospace engineering

## Getting Help

For questions about:
- **Historical context** - Try NASA technical reports archives
- **Original usage** - Consult the included manuals (PDF files)
- **Modern FEA** - Refer to commercial NASTRAN vendors
- **Compilation issues** - Open an issue with detailed system information

## Acknowledgments

NASTRAN was developed through collaboration between:
- NASA
- Various aerospace contractors
- Academic institutions
- The broader engineering community

This archive represents the work of countless engineers, programmers, and scientists who pioneered computer-aided engineering.

## Related Projects

Consider contributing to:
- Modern open-source FEA software (CalculiX, Code_Aster, FEniCS)
- Historical software preservation initiatives
- Engineering education resources
- Scientific computing history projects

---

**Thank you for helping preserve this important piece of engineering history!**

For questions or discussions, open an issue on the repository.
