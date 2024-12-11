# x86 Vulnerability Checker

## Overview

`x86 Vulnerability Checker` is a Haskell-based tool for analyzing 32-bit x86 binaries for common vulnerabilities, such as format string vulnerabilities and stack integrity issues. It parses binary files, generates a Control Flow Graph (CFG), and scans for vulnerabilities, producing a report with detailed findings.

This project uses external tools to disassemble binaries and then processes the output for vulnerability detection. It is built with Haskell and leverages Cabal for dependency management and builds.

## Features

- **Binary Parsing**: Parses 32-bit x86 binaries to extract instructions.
- **CFG Generation**: Constructs a Control Flow Graph (CFG) from disassembled code.
- **Vulnerability Detection**: Analyzes binaries for common vulnerabilities.
- **Detailed Reporting**: Outputs a report listing identified vulnerabilities.

## Project Structure

```
x86-vulnerability-checker/
│
├── app/
│   ├── Main.hs                # Entry point for the executable
|
├── src/
│   ├── Types.hs               # defines data structure for vulnerability
│   ├── BinaryParser.hs        # Parses binaries and extracts instructions
│   ├── CFGGenerator.hs        # Generates CFG from instructions
│   ├── CFGPrinter.hs          # Prints CFG in a human-readable format
│   ├── VulnerabilityScanner.hs # Scans CFG for vulnerabilities
│   ├── FormatStringChecker.hs  # Detects format string vulnerabilities
│   ├── StackIntegrityChecker.hs  # Detects stack integrity vulnerabilities
│   ├── BufferOverflowChecker.hs  # Detects buffer overflow vulnerabilities
│   └── ReportGenerator.hs     # Generates vulnerability report
│
├── test/
│   ├── <module>Spec.hs        # Unit test for a module
│   └── Spec.hs                # Basic test cases for each module
│
├── x86-vulnerability-checker.cabal # Project configuration and dependencies
├── README.md                  # Project README
└── LICENSE                    # License information
```

## Installation

Ensure that you have **GHC** and **Cabal** installed. You can install them from [Haskell Platform](https://www.haskell.org/platform/) if they are not already installed.

### Steps

1. Clone this repository:

   ```bash
   git clone https://github.com/SYBNRBG-19/CSCI-6966.git 6966
   cd 6966/Project/code
   ```

2. Initialize and build the project:

   ```bash
   cabal update
   cabal build
   ```

3. (Optional) Run tests to verify the setup:

   ```bash
   cabal test
   ```

## Usage

To run the tool on a binary file, use the following command:

```bash
cabal run x86-vulnerability-checker -- path/to/binary
```

Example:

```bash
cabal run x86-vulnerability-checker -- ./test-binaries/simple.bin
```

This will output a report detailing any detected vulnerabilities in the specified binary. 
We have four testable binaries here: simple.bin, arithmetic.bin, format_string.bin and if_statement.bin. 

## Modules

### 1. BinaryParser
Handles parsing the binary file using external tools (e.g., `objdump`) and extracts instructions.

### 2. CFGGenerator
Converts the list of instructions into a Control Flow Graph (CFG), which is a basic building block of program analysis.

### 3. VulnerabilityScanner
Analyzes the AST for known vulnerability patterns. Uses various checker modules to detect specific issues.

### 4. Checkers
Identifies three types of vulnerabilities by examining certain function calls like `printf`.

### 5. ReportGenerator
Produces a report summarizing the vulnerabilities found during the scan.

## Testing

This project includes unit tests to validate core functionality. Run the tests with:

```bash
cabal test
```

## External Tool Dependencies

The project relies on external disassembly tools such as `objdump` (Linux). Install `binutils` if not already installed:

```bash
sudo apt-get install binutils
```

Ensure the required tools are available in your system’s PATH.

## Future Enhancements

- **Enhanced Vulnerability Detection**: Add additional vulnerability patterns.
- **Cross-Platform Support**: Improve compatibility with Windows and macOS.
- **Improved Reporting**: Format reports in multiple output formats (JSON, HTML).

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request with any changes or improvements.
