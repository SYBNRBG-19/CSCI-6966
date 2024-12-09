# x86 Vulnerability Checker

## Overview

`x86 Vulnerability Checker` is a Haskell-based tool for analyzing 32-bit x86 binaries for common vulnerabilities, such as format string vulnerabilities and stack integrity issues. It parses binary files, generates an Abstract Syntax Tree (AST), and scans for vulnerabilities, producing a report with detailed findings.

This project uses external tools to disassemble binaries and then processes the output for vulnerability detection. It is built with Haskell and leverages Cabal for dependency management and builds.

## Features

- **Binary Parsing**: Parses 32-bit x86 binaries to extract instructions.
- **AST Generation**: Constructs an Abstract Syntax Tree (AST) from disassembled code.
- **Vulnerability Detection**: Analyzes binaries for common vulnerabilities.
- **Detailed Reporting**: Outputs a report listing identified vulnerabilities.

## Project Structure

```
x86-vulnerability-checker/
│
├── src/
│   ├── Main.hs                # Entry point for the executable
│   ├── BinaryParser.hs        # Parses binaries and extracts instructions
│   ├── ASTGenerator.hs        # Generates AST from instructions
│   ├── VulnerabilityScanner.hs # Scans AST for vulnerabilities
│   ├── FormatStringChecker.hs  # Detects format string vulnerabilities
│   └── ReportGenerator.hs     # Generates vulnerability report
│
├── test/
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
   git clone https://github.com/Lianting-Wang/x86-vulnerability-checker.git
   cd x86-vulnerability-checker
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
cabal run x86-vulnerability-checker -- ./test-binaries/example.bin
```

This will output a report detailing any detected vulnerabilities in the specified binary.

## Modules

### 1. BinaryParser
Handles parsing the binary file using external tools (e.g., `objdump`) and extracts instructions.

### 2. ASTGenerator
Converts the list of instructions into an Abstract Syntax Tree (AST), which represents the binary's structure.

### 3. VulnerabilityScanner
Analyzes the AST for known vulnerability patterns. Uses various checker modules to detect specific issues.

### 4. FormatStringChecker
Identifies format string vulnerabilities by examining certain function calls like `printf`.

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
