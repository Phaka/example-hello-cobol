# COBOL Hello World Example

A minimal COBOL implementation of "Hello, World!" using GnuCOBOL. This example demonstrates COBOL's distinctive syntax and structure while maintaining our focus on simplicity and cross-platform compatibility.

## Understanding COBOL's Structure

COBOL (Common Business-Oriented Language) was designed to be readable and self-documenting. Its source code follows a strict columnar format that reflects its punch card heritage:

```
Columns   Purpose
1-6       Sequence number area (optional)
7         Indicator area (* for comments)
8-11      Area A (divisions, sections, paragraphs)
12-72     Area B (statements, clauses)
73-80     Identification area (historically for card deck management)
```

Our implementation demonstrates this structure while keeping the program as minimal as possible.

## Prerequisites

You'll need GnuCOBOL installed on your system. Here's how to install it on different platforms:

### Linux
On Debian/Ubuntu:
```bash
sudo apt update
sudo apt install gnucobol
```

On Fedora:
```bash
sudo dnf install gnucobol
```

### macOS
Using Homebrew:
```bash
brew install gnu-cobol
```

### Windows
Download the GnuCOBOL installer from SourceForge:
https://sourceforge.net/projects/gnucobol/files/

The installer will add the necessary tools to your PATH.

## Compiling

GnuCOBOL provides a straightforward compilation command that works consistently across platforms:

### Linux and macOS
```bash
cobc -x -free hello.cob -o hello
```

### Windows
```cmd
cobc -x -free hello.cob -o hello.exe
```

The flags used are:
- `-x`: Create an executable
- `-free`: Allow free-format source code (relaxes the column requirements)

## Running

### Linux and macOS
```bash
./hello
```

### Windows
```cmd
hello.exe
```
Or from PowerShell:
```powershell
.\hello.exe
```

## Code Explanation

Let's examine our COBOL implementation in detail:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
```

The program demonstrates several fundamental COBOL concepts:

1. COBOL programs are divided into four main divisions:
   - IDENTIFICATION DIVISION: Contains program metadata
   - ENVIRONMENT DIVISION: Describes the computing environment
   - DATA DIVISION: Declares data structures
   - PROCEDURE DIVISION: Contains the actual program logic

2. Even in this minimal example, we must include all four divisions to maintain proper COBOL structure, though only IDENTIFICATION and PROCEDURE are strictly required.

3. The PROGRAM-ID is required and provides the program name.

4. The PROCEDURE DIVISION contains our actual code:
   - DISPLAY outputs text to the console
   - STOP RUN terminates the program
   - Each statement ends with a period (.)

5. Indentation is not just for readability—it reflects COBOL's strict columnar format requirements.

## Verifying the Build

You can examine the compiled program:

### Linux
```bash
# Check binary size
ls -l hello
# Check dynamic dependencies
ldd hello
```

### macOS
```bash
# Check binary size
ls -l hello
# Check dynamic dependencies
otool -L hello
```

### Windows
```cmd
dir hello.exe
dumpbin /dependents hello.exe
```

## Historical Context

COBOL was designed in 1959 by the Conference on Data Systems Languages (CODASYL) and was heavily influenced by Grace Hopper's FLOW-MATIC language. Its English-like syntax was revolutionary for its time, making programs more accessible to business users and auditors.

The fixed-format source code structure might seem unusual today, but it reflects COBOL's origin in the era of punch cards. Each column had a specific purpose, and this structure helped prevent errors in card punching and reading.

## Further Reading

- [GnuCOBOL Programmer's Guide](https://gnucobol.sourceforge.io/guides/GNU_COBOL_Programmers_Guide.pdf)
- [COBOL Programming - Tutorials Point](https://www.tutorialspoint.com/cobol/index.htm)
- [The COBOL85 Programming Language](https://www.cs.vu.nl/grammarware/cobol/)
