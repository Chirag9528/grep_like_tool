# Grep like tool

This project implements a tool similar to grep regular expression checker. It takes in a regular expression and a string and checks if the string belongs to the language of the regular expression.

## Structure of the Project
```
|-project_grep
    |-src               #directory containing the code
    |-TestCases         #directory containing test cases
    |-Makefile           
    |-README.md

```
# Overview
The implementation is structured into multiple stages, based on the concepts and techniques learned during the course.

## Project Stages:
### Stage 1: Building Fundamental Components
This stage involves implementing the foundational building blocks required for the project. Each sub-stage is independent and focuses on a specific task:

>Stage 1a: Create an NFA (Non-deterministic Finite Automaton) that accepts a specific string and rejects all others.

>Stage 1b: Parse a regular expression and construct an equivalent NFA or DFA (Deterministic Finite Automaton).

>Stage 1c: Implement subset construction to convert an NFA into an equivalent DFA.

>Stage 1d: Minimize a given DFA using the DFA minimization algorithm to create a minimal DFA.

### Stage 2: Construct Minimal DFAs
>Use Stage 1a and Stage 1c to obtain a DFA that accepts the input string.

>Use Stage 1b and Stage 1c to construct a DFA that accepts the complement of the language defined by the regular expression.

>Minimize both resulting DFAs using Stage 1d.

### Stage 3: Compute Intersection of DFAs
> Use product construction to compute the intersection of the two minimal DFAs from Stage 2. The resulting DFA accepts the intersection of the two languages.

### Stage 4: Final Verification
> Check if the input string belongs to the language defined by the regular expression by verifying whether the resulting DFA from Stage 3 accepts the string.

# How to Use

## 1. Building the Project
To Complile the project:
```
make build
``` 

## 2. Running the Project
To run the project:
```
make run
```

## 3. Testing the Project
To test the project using predefined test cases located in the TestCases directory:
```
make test
```
Note: If you want to test against your custom test file , then include it in the TestCases directory like predefined test cases.

## 4. Cleaning Build Files
To clean the build artifacts:
```
make clean
```

## 5. Cleaning Test Output Files
To delete all generated output files
```
make clean_output_files
```


# Input and Output Format
## Input
The program expects an input file containing:
-    An integer n (number of test cases).
-   For each test case:
    -   A regular expression (on one line).
    -   A string to check against the regular expression (on the next line).

### Note: Don't keep spaces in between the regular expression and the supported operations in the regular expression are union (+), concat (·) and star (∗).

Example: <b>test1.in</b>
```
3
star(symbol(a))
aaaa
concat(star(symbol(b)),symbol(a))
bbaa
union(symbol(a),symbol(b))
b

```

## Output
For each test case, the program generates an output file indicating whether the string matches the regular expression. The output file will contain:

-   <b>Yes</b> if the string matches.
-   <b>No</b> otherwise.

Example: <b>output.out</b>
```
Yes
No
Yes
```

## Submitted by:
-   ### Chirag Varshney (112201023)
