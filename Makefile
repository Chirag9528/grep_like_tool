TESTCASES_DIR := TestCases
CARGO := cargo

# Rust binary path
BINARY := target/debug/project_grep

# Build, Run, and Clean commands
BUILD := $(CARGO) build --manifest-path Cargo.toml
RUN := $(CARGO) run --manifest-path Cargo.toml
CLEAN := $(CARGO) clean --manifest-path Cargo.toml

# Find all input test files (e.g., TestCases/1/test1.in, TestCases/2/test2.in, etc.)
TEST_INPUT_FILES := $(shell find $(TESTCASES_DIR) -type f -name "*.in")

.PHONY: all build run clean test

# Default target: test the project
all: test

# Build the project
build:
	@echo "Building the project..."
	$(BUILD)

# Run the project (provide file path interactively)
run: build
	@echo "Running the project..."
	@for test_input in $(TEST_INPUT_FILES); do \
		$(BINARY) $$test_input; \
	done; \
	echo "Corresponding outputs has been produced and it is in the same directory in which the test file is!"


# Test the project with all test cases
test: build run
	@echo "\nTesting the project with all test cases...\n"
	@for test_input in $(TEST_INPUT_FILES); do \
		test_dir=$$(dirname $$test_input); \
		expected_output=$$test_dir/$$(basename $$test_input .in).out; \
		generated_output=$$test_dir/output.out; \
		echo "Testing with $$test_input..."; \
		if diff -q $$generated_output $$expected_output > /dev/null; then \
			echo "Test passed for $$test_input\n"; \
			echo PASS >> result.log; \
		else \
			echo "Test failed for $$test_input\n"; \
			echo FAIL >> result.log; \
		fi; \
	done; \
	pass_count=$$(grep -c PASS result.log); \
	fail_count=$$(grep -c FAIL result.log); \
	rm -f result.log; \
	echo "Summary: $$pass_count passed, $$fail_count failed."


# Clean the project
clean:
	@echo "Cleaning the project..."
	$(CLEAN)
	@echo "Clean complete!"

clean_output_files:
	@echo "Deleting output files..."
	@for test_input in $(TEST_INPUT_FILES); do \
		test_dir=$$(dirname $$test_input); \
		rm -f $$test_dir/output.out; \
	done