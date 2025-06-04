# compiler to use
CC = clang

# flags to pass compiler
CFLAGS = -O2 -Wall -Wshadow -Wextra

# name for executable
EXE = vm

# space-separated list of header files
HDRS =

# space-separated list of libraries, if any,
# each of which should be prefixed with -l
LIBS = -lgc

# space-separated list of source files
SRCS = vm.c

# automatically generated list of object files
OBJS = $(SRCS:.c=.o)


# default target
# $(EXE): $(OBJS) $(HDRS) Makefile
# 	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

# dependencies
$(OBJS): $(HDRS) Makefile

# housekeeping
.PHONY: clean
clean:
	rm -f core $(EXE) *.o tests/*.c tests/*_runner tests/*.output tests/output/*.*


# Testing
TEST_FILES = $(wildcard tests/test_*.scm)
# TEST_BASE_NAMES will contain the part after "test_", e.g., "simple_display", "lambda_capture_one_var"
TEST_BASE_NAMES = $(patsubst tests/test_%.scm,%,$(TEST_FILES))

.PHONY: test
test: vm.o
	@echo "Running tests..."
	@mkdir -p tests/output # Ensure output directory exists
	@for test_base_name in $(TEST_BASE_NAMES); do \
		echo "----------------------------------------"; \
		echo "Running test: test_$$test_base_name"; \
		guile -l scheme-to-c.scm -c "(call-with-output-file \"tests/output/test_$$test_base_name.c\" (lambda (p) (parameterize ((compile-port p)) (emit-program (call-with-input-file \"tests/test_$$test_base_name.scm\" read)))))" ; \
		$(CC) $(CFLAGS) -o tests/output/test_$$test_base_name_runner tests/output/test_$$test_base_name.c vm.o $(LIBS) ; \
		./tests/output/test_$$test_base_name_runner | perl -pe 's/\r?\n?$$//' > tests/output/test_$$test_base_name.actual ; \
		if diff -w -u tests/test_$$test_base_name.expected tests/output/test_$$test_base_name.actual; then \
			echo "PASS: test_$$test_base_name"; \
		else \
			echo "FAIL: test_$$test_base_name. See diff below."; \
			diff -w -u tests/test_$$test_base_name.expected tests/output/test_$$test_base_name.actual; \
			# exit 1; # Uncomment to stop on first failure \
		fi ; \
	done
	@echo "----------------------------------------"
	@echo "Test run complete."

# Add vm.o to the default target dependencies if it isn't rebuilt every time by the test target logic
# $(EXE): $(OBJS) $(HDRS) Makefile vm.o # Assuming vm.o is built as part of $(OBJS)
