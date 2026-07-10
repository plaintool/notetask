# Lazarus build tool
LAZBUILD = lazbuild

# Main project file
PROJECT = pobatch.lpi

# Build mode
BUILD_MODE = Release

# Additional options (e.g. --cpu=i386 for 32-bit)
LAZBUILD_OPTS =

# Dependency LPK packages to build first
DEP_LPKS = \
    libs/darkmode/darkmode.lpk \
    libs/dcpcrypt/dcpcrypt.lpk \
    libs/gridprinter/gridprinter.lpk \
    libs/tagedit/tagedit.lpk
    
# Default target: build dependencies then the project
all: deps
	@echo "Building main project..."
	$(LAZBUILD) --build-mode=$(BUILD_MODE) $(LAZBUILD_OPTS) $(PROJECT)

# Build all dependency packages
deps:
	@for lpk in $(DEP_LPKS); do \
		echo "Building $$lpk"; \
		$(LAZBUILD) $$lpk $(LAZBUILD_OPTS) -q || exit 1; \
	done

# Remove compiled units and the final binary
clean:
	find . -type f \( -name "*.o" -o -name "*.ppu" -o -name "*.compiled" \) -delete
	rm -f pobatch

.PHONY: all deps clean