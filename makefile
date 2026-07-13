# Lazarus build tool
LAZBUILD = lazbuild

# Main project file
PROJECT = notetask.lpi

# Build mode
BUILD_MODE = Release

# Additional options (e.g. --cpu=i386 for 32-bit)
LAZBUILD_OPTS =

# Dependency LPK packages to build first
DEP_LPKS = \
    libs/darkmode/darkmode.lpk \
    libs/dcpcrypt/dcpcrypt.lpk \
    libs/filemanager/filemanager.lpk \
    libs/gridprinter/gridprinter.lpk \
    libs/helpers/helpers.lpk \
    libs/tagedit/tageditpackage.lpk \
    libs/toolkit/toolkit.lpk

# Default target: get submodules, build dependencies then the project
all: submodules deps
	@echo "Building main project..."
	$(LAZBUILD) --build-mode=$(BUILD_MODE) $(LAZBUILD_OPTS) $(PROJECT)

# Get submodules at the versions recorded by the repository
submodules:
	@if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then \
		git submodule update --init --recursive || true; \
	fi

# Build all dependency packages
deps:
	@for lpk in $(DEP_LPKS); do \
		if [ -f "$$lpk" ]; then \
			echo "Building $$lpk"; \
			$(LAZBUILD) $$lpk $(LAZBUILD_OPTS) -q || echo "WARNING: Failed to build $$lpk, skipping"; \
		else \
			echo "WARNING: $$lpk not found, skipping"; \
		fi; \
	done

# Remove compiled units and the final binary
clean:
	find . -type f \( -name "*.o" -o -name "*.ppu" -o -name "*.compiled" \) -delete
	rm -f notetask

.PHONY: all submodules deps clean