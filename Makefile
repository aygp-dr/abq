# Agent Bus Queue (abq) Makefile
# Org-mode tooling + Python development with uv

EMACS ?= emacs
UV ?= uv
PYTHON ?= python3
ORG_FILES := $(wildcard *.org) $(wildcard **/*.org)
TANGLE_TARGET := abq-spec.org

.PHONY: all help dev test lint clean install

all: lint test ## Run lint and tests

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

#---------------------------------------------------------------------------
# Python Development (uv)
#---------------------------------------------------------------------------

dev: ## Set up development environment with uv
	$(UV) venv
	$(UV) pip install -e ".[dev]"
	@echo "Run 'source .venv/bin/activate' to activate"

test: ## Run Python tests with pytest
	$(UV) run pytest tests/ -v

test-cov: ## Run tests with coverage
	$(UV) run pytest tests/ -v --cov=lib/python/abq --cov-report=term-missing

lint-py: ## Lint Python code with ruff
	$(UV) run ruff check lib/python/abq tests

lint-py-fix: ## Auto-fix Python lint issues
	$(UV) run ruff check --fix lib/python/abq tests

format: ## Format Python code with ruff
	$(UV) run ruff format lib/python/abq tests

typecheck: ## Type check with mypy
	$(UV) run mypy lib/python/abq

#---------------------------------------------------------------------------
# Org-mode Tangling
#---------------------------------------------------------------------------

tangle: ## Tangle all code blocks from agent-bus-spec.org
	@echo "Tangling $(TANGLE_TARGET)..."
	$(EMACS) --batch \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(org-babel-tangle-file \"$(TANGLE_TARGET)\")"
	@echo "Making scripts executable..."
	chmod +x ~/.local/bin/abq* 2>/dev/null || true
	chmod +x ~/.local/share/abq/handlers/*.sh 2>/dev/null || true
	chmod +x ~/.local/share/abq/handlers/*.py 2>/dev/null || true
	chmod +x ~/.local/share/abq/handlers/*.rb 2>/dev/null || true
	@echo "Done. Run 'abq init' to initialize the agent bus."

tangle-dry: ## Show what would be tangled (dry run)
	@echo "Files that would be created from $(TANGLE_TARGET):"
	@$(EMACS) --batch \
		--eval "(require 'org)" \
		--eval "(with-temp-buffer \
			(insert-file-contents \"$(TANGLE_TARGET)\") \
			(org-mode) \
			(org-babel-map-src-blocks nil \
				(when (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info)))) \
					(princ (format \"%s\n\" (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info)))))))))"

#---------------------------------------------------------------------------
# Org-mode Linting
#---------------------------------------------------------------------------

lint: lint-org lint-py ## Run all linters

lint-org: ## Lint org files for common issues
	@echo "Linting org files..."
	@for f in $(ORG_FILES); do \
		echo "  Checking $$f..."; \
		$(EMACS) --batch \
			--eval "(require 'org)" \
			--eval "(setq org-element-use-cache nil)" \
			--visit="$$f" \
			--eval "(org-lint)" \
			--eval "(let ((warnings (org-lint))) \
				(when warnings \
					(dolist (w warnings) \
						(princ (format \"  %s:%d: %s\n\" \"$$f\" (car w) (cadr w))))))" \
		2>&1 | grep -v "^Loading" || true; \
	done
	@echo "Lint complete."

lint-json: ## Validate JSON in org src blocks (requires jq)
	@echo "Checking JSON blocks..."
	@for f in $(ORG_FILES); do \
		grep -Pzo '(?s)#\+begin_src json\n\K.*?(?=\n#\+end_src)' "$$f" 2>/dev/null | \
		while IFS= read -r -d '' block; do \
			echo "$$block" | jq . > /dev/null 2>&1 || echo "  Invalid JSON in $$f"; \
		done; \
	done || true
	@echo "JSON check complete."

#---------------------------------------------------------------------------
# Installation
#---------------------------------------------------------------------------

install: ## Install abq CLI via pip/uv
	$(UV) pip install -e .
	@echo "abq installed. Run 'abq init' to initialize."

install-global: ## Install abq globally
	$(UV) tool install .
	@echo "abq installed globally."

install-bin: ## Install standalone bin/abq to ~/.local/bin
	mkdir -p ~/.local/bin
	cp bin/abq ~/.local/bin/abq
	chmod +x ~/.local/bin/abq
	@echo "Installed to ~/.local/bin/abq"

#---------------------------------------------------------------------------
# Development Utilities
#---------------------------------------------------------------------------

run: ## Run abq directly (for testing)
	PYTHONPATH=lib/python $(PYTHON) -m abq.cli $(ARGS)

watch: ## Watch org files and re-tangle on change (requires fswatch)
	@echo "Watching for changes to $(TANGLE_TARGET)..."
	fswatch -o $(TANGLE_TARGET) | xargs -n1 -I{} make tangle

watch-test: ## Watch Python files and re-run tests (requires fswatch)
	@echo "Watching for changes..."
	fswatch -o lib/python tests | xargs -n1 -I{} make test

#---------------------------------------------------------------------------
# Export
#---------------------------------------------------------------------------

export-html: ## Export org files to HTML
	@for f in $(ORG_FILES); do \
		echo "Exporting $$f to HTML..."; \
		$(EMACS) --batch \
			--eval "(require 'org)" \
			--eval "(require 'ox-html)" \
			--visit="$$f" \
			--eval "(org-html-export-to-html)"; \
	done

export-md: ## Export org files to Markdown (GitHub-flavored)
	@for f in $(ORG_FILES); do \
		echo "Exporting $$f to Markdown..."; \
		$(EMACS) --batch \
			--eval "(require 'org)" \
			--eval "(require 'ox-md)" \
			--visit="$$f" \
			--eval "(org-md-export-to-markdown)"; \
	done

#---------------------------------------------------------------------------
# Cleanup
#---------------------------------------------------------------------------

clean: ## Clean build artifacts
	rm -rf .venv __pycache__ .pytest_cache .mypy_cache .ruff_cache
	rm -rf lib/python/abq/__pycache__ tests/__pycache__
	rm -rf *.egg-info dist build
	find . -name "*.pyc" -delete

clean-all: clean ## Clean everything including tangled files
	@echo "This would remove tangled files. Not implemented for safety."
	@echo "Manually remove: ~/.local/bin/abq* ~/.local/share/abq/"

#---------------------------------------------------------------------------
# Info
#---------------------------------------------------------------------------

info: ## Show project info
	@echo "Agent Bus Queue (abq)"
	@echo "====================="
	@echo ""
	@echo "Org files: $(ORG_FILES)"
	@echo "Emacs: $(shell $(EMACS) --version 2>/dev/null | head -1 || echo 'not found')"
	@echo "uv: $(shell $(UV) --version 2>/dev/null || echo 'not found')"
	@echo "Python: $(shell $(PYTHON) --version 2>/dev/null || echo 'not found')"
	@echo ""
	@echo "Tangled targets from $(TANGLE_TARGET):"
	@grep -E "^#\+begin_src.+:tangle" $(TANGLE_TARGET) 2>/dev/null | sed 's/.*:tangle /  /' | sed 's/ .*//' | sort -u || echo "  (none)"
