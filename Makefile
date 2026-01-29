# Agent Bus Queue (abq) Makefile
# Org-mode tooling + Python development with uv

EMACS ?= emacs
UV ?= uv
PYTHON ?= python3
ORG_FILES := $(wildcard *.org) $(wildcard **/*.org)
TANGLE_TARGET := abq-spec.org

.PHONY: all help dev test lint clean install dot-render dot-tangle detangle presentations

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
# Org-mode Tangling (bidirectional)
#
# Workflow:
#   abq-spec.org ──tangle──▶ docs/*.dot, ~/.local/bin/abq*, ...
#                ◀─detangle── (requires :comments link on src blocks)
#
# The org file is the single source of truth. Tangled files are generated.
# If you edit a tangled file directly, run `gmake detangle` to sync changes
# back into abq-spec.org before committing. The pre-commit hook warns if
# generated files are staged without a corresponding org source change.
#
# Full rebuild:  gmake tangle dot-render
# Round-trip:    gmake tangle dot-render detangle
#---------------------------------------------------------------------------

tangle: ## Tangle all code blocks from abq-spec.org
	@echo "Tangling $(TANGLE_TARGET)..."
	$(EMACS) --batch \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(add-to-list 'org-src-lang-modes '(\"dot\" . c))" \
		--eval "(org-babel-tangle-file \"$(TANGLE_TARGET)\")"
	@echo "Making scripts executable..."
	chmod +x ~/.local/bin/abq* 2>/dev/null || true
	chmod +x ~/.local/share/abq/handlers/*.sh 2>/dev/null || true
	chmod +x ~/.local/share/abq/handlers/*.py 2>/dev/null || true
	chmod +x ~/.local/share/abq/handlers/*.rb 2>/dev/null || true
	@echo "Done. Run 'abq init' to initialize the agent bus."

detangle: ## Detangle: sync edits from tangled files back into org source
	@echo "Detangling back into $(TANGLE_TARGET)..."
	$(EMACS) --batch \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--visit="$(TANGLE_TARGET)" \
		--eval "(org-babel-detangle)"
	@echo "Detangle complete."

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
# Graphviz / Dot Rendering
#---------------------------------------------------------------------------

DOT ?= dot
DOT_FILES := $(wildcard docs/*.dot)
DOT_SVG := $(DOT_FILES:.dot=.svg)
DOT_PNG := $(DOT_FILES:.dot=.png)

dot-render: $(DOT_PNG) $(DOT_SVG) ## Render dot files to PNG and SVG

docs/%.png: docs/%.dot
	$(DOT) -Tpng -o $@ $<

docs/%.svg: docs/%.dot
	$(DOT) -Tsvg -o $@ $<

dot-tangle: tangle dot-render ## Tangle org then render dot files

#---------------------------------------------------------------------------
# Remote Sync (configure REMOTE_HOST in .env)
#---------------------------------------------------------------------------

-include .env

.env: .env.template
	cp .env.template .env
	@echo "Created .env from template. Edit it to set REMOTE_HOST."

sync-down: ## Pull channels from remote host
	@test -n "$(REMOTE_HOST)" || { echo "Error: REMOTE_HOST not set. Copy .env.template to .env and configure." >&2; exit 1; }
	$(UV) run abq sync-remote $(REMOTE_HOST) down -v

sync-up: ## Push channels to remote host
	@test -n "$(REMOTE_HOST)" || { echo "Error: REMOTE_HOST not set. Copy .env.template to .env and configure." >&2; exit 1; }
	$(UV) run abq sync-remote $(REMOTE_HOST) up -v

sync-both: ## Bidirectional sync with remote host
	@test -n "$(REMOTE_HOST)" || { echo "Error: REMOTE_HOST not set. Copy .env.template to .env and configure." >&2; exit 1; }
	$(UV) run abq sync-remote $(REMOTE_HOST) both -v

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
# Research Papers (downloaded to data/resources/research/)
#---------------------------------------------------------------------------

RESEARCH_DIR := data/resources/research

# Non-phony targets - only downloads if file doesn't exist
$(RESEARCH_DIR)/2505.02279.pdf:
	@mkdir -p $(RESEARCH_DIR)
	curl -L -o $@ https://arxiv.org/pdf/2505.02279.pdf
	@echo "Downloaded: Survey of Agent Interoperability Protocols (MCP, ACP, A2A, ANP)"

$(RESEARCH_DIR)/2502.14321.pdf:
	@mkdir -p $(RESEARCH_DIR)
	curl -L -o $@ https://arxiv.org/pdf/2502.14321.pdf
	@echo "Downloaded: Beyond Self-Talk - Multi-Agent Communication Survey"

$(RESEARCH_DIR)/1704.00411.pdf:
	@mkdir -p $(RESEARCH_DIR)
	curl -L -o $@ https://arxiv.org/pdf/1704.00411.pdf
	@echo "Downloaded: Survey of Distributed Message Broker Queues"

.PHONY: research-papers
research-papers: $(RESEARCH_DIR)/2505.02279.pdf $(RESEARCH_DIR)/2502.14321.pdf $(RESEARCH_DIR)/1704.00411.pdf ## Download research papers from arxiv

#---------------------------------------------------------------------------
# Presentations (PDF export from org-mode)
#---------------------------------------------------------------------------

PRES_DIR := docs/presentations
PRES_ORG := $(wildcard $(PRES_DIR)/*.org)
PRES_PDF := $(PRES_ORG:.org=.pdf)

# Non-phony targets - only rebuilds if org file changed
$(PRES_DIR)/%.pdf: $(PRES_DIR)/%.org
	@echo "Exporting $< to PDF..."
	$(EMACS) --batch \
		--eval "(require 'org)" \
		--eval "(require 'ox-latex)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\"))" \
		--visit="$<" \
		--eval "(org-latex-export-to-pdf)"
	@echo "Created $@"

presentations: $(PRES_PDF) ## Build presentation PDFs from org files
	@echo "Built $(words $(PRES_PDF)) presentation PDFs"

presentations-clean: ## Remove generated presentation PDFs
	rm -f $(PRES_DIR)/*.pdf $(PRES_DIR)/*.tex $(PRES_DIR)/*.aux $(PRES_DIR)/*.log $(PRES_DIR)/*.out
	@echo "Cleaned presentation artifacts"

presentations-list: ## List available presentations
	@echo "Presentations in $(PRES_DIR):"
	@ls -1 $(PRES_DIR)/*.org 2>/dev/null | sed 's|.*/||; s|\.org$$||' | nl

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
