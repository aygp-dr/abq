# Agent Bus Makefile
# Org-mode tooling for tangling and linting

EMACS ?= emacs
ORG_FILES := $(wildcard *.org) $(wildcard **/*.org)
TANGLE_TARGET := agent-bus-spec.org

.PHONY: all tangle lint clean install help

all: lint tangle ## Run lint and tangle

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

# Tangling
tangle: ## Tangle all code blocks from agent-bus-spec.org
	@echo "Tangling $(TANGLE_TARGET)..."
	$(EMACS) --batch \
		--eval "(require 'org)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(org-babel-tangle-file \"$(TANGLE_TARGET)\")"
	@echo "Making scripts executable..."
	chmod +x ~/.local/bin/aq* 2>/dev/null || true
	chmod +x ~/.local/share/aq/handlers/*.sh 2>/dev/null || true
	chmod +x ~/.local/share/aq/handlers/*.py 2>/dev/null || true
	chmod +x ~/.local/share/aq/handlers/*.rb 2>/dev/null || true
	@echo "Done. Run 'aq-init' to initialize the agent bus."

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

# Linting
lint: lint-org ## Run all linters

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

# Installation
install: tangle ## Tangle and initialize agent bus
	@echo "Initializing agent bus..."
	~/.local/bin/aq-init || echo "Run 'aq-init' manually after adding ~/.local/bin to PATH"

# Cleanup
clean: ## Remove tangled files (careful!)
	@echo "This would remove tangled files. Not implemented for safety."
	@echo "Manually remove: ~/.local/bin/aq* ~/.local/share/aq/"

# Development
watch: ## Watch org files and re-tangle on change (requires fswatch)
	@echo "Watching for changes to $(TANGLE_TARGET)..."
	fswatch -o $(TANGLE_TARGET) | xargs -n1 -I{} make tangle

# Export
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

# Testing
test: ## Run basic tests
	@echo "Testing aq CLI..."
	@if [ -x ~/.local/bin/aq ]; then \
		~/.local/bin/aq --help > /dev/null && echo "  aq: OK" || echo "  aq: FAIL"; \
	else \
		echo "  aq not installed. Run 'make install' first."; \
	fi

# Info
info: ## Show project info
	@echo "Agent Bus (aq)"
	@echo "=============="
	@echo "Org files: $(ORG_FILES)"
	@echo "Emacs: $(shell $(EMACS) --version | head -1)"
	@echo ""
	@echo "Tangled targets from $(TANGLE_TARGET):"
	@grep -E "^#\+begin_src.+:tangle" $(TANGLE_TARGET) | sed 's/.*:tangle /  /' | sed 's/ .*//' | sort -u
