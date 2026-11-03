# Build and update the Home Manager configuration for the current system.
#
#   make           build ./result and show what would change
#   make switch    build and activate
#   make update    update flake.lock to the latest inputs
#   make upgrade   update, build and show what would change (activate with `make switch`)

# Works even if nix-command/flakes are not enabled in nix.conf.
NIX     := nix --extra-experimental-features 'nix-command flakes'
PROFILE := $(HOME)/.local/state/nix/profiles/home-manager

.PHONY: all build diff switch update upgrade clean help

all: build diff

## build: build the configuration into ./result (nothing is activated)
build:
	$(NIX) build

## diff: show package version changes between the active generation and ./result
diff:
	@if [ -e "$(PROFILE)" ] && [ -e result ]; then \
		$(NIX) store diff-closures "$(PROFILE)" ./result; \
	else \
		echo "Nothing to compare (no active generation or no ./result)."; \
	fi

## switch: build and activate the configuration
switch: build
	./result/activate

## update: update every flake input in flake.lock (INPUT=nixpkgs to update just one)
update:
	$(NIX) flake update $(INPUT)

## upgrade: update inputs, build, and show what would change
upgrade: update build diff
	@echo
	@echo "Run 'make switch' to activate, then commit flake.lock."

## clean: remove the ./result link
clean:
	rm -f result
