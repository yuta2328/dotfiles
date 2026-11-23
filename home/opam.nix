# Declarative opam setup: `make switch` creates the opam root, repositories
# and switches below if they are missing, and installs any listed package
# that is not installed yet. It never removes anything.
{ lib, pkgs, ... }:
let
  opam = "${pkgs.opam}/bin/opam";

  # Extra repositories (the "default" opam.ocaml.org repo comes from `opam init`).
  repos = {
    roc-released = "https://rocq-prover.org/opam/released";
  };

  # Repo order = priority (first wins).
  switches = {
    "5.5.0" = {
      compiler = "ocaml-base-compiler.5.5.0";
      repos = [ "default" ];
      packages = [
        "dune"
        "menhir"
        "alcotest"
        "dolog"
        "qcheck"
        "ppx_deriving_yojson"
        "ppx_inline_test"
        "merlin"
        "ocaml-lsp-server"
        "ocamlformat"
        "ocamlformat-rpc"
        "ocp-indent"
        "tuareg"
        "utop"
      ];
    };
  };
  defaultSwitch = "5.5.0";

  q = lib.escapeShellArg;

  addRepo = name: url: ''
    ${opam} repo list --all --short | grep -qxF ${q name} \
      || run ${opam} repo add ${q name} ${q url} --dont-select
  '';

  mkSwitch = name: s: ''
    if ! ${opam} switch list --short | grep -qxF ${q name}; then
      run ${opam} switch create ${q name} ${q s.compiler} \
        --repos=${lib.concatStringsSep "," s.repos} --no-switch
    fi
    run ${opam} install --switch=${q name} ${lib.escapeShellArgs s.packages}
  '';
in
{
  home.activation.opam = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    export OPAMROOT="$HOME/.opam" OPAMYES=1 OPAMCOLOR=never
    # The activation PATH is minimal; give opam the tools it shells out to.
    export PATH="${
      lib.makeBinPath (
        with pkgs;
        [
          opam
          git
          curl
          gnumake
          gnupatch
          gnum4
          unzip
          rsync
          pkg-config
        ]
      )
    }:$PATH"

    if [ ! -f "$OPAMROOT/config" ]; then
      run ${opam} init --bare --no-setup default https://opam.ocaml.org
    fi

    ${lib.concatStrings (lib.mapAttrsToList addRepo repos)}
    ${lib.concatStrings (lib.mapAttrsToList mkSwitch switches)}

    run ${opam} switch set ${q defaultSwitch}
  '';
}
