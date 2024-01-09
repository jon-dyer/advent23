default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
    nix fmt

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid -c "cabal repl exe:advent23" --warnings -T :main

test:
    cabal test test:tests --test-show-details=streaming

profile:
    cabal v2-run --enable-profiling exes --  +RTS -p
