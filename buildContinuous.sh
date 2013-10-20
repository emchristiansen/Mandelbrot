while inotifywait -qq -r -e modify .; do cabal configure; cabal build; echo "Done"; done
