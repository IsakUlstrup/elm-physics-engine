tmux new-session -d -s my_session 'vite' \; \
   split-window -v 'elm-review --watch' \; \
   split-window -v 'elm-test --watch' \; \
   select-layout even-vertical \; \
   attach-session