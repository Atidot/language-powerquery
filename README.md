# langauge-powerquery
Tools for working with and analyzing PowerQuery (M Language) scripts
- Haskell data structures and AST ([language-powerquery-ast])
- Haskell lexer/parser/printer    ([language-powerquery])
- PowerBI `.pbix` file reader     ([pbix])
- Example IHaskell (Jupyter) [notebooks]
   - [Pbix Example]

# Build
- [Install Nixpkgs]
- `make` (executable)
~~~ shell
12:52 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ make
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz
/nix/store/3lzlivfyy7cqnxw0q594amsj94yswk43-powerquery-env
12:52 barak@berkos:~/Development/atidot/language-powerquery/build (master) $
~~~
- `make` (JavaScript / NodeJS)
    - TODO:

- `make` (Docker)
    - TODO:

- `make` (notebook) - build and run IHaskell (Jupyter) server
~~~ shell
13:36 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ make notebook
nix-shell --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -A env notebook.nix --command "jupyter lab --notebook-dir=../examples/"
[W 13:37:07.009 LabApp] JupyterLab server extension not enabled, manually loading...
[I 13:37:07.014 LabApp] JupyterLab extension loaded from /nix/store/wp1ddiscc9bha9gsf4k73mb0lcxcjbi2-python3.7-jupyterlab-0.35.4/lib/python3.7/site-packages/jupyterlab
[I 13:37:07.015 LabApp] JupyterLab application directory is /nix/store/g22w2r0a6vzax71np2fzpxwzpx0wq1kf-python3.7-jupyterlab-0.35.4/share/jupyter/lab
[I 13:37:07.017 LabApp] Serving notebooks from local directory: /home/barak/Development/atidot/language-powerquery/examples
[I 13:37:07.017 LabApp] The Jupyter Notebook is running at:
[I 13:37:07.017 LabApp] http://localhost:8888/?token=9b2938f88185aba667fd5858051083843361becd94208625
[I 13:37:07.018 LabApp] Use Control-C to stop this server and shut down all kernels (twice to skip confirmation).
[C 13:37:07.130 LabApp]

    Copy/paste this URL into your browser when you connect for the first time,
    to login with a token:
        http://localhost:8888/?token=9b2938f88185aba667fd5858051083843361becd94208625
[I 13:37:07.300 LabApp] Accepting one-time-token-authenticated connection from ::1
~~~

## TODO
- [ ] Hackage packages
- [ ] Javascript version
- [ ] [Reflex] Editor (CodeMirror, JSONEditor)

[language-powerquery-ast]: https://github.com/Atidot/language-powerquery/tree/master/language-powerquery-ast
[language-powerquery]: https://github.com/Atidot/language-powerquery/tree/master/language-powerquery
[pbix]: https://github.com/Atidot/language-powerquery/tree/master/pbix
[notebooks]: https://github.com/Atidot/language-powerquery/tree/master/examples
[Install Nixpkgs]: https://nixos.org/nix/download.html
[Reflex]: https://reflex-frp.org/
[Pbix Example]: https://github.com/Atidot/language-powerquery/blob/master/examples/PowerBI_File_PBIX/Pbix_Example.ipynb
