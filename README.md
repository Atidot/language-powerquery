# langauge-powerquery
Tools for working with and analyzing PowerQuery (M Language) scripts
- Haskell data structures and AST ([language-powerquery-ast])
- Haskell lexer/parser/printer    ([language-powerquery])
   - A CLI tools for lexing/parsing/printing
   - A Javascript/NodeJS version (built with GHCJS)
- PowerBI `.pbix` file reader     ([pbix])
   - A CLI tools to extract (and parse) formulas from `.pbix` files
- Example IHaskell (Jupyter) [notebooks]
   - [Pbix Example]

# Build
- [Install Nixpkgs]

- `make` (executable)
~~~ shell
13:54 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ make
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz
/nix/store/a53pzr8ph5gqriwx2dw6p0b8ddpvdhs0-language-powerquery-env
~~~

- `make` (JavaScript / NodeJS)
~~~ shell
13:54 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ make build-js
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz nodejs.nix
/nix/store/drmnqrv4m9s7nik6gsjh7h4gpf5snrhj-language-powerquery-js-env
13:54 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ node ./result/bin/pbix.jsexe/all.js
Missing: COMMAND

Usage: all.js COMMAND
  PowerBI .pbix file analyzer
~~~

- `make` (Static Executables)
~~~ shell
13:55 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ make build-static
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz static.nix --show-trace
/nix/store/03nv9qxyf3d2cw2zj2h3y82b428wg2ya-language-powerquery-static-env
13:54 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ ldd ./result/bin/pbix
        not a dynamic executable
~~~

- `make` (Docker)
~~~ shell
13:55 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ make docker
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz docker.nix -o docker.tar.gz
/nix/store/76bh3g4z5bsfi9nyyg1dwxdx56gbkjsp-docker-image-language-powerquery.tar.gz
~~~

- `make` (Reflex GHCJS Editor)
~~~ shell
13:55 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ make editor
nix-build --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -A ghcjs.language-powerquery-editor editor.nix
/nix/store/3fcb8zbcsp8pj2aip3z6smn3x3nhbw5p-language-powerquery-editor-0.1.0.0
~~~

- `make` (notebook) - build and run IHaskell (Jupyter) server
~~~ shell
13:56 barak@berkos:~/Development/atidot/language-powerquery/build (master) $ make notebook
nix-shell --cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -A env notebook.nix --command "jupyter lab --notebook-dir=../examples/"
[W 13:57:03.859 LabApp] JupyterLab server extension not enabled, manually loading...
[I 13:57:03.866 LabApp] JupyterLab extension loaded from /nix/store/gzymczahplzh4azw94bsp83nkzgilw6y-python3.7-jupyterlab-0.35.4/lib/python3.7/site-packages/jupyterlab
[I 13:57:03.866 LabApp] JupyterLab application directory is /nix/store/g22w2r0a6vzax71np2fzpxwzpx0wq1kf-python3.7-jupyterlab-0.35.4/share/jupyter/lab
[I 13:57:03.869 LabApp] Serving notebooks from local directory: /home/barak/Development/atidot/language-powerquery/examples
[I 13:57:03.869 LabApp] The Jupyter Notebook is running at:
[I 13:57:03.869 LabApp] http://localhost:8888/?token=baeb27f55264e554e813a610028f06bfe48e6c6fca96d5af
[I 13:57:03.869 LabApp] Use Control-C to stop this server and shut down all kernels (twice to skip confirmation).
[C 13:57:03.982 LabApp]

    Copy/paste this URL into your browser when you connect for the first time,
    to login with a token:
        http://localhost:8888/?token=baeb27f55264e554e813a610028f06bfe48e6c6fca96d5af
[I 13:57:04.209 LabApp] Accepting one-time-token-authenticated connection from ::1
[I 13:57:05.261 LabApp] Node v8.15.0

[I 13:57:05.428 LabApp] Build is up to date
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
