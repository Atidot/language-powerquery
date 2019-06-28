{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;
let
  notebook-env = import ./notebook.nix {};
in
nixpkgs.dockerTools.buildImage {
  name = "atidot/language-powerquery";
  tag = "latest";
  fromImage = dockerTools.pullImage {
    imageName = "ubuntu";
    sha256 = "105lm1rwnawg9hx7jmxci146x21s90dw8lchq5rlbb2rsh84dk83";
    imageDigest = "sha256:f961d3d101e66017fc6f0a63ecc0ff15d3e7b53b6a0ac500cd1619ded4771bd6";
  };
  contents = [ nixpkgs.bash
               nixpkgs.nss
               nixpkgs.cacert
               nixpkgs.coreutils
               notebook-env
             ];

  runAsRoot = ''
    #!${pkgs.stdenv.shell}
    ${nixpkgs.dockerTools.shadowSetup}
  '';

  config = {
    Entrypoint = [
        "jupyter-lab" "--allow-root"
      ];
    WorkingDir = "/examples";
    Volumes = {
      "/examples" = {};
    };
  };
}
