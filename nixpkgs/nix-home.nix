{ stdenv, python, fetchFromGitHub }:
#with import <nixpkgs>{};
stdenv.mkDerivation rec {
  version = "0.3.4";
  name = "nix-home-${version}";

  src = fetchFromGitHub {
    rev = version;
    repo = "nix-home";
    owner = "NeQuissimus";
    sha256 = "sha256-XItVrDAZ9cHCwJpeXa2OKxBiwCUvRpG4SrQdYZicy1c=";
  };

  patchPhase = ''
    substituteInPlace nix-home --replace "NIXHOME" "$out/nix/lib"
    substituteInPlace nix-build-home --replace "NIXHOME" "$out/nix/lib"
  '';

  installPhase = ''
    # install binary
    mkdir -p $out/bin
    cp nix-home $out/bin
    cp nix-build-home $out/bin
    chmod +x $out/bin/nix-build-home
    chmod +x $out/bin/nix-home

    # install nix-home lib
    mkdir -p $out/nix
    cp -a lib $out/nix
  '';

  meta = {
    homepage = "https://github.com/sheenobu/nix-home";
    description = "Per-user configuration management via Nix";
    licenses = [ stdenv.lib.licenses.mit ];
    platforms = stdenv.lib.platforms.unix;
    inherit version;
  };
}
