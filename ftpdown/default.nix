with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "ftpdown";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    python37
    python37Packages.virtualenv
    python37Packages.pip
    python37Packages.aiohttp
    python37Packages.aiofiles
    python37Packages.lxml
  ];
}
