with import ~/.nix-defexpr/channels/nixos-19.09 {} ;
let 
  queryparser_git = fetchFromGitHub {
      owner = "uber" ;
      repo = "queryparser" ;
      rev = "6015e8f273f4498326fec0315ac5580d7036f8a4" ;
      sha256 = "05pnifm5awyqxi6330v791b1cvw26xbcn2r20pqakvl8d3xyaxa4" ;
  } ; 
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      queryparser = appendConfigureFlag 
                      (dontHaddock (doJailbreak (self.callCabal2nix "queryparser" queryparser_git {})))
                      "--ghc-options=-XNoMonadFailDesugaring" ;
      queryparser-hive = dontHaddock (doJailbreak (self.callCabal2nix "queryparser-hive" (queryparser_git + /dialects/hive) {})) ;
    } ;
  };
in 
mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages ( p: 
      [ p.pretty p.bytestring p.text p.string-conversions
        p.aeson p.aeson-qq p.aeson-pretty 
        p.these p.lens p.conduit
        p.hssqlppp p.queryparser p.queryparser-hive ]
    ))
  ];
}
