{ self ? import ./. {} }:
{
  inherit (self) exe;
  shells-ghc = self.shells.ghc;
}
