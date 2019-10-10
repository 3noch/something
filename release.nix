let
  app = import ./. {};
in {
  inherit (app) exe;
}
