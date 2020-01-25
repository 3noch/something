{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
, projectOverrides ? {}
, withHoogle ? false
}:
with obelisk;
let
  app = project ./. ({ hackGet, pkgs, ... }:
    let
      beamSrc = hackGet dep/beam;
    in {
    inherit withHoogle;
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";

    packages = {
      beam-core = beamSrc + /beam-core;
      beam-postgres = beamSrc + /beam-postgres;
      beam-migrate = beamSrc + /beam-migrate;
    };

    overrides = self: super: {
      beam-postgres = pkgs.haskell.lib.dontCheck super.beam-postgres;  # Requires PG to run tests
    };
  } // projectOverrides);
in app // {
  server = { hostName, adminEmail, routeHost, enableHttps, version }@args:
    import (app.obelisk.nixpkgs.path + /nixos) {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (serverModules.mkObeliskApp (args // { exe = app.linuxExe; }))
        ];
      };
    };
}
