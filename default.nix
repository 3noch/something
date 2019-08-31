{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
, projectOverrides ? {}
}:
with obelisk;
project ./. ({ hackGet, pkgs, ... }:
  let
    beamSrc = hackGet dep/beam;
    gargoyleSrc = hackGet dep/gargoyle;
  in {
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
  } // import gargoyleSrc self;
} // projectOverrides)
