Something
---------

## Contributing

### Technology

This project uses [Haskell](https://www.haskell.org/) for all parts of the application, including:
  1. Backend server
  2. Frontend JavaScript (Haskell compiled to JavaScript via GHCJS)
  3. Android app
  4. iOS app

The server uses [PostgreSQL](https://www.postgresql.org/) for the database.

The development/deployment tool is [Obelisk](https://github.com/obsidiansystems/obelisk) which is heavily based on the [Nix](https://nixos.org/nix/) package manager.

### Set up your machine

1. Use macOS or Linux.
2. Install [Nix](https://nixos.org/nix/) and configure the [cachix](https://cachix.org/) build cache. Follow the instructions here: https://3noch.cachix.org/
3. Install [Obelisk](https://github.com/obsidiansystems/obelisk): `nix-env -f https://github.com/obsidiansystems/obelisk/archive/master.tar.gz -iA command`
4. Clone this reposotory and `cd` into it.
5. Seed the database: `nix-shell -A ghc.gargoyle-postgresql-nix --run 'gargoyle-pg-run db psql < kjv-only.sql'`
6. `ob run -v` to start up the development server. The first time it runs it will download dependencies from the cache.

`ob run` start its own [PostgreSQL](https://www.postgresql.org/) database in a directory called `db`. To reset your database, simply delete the `db` directory and reseed the database.

*IMPORTANT*: `ob run` will serve a development version of the application on `http://localhost:8000` (by default) and reload whenever you save a source Haskell file. **Only Chromium-based browsers support the development server properly.**
