# obelisk-rhyolite-template

A template repository for your [Obelisk](https://github.com/obsidiansystems/obelisk)+[Rhyolite](https://github.com/obsidiansystems/rhyolite) projects (based on [3noch/something](https://github.com/3noch/something)).

## What else is included

- [Beam](https://tathougies.github.io/beam/) ORM for postgres.

## Running

- Install obelisk
- Seed the database: `nix-shell -A ghc.gargoyle-postgresql-nix --run 'gargoyle-pg-run db psql < kjv-only.sql'`
- Run `ob run`
