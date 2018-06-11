

# README

**harnais\_runner**: The Test Runner for the Harnais Family

The package is based on the *runner* in `Harnais` `v0.2.0`.

It is almost a complete re-write of the old code but the public API has not changed.

The documentation now uses [*correr*](`Harnais.Runner.Correr`) to
describe the run specification (*run spec*) ,
[*prova*](`Harnais.Runner.Prova`) to describe a test specification
(*test spec*), and [*cridar*](`Harnais.Runner.Cridar`) to describe the
call specification (*test call*).

Tests are now run concurrently.

## Installation

Add **harnais\_runner** to your list of dependencies in *mix.exs*:

    def deps do
      [{:harnais_runner, "~> 0.1.0"}]
    end


## Examples

See the examples in the [API Reference](https://hexdocs.pm/harnais_runner/readme.html)

