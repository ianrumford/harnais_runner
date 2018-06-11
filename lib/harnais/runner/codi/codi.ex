defmodule Harnais.Runner.Codi do
  @moduledoc false

  # do this at run time as saving a vekil take a long time to compile
  def __vekil__() do
    [
      Plymio.Vekil.Codi,

      # overrides and corrections to fontais v0.2.0
      %{
        state_def_update_doc:
          quote do
            @doc ~S"""
            `update/2` takes an `instance` of the module's `struct` and an optional *opts*.

            The *opts* are normalised by calling the module's `update_canonical_opts/1`
            and then reduced with `update_field/2`:

                 opts |> Enum.reduce(instance, fn {k,v}, s -> s |> update_field({k,v}) end)

            `{:ok, instance}` is returned.
            """
          end
      }
    ]
    |> Plymio.Vekil.Utility.create_form_vekil!()
  end
end
