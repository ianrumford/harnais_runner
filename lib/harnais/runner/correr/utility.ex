defmodule Harnais.Runner.Correr.Utility do
  @moduledoc false

  alias Harnais.Runner.Prova, as: PROVA
  alias Harnais.Runner.Correr, as: CORRER
  alias Harnais.Runner.Prova, as: PROVA
  use Harnais.Runner.Attribute

  import Harnais.Error,
    only: [
      new_error_result: 1
    ],
    warn: false

  import Plymio.Fontais.Guard,
    only: [
      is_value_unset_or_nil: 1
    ]

  import Plymio.Fontais.Option,
    only: [
      opts_validate: 1
    ]

  import Plymio.Funcio.Enum.Map.Collate,
    only: [
      map_concurrent_collate2_enum: 2
    ]

  # Any fun that returns nil, short circuits the reduce pipeline

  defp prova_spec_reduce_transform_funs(funs) do
    funs =
      funs
      |> List.wrap()
      |> List.flatten()
      |> Enum.reject(&is_nil/1)

    funs
    |> length
    |> case do
      # identity
      0 ->
        fn prova_spec, _spec, _opts -> prova_spec end

      _ ->
        cond do
          # one arg i.e. just the prova_spec?
          Enum.all?(funs, &is_function(&1, 1)) ->
            fn prova_spec, _correr, _opts ->
              funs
              |> Enum.reduce_while(prova_spec, fn
                _f, nil ->
                  {:halt, nil}

                f, s ->
                  f.(s)
                  |> case do
                    {:error, %{__struct__: _}} = result -> {:halt, result}
                    {:ok, value} -> {:cont, value}
                    value -> {:cont, value}
                  end
              end)
            end

          # two args i.e. prova_spec + correr?
          Enum.all?(funs, &is_function(&1, 2)) ->
            fn prova_spec, correr, _opts ->
              funs
              |> Enum.reduce_while(prova_spec, fn
                _f, nil ->
                  {:halt, nil}

                f, s ->
                  f.(s, correr)
                  |> case do
                    {:error, %{__struct__: _}} = result -> {:halt, result}
                    {:ok, value} -> {:cont, value}
                    value -> {:cont, value}
                  end
              end)
            end

          # three args i.e. prova_spec + correr + opts?
          Enum.all?(funs, &is_function(&1, 3)) ->
            fn prova_spec, correr, opts ->
              funs
              |> Enum.reduce_while(prova_spec, fn
                _f, nil ->
                  {:halt, nil}

                f, s ->
                  f.(s, correr, opts)
                  |> case do
                    {:error, %{__struct__: _}} = result -> {:halt, result}
                    {:ok, value} -> {:cont, value}
                    value -> {:cont, value}
                  end
              end)
            end

          # must be mixed arities
          true ->
            fn prova_spec, correr, opts ->
              funs
              |> Enum.reduce_while(prova_spec, fn
                _f, nil ->
                  {:halt, nil}

                f, s when is_function(f, 1) ->
                  f.(s)
                  |> case do
                    {:error, %{__struct__: _}} = result -> {:halt, result}
                    {:ok, value} -> {:cont, value}
                    value -> {:cont, value}
                  end

                f, s when is_function(f, 2) ->
                  f.(s, correr)
                  |> case do
                    {:error, %{__struct__: _}} = result -> {:halt, result}
                    {:ok, value} -> {:cont, value}
                    value -> {:cont, value}
                  end

                f, s when is_function(f, 3) ->
                  f.(s, correr, opts)
                  |> case do
                    {:error, %{__struct__: _}} = result -> {:halt, result}
                    {:ok, value} -> {:cont, value}
                    value -> {:cont, value}
                  end
              end)
            end
        end
    end
    |> case do
      x when is_function(x, 3) ->
        {:ok, x}

      x ->
        new_error_result(m: "prova spec transform not fun/3", v: x)
    end
  end

  def correr_resolve_test_transform(correr) do
    # build a reducer fun
    correr
    |> CORRER.correr_test_transform_fetch()
    |> case do
      # has a test_transform => must be complete
      {:ok, x} ->
        x

      _ ->
        [
          # use supplied mappers - any number of
          with {:ok, test_mapper} <- correr |> CORRER.correr_test_mapper_fetch() do
            test_mapper
          else
            _ -> nil
          end,

          # normalise the prova_spec
          &Harnais.Runner.Prova.Utility.prova_spec_normalise/3
        ]
    end
    |> prova_spec_reduce_transform_funs
  end

  defp correr_spec_normalise_test_spec(test_spec, test_transform)

  defp correr_spec_normalise_test_spec(%PROVA{} = test_spec, _test_transform) do
    {:ok, test_spec}
  end

  defp correr_spec_normalise_test_spec(test_spec, test_transform) do
    test_spec
    |> test_transform.()
    |> case do
      # drop test?
      x when is_value_unset_or_nil(x) ->
        nil

      {:error, %{__struct__: _}} = result ->
        result

      {:ok, %PROVA{}} = result ->
        result

      {:ok, prova_spec} ->
        prova_spec |> PROVA.new()

      prova_spec ->
        prova_spec |> PROVA.new()
    end
  end

  # header
  def correr_spec_normalise_kv(spec, kv, opts \\ [])

  def correr_spec_normalise_kv(correr, {:test_specs, prova_specs}, opts) do
    with {:ok, opts} <- opts |> opts_validate,
         {:ok, %CORRER{} = correr} <- correr |> CORRER.correr_test_specs_put(prova_specs),
         {:ok, fun_prova_spec_transform} <- correr |> correr_resolve_test_transform do
      fun_test_spec_transform = fn test_spec ->
        test_spec
        |> fun_prova_spec_transform.(correr, opts)
      end

      fun_transform = fn test_spec ->
        test_spec
        |> correr_spec_normalise_test_spec(fun_test_spec_transform)
      end

      prova_specs
      |> List.wrap()
      |> map_concurrent_collate2_enum(fun_transform)
      |> case do
        {:error, %{__exception__: true}} = result -> result
        {:ok, test_provas} -> correr |> CORRER.correr_test_provas_put(test_provas)
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end
end
