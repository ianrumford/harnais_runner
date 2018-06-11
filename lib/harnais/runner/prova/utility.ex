defmodule Harnais.Runner.Prova.Utility do
  @moduledoc false

  alias Harnais.Runner.Prova, as: PROVA
  alias Harnais.Runner.Prova, as: PROVA
  use Harnais.Runner.Attribute

  import Harnais.Error,
    only: [
      new_error_result: 1
    ],
    warn: false

  import Plymio.Fontais.Option,
    only: [
      opts_normalise: 1,
      opts_validate: 1
    ]

  @doc false
  defp prova_spec_normalise_kv(kv)

  # if test_result is nil, return a fun to compare with nil
  defp prova_spec_normalise_kv({:test_result, nil}) do
    {:test_result, fn v -> is_nil(v) end}
  end

  # default
  defp prova_spec_normalise_kv(kv) do
    kv
  end

  defp prova_spec_normalise_base(prova_spec)

  defp prova_spec_normalise_base(prova_spec) when is_map(prova_spec) do
    prova_spec
    |> Map.to_list()
    |> prova_spec_normalise_base
  end

  defp prova_spec_normalise_base(prova_spec) when is_list(prova_spec) do
    with {:ok, opts} <- prova_spec |> PROVA.update_canonical_opts(),
         true <- true do
      opts
      |> Enum.reduce_while(
        [],
        fn kv, kvs ->
          kv
          |> prova_spec_normalise_kv
          |> case do
            {:error, %{__struct__: _}} = result -> {:halt, result}
            {k, v} -> {:cont, [{k, v} | kvs]}
          end
        end
      )
      |> case do
        {:error, %{__exception__: true}} = result -> result
        kvs -> {:ok, kvs |> Enum.into(%{})}
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp prova_spec_normalise_base(prova_spec) do
    new_error_result(m: "prov spec invalid", v: prova_spec)
  end

  defp prova_spec_normalise_form(test_spec, correr, opts)

  defp prova_spec_normalise_form(test_spec, _correr, _opts) when is_list(test_spec) do
    cond do
      Keyword.keyword?(test_spec) ->
        test_spec

      true ->
        case test_spec |> length do
          5 ->
            [@harnais_runner_test_spec_tuple_key_order, test_spec]

          # no test_value
          4 ->
            [@harnais_runner_test_spec_tuple_key_order, test_spec]

          # no test_value and test_result
          3 ->
            [@harnais_runner_test_spec_tuple_key_order, test_spec]
        end
        |> Enum.zip()
    end
    |> opts_validate
  end

  defp prova_spec_normalise_form(test_spec, _correr, _opts) when is_tuple(test_spec) do
    case test_spec |> tuple_size do
      5 ->
        [@harnais_runner_test_spec_tuple_key_order, test_spec |> Tuple.to_list()]

      # no test_value
      4 ->
        [@harnais_runner_test_spec_tuple_key_order, test_spec |> Tuple.to_list()]

      # no test_value and test_result
      3 ->
        [@harnais_runner_test_spec_tuple_key_order, test_spec |> Tuple.to_list()]
    end
    |> Enum.zip()
    |> opts_validate
  end

  defp prova_spec_normalise_form(test_spec, _correr, _opts) when is_map(test_spec) do
    test_spec |> opts_normalise
  end

  @doc false
  def prova_spec_normalise(prova_spec, correr \\ nil, opts \\ [])

  def prova_spec_normalise(prova_spec, correr, opts) do
    with {:ok, opts} <- opts |> opts_normalise,
         {:ok, prova_spec} <- prova_spec |> prova_spec_normalise_form(correr, opts),
         {:ok, prova_spec} <- prova_spec |> prova_spec_normalise_base,
         true <- true do
      {:ok, prova_spec}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false

  def prova_spec_create_result_fun(prova_spec)

  def prova_spec_create_result_fun(prova_spec) when is_function(prova_spec) do
    {:ok, prova_spec}
  end

  def prova_spec_create_result_fun(prova_spec) do
    with {:ok, prova_spec} <- prova_spec |> prova_spec_normalise,
         {:ok, result_prova} <- prova_spec |> PROVA.new() do
      # create a result function for the original test prova comparing
      # with the actual (test) result with the result prova
      fun_result = fn test_actual,
                      %PROVA{@harnais_runner_key_test_value => test_value} = _test_prova ->
        with {:ok, %PROVA{} = result_prova} <-
               result_prova |> PROVA.prova_test_value_maybe_put(test_value),
             {:ok, %PROVA{} = result_prova} <-
               result_prova |> PROVA.prova_test_result_maybe_put(test_actual),
             {:ok, _} = result <- result_prova |> PROVA.test() do
          result
        else
          {:error, %{__exception__: true}} = result -> result
        end
      end

      {:ok, fun_result}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end
end
