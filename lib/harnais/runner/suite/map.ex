defmodule Harnais.Runner.Suite.Map.Helper do
  @moduledoc false

  def runner_suite_map_new_old_tuple(v) do
    {v, :new_value}
  end

  def runner_suite_map_value() do
    42
  end

  def runner_suite_map_passthru(v) do
    v
  end
end

defmodule Harnais.Runner.Suite.Map do
  @moduledoc false

  require Harnais.Runner.Suite.Map.Helper, as: HRTMH
  use Harnais.Attribute
  use Harnais.Attribute.Data

  @harnais_runner_tests_state_deep @harnais_state_deep

  @harnais_runner_suite_map %{
    default:
      [
        [:r, :delete, [:a]],
        [:r, :delete, [:x]],
        [:r, :drop, [[:a, :b, :c]]],
        [:r, :drop, [[:a, :x]]],
        [:r, :equal?, [@harnais_runner_tests_state_deep]],
        [:r, :equal?, [%{}]],
        [:r, :fetch, [:a]],
        [:r, :fetch, [:x]],
        [:r, :fetch!, [:a]],
        [{:e, KeyError}, :fetch!, [:x]],
        [:r, :get, [:a]],
        [:r, :get, [:x]],
        [:r, :get_and_update, [:a, &HRTMH.runner_suite_map_new_old_tuple/1]],
        [:r, :get_and_update, [:x, &HRTMH.runner_suite_map_new_old_tuple/1]],
        [:r, :get_and_update!, [:a, &HRTMH.runner_suite_map_new_old_tuple/1]],
        [{:e, KeyError}, :get_and_update!, [:x, &HRTMH.runner_suite_map_new_old_tuple/1]],
        [:r, :get_lazy, [:a, &HRTMH.runner_suite_map_value/0]],
        [:r, :get_lazy, [:x, &HRTMH.runner_suite_map_value/0]],
        [:r, :has_key?, [:a]],
        [:r, :has_key?, [:x]],
        [:r, :keys],
        [:r, :merge, [%{a: 1, b: 2}]],
        [:r, :new],
        [:r, :new, [&HRTMH.runner_suite_map_passthru/1]],
        [:r, :pop, [:a]],
        [:r, :pop, [:a, 42]],
        [:r, :pop, [:x]],
        [:r, :pop, [:x, 42]],
        [:r, :pop_lazy, [:a, &HRTMH.runner_suite_map_value/0]],
        [:r, :put, [:a, 42]],
        [:r, :put, [:x, 42]],
        [:r, :put_new, [:a, 42]],
        [:r, :put_new, [:x, 42]],
        [:r, :put_new_lazy, [:a, &HRTMH.runner_suite_map_value/0]],
        [:r, :put_new_lazy, [:x, &HRTMH.runner_suite_map_value/0]],
        [:r, :split, [[:a]]],
        [:r, :split, [[:a, :x]]],
        [:r, :take, [[:a, :c]]],
        [:r, :take, [[:a, :x]]],
        [:r, :to_list],
        [:r, :update, [:a, 42, &HRTMH.runner_suite_map_passthru/1]],
        [:r, :update, [:x, 42, &HRTMH.runner_suite_map_passthru/1]],
        [:r, :update!, [:a, &HRTMH.runner_suite_map_passthru/1]],
        [{:e, KeyError}, :update!, [:x, &HRTMH.runner_suite_map_passthru/1]],
        [:r, :values]
      ]
      |> Stream.map(fn
        [flag, call] ->
          [f: flag, c: call, a: [], compare: [d: Map, c: call, a: []]]

        [flag, call, args] ->
          [f: flag, c: call, a: args, compare: [d: Map, c: call, a: args]]

        [flag, call, args, value] ->
          [f: flag, c: call, a: args, v: value, compare: [d: Map, c: call, a: args, v: value]]

        [flag, call, args, value, result] ->
          [f: flag, c: call, a: args, v: value, r: result]
      end)
      |> Enum.map(fn test_spec ->
        with {:ok, test_spec} <-
               test_spec
               |> Harnais.Runner.Prova.Utility.prova_spec_normalise() do
          test_spec
        else
          {:error, %{__struct__: _} = error} -> raise error
        end
      end)
  }

  @args_vars 0..5 |> Enum.map(fn n -> "arg#{n}" |> String.to_atom() |> Macro.var(nil) end)

  @wrappers [tests_get: 1, tests_get: 2]

  for {name, arity} <- @wrappers do
    args = @args_vars |> Enum.take(arity)

    def unquote(name)(unquote_splicing(args)) do
      Harnais.Runner.Suite.Utility.unquote(name)(
        @harnais_runner_suite_map,
        unquote_splicing(args)
      )
    end
  end
end
