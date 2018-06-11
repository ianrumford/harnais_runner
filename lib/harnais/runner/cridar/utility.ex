defmodule Harnais.Runner.Cridar.Utility do
  @moduledoc false

  alias Harnais.Runner.Cridar, as: CRIDAR
  use Harnais.Runner.Attribute

  import Harnais.Error,
    only: [
      new_error_result: 1
    ]

  import Plymio.Funcio.Enum.Map.Collate,
    only: [
      map_collate0_enum: 2
    ]

  # cridar
  def cridar_spec_normalise(%CRIDAR{} = test_call) do
    {:ok, test_call}
  end

  # nil => identity function
  def cridar_spec_normalise(test_call) when is_nil(test_call) do
    [
      {@harnais_runner_key_cridar_fun, & &1}
    ]
    |> CRIDAR.new()
  end

  # a function of the test_module
  def cridar_spec_normalise(test_call) when is_atom(test_call) do
    [
      {@harnais_runner_key_cridar_fun, test_call}
    ]
    |> CRIDAR.new()
  end

  def cridar_spec_normalise(test_call) when is_function(test_call) do
    [
      {@harnais_runner_key_cridar_fun, test_call}
    ]
    |> CRIDAR.new()
  end

  def cridar_spec_normalise({m, f, a} = _test_call)
      when is_atom(m) and is_atom(f) and is_list(a) do
    [
      {@harnais_runner_key_cridar_module, m},
      {@harnais_runner_key_cridar_fun, f},
      {@harnais_runner_key_cridar_rest_args, a}
    ]
    |> CRIDAR.new()
  end

  def cridar_spec_normalise({m, f, a} = _test_call)
      when is_atom(m) and is_atom(f) and is_tuple(a) do
    [
      {@harnais_runner_key_cridar_module, m},
      {@harnais_runner_key_cridar_fun, f},
      {@harnais_runner_key_cridar_args, a |> Tuple.to_list()}
    ]
    |> CRIDAR.new()
  end

  def cridar_spec_normalise({m, f} = _test_call)
      when is_atom(m) and is_atom(f) do
    [
      {@harnais_runner_key_cridar_module, m},
      {@harnais_runner_key_cridar_fun, f}
    ]
    |> CRIDAR.new()
  end

  def cridar_spec_normalise(test_calls) when is_list(test_calls) do
    cond do
      Keyword.keyword?(test_calls) ->
        test_calls |> CRIDAR.new()

      true ->
        test_calls
        |> map_collate0_enum(&cridar_spec_normalise/1)
    end
  end

  def cridar_spec_normalise(test_call) do
    new_error_result(m: "cridar spec invalid", v: test_call)
  end
end
