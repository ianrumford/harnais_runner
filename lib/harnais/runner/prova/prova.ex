defmodule Harnais.Runner.Prova do
  @moduledoc ~S"""
  The *prova* manages a complete test with one or more test calls.

  Each test specification is used to create a *prova*.

  Each test call specification is used to create a [*cridar*](`Harnais.Runner.Cridar`).

  See `Harnais.Runner` for the overview.

  ## Prova State

  A *prova* has the following fields:

  | Key | Aliases |
  | :---------------- | -------------------: |
  | `:test_flag`   | *:f, :flag* |
  | `:test_call`   | *:c, :call* |
  | `:test_value`  | *:v, :value* |
  | `:test_args`   | *:a, :args* |
  | `:test_module` | *:d, :module* |
  | `:test_result` | *:r, :result* |
  | `:test_comp` | *:comp, :compare, :test_compare* |
  | `:test_namer`  | *:n, :namer* |
  | `:test_spec`  | |
  | `:test_cridars` | |

  The default for all fields is *the unset value* (`Plymio.Fontais.Guard.the_unset_value/0`).

  ### Prova Field: `:test_flag`

  The `:test_flag` is optional in the `Map` and `Keyword` test forms
  but required in the positional `Tuple` and `List` forms.

  Even when required, usually the `:test_flag` can be nil; it is ignored.

  The `:test_flag` is most frequently used to catch an exception e.g. `{:e ArgumentError}`.

  The flag is important though when using the `reduce` runner
  (`Harnais.Runner.run_tests_reduce_test_value/1`) as described above when a
  value of `:w` will set the `:test_value` for the next test to the
  result of the current test.

  ### Prova Field: `:test_call`

  The test call holds one or more (`List`) call specifications each
  of which is used to create a [*cridar*](`Harnais.Runner.Cridar`).

  ### Prova Field: `:test_value`

  The `:test_value` supplies the 2nd argument to the
  `Harnais.Runner.Cridar.call/2` (the first being the *cridar*
  itself).

  ### Prova Field: `:test_args`

  If the *cridar's* `:rest_args` (sic) is unset, the *prova's*
  `:test_args` is used to set the *cridar's* `:rest_args`.

  The expectation is that the *cridar's*
  `:rest_args` will be used often with the `:test_value` in a MFA apply.

  ### Prova Field: `:test_module`

  If the *cridar's* `:test_module` is unset, the *prova's* `:test_module` is used.

  ### Prova Field: `:test_result`

  Each *prova* (`test spec`) must have a `:test_result` value.

  If the value is a function of arity one, it is called with
  the actual result.

  If the value is a function of arity two, it is called with
  the actual result and the *prova*.

  The answer from a function is normalised with a definition
  of *extended truthy*: The usual `nil`, `false` and `true` are supplemented
  with `{:ok, value}` (`true`), `{error, error}` (`false`), value is an `Exception` (`false`), and `value`
  (`true`).

  Any other value is just compared (`==`) with the actual result and the compare asserted.

  ### Prova Field: `:test_comp`

  The `:test_comp` field is used to specify another test that should
  return the same answer as the current test.

  If the `:test_comp` is a function, it is treated as the
  `:test_result`.

  Otherwise it is treated as a new test specification that is
  instantiated, tested and compared with the answer from the current
  test.

  See the examples for more specifics.

  ### Prova Field: `:test_namer`

  When the `:test_call` has an `Atom` function name (e.g. `:get`), the namer
  function is called with the function name and should return the
  actual function (`Atom`) to call in the `:test_module`.

  ### Prova Field: `:test_spec`

  The original (pre transform) test specification.

  ### Prova Field: `:test_cridars`

  Each test call specification in the `:test_call` is used to create a
  [*cridar*](`Harnais.Runner.Cridar`). The `:test_cridars` holds the list
  of *cridars*.
  """

  require Plymio.Codi, as: CODI
  require Plymio.Fontais.Option
  require Harnais.Runner.Utility.Macro, as: HUM
  alias Harnais.Runner.Cridar, as: CRIDAR
  alias Harnais.Utility, as: HUU
  use Plymio.Fontais.Attribute
  use Plymio.Codi.Attribute
  use Harnais.Runner.Attribute

  @codi_opts [
    {@plymio_codi_key_vekil, Harnais.Runner.Codi.__vekil__()}
  ]

  import Harnais.Error,
    only: [
      new_error_result: 1
    ],
    warn: false

  import Plymio.Fontais.Guard,
    only: [
      is_value_set: 1,
      is_value_unset: 1
    ]

  import Plymio.Fontais.Option,
    only: [
      opts_create_aliases_dict: 1,
      opts_canonical_keys: 2
    ]

  import Plymio.Funcio.Enum.Map.Collate,
    only: [
      map_collate0_enum: 2
    ]

  @harnais_runner_prova_kvs_aliases [
    @harnais_runner_alias_test_flag,
    @harnais_runner_alias_test_spec,
    @harnais_runner_alias_test_call,
    @harnais_runner_alias_test_value,
    @harnais_runner_alias_test_args,
    @harnais_runner_alias_test_module,
    @harnais_runner_alias_test_result,
    @harnais_runner_alias_test_namer,
    @harnais_runner_alias_test_comp,
    @harnais_runner_alias_test_cridars
  ]

  @harnais_runner_prova_keys_aliases @harnais_runner_prova_kvs_aliases |> Keyword.keys()

  @harnais_runner_prova_dict_aliases @harnais_runner_prova_kvs_aliases
                                     |> opts_create_aliases_dict

  @doc false
  def update_canonical_opts(opts, dict \\ @harnais_runner_prova_dict_aliases) do
    opts |> opts_canonical_keys(dict)
  end

  @doc false
  def update_sorted_canonical_opts(opts, dict \\ @harnais_runner_prova_dict_aliases) do
    with {:ok, opts} <- opts |> update_canonical_opts(dict) do
      {:ok, opts |> HUU.opts_sort_keys(@harnais_runner_prova_keys_aliases)}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @harnais_runner_prova_defstruct @harnais_runner_prova_kvs_aliases
                                  |> Enum.map(fn {k, _v} ->
                                    {k, @plymio_fontais_the_unset_value}
                                  end)

  defstruct @harnais_runner_prova_defstruct

  @type t :: %__MODULE__{}
  @type opts :: Harnais.opts()
  @type kv :: Harnais.kv()
  @type error :: Harnais.error()

  @doc false
  HUM.def_struct_get()
  @doc false
  HUM.def_struct_fetch()
  @doc false
  HUM.def_struct_put()
  @doc false
  HUM.def_struct_delete()

  struct_accessor_spec_default = %{funs: [:get, :fetch, :put]}

  struct_accessor_specs =
    [
      {@harnais_runner_key_test_spec, %{funs: [:get, :fetch, :put]}},
      {@harnais_runner_key_test_flag, nil},
      {@harnais_runner_key_test_module, %{funs: [:get, :fetch, :maybe_put, :put]}},
      {@harnais_runner_key_test_namer, %{funs: [:get, :fetch, :maybe_put, :put]}},
      {@harnais_runner_key_test_call, %{funs: [:get, :fetch, :put, :delete]}},
      {@harnais_runner_key_test_value, %{funs: [:get, :fetch, :put, :maybe_put, :delete]}},
      {@harnais_runner_key_test_args, nil},
      {@harnais_runner_key_test_result, %{funs: [:get, :fetch, :put, :maybe_put]}},
      {@harnais_runner_key_test_cridars, nil},
      {@harnais_runner_key_test_comp, nil}
    ]
    |> Enum.map(fn
      {name, spec} when is_nil(spec) -> {name, struct_accessor_spec_default}
      x -> x
    end)

  [
    specs: struct_accessor_specs,
    namer: fn name, fun -> ["prova_", name, "_", fun] |> Enum.join() |> String.to_atom() end
  ]
  |> HUM.custom_struct_accessors()

  spec_accessor_spec_default = %{funs: [:get, :fetch, :put]}

  spec_accessor_specs =
    [
      test_call: nil,
      test_args: nil
    ]
    |> Enum.map(fn
      {name, spec} when is_nil(spec) -> {name, spec_accessor_spec_default}
      x -> x
    end)

  for {name, spec} <- spec_accessor_specs do
    spec
    |> Map.get(:funs)
    |> Enum.map(fn fun ->
      fun_name = ["prova_spec_", name, "_", fun] |> Enum.join() |> String.to_atom()

      case fun do
        :get ->
          @doc false
          def unquote(fun_name)(state, default \\ nil) do
            state
            |> Map.get(unquote(name))
            |> case do
              x when is_value_unset(x) -> {:ok, default}
              x -> {:ok, x}
            end
          end

        :fetch ->
          @doc false
          def unquote(fun_name)(state) do
            state
            |> Map.get(unquote(name))
            |> case do
              x when is_value_unset(x) ->
                new_error_result(m: "spec key #{to_string(unquote(name))} not set")

              x ->
                {:ok, x}
            end
          end

        :put ->
          @doc false
          def unquote(fun_name)(state, value) do
            {:ok, Map.put(state, unquote(name), value)}
          end

        :delete ->
          @doc false
          def unquote(fun_name)(state) do
            {:ok, Map.delete(state, unquote(name))}
          end
      end
    end)
  end

  [
    # updates
    {@plymio_codi_pattern_proxy_put,
     [
       state_def_new_since: quote(do: @since("0.1.0")),
       state_def_new_since!: quote(do: @since("0.1.0")),
       state_def_update_since: quote(do: @since("0.1.0")),
       state_def_update_since!: quote(do: @since("0.1.0"))
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_def_update},
       {@plymio_codi_key_forms_edit,
        [
          {@plymio_fontais_key_rename_funs,
           [update_canonical_opts: :update_sorted_canonical_opts]}
        ]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       :state_def_new,
       :state_def_new!,
       :state_def_update!,
       :state_defp_update_field_header
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_passthru},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_test_flag]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_atom},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_test_module]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_passthru},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_test_args]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_passthru},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_test_value]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_passthru},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_test_result]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_fun1},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_test_namer]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_passthru},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_test_spec]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_normalise_list},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_test_cridars]}]}
     ]}
  ]
  |> CODI.reify_codi(@codi_opts)

  defp update_field(%__MODULE__{} = state, {k, v})
       when k == @harnais_runner_key_test_call do
    with {:ok, %__MODULE__{} = state} <- state |> prova_test_call_put(v),
         {:ok, %__MODULE__{}} = result <- state |> normalise_cridar_spec do
      result
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp update_field(%__MODULE__{} = state, {k, v})
       when k == @harnais_runner_key_test_comp do
    with {:ok, result_fun} <- v |> Harnais.Runner.Prova.Utility.prova_spec_create_result_fun(),
         {:ok, %__MODULE__{}} = result <-
           state |> update([{@harnais_runner_key_test_result, result_fun}]) do
      result
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  [{@plymio_codi_pattern_proxy_fetch, [:state_defp_update_field_unknown]}]
  |> CODI.reify_codi(@codi_opts)

  @harnais_runner_prova_defstruct_updaters @harnais_runner_prova_defstruct

  for {name, _} <- @harnais_runner_prova_defstruct_updaters do
    update_fun = "update_#{name}" |> String.to_atom()

    @doc false
    def unquote(update_fun)(%__MODULE__{} = state, value) do
      state |> update([{unquote(name), value}])
    end
  end

  defp call_cridars(prova)

  defp call_cridars(
         %__MODULE__{
           @harnais_runner_key_test_cridars => test_cridars,
           @harnais_runner_key_test_value => test_value
         } = prova
       )
       when is_value_unset(test_cridars) do
    {:ok, {test_value, prova}}
  end

  defp call_cridars(%__MODULE__{} = prova) do
    with {:ok, test_value} <- prova |> prova_test_value_fetch,
         {:ok, test_args} <- prova |> prova_test_args_get([]),
         {:ok, test_module} <- prova |> prova_test_module_get,
         {:ok, test_cridars} <- prova |> prova_test_cridars_fetch,
         true <- true do
      test_cridars
      |> Enum.reduce_while(
        {test_value, []},
        fn %CRIDAR{} = cridar, {test_value, cridars} ->
          with {:ok, %CRIDAR{} = cridar} <- cridar |> CRIDAR.cridar_module_maybe_put(test_module),
               {:ok, %CRIDAR{} = cridar} <-
                 cridar |> CRIDAR.cridar_rest_args_maybe_put(test_args),
               {:ok, {test_value, %CRIDAR{} = cridar}} <- cridar |> CRIDAR.call(test_value),
               true <- true do
            {:cont, {test_value, [cridar | cridars]}}
          else
            {:error, %{__struct__: _}} = result -> {:halt, result}
          end
        end
      )
      |> case do
        {:error, %{__exception__: true}} = result ->
          result

        {test_value, cridars} ->
          with {:ok, %__MODULE__{} = prova} <-
                 prova
                 |> update_test_cridars(cridars |> Enum.reverse()) do
            {:ok, {test_value, prova}}
          else
            {:error, %{__exception__: true}} = result -> result
          end
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @spec compare_actual_expect(t, any, any) :: {:ok, {any, t}} | {:error, error}

  defp compare_actual_expect(prova, actual_result, expect_result)

  defp compare_actual_expect(%__MODULE__{} = prova, actual, expect)
       when is_function(expect, 1) do
    expect.(actual)
    |> normalise_compare_result
    |> case do
      {:ok, _} -> {:ok, {actual, prova}}
      _ -> new_error_result(m: "result invalid", v: actual)
    end
  end

  defp compare_actual_expect(%__MODULE__{} = prova, actual, expect)
       when is_function(expect, 2) do
    expect.(actual, prova)
    |> normalise_compare_result
    |> case do
      {:ok, _} ->
        {:ok, {actual, prova}}

      _ ->
        new_error_result(m: "result invalid", v: actual)
    end
  end

  defp compare_actual_expect(%__MODULE__{} = prova, actual, expect) do
    actual
    |> case do
      ^expect ->
        {:ok, {actual, prova}}

      _ ->
        new_error_result(
          m: "result mismatch; expect #{inspect(expect)}; actual #{inspect(actual)}"
        )
    end
  end

  defp normalise_compare_result(result)

  defp normalise_compare_result({:ok, _} = result) do
    result
  end

  defp normalise_compare_result({:error, _} = result) do
    result
  end

  defp normalise_compare_result(result) when result in [true] do
    {:ok, result}
  end

  defp normalise_compare_result(result) when result in [nil, false] do
    new_error_result(m: "result invalid", v: result)
  end

  defp normalise_compare_result(value) do
    value
    |> Exception.exception?()
    |> case do
      true -> {:error, value}
      _ -> {:ok, value}
    end
  end

  defp test_expect_exception(%__MODULE__{} = prova, exception) do
    try do
      with {:ok, {result, %__MODULE__{}}} <- prova |> call_cridars do
        new_error_result(m: "expected exception", v: result)
      else
        {:error, error} -> raise error
      end
    rescue
      error ->
        error
        |> Map.get(:__struct__)
        |> case do
          ^exception ->
            {:ok, {error, prova}}

          _ ->
            expect_message = exception |> Exception.message()

            actual_message = error |> Exception.message()

            new_error_result(
              m: "unexpected exception; expect #{expect_message}; actual #{actual_message}",
              v: prova
            )
        end
    end
  end

  defp test_expect_value(%__MODULE__{} = prova) do
    with {:ok, {actual_result, %__MODULE__{} = prova}} <- prova |> call_cridars do
      # if a test_result, compare with it.
      prova
      |> prova_test_result_fetch
      |> case do
        {:ok, expect_result} when is_value_set(expect_result) ->
          with {:ok, {_answer, %__MODULE__{}}} = result <-
                 prova
                 |> compare_actual_expect(actual_result, expect_result) do
            result
          else
            {:error, %{__exception__: true}} = result -> result
          end

        # no test_value. make the actual result the test result
        _ ->
          with {:ok, %__MODULE__{} = prova} <- prova |> update_test_result(actual_result) do
            {:ok, {actual_result, prova}}
          else
            {:error, %{__exception__: true}} = result -> result
          end
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc ~S"""
  `test/1` takes a *prova* and runs each test call (*cridar*)

  ## Examples

  A simple example of a `Map.get/2` call where the result is expected to be `42`:

      iex> {:ok, {answer, %PROVA{}}} = [
      ...>    module: Map, value: %{a: 42},
      ...>    args: [:a], call: [:get], result: 42,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      42

  The `:test_result` here is `99` so an error is returned:

      iex> {:error, error} = [
      ...>    module: Map, value: %{a: 42},
      ...>    args: [:a], call: [:get], result: 99,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> error |> Exception.message
      "result mismatch; expect 99; actual 42"

  Here the `:test_result` is a fun/1 that always returns `false` causing an error to be returned.

      iex> {:error, error} = [
      ...>    module: Map, value: %{a: 42},
      ...>    args: [:a], call: [:get], result: fn _ -> false end,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> error |> Exception.message
      "result invalid, got: 42"

  Precreated *cridars* can be used in the `:test_call`:

      iex> {:ok, %CRIDAR{} = cridar} = [
      ...>    mod: Map, fun: :get, args: [%{a: 42}, :a]
      ...> ] |> CRIDAR.new
      ...> {:ok, {answer, %PROVA{}}} = [
      ...>    module: List, value: %{b: 99},
      ...>    args: [:a], call: cridar, result: 42,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      42

  In this example two precreated *cridars* are chained together: the result
  of the first is used as the `:test_value` for the second.  Note the
  *cridars* have `:rest_args` (not `:args`) as they will be applied
  together with the `:test_value`:

      iex> call1 = [f: :delete, rest_args: :a] |> CRIDAR.new!
      ...> call2 = [f: :get, rest_args: [:a, :no_a_found]] |> CRIDAR.new!
      ...> {:ok, {answer, %PROVA{}}} = [
      ...>    mod: Map, call: [call1, call2],
      ...>    value: %{a: 1}, result: :no_a_found,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      :no_a_found

  Test call specifications can be mixed: A variation of the example
  above where a value is put for `:a` using an explicit fun:

      iex> call1 = [f: :delete, rest_args: :a] |> CRIDAR.new!
      ...> call2 = [f: :get, rest_args: [:a, :no_a_found]] |> CRIDAR.new!
      ...> call3 = fn test_value -> Map.put(test_value, :a, 42) end
      ...> {:ok, {answer, %PROVA{}}} = [
      ...>    mod: Map, call: [call1, call3, call2],
      ...>    value: %{a: 1}, result: 42,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      42

  Same example but the put for `:a` is `Keyword` format call specification:

      iex> call1 = [f: :delete, rest_args: :a] |> CRIDAR.new!
      ...> call2 = [f: :get, rest_args: [:a, :no_a_found]] |> CRIDAR.new!
      ...> call3 = [f: :put, rest_args: [:a, 99]]
      ...> {:ok, {answer, %PROVA{}}} = [
      ...>    mod: Map, call: [call1, call3, call2],
      ...>    value: %{a: 1}, result: 99,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      99

  These two examples show the use of the `:test_namer`.  Note it is applied to precreated *cridars* as well.

      iex> {:ok, {answer, %PROVA{}}} = [
      ...>    test_namer: fn :g -> :get end,
      ...>    module: Map, value: %{a: 42},
      ...>    args: [:a], call: [:g], result: 42,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      42

      iex> call1 = [f: :d, rest_args: :a] |> CRIDAR.new!
      ...> call3 = [f: :p, rest_args: [:a, 99]]
      ...> call2 = [f: :g, rest_args: [:a, :no_a_found]] |> CRIDAR.new!
      ...> test_namer = fn
      ...>   :g -> :get
      ...>   :p -> :put
      ...>   :d -> :delete
      ...> end
      ...> {:ok, {answer, %PROVA{}}} = [
      ...>    namer: test_namer,
      ...>    mod: Map, call: [call1, call3, call2],
      ...>    value: %{a: 1}, result: 99,
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      99

  The new few examples demonstrate the use of `:test_comp`. First, if
  the `:test_comp` is a function, it is treated as a `:test_result`
  function. Here is always returns `true`.

      iex> {:ok, {answer, %PROVA{}}} = [
      ...>    compare: fn _ -> true end,
      ...>    module: Map, value: %{a: 42},
      ...>    args: [:a], call: [:get],
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      42

  Otherwise the  `:test_comp` is used to create a *compare prova*.
  which is then, in turn, used to create a `:test_result` arity 2 function for the  *test prova*.

  When the *test prova's* `:test_result` fun/2 function is called, the first
  argument (the *test answer*) is used to set the *compare prova's* `:test_result` value
  (but only if not already set).

  Similarly the `:test_value` from the second argument (the *test prova*)
  is used to set the *compare prova's* `:test_value` (but
  again only if not already set).

  The *compare prova* is then tested and if it returns `true` according to the *extended truthy* definition,
  `{:ok, {test_answer, test_prova}}` is returned.

      iex> {:ok, {answer, %PROVA{}}} = [
      ...>    compare: [call: fn test_value -> test_value |> Map.get(:a) end],
      ...>    module: Map, value: %{a: 42},
      ...>    args: [:a], call: [:get],
      ...> ] |> PROVA.new! |> PROVA.test
      ...> answer
      42

      iex> {:error, error} = [
      ...>    compare: [call: fn test_value -> test_value |> Map.get(:b) end],
      ...>    module: Map, value: %{a: 42},
      ...>    args: [:a], call: [:get],
      ...> ] |> PROVA.new! |> PROVA.test
      ...> error |> Exception.message
      "result invalid, got: 42"

  """

  @since "0.1.0"

  @spec test(t) :: {:ok, {any, t}} | {:error, error}

  def test(%__MODULE__{} = prova) do
    with {:ok, test_flag} <-
           prova
           |> prova_test_flag_get(@harnais_runner_key_test_flag_default) do
      test_flag
      |> case do
        {:e, exception} ->
          prova |> test_expect_exception(exception)

        _flag ->
          prova |> test_expect_value
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp normalise_cridar_spec(
         %__MODULE__{
           @harnais_runner_key_test_namer => fun_namer,
           @harnais_runner_key_test_call => cridar_spec
         } = prova
       ) do
    with {:ok, test_cridars} <-
           cridar_spec |> Harnais.Runner.Cridar.Utility.cridar_spec_normalise() do
      fun_namer
      |> is_value_set
      |> case do
        true ->
          test_cridars
          |> map_collate0_enum(fn
            %CRIDAR{@harnais_runner_key_cridar_fun => test_fun} = cridar when is_atom(test_fun) ->
              test_fun
              |> fun_namer.()
              |> case do
                name when is_atom(name) -> cridar |> CRIDAR.update_fun(name)
                name -> new_error_result(m: "test fun name invalid", v: name)
              end

            %CRIDAR{} = cridar ->
              {:ok, cridar}
          end)

        _ ->
          {:ok, test_cridars}
      end
      |> case do
        {:error, %{__struct__: _}} = result -> result
        {:ok, test_cridars} -> prova |> update_test_cridars(test_cridars)
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end
end

defimpl Inspect, for: Harnais.Runner.Prova do
  use Harnais.Runner.Attribute

  import Plymio.Fontais.Guard,
    only: [
      is_value_unset: 1
    ]

  def inspect(
        %Harnais.Runner.Prova{
          @harnais_runner_key_test_flag => test_flag,
          @harnais_runner_key_test_call => test_call,
          @harnais_runner_key_test_cridars => test_cridars,
          @harnais_runner_key_test_args => test_args,
          @harnais_runner_key_test_value => test_value,
          @harnais_runner_key_test_result => test_result
        },
        _opts
      ) do
    # test_call_telltale = test_call |> inspect

    test_flag_telltale =
      test_flag
      |> case do
        x when is_value_unset(x) -> nil
        x -> "F=#{inspect(x)}"
      end

    test_call_telltale =
      test_call
      |> case do
        x when is_value_unset(x) -> nil
        x when is_function(x) -> "L=FUN/n"
        x -> "L=#{inspect(x)}"
      end

    test_cridars_telltale =
      test_cridars
      |> case do
        x when is_value_unset(x) -> nil
        x -> "C=#{inspect(x)}"
      end

    prova_args_telltale =
      test_args
      |> case do
        x when is_value_unset(x) -> nil
        x -> "A=#{inspect(x)}"
      end

    test_value_telltale =
      test_value
      |> case do
        x when is_value_unset(x) -> nil
        _ -> "+V"
      end

    test_result_telltale =
      test_result
      |> case do
        x when is_value_unset(x) -> nil
        x when is_function(x, 1) -> "R=FUN/1"
        x when is_function(x, 2) -> "R=FUN/2"
        x -> "R=#{inspect(x)}"
      end

    prova_telltale =
      [
        test_flag_telltale,
        prova_args_telltale,
        test_value_telltale,
        test_result_telltale,
        test_cridars_telltale,
        test_call_telltale
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.join("; ")

    "PROVA(#{prova_telltale})"
  end
end
