defmodule Harnais.Runner.Correr do
  @moduledoc ~S"""
  The *correr* manages a complete test run with one or more tests ([*provas*](`Harnais.Runner.Prova`)).

  Each test runner create a *correr* to manage the run.

  See `Harnais.Runner` for the overview.

  ## Correr State

  A *correr* has the following fields:

  | Key | Aliases |
  | :---------------- | -------------------: |
  | `:test_type`   | *:type* |
  | `:test_runner` | *:runner* |
  | `:test_specs`  | *:t, :tests, :specs, :test_specifications* |
  | `:test_value`  | *:v, :value* |
  | `:test_module` | *:d, :module* |
  | `:test_namer`  | *:n, :namer* |
  | `:test_mapper` | *:m, :mapper* |
  | `:test_transform` | *:p, :transform* |
  | `:test_provas`  | |

  The default for all fields is *the unset value* (`Plymio.Fontais.Guard.the_unset_value/0`).

  ### Correr Field: `:test_type`

  The type of the run (e.g. `reduce`) derived from the *test runner* name (e.g. `run_tests_reduce_test_value/1`)

  ### Correr Field: `:test_runner`

  The default function to run each test. Each `:test_type` (e.g. `reduce`) has its own default but can be overridden.

  ### Correr Field: `:test_specs`

  The `:test_specs` passed to the *test_runner*.

  ### Correr Field: `:test_value`

  The default value passed as the first argument to the `:test_call` (see the [*prova*](`Harnais.Runner.Prova`)).

  ### Correr Field: `:test_module`

  The name of the default module to use in the `:test_call` (see the [*prova*](`Harnais.Runner.Prova`)).

  ### Correr Field: `:test_namer`

  The default test namer passed to each [*prova*](`Harnais.Runner.Prova`).

  ### Correr Field: `:test_mapper`

  See the explanation [here](https://hexdocs.pm/harnais_runner/Harnais.Runner.html#module-the-test-mapper).

  ### Correr Field: `:test_transform`

  See the explanation [here](https://hexdocs.pm/harnais_runner/Harnais.Runner.html#module-the-test-transform).

  ### Correr Field: `:test_provas`

  Each specification in the `:test_specs` is used to create a
  [*prova*](`Harnais.Runner.Prova`). The `:test_provas` holds the list
  of *provas*.
  """

  require Plymio.Codi, as: CODI
  require Plymio.Fontais.Option
  require Harnais.Runner.Utility.Macro, as: HUM
  alias Harnais.Runner.Correr.Utility, as: CORRERUTIL
  alias Harnais.Utility, as: HUU
  alias Harnais.Runner.Prova, as: PROVA
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
      is_value_set: 1
    ]

  import Plymio.Fontais.Option,
    only: [
      opts_create_aliases_dict: 1,
      opts_canonical_keys: 2,
      opts_normalise: 1,
      opts_validate: 1
    ],
    warn: false

  import Plymio.Funcio.Enum.Map.Collate,
    only: [
      map_concurrent_collate0_enum: 2
    ]

  # order can be important
  @harnais_runner_correr_kvs_aliases [
    @harnais_runner_alias_test_type,
    @harnais_runner_alias_test_runner,
    @harnais_runner_alias_test_value,
    @harnais_runner_alias_test_module,
    @harnais_runner_alias_test_namer,
    @harnais_runner_alias_test_mapper,
    @harnais_runner_alias_test_transform,
    @harnais_runner_alias_test_specs,
    @harnais_runner_alias_test_provas
  ]

  @harnais_runner_correr_keys_aliases @harnais_runner_correr_kvs_aliases |> Keyword.keys()

  @harnais_runner_correr_dict_aliases @harnais_runner_correr_kvs_aliases
                                      |> opts_create_aliases_dict

  @doc false
  def update_canonical_opts(opts, dict \\ @harnais_runner_correr_dict_aliases) do
    opts |> opts_canonical_keys(dict)
  end

  @doc false
  def update_sorted_canonical_opts(opts, dict \\ @harnais_runner_correr_dict_aliases) do
    with {:ok, opts} <- opts |> update_canonical_opts(dict) do
      {:ok, opts |> HUU.opts_sort_keys(@harnais_runner_correr_keys_aliases)}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @harnais_runner_correr_defstruct @harnais_runner_correr_kvs_aliases
                                   |> Enum.map(fn {k, _v} ->
                                     {k, @plymio_fontais_the_unset_value}
                                   end)

  defstruct @harnais_runner_correr_defstruct

  @type t :: %__MODULE__{}
  @type opts :: Harnais.opts()
  @type kv :: {any, any}
  @type error :: Harnais.error()

  @doc false
  HUM.def_struct_get()
  @doc false
  HUM.def_struct_fetch()
  @doc false
  HUM.def_struct_put()
  @doc false
  HUM.def_struct_delete()

  accessor_spec_default = %{funs: []}

  accessor_specs =
    [
      test_type: %{funs: [:fetch, :maybe_put, :put]},
      test_flag: nil,
      test_mapper: %{funs: [:fetch]},
      test_transform: %{funs: [:fetch]},
      test_runner: %{funs: [:fetch, :maybe_put]},
      test_module: %{funs: [:get]},
      test_namer: %{funs: [:get]},
      test_value: %{funs: [:get, :fetch, :put, :delete]},
      test_specs: %{funs: [:get, :fetch, :put, :delete]},
      test_provas: %{funs: [:get, :fetch, :put, :delete]},
      compare_module: %{funs: [:get]}
    ]
    |> Enum.map(fn
      {name, spec} when is_nil(spec) -> {name, accessor_spec_default}
      x -> x
    end)

  [
    specs: accessor_specs,
    namer: fn name, fun -> ["correr_", name, "_", fun] |> Enum.join() |> String.to_atom() end
  ]
  |> HUM.custom_struct_accessors()

  [
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
     ]}
  ]
  |> CODI.reify_codi(@codi_opts)

  defp update_field(%__MODULE__{} = state, {k, v})
       when k in [
              @harnais_runner_key_test_specs
            ] do
    state
    |> CORRERUTIL.correr_spec_normalise_kv({k, v})
    |> case do
      {:error, %{__exception__: true}} = result -> result
      {:ok, %__MODULE__{}} = result -> result
      %__MODULE__{} = state -> {:ok, state}
      value -> {:ok, state |> struct!([{k, value}])}
    end
  end

  [{@plymio_codi_pattern_proxy_fetch, :state_defp_update_field_passthru}]
  |> CODI.reify_codi(@codi_opts)

  @harnais_runner_correr_defstruct_updaters @harnais_runner_correr_defstruct

  for {name, _} <- @harnais_runner_correr_defstruct_updaters do
    update_fun = "update_#{name}" |> String.to_atom()

    @doc false
    def unquote(update_fun)(%__MODULE__{} = state, value) do
      state |> update([{unquote(name), value}])
    end
  end

  @doc ~S"""
  `run/1` takes a *correr* and runs each *prova*.

  ## Examples

  Tests for a *correr* look very similar to *test runner* ones
   e.g. [same](https://hexdocs.pm/harnais_runner/Harnais.Runner.html#module-test-runner-run_tests_same_test_value).

      iex> {:ok, {answer, %CORRER{}}} = [
      ...>    type: :same, runner: &Harnais.Runner.run_tests_same_test_runner/2,
      ...>    module: Map, value: %{a: 42},
      ...>    test_specs: [[args: [:a], call: [:get], result: 42]],
      ...> ] |> CORRER.new! |> CORRER.run
      ...> answer
      42

  """

  @since "0.1.0"

  @spec run(t) :: {:ok, {any, t}} | {:error, error}

  def run(state) do
    with {:ok, correr_test_runner} <- state |> correr_test_runner_fetch,
         {:ok, %__MODULE__{} = state} <- state |> correr_test_provas_complete,
         {:ok, collated_provas} <- state |> correr_test_provas_collate(),
         {:ok, test_value} <- state |> correr_test_value_get do
      collated_provas
      |> Enum.reduce(
        {test_value, [], []},
        fn test_provas, {test_value, oks, errors} ->
          prova_test_runner = fn prova ->
            prova
            |> correr_test_runner.(test_value)
          end

          test_provas
          |> run_provas(prova_test_runner)
          |> case do
            {:ok, {result, provas}} -> {result, [provas | oks], errors}
            {:error, %{__struct__: _}} = result -> {test_value, oks, [result | errors]}
          end
        end
      )
      |> case do
        # no errors
        {test_value, oks, []} ->
          test_provas = oks |> Enum.reverse() |> List.flatten()

          with {:ok, %__MODULE__{} = state} <- state |> correr_test_provas_put(test_provas) do
            {:ok, {test_value, state}}
          else
            {:error, %{__exception__: true}} = result -> result
          end

        {_test_value, _oks, errors} ->
          # return last error
          errors
          |> Enum.reverse()
          |> List.flatten()
          |> List.last()
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp correr_test_provas_complete(correr)

  defp correr_test_provas_complete(%__MODULE__{} = correr) do
    with {:ok, test_provas} <- correr |> correr_test_provas_fetch,
         {:ok, test_namer} <- correr |> correr_test_namer_get,
         {:ok, test_module} <- correr |> correr_test_module_get,
         true <- true do
      test_provas
      |> map_concurrent_collate0_enum(fn prova ->
        with {:ok, %PROVA{} = prova} <- prova |> PROVA.prova_test_module_maybe_put(test_module),
             {:ok, %PROVA{}} = result <- prova |> PROVA.prova_test_namer_maybe_put(test_namer),
             true <- true do
          result
        else
          {:error, %{__exception__: true}} = result -> result
        end
      end)
      |> case do
        {:error, %{__exception__: true}} = result ->
          result

        {:ok, test_provas} ->
          with {:ok, %__MODULE__{}} = result <- correr |> correr_test_provas_put(test_provas) do
            result
          else
            {:error, %{__exception__: true}} = result -> result
          end
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp correr_test_provas_collate(correr)

  defp correr_test_provas_collate(%__MODULE__{@harnais_runner_key_test_type => test_type} = state)
       when test_type in [
              @harnais_runner_type_default,
              @harnais_runner_type_same
            ] do
    with {:ok, test_provas} <- state |> correr_test_provas_get([]) do
      {:ok, [test_provas]}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp correr_test_provas_collate(%__MODULE__{@harnais_runner_key_test_type => test_type} = state)
       when test_type in [
              @harnais_runner_type_reduce
            ] do
    with {:ok, test_provas} <- state |> correr_test_provas_get([]) do
      test_provas
      |> Stream.cycle()
      |> Enum.reduce_while({test_provas, [], []}, fn
        # no more provas
        _, {[], current_chunk, all_chunks} ->
          chunks =
            [current_chunk | all_chunks]
            |> Enum.map(&Enum.reverse/1)
            |> Enum.reverse()

          {:halt, chunks}

        _, {provas, current_chunk, all_chunks} ->
          [%PROVA{@harnais_runner_key_test_flag => test_flag} = prova | rest_provas] = provas

          test_flag
          |> case do
            # close current chunk and start a new one
            :w ->
              {:cont, {rest_provas, [], [[prova | current_chunk] | all_chunks]}}

            # just add current prova to current chunk
            _ ->
              {:cont, {rest_provas, [prova | current_chunk], all_chunks}}
          end
      end)
      |> case do
        {:error, %{__exception__: true}} = result -> result
        collated_provas -> {:ok, collated_provas}
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp correr_test_provas_collate(%__MODULE__{} = state) do
    with {:ok, test_provas} <- state |> correr_test_provas_get([]) do
      test_provas
      |> Enum.map(&List.wrap/1)
      |> case do
        collated_provas -> {:ok, collated_provas}
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  defp run_provas(test_provas, test_runner)

  defp run_provas(test_provas, test_runner)
       when is_list(test_provas) and is_function(test_runner, 1) do
    with {:ok, results} <- test_provas |> map_concurrent_collate0_enum(test_runner),
         true <- true do
      results
      |> Enum.unzip()
      |> (fn {values, provas} ->
            {:ok, {values |> List.last(), provas}}
          end).()
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end
end

defimpl Inspect, for: Harnais.Runner.Correr do
  use Harnais.Runner.Attribute

  import Plymio.Fontais.Guard,
    only: [
      is_value_unset: 1
    ]

  def inspect(
        %Harnais.Runner.Correr{
          @harnais_runner_key_test_specs => test_specs
        },
        _opts
      ) do
    prova_telltale =
      test_specs
      |> case do
        x when is_value_unset(x) -> "P=X"
        x when is_nil(x) -> "P=NIL"
        x when is_list(x) -> "P=#{length(x)}"
        _ -> "P=?"
      end

    correr_telltale =
      [
        prova_telltale
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.join("; ")

    "CORRER(#{correr_telltale})"
  end
end
