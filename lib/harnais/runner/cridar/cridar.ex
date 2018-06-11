defmodule Harnais.Runner.Cridar do
  @moduledoc ~S"""
  The *cridar* manages a test call.

  Each test call specification is used to create a *cridar*.

  See `Harnais.Runner` for the overview.

  ## Cridar State

  A *cridar* has the following fields:

  | Key | Aliases |
  | :---------------- | -------------------: |
  | `:module`   | *:m, :d, :mod, :test_mod, :test_module* |
  | `:fun`   | *:f, :function, :test_fun, :test_function* |
  | `:args`  | *:a, :test_args* |
  | `:rest_args`   | *:ra, :test_rest_args* |

  The default for all fields is *the unset value* (`Plymio.Fontais.Guard.the_unset_value/0`).

  ### Cridar Field: `:module`

  The `:module` holds the name of the module to be used in an MFA apply (`Kernel.apply/3`).

  ### Cridar Field: `:fun`

  The `:fun` can hold either an atom to be use in a MFA appply, or a function.

  ### Cridar Field: `:args`

  If set, the `:args` holds *all* of the argumemenst for a call.

  ### Cridar Field: `rest_args`

  If set, the `:rest_args` is intended to be used together with other arguments.

  """

  require Plymio.Codi, as: CODI
  require Plymio.Fontais.Option
  require Harnais.Runner.Utility.Macro, as: HUM
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
      is_value_set: 1
    ]

  import Plymio.Fontais.Option,
    only: [
      opts_create_aliases_dict: 1,
      opts_canonical_keys: 2
    ]

  @harnais_runner_cridar_kvs_aliases [
    @harnais_runner_alias_cridar_module,
    @harnais_runner_alias_cridar_fun,
    @harnais_runner_alias_cridar_args,
    @harnais_runner_alias_cridar_rest_args
  ]

  @harnais_runner_cridar_keys_aliases @harnais_runner_cridar_kvs_aliases |> Keyword.keys()

  @harnais_runner_cridar_dict_aliases @harnais_runner_cridar_kvs_aliases
                                      |> opts_create_aliases_dict

  @doc false
  def update_canonical_opts(opts, dict \\ @harnais_runner_cridar_dict_aliases) do
    opts |> opts_canonical_keys(dict)
  end

  @doc false
  def update_sorted_canonical_opts(opts, dict \\ @harnais_runner_cridar_dict_aliases) do
    with {:ok, opts} <- opts |> update_canonical_opts(dict) do
      {:ok, opts |> HUU.opts_sort_keys(@harnais_runner_cridar_keys_aliases)}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @harnais_runner_cridar_defstruct @harnais_runner_cridar_kvs_aliases
                                   |> Enum.map(fn {k, _v} ->
                                     {k, @plymio_fontais_the_unset_value}
                                   end)

  defstruct @harnais_runner_cridar_defstruct

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

  struct_accessor_spec_default = %{funs: [:get, :fetch, :put, :maybe_put]}

  struct_accessor_specs =
    @harnais_runner_cridar_defstruct
    |> Enum.map(fn {name, _} -> {name, struct_accessor_spec_default} end)

  [
    specs: struct_accessor_specs,
    namer: fn name, fun -> ["cridar_", name, "_", fun] |> Enum.join() |> String.to_atom() end
  ]
  |> HUM.custom_struct_accessors()

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
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_atom},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_cridar_module]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_passthru},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_cridar_fun]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_normalise_list},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_cridar_args]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch,
     [
       {@plymio_codi_key_proxy_name, :state_defp_update_proxy_field_normalise_list},
       {@plymio_fontais_key_forms_edit,
        [{@plymio_fontais_key_rename_atoms, [proxy_field: @harnais_runner_key_cridar_rest_args]}]}
     ]},
    {@plymio_codi_pattern_proxy_fetch, [:state_defp_update_field_unknown]}
  ]
  |> CODI.reify_codi(@codi_opts)

  @harnais_runner_cridar_defstruct_updaters @harnais_runner_cridar_defstruct

  for {name, _} <- @harnais_runner_cridar_defstruct_updaters do
    update_fun = "update_#{name}" |> String.to_atom()

    @doc false
    def unquote(update_fun)(%__MODULE__{} = state, value) do
      state |> update([{unquote(name), value}])
    end
  end

  @doc ~S"""
  `call/2` take a *cridar* and and optional *test value* and actions the test call
  returning `{:ok, {answer, cridar}` or `{error, error}`.

  ## Examples

  An MFA apply. Note the `:args` are "listified" (`List.wrap/1`):

      iex> {:ok, {answer, %CRIDAR{}}} = [
      ...>    mod: Map, fun: :get, args: [%{a: 42}, :a]
      ...> ] |> CRIDAR.new! |> CRIDAR.call
      ...> answer
      42

  Another MFA but the `:rest_args` is set. The "listified" `:rest_args` are prepended
  with the 2nd argument:

      iex> {:ok, {answer, %CRIDAR{}}} = [
      ...>    m: Map, f: :get, rest_args: [:a]
      ...> ] |> CRIDAR.new! |> CRIDAR.call(%{a: 42})
      ...> answer
      42

  The `:fun` is an arity 0 function so the `:args`,
  `:rest_args` and *test value* are ignored:

      iex> {:ok, {value, %CRIDAR{}}} = [
      ...>    f: fn -> 123 end, ra: [:a]
      ...> ] |> CRIDAR.new! |> CRIDAR.call(%{a: 42})
      ...> value
      123

  An arity 1 function is called just with the *test value*:

      iex> {:ok, {value, %CRIDAR{}}} = [
      ...>    fun: fn value -> value |> Map.fetch(:b) end,
      ...>    args: :will_be_ignored, rest_args: :will_be_ignored
      ...> ] |> CRIDAR.new! |> CRIDAR.call(%{b: 222})
      ...> value
      {:ok, 222}

  When `:fun` is any other arity, and the `:args` is set, it is
  called with the "vanilla" `:args`:

      iex> {:ok, {answer, %CRIDAR{}}} = [
      ...>    fun: fn _x,y,_z -> y end,
      ...>    a: [1,2,3], ra: :this_is_rest_args
      ...> ] |> CRIDAR.new! |> CRIDAR.call(%{b: 2})
      ...> answer
      2

  When `:fun` is any other arity, `:args` is not set but `:rest_args` is, it is
  called (`Kernel.apply/2`) with the *test value* and "listified" `:rest_args`:

      iex> {:ok, {answer, %CRIDAR{}}} = [
      ...>    fun: fn _p,_q,_r,s -> s end,
      ...>    rest_args: [:a,:b,:c]
      ...> ] |> CRIDAR.new! |> CRIDAR.call(:any_answer)
      ...> answer
      :c

  When `:fun` is any other arity, and neither `:args` not `:rest_args`
  is set, an error will be returned:

      iex> {:error, error} = [
      ...>    f: fn _p,_q,_r,s -> s end,
      ...> ] |> CRIDAR.new! |> CRIDAR.call(:any_value)
      ...> error |> Exception.message |> String.starts_with?("cridar invalid")
      true

  """

  @since "0.1.0"

  @spec call(t, any) :: {:ok, {any, t}} | {:error, error}

  def call(cridar, test_value \\ nil)

  def call(
        %__MODULE__{
          @harnais_runner_key_cridar_fun => fun,
          @harnais_runner_key_cridar_args => args,
          @harnais_runner_key_cridar_rest_args => rest_args
        } = cridar,
        test_value
      )
      when is_function(fun) do
    try do
      fun
      |> case do
        fun when is_function(fun, 0) ->
          {:ok, {apply(fun, []), cridar}}

        fun when is_function(fun, 1) ->
          {:ok, {apply(fun, [test_value]), cridar}}

        fun ->
          cond do
            is_value_set(args) ->
              {:ok, {apply(fun, args), cridar}}

            is_value_set(rest_args) ->
              {:ok, {apply(fun, [test_value | rest_args]), cridar}}

            true ->
              new_error_result(m: "cridar invalid", v: cridar)
          end
      end
    catch
      error -> {:error, error}
    end
  end

  def call(
        %__MODULE__{
          @harnais_runner_key_cridar_module => mod,
          @harnais_runner_key_cridar_fun => fun,
          @harnais_runner_key_cridar_args => args
        } = cridar,
        _test_value
      )
      when is_value_set(args) do
    try do
      {:ok, {apply(mod, fun, args), cridar}}
    rescue
      error -> {:error, error}
    end
  end

  def call(
        %__MODULE__{
          @harnais_runner_key_cridar_module => mod,
          @harnais_runner_key_cridar_fun => fun,
          @harnais_runner_key_cridar_rest_args => rest_args
        } = cridar,
        test_value
      )
      when is_value_set(rest_args) do
    try do
      {:ok, {apply(mod, fun, [test_value | rest_args |> List.wrap()]), cridar}}
    rescue
      error -> {:error, error}
    end
  end

  def call(cridar, _test_value) do
    new_error_result(m: "cridar invalid", v: cridar)
  end
end

defimpl Inspect, for: Harnais.Runner.Cridar do
  use Harnais.Runner.Attribute

  import Plymio.Fontais.Guard,
    only: [
      is_value_unset: 1
    ]

  def inspect(
        %Harnais.Runner.Cridar{
          @harnais_runner_key_cridar_module => cridar_mod,
          @harnais_runner_key_cridar_fun => cridar_fun,
          @harnais_runner_key_cridar_args => cridar_args,
          @harnais_runner_key_cridar_rest_args => cridar_rest_args
        },
        _opts
      ) do
    cridar_mod_telltale =
      cridar_mod
      |> case do
        x when is_value_unset(x) -> nil
        x -> "M=#{inspect(x)}"
      end

    cridar_fun_telltale =
      cridar_fun
      |> case do
        x when is_value_unset(x) -> nil
        x when is_function(x, 1) -> "F=FUN/1"
        x when is_function(x, 2) -> "F=FUN/2"
        x -> "F=#{inspect(x)}"
      end

    cridar_args_telltale =
      cridar_args
      |> case do
        x when is_value_unset(x) -> nil
        _ -> "+A"
      end

    cridar_rest_args_telltale =
      cridar_rest_args
      |> case do
        x when is_value_unset(x) -> nil
        x -> "RA=#{inspect(x)}"
      end

    cridar_telltale =
      [
        cridar_mod_telltale,
        cridar_fun_telltale,
        cridar_args_telltale,
        cridar_rest_args_telltale
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.join("; ")

    "CRIDAR(#{cridar_telltale})"
  end
end
