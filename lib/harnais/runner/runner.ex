defmodule Harnais.Runner do
  @moduledoc ~S"""
  A harness for writing and running `ExUnit` tests

  ## Test Runners

  `Harnais.Runner` has 3 different *test runners*, distinguished by how they select the test value for each test.

  The arguments to a *test runner* are used to create the [*correr*](`Harnais.Runner.Correr`) which supervises the run.

  ### Test Runner - run_tests_default_test_value

  The test runner `Harnais.Runner.run_tests_default_test_value/1` uses the default `:test_value`,
  unless overridden by a test-specific value.

      iex> Harnais.Runner.run_tests_default_test_value(
      ...> # the default module to test
      ...> test_module: Map,
      ...> # the default test value
      ...> test_value: %{a: 1, b: 2, c: 3},
      ...> # the tests
      ...> test_specifications: [
      ...>  # using full key names
      ...>  [test_call: :get, test_args: [:c], test_result: 3],
      ...>  # using key aliases
      ...>  [call: :get, args: [:c], result: 3],
      ...>  [c: :get, a: [:c], r: 3],
      ...>  # with a test-specific test value (v is an alias of test_value)
      ...>  [c: :get, a: [:c], r: 42, v: %{c: 42}],
      ...>  # using list format - the first item is the test_flag (nil in this test)
      ...>  [nil, :put, [:d, 4], %{a: 1, b: 2, c: 3, d: 4}],
      ...>  [nil, :put, [:d, 4], %{d: 4}, %{}],
      ...>  # using tuple format - the first item is the test_flag (nil in next two tests)
      ...>  {nil, :put, [:d, 4], %{a: 1, b: 2, c: 3, d: 4}},
      ...>  {nil, :put, [:d, 4], %{d: 4}, %{}},
      ...>  # using map format with alias keys
      ...>  %{c: :put, a: [:d, 4], r: %{a: 1, b: 2, c: 3, d: 4}},
      ...>  %{c: :put, a: [:d, 4], r: %{d: 4}, v: %{}},
      ...>  # use a function rather that the test_module
      ...>  [c: fn test_value -> Kernel.map_size(test_value) + 2 end, r: 5],
      ...>  # use MFA format call rather that the test_module + test_args
      ...>  [c: {Kernel, :map_size, []}, r: 3],
      ...>  # MFA-like but args is a tuple => Tuple.to_list and *ignore* test_value
      ...>  [c: {Map, :get, {%{b: 2}, :b}}, v:  %{a: 1}, r: 2],
      ...>  [c: {Map, :put, {%{a: 1}, :b, 42}}, v:  %{a: 1}, r: %{a: 1, b: 42}],
      ...>  # a pipeline (Enum.reduce/3) of calls: e.g. MFA + fun + fun
      ...>  [c: [{Kernel, :map_size, []}, fn v -> v * v end, fn v -> v - 5 end], r: 4],
      ...>  # using a function to validate the result
      ...>  [c: :get, a: [:b], r: fn v -> v == 2 end, v: %{a: 1, b: 2, c: 3}],
      ...>  [c: :put, a: [:d, 4], r: fn m -> map_size(m) == 4 end, v: %{a: 1, b: 2, c: 3}],
      ...>  # catch some errors - the test_flag is set to a 2tuple {:e, ExceptionName}
      ...>  [f: {:e, BadMapError}, c: :get, a: [:x], r: nil, v: []],
      ...>  [f: {:e, BadMapError}, c: :put, a: [:x, 42], r: nil, v: nil],
      ...>  [f: {:e, UndefinedFunctionError}, c: :not_a_fun, a: [:x, 42], r: nil, v: nil],
      ...>  [f: {:e, FunctionClauseError}, c: {Kernel, :put_in, [:b, :b21]}, r: nil, v: %{b: 42}],
      ...> ])
      :ok

  ### Test Runner - run_tests_reduce_test_value

  Using the test runner `Harnais.Runner.run_tests_reduce_test_value/1` offers
  more control of the test value from test to test,  allowing the
  result of the last test to be set as the value for the next test: to
  do so requires the `:test_flag` to be set to *:w* ("write").

  Alternatively, the `:test_value` can be reinitialised by including an explicit
  value in the test specification.

      iex> Harnais.Runner.run_tests_reduce_test_value(
      ...> # the default module to test
      ...> test_module: Map,
      ...> # the default test value
      ...> value: %{a: 1, b: %{b21: 21, b22: 22}, c: 3},
      ...> t: [
      ...>  # get value of :b and make it the input for next test - note flag is :w
      ...>  {:w, :get, [:b], %{b21: 21, b22: 22}},
      ...>  # use the value of b for some tests
      ...>  {:r, :keys, [], [:b21, :b22]},
      ...>  {:r, :to_list, [], [b21: 21, b22: 22]},
      ...>  # update the test value to the list of values
      ...>  {:w, :values, [], [21, 22]},
      ...>  # now update the test value again, adding the two values
      ...>  {:w, fn [v1, v2] -> v1 + v2 end, [], 43},
      ...>  # confirm test_value now result of previous test
      ...>  [r: 43],
      ...>  # provide an explicit test_value to reinitialise test value and update again
      ...>  {:w, :get, [:d], 4, %{d: 4}},
      ...>  # apply a function
      ...>  [f: :w, c: fn v -> v * v end, r: 16],
      ...>  # confirmation again
      ...>  [r: 16],
      ...> ])
      :ok

  ### Test Runner - run_tests_same_test_value

  The final runner, `Harnais.Runner.run_tests_same_test_value/1`, **always** uses the
  `:test_value` given to the runner, ignoring any test-specific value
  (or *:w* flags).

      iex> Harnais.Runner.run_tests_same_test_value(
      ...> # the default module to test
      ...> d: Map,
      ...> # the default test value
      ...> v: %{a: 1, b: 2, c: 3},
      ...> t: [
      ...>  # always use the test_value above
      ...>  [c: :get, a: [:c], r: 3, v: %{c: 42}],
      ...>  {:w, :put, [:d, 4], %{a: 1, b: 2, c: 3, d: 4}, v: %{c: 42}},
      ...>  [c: fn v -> Kernel.map_size(v) + 2 end, r: 5, v: %{c: 42}],
      ...> ])
      :ok

  ## The Test Specification

  A test specification can be defined in a number of different
  formats: tuple, list, keyword and map.

  Each test specification is used to create a [*prova*](`Harnais.Runner.Prova`)

  ### Test Spec: tuple form

  4tuple and 5tuple forms are supported where the elements map to:

      {test_flag, test_call, test_args, test_result}
      {test_flag, test_call, test_args, test_result, test_value}

  ### Test Spec: list form

  The order of elements in the `List` form is the same as the tuple:

      [test_flag, test_call, test_args, test_result]
      [test_flag, test_call, test_args, test_result, test_value]

  ### Test Spec: keyword form

  The `Keyword` form is as expected, here using key aliases,  e.g.

      [c: :get, a: [:c], r: 3]

  ### Test Spec: map form

    Similarly the `Map` form e.g.

      %{call: :get, args: [:c], r: 3}

  ## The Test Call Specification

  A test call specification is used to create a [*cridar*](`Harnais.Runner.Cridar`).

  The valid forms of the `call spec` are:

  ### Test Call - `function name` (`Atom`)

    The name of a function (e.g. *:get*) to call in the default `test_module`

  ### Test Call - `function`

    An arity one function to be called with the `test_value`

  ### Test Call - `MFA`

    An MFA tuple ({module, function, args} where the args are a (maybe empty) list.

    The `test_value` will be added as first argument of the args.

  ### Test Call - `MFA-like` but args is a tuple

    In this case the args tuple is converted to a list
    (`Tuple.to_list/1`) to form *all* the arguments; the `test_value`
    is ignored.

  ### Test Call - `nil`

    `nil` implies just compare (assert) the test value is the same as the `test_result`.

  ## The Test Mapper

  Before each test specification in the `:test_specs` is used to create a
  [*prova*](`Harnais.Runner.Prova`), it can be mapped using a
  `:test_mapper` that must be zero, one or more functions.

  An arity one mapper is passed just the `test_spec`.

  Any arity two mapper is passed both the `test spec` and the `correr`.

  Each mapper is applied to the `test spec` in an `Enum.reduce/3` pipeline.

  > The last mapper must return one of the valid forms of a `test spec` (see the multi mapper example below).

  > Any mapper may return `nil` and cause the test to be discarded; the mapping pipeline is short circuited.

  After the mapper(s) have been applied,
  `Harnais.Runner.Prova.Utility.prova_spec_normalise/2` is called to
  e.g ensure `Map` form, keys are canonical, etc and then the `test spec`
  is passed to `Harnais.Runner.Prova.new/1`.

  The example just below is not specific to
  `Harnais.Runner.run_tests_default_test_value/1` but is intended to show how
  a mapper can be used to build the `Keyword` form of a `test spec`.

  In the example, the mapper finds the expected `test result`
  directly from the `test value` in the `correr` and builds the
  `test spec`. (*In essence the mapper "second guesses" the test.*)

      iex> Harnais.Runner.run_tests_default_test_value(
      ...> d: Map,
      ...> v: %{a: 1, b: 2, c: 3},
      ...> mapper: fn
      ...>   # 1st arg: the test spec
      ...>   {test_call, test_args},
      ...>   # 2nd arg: the correr
      ...>   %{test_value: test_value, test_module: test_module} = _correr ->
      ...>   test_result = apply(test_module, test_call, [test_value | test_args])
      ...>   # final test_spec
      ...>   [c: test_call, a: test_args, r: test_result]
      ...> end,
      ...> tests: [
      ...>  {:get, [:a]},
      ...>  {:put, [:d, 4]},
      ...>  {:has_key?, [:d]},
      ...> ])
      :ok

   This example is a variation of the above one but showing 3
   mappers with different arities. Note the second mapper can return `nil`
   when `:values` is passed to it causing the test to be discarded.

      iex> test_namer = fn name -> "#{name}" |> String.to_atom end
      ...> test_value = %{a: 1, b: 2, c: 3}
      ...> Harnais.Runner.run_tests_default_test_value(
      ...> d: Map,
      ...> v: test_value,
      ...> m: [
      ...> test_namer,
      ...> fn
      ...>   :values -> nil
      ...>   :get -> {:get, [:a]}
      ...>   :put -> {:put, [:d, 4]}
      ...>   test_call -> {test_call, []}
      ...> end,
      ...> fn
      ...>   # 1st arg: the test spec
      ...>   {test_call, test_args},
      ...>   # 2nd arg: the correr
      ...>   %{test_value: test_value} = _correr ->
      ...>   test_result = apply(Map, test_call, [test_value | test_args])
      ...>   [c: test_call, a: test_args, r: test_result]
      ...> end],
      ...> t: ["get", :put, "keys", :values])
      :ok

  ## The Test Transform

  The test transform works similarly to the *test_mapper* but *must*
  define the **complete** test specification transformation pipeline.

  The utility function `Harnais.Runner.Prova.Utility.prova_spec_normalise/2` can be
  used (called) in the pipeline to perform the basic normalisation.

  > The `test_transform` does NOT use the `test_mapper`.

  This example is a variation of the above one for the *test mapper* but
  showing the use of the *test_transform*.  (`:test_transform`
  has an alias `:p` - for `pipeline`.) Note the second mapper returns
  `nil` when `:values` is passed to it causing the test to be
  discarded.

      iex> test_namer = fn name -> "#{name}" |> String.to_atom end
      ...> test_value = %{a: 1, b: 2, c: 3}
      ...> Harnais.Runner.run_tests_default_test_value(
      ...> d: Map,
      ...> v: test_value,
      ...> # test_transform (alias p)
      ...> p: [
      ...> # transform 1: test_namer
      ...> test_namer,
      ...> # transform 2: initial expansion
      ...> fn
      ...>   :values -> nil
      ...>   :get -> [c: :get, a: [:a]]
      ...>   :put -> [c: :put, a: [:d, 4]]
      ...>   test_call -> [c: test_call, a: []]
      ...> end,
      ...> # transform 3: basic normalisation create a map with canonical keys
      ...> &Harnais.Runner.Prova.Utility.prova_spec_normalise/2,
      ...> fn
      ...>   # 1st arg: the test spec
      ...>   %{test_call: test_call, test_args: test_args} = test_spec,
      ...>   # 2nd arg: the correr
      ...>   %{test_value: test_value} ->
      ...>   test_result = apply(Map, test_call, [test_value | test_args])
      ...>   # note: put with canonical key names or call test_spec_normalise/3 again
      ...>   test_spec |> Map.put(:test_result, test_result)
      ...> end,
      ...> # transform 4: renormalise to ensure canon keys
      ...> &Harnais.Runner.Prova.Utility.prova_spec_normalise/2
      ...> ],
      ...> t: ["get", :put, "keys", :values])
      :ok

  """

  alias Harnais.Runner.Correr, as: CORRER
  alias Harnais.Runner.Prova, as: PROVA
  use Plymio.Codi.Attribute
  use Harnais.Runner.Attribute

  @doc false
  def run_tests_same_test_runner(%PROVA{} = prova, test_value) do
    # always use the provided test value?
    prova
    |> PROVA.prova_test_value_put(test_value)
    |> case do
      {:error, %{__exception__: true}} = result ->
        result

      {:ok, %PROVA{} = prova} ->
        with {:ok, {_, %PROVA{} = prova}} <- prova |> PROVA.test() do
          # always return the passed test_value
          {:ok, {test_value, prova}}
        else
          {:error, %{__exception__: true}} = result -> result
        end
    end
  end

  def run_tests_same_test_value(opts \\ []) do
    with {:ok, %CORRER{} = correr} <- opts |> CORRER.new(),
         {:ok, %CORRER{} = correr} <-
           correr
           |> CORRER.correr_test_runner_maybe_put(&run_tests_same_test_runner/2),
         {:ok, %CORRER{} = correr} <-
           correr
           |> CORRER.correr_test_type_put(@harnais_runner_type_same),
         {:ok, {_, %CORRER{}}} <- correr |> CORRER.run(),
         true <- true do
      :ok
    else
      {:error, error} -> raise error
    end
  end

  @doc false
  def run_tests_default_test_runner(%PROVA{} = prova, test_value) do
    # need a test value?
    prova
    |> PROVA.prova_test_value_maybe_put(test_value)
    |> case do
      {:error, %{__exception__: true}} = result ->
        result

      {:ok, %PROVA{} = prova} ->
        with {:ok, {_, %PROVA{} = prova}} <- prova |> PROVA.test() do
          # always return the passed test_value
          {:ok, {test_value, prova}}
        else
          {:error, %{__exception__: true}} = result -> result
        end
    end
  end

  def run_tests_default_test_value(opts \\ []) do
    with {:ok, %CORRER{} = correr} <- opts |> CORRER.new(),
         {:ok, %CORRER{} = correr} <-
           correr
           |> CORRER.correr_test_runner_maybe_put(&run_tests_default_test_runner/2),
         {:ok, %CORRER{} = correr} <-
           correr
           |> CORRER.correr_test_type_put(@harnais_runner_type_default),
         {:ok, {_, %CORRER{}}} <- correr |> CORRER.run(),
         true <- true do
      :ok
    else
      {:error, error} -> raise error
    end
  end

  @doc false
  def run_tests_reduce_test_runner(%PROVA{} = prova, test_value) do
    # need a test value?
    prova
    |> PROVA.prova_test_value_maybe_put(test_value)
    |> case do
      {:error, %{__exception__: true}} = result ->
        result

      {:ok, %PROVA{} = prova} ->
        with {:ok, {result, %PROVA{@harnais_runner_key_test_flag => test_flag} = prova}} <-
               prova |> PROVA.test() do
          case test_flag do
            :w ->
              # return the test's result
              {:ok, {result, prova}}

            _ ->
              # return the passed test_value
              {:ok, {test_value, prova}}
          end
        else
          {:error, %{__exception__: true}} = result -> result
        end
    end
  end

  def run_tests_reduce_test_value(opts \\ []) do
    with {:ok, %CORRER{} = correr} <- opts |> CORRER.new(),
         {:ok, %CORRER{} = correr} <-
           correr
           |> CORRER.correr_test_runner_maybe_put(&run_tests_reduce_test_runner/2),
         {:ok, %CORRER{} = correr} <-
           correr
           |> CORRER.correr_test_type_put(@harnais_runner_type_reduce),
         {:ok, {_, %CORRER{}}} <- correr |> CORRER.run(),
         true <- true do
      :ok
    else
      {:error, error} -> raise error
    end
  end
end
