defmodule Harnais.Runner.Attribute do
  @moduledoc false

  defmacro __using__(_opts \\ []) do
    quote do
      use Harnais.Attribute

      @harnais_runner_key_test_spec :test_spec
      @harnais_runner_key_test_comp :test_comp
      @harnais_runner_key_test_type :test_type
      @harnais_runner_key_test_flag :test_flag
      @harnais_runner_key_test_call :test_call
      @harnais_runner_key_test_mapper :test_mapper
      @harnais_runner_key_test_transform :test_transform
      @harnais_runner_key_test_runner :test_runner
      @harnais_runner_key_test_module :test_module
      @harnais_runner_key_test_namer :test_namer
      @harnais_runner_key_test_value :test_value
      @harnais_runner_key_test_args :test_args
      @harnais_runner_key_test_result :test_result
      @harnais_runner_key_test_specs :test_specs
      @harnais_runner_key_test_provas :test_provas
      @harnais_runner_key_test_cridars :test_cridars

      @harnais_runner_key_cridar_module :module
      @harnais_runner_key_cridar_fun :fun
      @harnais_runner_key_cridar_args :args
      @harnais_runner_key_cridar_rest_args :rest_args

      @harnais_runner_test_spec_tuple_key_order [
        @harnais_runner_key_test_flag,
        @harnais_runner_key_test_call,
        @harnais_runner_key_test_args,
        @harnais_runner_key_test_result,
        @harnais_runner_key_test_value
      ]

      @harnais_runner_key_test_flag_default :r

      @harnais_runner_key_run_test :run_test

      @harnais_runner_key_test_spec_normalise :test_spec_normalise

      @harnais_runner_alias_test_spec {@harnais_runner_key_test_spec, []}
      @harnais_runner_alias_test_comp {@harnais_runner_key_test_comp,
                                       [:compare, :comp, :test_compare]}
      @harnais_runner_alias_test_type {@harnais_runner_key_test_type, [:type]}
      @harnais_runner_alias_test_flag {@harnais_runner_key_test_flag, [:f, :flag]}
      @harnais_runner_alias_test_call {@harnais_runner_key_test_call, [:c, :call]}
      @harnais_runner_alias_test_mapper {@harnais_runner_key_test_mapper, [:m, :mapper]}
      @harnais_runner_alias_test_transform {@harnais_runner_key_test_transform, [:p, :transform]}
      @harnais_runner_alias_test_runner {@harnais_runner_key_test_runner, [:runner]}
      @harnais_runner_alias_test_module {@harnais_runner_key_test_module, [:d, :mod, :module]}
      @harnais_runner_alias_test_namer {@harnais_runner_key_test_namer, [:n, :namer]}
      @harnais_runner_alias_test_value {@harnais_runner_key_test_value, [:v, :value]}
      @harnais_runner_alias_test_args {@harnais_runner_key_test_args, [:a, :args]}
      @harnais_runner_alias_test_result {@harnais_runner_key_test_result, [:r, :result]}
      @harnais_runner_alias_test_specs {@harnais_runner_key_test_specs,
                                        [
                                          :t,
                                          :tests,
                                          :specs,
                                          :test_specifications,
                                          :specifications
                                        ]}
      @harnais_runner_alias_test_provas {@harnais_runner_key_test_provas, nil}
      @harnais_runner_alias_test_cridars {@harnais_runner_key_test_cridars, nil}

      @harnais_runner_alias_compare_module {@harnais_key_compare_module, []}

      @harnais_runner_alias_cridar_module {@harnais_runner_key_cridar_module,
                                           [:m, :d, :mod, :test_mod, :test_module]}
      @harnais_runner_alias_cridar_fun {@harnais_runner_key_cridar_fun,
                                        [:f, :function, :test_fun, :test_function]}
      @harnais_runner_alias_cridar_args {@harnais_runner_key_cridar_args, [:a, :test_args]}
      @harnais_runner_alias_cridar_rest_args {@harnais_runner_key_cridar_rest_args,
                                              [:ra, :test_rest_args]}

      @harnais_runner_type_default :default
      @harnais_runner_type_reduce :reduce
      @harnais_runner_type_same :same
    end
  end
end
