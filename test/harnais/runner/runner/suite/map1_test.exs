defmodule HarnaisExampleSuiteMap1Test do
  use HarnaisRunnerHelperTest

  test "ex_runner_map1: [:b]" do
    test_specs = Harnais.Runner.Suite.Map.tests_get(:default)

    test_value = @harnais_state_deep

    Harnais.Runner.run_tests_default_test_value(
      d: Map,
      v: test_value,
      t: test_specs
    )
  end
end
