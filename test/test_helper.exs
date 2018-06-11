ExUnit.start()

defmodule HarnaisRunnerHelperTest do
  defmacro __using__(_opts \\ []) do
    quote do
      use ExUnit.Case, async: true
      use Harnais

      alias Harnais.Runner.Correr, as: CORRER
      alias Harnais.Runner.Prova, as: PROVA
      alias Harnais.Runner.Cridar, as: CRIDAR

      import Harnais.Error,
        only: [
          harnais_error_export_result: 1
        ]

      use Harnais.Attribute.Data
    end
  end
end
