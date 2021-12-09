defmodule Evision.MixProject do
  use Mix.Project

  @app :evision
  @version "0.1.0-dev"
  @opencv_version "4.5.4"
  @source_url "https://github.com/cocoa-xu/evision/tree/#{@opencv_version}"

  def project do
    [
      app: @app,
      name: "Evision",
      version: @version,
      elixir: "~> 1.11-dev",
      deps: deps(),
      docs: docs(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      elixirc_paths: elixirc_paths(Mix.env()),
      source_url: "https://github.com/cocox-xu/evision",
      description: description(),
      package: package(),
      make_env: %{
        "OPENCV_VER" => @opencv_version,
        "MAKE_BUILD_FLAGS" => System.get_env("MAKE_BUILD_FLAGS", "-j#{System.schedulers_online()}"),
        "CMAKE_OPTIONS" => generate_cmake_options()
      }
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  def read_config do
    {
      [evision:
        [
        enabled_modules: enabled_modules,
        disabled_modules: disabled_modules,
        enabled_img_coder: enabled_img_coder
        ]
      ],
      _
    } = Config.Reader.read_imports!("config/config.exs")
    {enabled_modules, disabled_modules, enabled_img_coder}
  end

  defp elixirc_paths(_), do: ~w(lib)

  defp generate_cmake_options do
    {enabled_modules, disabled_modules, enabled_img_coder} = read_config()
    options = (enabled_modules
      |> Enum.map(&("-D BUILD_opencv_#{Atom.to_string(&1)}=ON"))
      |> Enum.join(" "))
    <> " " <> (disabled_modules
      |> Enum.map(&("-D BUILD_opencv_#{Atom.to_string(&1)}=OFF"))
      |> Enum.join(" "))
    <> " " <> (enabled_img_coder
      |> Enum.map(&("-D BUILD_#{Atom.to_string(&1) |> String.upcase}=ON"))
      |> Enum.join(" "))
    <> " "
    options
  end

  defp deps do
    [
      {:elixir_make, "~> 0.6"},
      {:ex_doc, "~> 0.23", only: :dev, runtime: false},
      {:nx, "~> 0.1.0-dev", github: "elixir-nx/nx", branch: "main", sparse: "nx"}
    ]
  end

  defp docs do
    [
      main: "OpenCV",
      source_ref: "v#{@version}",
      source_url: @source_url
    ]
  end

  defp description() do
    "OpenCV-Erlang/Elixir bindings."
  end

  defp package() do
    [
      name: "evision",
      # These are the default files included in the package
      files: ~w(lib c_src py_src nerves 3rd_party priv .formatter.exs mix.exs README* readme* LICENSE*
                license* CHANGELOG* changelog* src),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/cocoa-xu/evision"}
    ]
  end
end
