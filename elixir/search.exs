# search.exs
# Параллельный поиск подстрок из patterns.txt в большом тексте из input.txt
# Результат — список совпадений (подстрока: строка_текста) в output.txt

defmodule SubstringSearch do
  def main do
    {options, _, _} = OptionParser.parse(System.argv(), switches: [input: :string, patterns: :string, output: :string])

    input_path = Keyword.get(options, :input, "input.txt")
    patterns_path = Keyword.get(options, :patterns, "patterns.txt")
    output_path = Keyword.get(options, :output, "output.txt")

    text = File.read!(input_path)
    lines = String.split(text, "\n", trim: true)

    patterns = File.read!(patterns_path)
               |> String.split("\n", trim: true)

    # Параллельный поиск: каждая подстрока в отдельном процессе
    results = patterns
              |> Task.async_stream(fn pattern ->
                   matching_lines = lines
                                    |> Enum.filter(&String.contains?(&1, pattern))
                                    |> Enum.map(&"#{pattern}: #{&1}")

                   matching_lines
                 end, max_concurrency: length(patterns), timeout: :infinity)
              |> Enum.flat_map(fn {:ok, matches} -> matches end)
              |> Enum.sort()

    result_text = if Enum.empty?(results) do
      "Совпадений не найдено"
    else
      Enum.join(results, "\n")
    end

    File.write!(output_path, result_text)
    IO.puts("Поиск завершён. Найдено #{length(results)} совпадений → #{output_path}")
  end
end

SubstringSearch.main()