# csv_to_json.jl
# Конвертация простого CSV-файла в JSON (массив объектов)
# Работает без дополнительных пакетов кроме CSV и JSON

using CSV
using JSON

# Простой ручной парсинг аргументов (надёжный способ)
function get_args()
    input_path = "input.txt"
    output_path = "output.txt"
    
    i = 1
    while i <= length(ARGS)
        arg = ARGS[i]
        if arg in ["-i", "--input"]
            i += 1
            if i <= length(ARGS)
                input_path = ARGS[i]
            end
        elseif arg in ["-o", "--output"]
            i += 1
            if i <= length(ARGS)
                output_path = ARGS[i]
            end
        end
        i += 1
    end
    return input_path, output_path
end

function main()
    input_path, output_path = get_args()

    # Читаем CSV как rows (каждая строка — NamedTuple с заголовками)
    csv_rows = CSV.Rows(input_path; header = 1, types = String)  # сначала всё как строки

    records = []
    for row in csv_rows
        record = Dict{String, Any}()
        for name in propertynames(row)
            value_str = getproperty(row, name)
            # Пытаемся превратить в число или булево
            value = value_str
            if occursin(r"^-?\d+(\.\d+)?$", strip(value_str))
                value = parse(Float64, value_str)
            elseif strip(lowercase(value_str)) == "true"
                value = true
            elseif strip(lowercase(value_str)) == "false"
                value = false
            end
            record[string(name)] = value
        end
        push!(records, record)
    end

    if isempty(records)
        open(output_path, "w") do f
            write(f, "[]\n")
        end
        println("Пустой CSV → пустой JSON записан в $output_path")
        return
    end

    # Записываем pretty JSON
    json_string = JSON.json(records, 4)

    open(output_path, "w") do f
        write(f, json_string * "\n")
    end

    println("Конвертация завершена. Результат записан в $output_path")
end

main()