-- parse_logs.lua
-- Парсинг логов веб-сервера в формате Common Log Format
-- Вывод в читаемом виде с разделителями
-- Совместим с Lua 5.1 и новее

-- Ручной парсинг аргументов командной строки
local input_path = "input.txt"
local output_path = "output.txt"

for i = 1, #arg do
    if arg[i] == "-i" or arg[i] == "--input" then
        if i + 1 <= #arg then input_path = arg[i + 1] end
    elseif arg[i] == "-o" or arg[i] == "--output" then
        if i + 1 <= #arg then output_path = arg[i + 1] end
    end
end

-- Паттерн для разбора строки лога
local pattern = '^(%S+) %S+ (%S+) %[([^%]]+)%] "(%S+) (%S+) (%S+)" (%d+) (%S+)'

local output_file = assert(io.open(output_path, "w"))

for line in io.lines(input_path) do
    line = line:gsub("^%s+", ""):gsub("%s+$", "")  -- обрезаем пробелы по краям
    
    if line ~= "" then  -- пропускаем пустые строки
        local ip, user, time, method, path, protocol, status, size =
            line:match(pattern)

        if ip then
            -- Форматируем время: убираем часовой пояс и заменяем первое : на пробел
            local formatted_time = time:gsub("%+%d+$", ""):gsub(":", " ", 1)
            
            output_file:write(string.format("IP: %s\n", ip))
            output_file:write(string.format("Время: %s\n", formatted_time))
            output_file:write(string.format("Метод: %s\n", method))
            output_file:write(string.format("Путь: %s\n", path))
            output_file:write(string.format("Протокол: %s\n", protocol))
            output_file:write(string.format("Статус: %s\n", status))
            output_file:write(string.format("Размер: %s байт\n\n", size == "-" and "0" or size))
        else
            -- Если строка не подошла под формат
            output_file:write(string.format("НЕРАСПАРСЕНА СТРОКА: %s\n\n", line))
        end
    end
end

output_file:close()
print(string.format("Парсинг завершён. Результат записан в %s", output_path))