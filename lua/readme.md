# Парсинг логов сервера (Lua)

## Постановка задачи
Программа читает логи веб-сервера в формате Common Log Format из `input.txt`,  
разбирает каждую строку и записывает в `output.txt` в читаемом виде:
IP, Время, Метод, Путь, Протокол, Статус, Размер (с разделителями).

## Формат входных данных
- Одна запись лога на строку
- Формат: IP - user [time] "METHOD path protocol" status size

## Тестовые файлы
- `input1.txt` — простые запросы
- `input2.txt` — разные методы и статусы
- `input3.txt` — большой лог

## Запуск программы (Lua)
```powershell
lua parse_logs.lua                    # input.txt → output.txt
lua parse_logs.lua -i input1.txt -o output1.txt
lua parse_logs.lua --input input2.txt --output output2.txt