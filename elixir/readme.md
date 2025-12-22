# Параллельный поиск подстрок (Elixir)

## Постановка задачи
Программа ищет все строки из `input.txt`, содержащие подстроки из `patterns.txt`.  
Каждый поиск выполняется в отдельном процессе.  
Результат — список "подстрока: строка_текста" в `output.txt`.

## Тестовые файлы
- `input1.txt` + `patterns1.txt` — маленький тест
- `input2.txt` + `patterns2.txt` — средний

## Запуск программы
```powershell
elixir search.exs --input input1.txt --patterns patterns1.txt --output output1.txt
elixir search.exs   # по умолчанию input.txt + patterns.txt → output.txt