# Извлечение валидных e-mail адресов

## Постановка задачи
Программа ищет в тексте все валидные e-mail адреса, убирает дубликаты,  
приводит к нижнему регистру, сортирует и записывает по одному на строку в `output.txt`.

## Формат входных данных
- Файл `input.txt` — произвольный текст в кодировке UTF-8.

## Тестовые файлы
- `input1.txt` — простой текст с несколькими адресами
- `input2.txt` — повторяющиеся адреса в разном регистре
- `input3.txt` — большой текст для проверки производительности

## Запуск программы (Ruby)
```powershell
ruby extract_emails.rb                    # input.txt → output.txt
ruby extract_emails.rb -i input1.txt -o output1.txt
ruby extract_emails.rb --input input2.txt --output output2.txt