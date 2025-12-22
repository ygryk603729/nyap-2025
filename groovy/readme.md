# Конвертация XML → YAML (Groovy)

## Постановка задачи
Программа читает XML-файл (`input.txt`), преобразует его структуру в YAML  
и записывает красиво отформатированный YAML в `output.txt`.

## Формат входных данных
- Файл в кодировке UTF-8
- Валидный XML с вложенными элементами и атрибутами

## Тестовые файлы
- `input1.txt` — простой XML-книжный магазин
- `input2.txt` — XML с заказами и вложенными списками

## Запуск программы (Groovy)
```powershell
groovy xml_to_yaml.groovy                    # input.txt → output.txt
groovy xml_to_yaml.groovy -i input1.txt -o output1.txt
groovy xml_to_yaml.groovy --input input2.txt --output output2.txt