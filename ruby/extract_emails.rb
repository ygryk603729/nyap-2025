# extract_emails.rb
# Извлечение всех валидных e-mail адресов из текста
# Запись уникальных адресов (в нижнем регистре) по одному на строку в output.txt

require 'set'

# Простой парсинг аргументов командной строки
def parse_args
  input_path = 'input.txt'
  output_path = 'output.txt'

  i = 0
  while i < ARGV.length
    case ARGV[i]
    when '-i', '--input'
      i += 1
      input_path = ARGV[i] if i < ARGV.length
    when '-o', '--output'
      i += 1
      output_path = ARGV[i] if i < ARGV.length
    end
    i += 1
  end

  [input_path, output_path]
end

def main
  input_path, output_path = parse_args

  # Регулярное выражение для валидного e-mail (достаточно строгое, соответствует большинству реальных адресов)
  email_regex = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b/

  # Читаем весь файл
  text = File.read(input_path, encoding: 'UTF-8')

  # Находим все совпадения, приводим к нижнему регистру, убираем дубликаты через Set
  emails = text.scan(email_regex).map(&:downcase).to_set

  # Записываем в файл по одному на строку, отсортированные
  File.open(output_path, 'w', encoding: 'UTF-8') do |f|
    emails.to_a.sort.each { |email| f.puts email }
  end

  puts "Извлечено #{emails.size} уникальных e-mail адресов. Результат записан в #{output_path}"
end

main