use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::sync::{Arc, Mutex};
use std::thread;

fn main() {
    // Путь к входному и выходному файлам (можно передать как аргументы, но для простоты фиксируем)
    let args: Vec<String> = env::args().collect();
    let input_path = if args.len() > 1 { &args[1] } else { "input3.txt" };
    let output_path = if args.len() > 2 { &args[2] } else { "output.txt" };

    // Читаем весь текст файла
    let content = fs::read_to_string(input_path).expect("Не удалось прочитать input.txt");

    // Разбиваем текст на слова: приводим к нижнему регистру, оставляем только буквы и цифры
    let words: Vec<String> = content
        .to_lowercase()
        .split_whitespace()
        .map(|s| {
            s.chars()
                .filter(|c| c.is_alphanumeric())
                .collect::<String>()
        })
        .filter(|s| !s.is_empty())
        .collect();

    if words.is_empty() {
        let mut file = fs::File::create(output_path).expect("Не удалось создать output.txt");
        return;
    }

    // Определяем количество потоков (по числу ядер CPU)
    let num_threads = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
        .min(words.len());

    let chunk_size = (words.len() + num_threads - 1) / num_threads;

    // Общий результат — Arc<Mutex<HashMap>> для безопасного доступа из потоков
    let frequency = Arc::new(Mutex::new(HashMap::new()));
    let mut handles = vec![];

    // Запускаем потоки
    for chunk in words.chunks(chunk_size) {
        let chunk_words = chunk.to_owned();
        let frequency_clone = Arc::clone(&frequency);

        let handle = thread::spawn(move || {
            let mut local_map: HashMap<String, u32> = HashMap::new();
            for word in chunk_words {
                *local_map.entry(word).or_insert(0) += 1;
            }
            // Блокируем мьютекс и сливаем локальный результат в общий
            let mut global = frequency_clone.lock().unwrap();
            for (word, count) in local_map {
                *global.entry(word).or_insert(0) += count;
            }
        });
        handles.push(handle);
    }

    // Ждём завершения всех потоков
    for handle in handles {
        handle.join().unwrap();
    }

    // Получаем финальный результат и сортируем по убыванию частоты
    let mut freq_vec: Vec<(String, u32)> = {
        let freq = frequency.lock().unwrap();
        freq.iter().map(|(k, v)| (k.clone(), *v)).collect()
    };

    freq_vec.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));

    // Записываем в output.txt
    let mut output_file = fs::File::create(output_path).expect("Не удалось создать output.txt");
    for (word, count) in freq_vec {
        writeln!(output_file, "{} {}", word, count).unwrap();
    }

    println!("Подсчёт завершён. Результат записан в {}", output_path);
}