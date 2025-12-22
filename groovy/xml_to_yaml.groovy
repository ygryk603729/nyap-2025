@Grab('org.yaml:snakeyaml:2.2')
import groovy.xml.XmlParser
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.DumperOptions

// Ручной парсинг аргументов
def inputPath = 'input.txt'
def outputPath = 'output.txt'

def i = 0
while (i < args.length) {
    def arg = args[i]
    if (arg in ['-i', '--input']) {
        i++
        if (i < args.length) inputPath = args[i]
    } else if (arg in ['-o', '--output']) {
        i++
        if (i < args.length) outputPath = args[i]
    }
    i++
}

// Парсинг XML
def parser = new XmlParser()
def root = parser.parse(new File(inputPath))

// Рекурсивная функция преобразования
def convertNode(node) {
    // Если это текстовый узел (String) — возвращаем текст
    if (node instanceof String) {
        return node.trim().isEmpty() ? null : node.trim()
    }

    // Если у узла нет дочерних элементов, но есть текст
    def text = node.text().trim()
    if (node.children().size() == 0 && !text.isEmpty()) {
        return text
    }

    def result = [:]

    // Атрибуты с префиксом @
    node.attributes().each { k, v ->
        result["@$k"] = v
    }

    // Группируем только настоящие элементы (GPathResult nodes)
    def elementChildren = node.children().findAll { it instanceof groovy.util.Node }

    if (elementChildren.isEmpty()) {
        return text.isEmpty() ? result : text
    }

    def grouped = elementChildren.groupBy { it.name() }

    grouped.each { name, children ->
        if (children.size() == 1) {
            result[name] = convertNode(children[0])
        } else {
            result[name] = children.collect { convertNode(it) }
        }
    }

    return result
}

// Формируем итоговый словарь с именем корневого элемента
def data = root ? [(root.name()): convertNode(root)] : [:]

// Настройка красивого YAML
def options = new DumperOptions()
options.defaultFlowStyle = DumperOptions.FlowStyle.BLOCK
options.prettyFlow = true
options.indent = 2

def yaml = new Yaml(options)
def yamlText = yaml.dump(data)

// Запись в файл
new File(outputPath).write(yamlText, 'UTF-8')

println "Конвертация XML в YAML завершена. Результат записан в $outputPath"