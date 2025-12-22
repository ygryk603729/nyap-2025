import Foundation

// Ручной парсинг аргументов: -i input.txt -o output.txt --host localhost --port 8080
var inputPath = "input.txt"
var outputPath = "output.txt"
var host = "localhost"
var port = 8080

var i = 1
while i < CommandLine.arguments.count {
    let arg = CommandLine.arguments[i]
    if arg == "-i" || arg == "--input" {
        i += 1
        if i < CommandLine.arguments.count { inputPath = CommandLine.arguments[i] }
    } else if arg == "-o" || arg == "--output" {
        i += 1
        if i < CommandLine.arguments.count { outputPath = CommandLine.arguments[i] }
    } else if arg == "--host" {
        i += 1
        if i < CommandLine.arguments.count { host = CommandLine.arguments[i] }
    } else if arg == "--port" {
        i += 1
        if i < CommandLine.arguments.count, let p = Int(CommandLine.arguments[i]) { port = p }
    }
    i += 1
}

let fileName = (inputPath as NSString).lastPathComponent

let request = "GET /\(fileName) HTTP/1.1\r\nHost: \(host):\(port)\r\n\r\n"
let socket = try TCPInternetSocket(address: InternetAddress(hostname: host, port: Int32(port)))
try socket.connect()

try socket.write(string: request)

var responseData = Data()
while true {
    let chunk = try socket.read()
    if chunk.isEmpty { break }
    responseData.append(chunk)
}

if let responseString = String(data: responseData, encoding: .utf8),
   responseString.contains("200 OK") {
    let bodyStart = responseString.range(of: "\r\n\r\n")?.upperBound ?? responseString.endIndex
    let bodyData = responseData.subdata(in: bodyStart..<responseData.endIndex)
    try bodyData.write(to: URL(fileURLWithPath: outputPath))
    print("Файл успешно получен и записан в \(outputPath)")
} else {
    print("Ошибка получения файла: \(responseString ?? "пустой ответ")")
}

socket.close()