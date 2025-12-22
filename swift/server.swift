import Foundation

// Ручной парсинг аргументов (порт из аргумента или 8080 по умолчанию)
let args = CommandLine.arguments
var port = 8080
if args.count > 1, let customPort = Int(args[1]) {
    port = customPort
}

print("Сервер запущен на порту \(port). Ожидает запросы...")

let server = try TCPInternetSocket(address: .any, port: Int32(port))
try server.bind()
try server.listen()

while true {
    let client = try server.accept()
    let requestData = try client.readAllAvailableData()
    
    guard let requestString = String(data: requestData, encoding: .utf8),
          let fileName = requestString.components(separatedBy: " ").dropFirst().first else {
        try client.write(string: "HTTP/1.1 400 Bad Request\r\n\r\n")
        client.close()
        continue
    }
    
    let cleanFileName = fileName.trimmingCharacters(in: .whitespacesAndNewlines)
    let filePath = FileManager.default.currentDirectoryPath + "/" + cleanFileName
    
    if FileManager.default.fileExists(atPath: filePath) {
        if let fileData = FileManager.default.contents(atPath: filePath) {
            let response = "HTTP/1.1 200 OK\r\nContent-Length: \(fileData.count)\r\n\r\n"
            try client.write(data: response.data(using: .utf8)!)
            try client.write(data: fileData)
        }
    } else {
        let response = "HTTP/1.1 404 Not Found\r\n\r\nFile not found"
        try client.write(string: response)
    }
    
    client.close()
}