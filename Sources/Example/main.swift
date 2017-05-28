/*
   Copyright 2017 Ryuichi Saito, LLC and the Yanagiba project contributors

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

import Foundation
import Frontend
import AST
import Parser
import Source
import Diagnostic

struct ExampleDiagnosticConsumer : DiagnosticConsumer {
  func consume(diagnostics: [Diagnostic]) {
    for d in diagnostics {
      print("\(d.location) \(d.level): \(d.kind.diagnosticMessage)")
      print()
    }
  }
}

public class KnobsExpression : Expression {
  public let expr: Expression

  public init(expr: Expression) {
    self.expr = expr
  }

  public var textDescription: String {
    return "knobs: \(expr.textDescription)"
  }

  public var lexicalParent: ASTNode? = nil
  public var sourceRange: SourceRange = .INVALID
}

class MyParser : Parser {
  override func parseExpression(
    config: ParserExpressionConfig = ParserExpressionConfig()
  ) throws -> Expression {
    if case .identifier(let myKeyword) = _lexer.look().kind,
      myKeyword == "knobs",
      _lexer.look(ahead: 1).kind == .colon
    {
      _lexer.advance(by: 2)
      let expr = try super.parseExpression(config: config)
      return KnobsExpression(expr: expr)
    } else {
      return try super.parseExpression(config: config)
    }
  }
}

var filePaths = CommandLine.arguments
filePaths.remove(at: 0)

for filePath in filePaths {
  guard let sourceFile = try? SourceReader.read(at: filePath) else {
    print("Can't read file, please double check the file path is correct.")
    exit(-1)
  }
  let diagnosticConsumer = ExampleDiagnosticConsumer()
  let parser = MyParser(source: sourceFile)
  guard let topLevelDecl = try? parser.parse() else {
    DiagnosticPool.shared.report(withConsumer: diagnosticConsumer)
    print("Failed in parsing '\(filePath)'")
    exit(-2)
  }
  DiagnosticPool.shared.report(withConsumer: diagnosticConsumer)

  print(topLevelDecl.textDescription)
}
