package Triangle.SyntacticAnalyzer;

public class TokenPrinter {

  private Scanner lexicalAnalyser;
  private Token currentToken;

  public TokenPrinter(Scanner lexer) {
    lexicalAnalyser = lexer;
  }

  public void printTokens() {
    currentToken = lexicalAnalyser.scan();

    while (currentToken.kind != Token.EOT) {
      System.out.print(currentToken.spelling);
      System.out.print('|');
      currentToken = lexicalAnalyser.scan();
    }
  }
}