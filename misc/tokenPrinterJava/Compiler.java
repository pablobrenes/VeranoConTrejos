package Triangle;

import Triangle.SyntacticAnalyzer.Scanner;
import Triangle.SyntacticAnalyzer.SourceFile;
import Triangle.SyntacticAnalyzer.TokenPrinter;

public class Compiler {

    private static Scanner scanner;
    private static TokenPrinter tokenPrinter;

    private static void compileProgram (String sourceName) {

        SourceFile source = new SourceFile(sourceName);

        scanner  = new Scanner(source);
        tokenPrinter = new TokenPrinter(scanner);

        tokenPrinter.printTokens();

    }

    /**
     * Triangle compiler main program.
     *
     * @param	args	the only command-line argument to the program specifies
     *                  the source filename.
     */
    public static void main(String[] args) {

        if (args.length != 1) {
            System.exit(1);
        }

        String sourceName = args[0];
        compileProgram(sourceName);
    }
}
