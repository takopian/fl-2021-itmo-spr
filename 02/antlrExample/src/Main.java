import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

public class Main {

    public static void main(String[] args) {
        ExprLexer lexer = new ExprLexer ( CharStreams.fromString ("1-2^3+4"));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        ExprParser parser = new ExprParser(tokens);
        ParseTree tree = parser.start();
        System.out.println(tree.toStringTree(parser));
        Evaluator evaluator = new Evaluator();
        System.out.println(evaluator.visit(tree));

    }
}
