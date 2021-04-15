import java.util.stream.Stream;

public class Evaluator extends ExprBaseVisitor<Integer> {
    @Override
    public Integer visitStart(ExprParser.StartContext ctx) {
        return visit(ctx.e());
    }

    @Override
    public Integer visitE(ExprParser.EContext ctx) {
        var subexpressions = ctx.f().stream().map(c -> visit(c));
        return subexpressions.reduce(0, (x, y) -> x + y);
    }

    @Override
    public Integer visitF(ExprParser.FContext ctx) {
        return Integer.parseInt(ctx.NUM().getText());
    }
}
