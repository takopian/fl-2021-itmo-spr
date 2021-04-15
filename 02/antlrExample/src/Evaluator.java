import java.util.stream.Stream;

public class Evaluator extends ExprBaseVisitor<Integer> {
    @Override
    public Integer visitStart(ExprParser.StartContext ctx) {
        return visit(ctx.e());
    }

    @Override
    public Integer visitSum(ExprParser.SumContext ctx) {
        Integer left = visit(ctx.e(0));
        Integer right = visit(ctx.e(1));
        if (ctx.op.getType() == ExprParser.PLUS) {
            return left + right;
        } else {
            return left - right;
        }
    }

    @Override
    public Integer visitPow(ExprParser.PowContext ctx) {
        Integer left = visit(ctx.e(0));
        Integer right = visit(ctx.e(1));
        return (int) Math.pow(left, right);
    }

    @Override
    public Integer visitNum(ExprParser.NumContext ctx) {
        return Integer.parseInt(ctx.NUM().getText());
    }
}