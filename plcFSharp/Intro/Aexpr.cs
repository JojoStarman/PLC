using System;
using System.Collections.Generic;

public abstract class Expr
{
    public abstract int Eval(Dictionary<string, int> env);
    public abstract Expr Simplify();

}

public class CstI : Expr
{
    public int i;

    public CstI(int i)
    {
        this.i = i;
    }

    public override string ToString()
    {
        return i.ToString();
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return i;
    }

    public override Expr Simplify()
    {
        return new CstI(i);
    }
}

public class Var : Expr
{
    public string s;

    public Var(string s)
    {
        this.s = s;
    }

    public override string ToString()
    {
        return s;
    }

        public override int Eval(Dictionary<string, int> env)
    {
        return env[s];
    }

        public override Expr Simplify()
    {
        return new Var(s);
    }
}

public abstract class Binop : Expr
{
    public Expr e1;
    public Expr e2;

    public Binop(Expr e1, Expr e2)
    {
        this.e1 = e1;
        this.e2 = e2;
    }
}

public class Add : Binop
{
    public Add(Expr e1, Expr e2)
        : base(e1, e2)
    {
    }

    public override string ToString()
    {
        return "(" + e1.ToString() + " + " + e2.ToString() + ")";
    }

        public override int Eval(Dictionary<string, int> env)
    {
        return e1.Eval(env) + e2.Eval(env);
    }

    public override Expr Simplify()
    {
        Expr s1 = e1.Simplify();
        Expr s2 = e2.Simplify();

        if (s1 is CstI c1 && c1.i == 0)
            return s2;

        if (s2 is CstI c2 && c2.i == 0)
            return s1;

        return new Add(s1, s2);
    }
}

public class Mul : Binop
{
    public Mul(Expr e1, Expr e2)
        : base(e1, e2)
    {
    }

    public override string ToString()
    {
        return "(" + e1.ToString() + " * " + e2.ToString() + ")";
    }

        public override int Eval(Dictionary<string, int> env)
    {
        return e1.Eval(env) * e2.Eval(env);
    }

        public override Expr Simplify()
    {
        Expr s1 = e1.Simplify();
        Expr s2 = e2.Simplify();

        if (s1 is CstI c1 && c1.i == 1)
            return s2;

        if (s2 is CstI c2 && c2.i == 1)
            return s1;

        if (s1 is CstI c3 && c3.i == 0)
            return new CstI(0);

        if (s2 is CstI c4 && c4.i == 0)
            return new CstI(0);

        return new Mul(s1, s2);
    }
}

public class Sub : Binop
{
    public Sub(Expr e1, Expr e2)
        : base(e1, e2)
    {
    }

    public override string ToString()
    {
        return "(" + e1.ToString() + " - " + e2.ToString() + ")";
    }

        public override int Eval(Dictionary<string, int> env)
    {
        return e1.Eval(env) - e2.Eval(env);
    }

        public override Expr Simplify()
    {
        Expr s1 = e1.Simplify();
        Expr s2 = e2.Simplify();

        // x - x = 0
        if (s1.ToString() == s2.ToString())
            return new CstI(0);

        // x - 0 = x
        if (s2 is CstI c2 && c2.i == 0)
            return s1;

        return new Sub(s1, s2);
    }
}

public class Program
{
    public static void Main(string[] args)
    {
        var env = new Dictionary<string, int>
        {
            { "a", 3 },
            { "b", 111 },
            { "c", 78 }
        };

        Expr e1 = new CstI(17);

        Expr e2 = new Add(
            new CstI(3),
            new Var("a")
        );

        Expr e3 = new Add(
            new Mul(new Var("b"), new CstI(9)),
            new Var("a")
        );

        Console.WriteLine(e1);
        Console.WriteLine(e1.Eval(env));

        Console.WriteLine(e2);
        Console.WriteLine(e2.Eval(env));

        Console.WriteLine(e3);
        Console.WriteLine(e3.Eval(env));

        Console.WriteLine(e2.Simplify());
        Console.WriteLine(e3.Simplify());
    }
}
