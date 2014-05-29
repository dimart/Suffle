using Suffle.Interpreter;
using Parser;

using Side.Interfaces.Services;

namespace InterpreterSIDE
{
    public class Controller : IInterpreter
    {
        public string Interpret(string code)
        {
            var inter = new Suffle.Interpreter.Expression.Interpreter();
            return inter.EvaluateProgram(Suffle.Parser.parse(code));
        }
    }
}
