
module Types

// *** AST ***

type ASTNode() = class end

type Program() = class end

type Expression() = class end

type Declaration() = class end

type DValue() = class inherit Declaration() end
type DType() = class inherit Declaration() end
type DDatatype() = class inherit Declaration() end
type DFunction() = class inherit Declaration() end

type ELiteral() = class inherit Expression() end
type EIfThenElse() = class inherit Expression() end
type ELetIn() = class inherit Expression() end
type ECaseOf() = class inherit Expression() end
type ELambda() = class inherit Expression() end
type EBinary() = class inherit Expression() end
type EUnary() = class inherit Expression() end
type EFunApplying() = class inherit Expression() end

type BListCons() = class inherit EBinary() end
type BListConcat() = class inherit EBinary() end

type BAdd() = class inherit EBinary() end
type BSub() = class inherit EBinary() end
type BDiv() = class inherit EBinary() end
type BMult() = class inherit EBinary() end

type BAnd() = class inherit EBinary() end
type BOr() = class inherit EBinary() end
type BEquals() = class inherit EBinary() end
type BNonequals() = class inherit EBinary() end

type UNegation() = class inherit EUnary() end
type ULogicalNegation() = class inherit EUnary() end

// *** Suffle types ***

// !!! - TODO

type Type = TUnit
          | TBool
          | TChar 
          | TInt 
          | TFloat
          | TFn // !!!
          | TDatatype // !!!

// *** Suffle values ***

type Value = VUnit 
           | VBool of bool
           | VChar of char
           | VInt of int
           | VFloat of float
           | VCons // !!! ; means applied constructor of some datatype
           | VClosure // !!! 


